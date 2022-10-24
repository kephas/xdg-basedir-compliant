{-# LANGUAGE DataKinds #-}

import           Data.Aeson                     ( decode )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Sum(..) )
import           Polysemy
import           Polysemy.Error
import           System.Environment             ( setEnv )
import qualified System.XDG                    as XDGIO
import           System.XDG.Env
import           System.XDG.Error
import           System.XDG.FileSystem
import           System.XDG.Internal
import           Test.Hspec


bareEnv :: EnvList
bareEnv = [("HOME", "/alice")]

fullEnv :: EnvList
fullEnv =
  [ ("HOME"           , "/alice")
  , ("XDG_CONFIG_HOME", "/config")
  , ("XDG_DATA_HOME"  , "/data")
  , ("XDG_STATE_HOME" , "/state")
  ]

userFiles :: FileList Integer
userFiles = [("/data/foo/bar", 1)]

sysFiles :: FileList Integer
sysFiles = [("/usr/local/share/foo/bar", 2), ("/usr/share/foo/bar", 3)]

allFiles :: FileList Integer
allFiles = userFiles ++ sysFiles

testXDG
  :: EnvList
  -> FileList Integer
  -> Sem '[Env , ReadFile Integer , Error XDGError] a
  -> Either XDGError a
testXDG env files = run . runError . runReadFileList files . runEnvList env

decodeInteger :: ByteString -> Sum Integer
decodeInteger = Sum . fromMaybe 0 . decode


main :: IO ()
main = hspec $ do
  describe "XDG" $ do
    describe "pure interpreter" $ do
      it "finds the data home" $ do
        testXDG bareEnv [] getDataHome `shouldBe` Right "/alice/.local/share"
        testXDG fullEnv [] getDataHome `shouldBe` Right "/data"
      it "finds the config home" $ do
        testXDG bareEnv [] getConfigHome `shouldBe` Right "/alice/.config"
        testXDG fullEnv [] getConfigHome `shouldBe` Right "/config"
      it "finds the state home" $ do
        testXDG bareEnv [] getStateHome `shouldBe` Right "/alice/.local/state"
        testXDG fullEnv [] getStateHome `shouldBe` Right "/state"
      it "opens a data file" $ do
        testXDG fullEnv allFiles (readDataFile "foo/bar") `shouldBe` Right 1
        testXDG fullEnv sysFiles (readDataFile "foo/bar") `shouldBe` Right 2
        testXDG fullEnv allFiles (readDataFile "foo/baz")
          `shouldBe` Left NoReadableFile
      it "merges data files" $ do
        testXDG fullEnv allFiles (readData show "foo/bar")
          `shouldBe` Right "123"
        testXDG fullEnv [] (readData show "foo/bar") `shouldBe` Right ""
    describe "IO interpreter" $ do
      it "merges data files" $ do
        setEnv "XDG_DATA_HOME" "./test/dir1"
        setEnv "XDG_DATA_DIRS" "./test/dir2:./test/dir3"
        XDGIO.readData decodeInteger "foo/bar.json" `shouldReturn` 3
        XDGIO.readData decodeInteger "foo/baz.json" `shouldReturn` 30

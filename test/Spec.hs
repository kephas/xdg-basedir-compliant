{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Maybe (
  fromJust,
  fromMaybe,
 )
import Data.Monoid (Sum (..))
import Data.String (IsString (..))
import Path hiding ((</>))
import Polysemy
import Polysemy.Error
import Polysemy.Operators
import System.Directory (getCurrentDirectory)
import System.Environment (setEnv)
import System.FilePath ((</>))
import qualified System.XDG as XDGIO
import System.XDG.Env
import System.XDG.Error
import System.XDG.FileSystem
import System.XDG.Internal
import Test.Hspec

instance IsString (Path Abs Dir) where
  fromString = fromMaybe undefined . parseAbsDir

instance IsString (Path Abs File) where
  fromString = fromMaybe undefined . parseAbsFile

bareEnv :: EnvList
bareEnv = [("HOME", "/alice")]

fullEnv :: EnvList
fullEnv =
  [ ("HOME", "/alice")
  , ("XDG_CONFIG_HOME", "/config")
  , ("XDG_DATA_HOME", "/data")
  , ("XDG_STATE_HOME", "/state")
  , ("XDG_CACHE_HOME", "/cache")
  , ("XDG_RUNTIME_DIR", "/runtime")
  ]

badEnv :: EnvList
badEnv =
  [ ("HOME", "/alice")
  , ("XDG_DATA_HOME", "data/")
  , ("XDG_DATA_DIRS", "/foo:./bar:/baz:foo/../bar")
  ]

userFiles :: FileList Integer
userFiles =
  [ ("/data/foo/bar", 1)
  , ("/config/foo/bar", 10)
  , ("/state/foo/bar", 100)
  , ("/cache/foo/bar", 200)
  , ("/runtime/foo/bar", 300)
  ]

sysFiles :: FileList Integer
sysFiles =
  [ ("/usr/local/share/foo/bar", 2)
  , ("/usr/share/foo/bar", 3)
  , ("/etc/xdg/foo/bar", 20)
  ]

allFiles :: FileList Integer
allFiles = userFiles ++ sysFiles

testXDG ::
  EnvList ->
  FileList Integer ->
  '[Env, ReadFile Integer, Error XDGError] @> a ->
  Either XDGError a
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
        testXDG badEnv [] getDataDirs
          `shouldBe` Right ["/alice/.local/share", "/foo", "/baz"]
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
        testXDG fullEnv [] (maybeRead $ readDataFile "foo/bar")
          `shouldBe` Right Nothing
      it "merges data files" $ do
        testXDG fullEnv allFiles (readData show "foo/bar")
          `shouldBe` Right "123"
        testXDG fullEnv [] (readData show "foo/bar") `shouldBe` Right ""
      it "opens a config file" $ do
        testXDG fullEnv userFiles (readConfigFile "foo/bar") `shouldBe` Right 10
        testXDG fullEnv sysFiles (readConfigFile "foo/bar") `shouldBe` Right 20
        testXDG fullEnv allFiles (readConfigFile "foo/baz")
          `shouldBe` Left NoReadableFile
      it "merges config files" $ do
        testXDG fullEnv allFiles (readConfig show "foo/bar")
          `shouldBe` Right "1020"
        testXDG fullEnv [] (readConfig show "foo/bar") `shouldBe` Right ""
      it "opens a state file" $ do
        testXDG fullEnv userFiles (readStateFile "foo/bar") `shouldBe` Right 100
      it "opens a cache file" $ do
        testXDG fullEnv userFiles (readCacheFile "foo/bar") `shouldBe` Right 200
      it "opens a runtime file" $ do
        testXDG fullEnv userFiles (readRuntimeFile "foo/bar")
          `shouldBe` Right 300
    describe "IO interpreter" $ do
      it "opens a data file" $ do
        cwd <- getCurrentDirectory
        setEnv "XDG_DATA_HOME" $ cwd </> "test/dir1"
        setEnv "XDG_DATA_DIRS" $
          intercalate ":" $
            map
              (cwd </>)
              ["test/dir2", "test/dir3"]
        (decodeInteger . fromJust)
          <$> XDGIO.readDataFile "foo/bar.json"
          `shouldReturn` 1
      it "merges data files" $ do
        XDGIO.readData decodeInteger "foo/bar.json" `shouldReturn` 3
        XDGIO.readData decodeInteger "foo/baz.json" `shouldReturn` 30

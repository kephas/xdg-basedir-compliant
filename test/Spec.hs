import           Polysemy
import           System.XDG
import           System.XDG.Env
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

main :: IO ()
main = hspec $ do
  describe "XDG" $ do
    it "finds the data home" $ do
      run (runEnvList bareEnv $ dataHome) `shouldBe` "/alice/.local/share"
      run (runEnvList fullEnv $ dataHome) `shouldBe` "/data"
    it "finds the config home" $ do
      run (runEnvList bareEnv $ configHome) `shouldBe` "/alice/.config"
      run (runEnvList fullEnv $ configHome) `shouldBe` "/config"
    it "finds the state home" $ do
      run (runEnvList bareEnv $ stateHome) `shouldBe` "/alice/.local/state"
      run (runEnvList fullEnv $ stateHome) `shouldBe` "/state"

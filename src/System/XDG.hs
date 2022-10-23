module System.XDG where

import           Data.Maybe                     ( fromMaybe )
import           Polysemy
import           System.FilePath                ( (</>) )
import           System.XDG.Env


dataHome :: Member Env r => Sem r FilePath
dataHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".local/share") <$> getEnv "XDG_DATA_HOME"

configHome :: Member Env r => Sem r FilePath
configHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".config") <$> getEnv "XDG_CONFIG_HOME"

stateHome :: Member Env r => Sem r FilePath
stateHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".local/state") <$> getEnv "XDG_STATE_HOME"


getDataHome :: IO FilePath
getDataHome = runM $ runEnvIO $ dataHome

getConfigHome :: IO FilePath
getConfigHome = runM $ runEnvIO $ configHome

getStateHome :: IO FilePath
getStateHome = runM $ runEnvIO $ stateHome

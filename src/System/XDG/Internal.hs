{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module System.XDG.Internal where

import           Data.Foldable                  ( fold )
import           Data.List.Split                ( endBy )
import           Data.Maybe                     ( fromMaybe )
import           Polysemy
import           Polysemy.Error
import           Polysemy.Operators
import           Prelude                 hiding ( readFile )
import           System.FilePath                ( (</>) )
import           System.XDG.Env
import           System.XDG.Error
import           System.XDG.FileSystem


getDataHome :: Env -@> FilePath
getDataHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".local/share") <$> getEnv "XDG_DATA_HOME"

getConfigHome :: Env -@> FilePath
getConfigHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".config") <$> getEnv "XDG_CONFIG_HOME"

getStateHome :: Env -@> FilePath
getStateHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".local/state") <$> getEnv "XDG_STATE_HOME"


getDataDirs :: Env -@> [FilePath]
getDataDirs = do
  dataHome <- getDataHome
  dirs     <-
    fromMaybe ["/usr/local/share/", "/usr/share/"]
    .   noEmpty
    .   fmap (endBy ":")
    <$> getEnv "XDG_DATA_DIRS"
  pure $ dataHome : dirs
 where
  noEmpty (Just []) = Nothing
  noEmpty x         = x


readDataFile :: FilePath -> '[Env , Error XDGError , ReadFile a] >@> a
readDataFile file = do
  dirs <- getDataDirs
  foldr tryOne (throw NoReadableFile) dirs
  where tryOne dir next = catch (readFile $ dir </> file) (const next)


readData
  :: Monoid b
  => (a -> b)
  -> FilePath
  -> '[Env , Error XDGError , ReadFile a] >@> b
readData parse file = do
  files <- map (</> file) <$> getDataDirs
  fold
    <$> traverse (\file -> catch (parse <$> readFile file) (pure mempty)) files


getConfigDirs :: Env -@> [FilePath]
getConfigDirs = do
  configHome <- getConfigHome
  dirs       <- endBy ":" . fromMaybe "/etc/xdg" <$> getEnv "XDG_CONFIG_DIRS"
  pure $ configHome : dirs

readConfigFile :: FilePath -> '[Env, Error XDGError, ReadFile a] >@> a
readConfigFile file = do
  dirs <- getConfigDirs
  foldr tryOne (throw NoReadableFile) dirs
  where tryOne dir next = catch (readFile $ dir </> file) (const next)

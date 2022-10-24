{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module System.XDG.Internal where

import           Data.Foldable                  ( fold )
import           Data.List.Split                ( endBy )
import           Data.Maybe                     ( fromMaybe )
import           Polysemy
import           Polysemy.Error
import           Prelude                 hiding ( readFile )
import           System.FilePath                ( (</>) )
import           System.XDG.Env
import           System.XDG.Error
import           System.XDG.FileSystem


getDataHome :: Member Env r => Sem r FilePath
getDataHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".local/share") <$> getEnv "XDG_DATA_HOME"

getConfigHome :: Member Env r => Sem r FilePath
getConfigHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".config") <$> getEnv "XDG_CONFIG_HOME"

getStateHome :: Member Env r => Sem r FilePath
getStateHome = do
  home <- fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME
  fromMaybe (home </> ".local/state") <$> getEnv "XDG_STATE_HOME"


getDataDirs :: Member Env r => Sem r [FilePath]
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


readDataFile
  :: Members '[Env , Error XDGError , ReadFile a] r => FilePath -> Sem r a
readDataFile file = do
  dirs <- getDataDirs
  foldr tryOne (throw NoReadableFile) dirs
  where tryOne dir next = catch (readFile $ dir </> file) (const next)


readData
  :: (Members '[Env , Error XDGError , ReadFile a] r, Monoid b)
  => (a -> b)
  -> FilePath
  -> Sem r b
readData parse file = do
  files <- map (</> file) <$> getDataDirs
  fold
    <$> traverse (\file -> catch (parse <$> readFile file) (pure mempty)) files

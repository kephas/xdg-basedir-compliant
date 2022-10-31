{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module System.XDG.Internal where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Foldable                  ( fold )
import           Data.List.Split                ( endBy )
import           Data.Maybe                     ( fromMaybe )
import           Polysemy
import           Polysemy.Error
import           Polysemy.Operators
import           Prelude                 hiding ( readFile )
import           System.FilePath                ( (</>) )
import qualified System.IO.Error               as IO
import           System.XDG.Env
import           System.XDG.Error
import           System.XDG.FileSystem


getDataHome :: Env -@> FilePath
getDataHome = getEnvHome "XDG_DATA_HOME" ".local/share"

getConfigHome :: Env -@> FilePath
getConfigHome = getEnvHome "XDG_CONFIG_HOME" ".config"

getStateHome :: Env -@> FilePath
getStateHome = getEnvHome "XDG_STATE_HOME" ".local/state"

getRuntimeDir :: '[Env, Error XDGError] >@> FilePath
getRuntimeDir = fromMaybe (throw $ MissingEnv env) getEnv env
  where env = "XDG_RUNTIME_DIR"

getDataDirs :: Env -@> [FilePath]
getDataDirs =
  getEnvDirs getDataHome "XDG_DATA_DIRS" ["/usr/local/share/", "/usr/share/"]

readDataFile :: FilePath -> '[Env , Error XDGError , ReadFile a] >@> a
readDataFile = readFileFromDirs getDataDirs

readData :: Monoid b => (a -> b) -> FilePath -> XDGReader a b
readData = appendEnvFiles getDataDirs

getConfigDirs :: Env -@> [FilePath]
getConfigDirs = getEnvDirs getConfigHome "XDG_CONFIG_DIRS" ["/etc/xdg"]

readConfigFile :: FilePath -> '[Env, Error XDGError, ReadFile a] >@> a
readConfigFile = readFileFromDirs getConfigDirs

readConfig :: Monoid b => (a -> b) -> FilePath -> XDGReader a b
readConfig = appendEnvFiles getConfigDirs

readStateFile :: FilePath -> XDGReader a a
readStateFile = readFileFromDir getStateHome


type XDGReader a b = '[Env , Error XDGError , ReadFile a] >@> b

getUserHome :: Env -@> FilePath
getUserHome = fromMaybe "" <$> getEnv "HOME" --TODO: throw error if no $HOME

getEnvHome :: String -> FilePath -> Env -@> FilePath
getEnvHome env defaultHome = do
  home <- getUserHome
  fromMaybe (home </> defaultHome) <$> getEnv env

getEnvDirs :: (Env -@> FilePath) -> String -> [String] -> Env -@> [FilePath]
getEnvDirs getHome env defaultDirs = do
  dirsHome <- getHome
  dirs     <- fromMaybe defaultDirs . noEmpty . fmap (endBy ":") <$> getEnv env
  pure $ dirsHome : dirs
 where
  noEmpty (Just []) = Nothing
  noEmpty x         = x

readFileFromDir :: Env -@> FilePath -> FilePath -> XDGReader a a
readFileFromDir getDir file = do
  dir <- getDir
  readFile $ dir </> file

readFileFromDirs
  :: Env -@> [FilePath]
  -> FilePath
  -> '[Env , Error XDGError , ReadFile a] >@> a
readFileFromDirs getDirs file = do
  dirs <- getDirs
  foldr tryOne (throw NoReadableFile) dirs
  where tryOne dir next = catch (readFile $ dir </> file) (const next)

appendEnvFiles
  :: Monoid b
  => Env -@> [FilePath]
  -> (a -> b)
  -> FilePath
  -> '[Env , Error XDGError , ReadFile a] >@> b
appendEnvFiles getDirs parse file = do
  files <- map (</> file) <$> getDirs
  fold
    <$> traverse (\file -> catch (parse <$> readFile file) (pure mempty)) files


readFileIO
  :: (FilePath -> '[Env, Error XDGError, ReadFile ByteString] >@> ByteString)
  -> FilePath
  -> IO ByteString
readFileIO reader file = do
  result <- runM $ runError $ runReadFileIO $ runEnvIO $ reader file
  either raiseIO pure result
 where
  raiseIO _error = IO.ioError $ IO.mkIOError IO.doesNotExistErrorType
                                             "System.XDG.readConfigFile"
                                             Nothing
                                             (Just file)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module System.XDG.Internal where

import qualified Control.Exception             as IO
import           Data.ByteString.Lazy           ( ByteString )
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


getDataHome :: XDGEnv FilePath
getDataHome = getEnvHome "XDG_DATA_HOME" ".local/share"

getConfigHome :: XDGEnv FilePath
getConfigHome = getEnvHome "XDG_CONFIG_HOME" ".config"

getStateHome :: XDGEnv FilePath
getStateHome = getEnvHome "XDG_STATE_HOME" ".local/state"

getCacheHome :: XDGEnv FilePath
getCacheHome = getEnvHome "XDG_CACHE_HOME" ".local/cache"

getRuntimeDir :: XDGEnv FilePath
getRuntimeDir = requireEnv "XDG_RUNTIME_DIR"

getDataDirs :: XDGEnv [FilePath]
getDataDirs =
  getEnvDirs getDataHome "XDG_DATA_DIRS" ["/usr/local/share/", "/usr/share/"]

readDataFile :: FilePath -> '[Env , Error XDGError , ReadFile a] >@> a
readDataFile = readFileFromDirs getDataDirs

readData :: Monoid b => (a -> b) -> FilePath -> XDGReader a b
readData = appendEnvFiles getDataDirs

getConfigDirs :: XDGEnv [FilePath]
getConfigDirs = getEnvDirs getConfigHome "XDG_CONFIG_DIRS" ["/etc/xdg"]

readConfigFile :: FilePath -> '[Env, Error XDGError, ReadFile a] >@> a
readConfigFile = readFileFromDirs getConfigDirs

readConfig :: Monoid b => (a -> b) -> FilePath -> XDGReader a b
readConfig = appendEnvFiles getConfigDirs

readStateFile :: FilePath -> XDGReader a a
readStateFile = readFileFromDir getStateHome

readCacheFile :: FilePath -> XDGReader a a
readCacheFile = readFileFromDir getCacheHome

readRuntimeFile :: FilePath -> XDGReader a a
readRuntimeFile = readFileFromDir getRuntimeDir


type XDGEnv a = '[Env , Error XDGError] >@> a

type XDGReader a b = '[Env , Error XDGError , ReadFile a] >@> b

requireEnv :: String -> XDGEnv String
requireEnv env = maybe (throw $ MissingEnv env) pure =<< getEnv env

getEnvHome :: String -> FilePath -> XDGEnv FilePath
getEnvHome env defaultDir = do
  home <- requireEnv "HOME"
  fromMaybe (home </> defaultDir) <$> getEnv env

getEnvDirs :: (XDGEnv FilePath) -> String -> [String] -> XDGEnv [FilePath]
getEnvDirs getUserDir env defaultDirs = do
  userDir <- getUserDir
  dirs    <- fromMaybe defaultDirs . noEmpty . fmap (endBy ":") <$> getEnv env
  pure $ userDir : dirs
 where
  noEmpty (Just []) = Nothing
  noEmpty x         = x

readFileFromDir :: XDGEnv FilePath -> FilePath -> XDGReader a a
readFileFromDir getDir file = do
  dir <- getDir
  readFile $ dir </> file

readFileFromDirs :: XDGEnv [FilePath] -> FilePath -> XDGReader a a
readFileFromDirs getDirs file = do
  dirs <- getDirs
  foldr tryOne (throw NoReadableFile) dirs
  where tryOne dir next = catch (readFile $ dir </> file) (const next)

appendEnvFiles
  :: Monoid b => XDGEnv [FilePath] -> (a -> b) -> FilePath -> XDGReader a b
appendEnvFiles getDirs parse file = do
  files <- map (</> file) <$> getDirs
  fold
    <$> traverse (\path -> catch (parse <$> readFile path) (pure mempty)) files

maybeRead :: XDGReader a a -> XDGReader a (Maybe a)
maybeRead action = catch
  (Just <$> action)
  (\case
    NoReadableFile -> pure Nothing
    err            -> throw err
  )


runXDGIO :: XDGReader ByteString a -> IO a
runXDGIO action = do
  result <- runM $ runError $ runReadFileIO $ runEnvIO action
  either IO.throwIO pure result

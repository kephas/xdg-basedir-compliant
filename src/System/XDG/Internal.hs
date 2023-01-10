{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module System.XDG.Internal where

import qualified Control.Exception as IO
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (fold)
import Data.List.Split (endBy)
import Data.Maybe (
  catMaybes,
  fromMaybe,
 )
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  mkRelDir,
  parseAbsDir,
  parseRelFile,
  (</>),
 )
import Polysemy
import Polysemy.Error
import Polysemy.Operators
import System.XDG.Env
import System.XDG.Error
import System.XDG.FileSystem
import Prelude hiding (readFile, writeFile)

type EnvAction a = '[Env, Error XDGError] >@> a

type ReadAction a b = '[Env, Error XDGError, ReadFile a] >@> b

type WriteAction a b = '[Env, Error XDGError, ReadFile a, WriteFile a] >@> b

getDataHome :: EnvAction (Path Abs Dir)
getDataHome = getEnvHome "XDG_DATA_HOME" $(mkRelDir ".local/share")

getConfigHome :: EnvAction (Path Abs Dir)
getConfigHome = getEnvHome "XDG_CONFIG_HOME" $(mkRelDir ".config")

getStateHome :: EnvAction (Path Abs Dir)
getStateHome = getEnvHome "XDG_STATE_HOME" $(mkRelDir ".local/state")

getCacheHome :: EnvAction (Path Abs Dir)
getCacheHome = getEnvHome "XDG_CACHE_HOME" $(mkRelDir ".local/cache")

getRuntimeDir :: EnvAction (Path Abs Dir)
getRuntimeDir = do
  dir <- requireEnv "XDG_RUNTIME_DIR"
  requireAbsDir dir

getDataDirs :: EnvAction [Path Abs Dir]
getDataDirs =
  getEnvDirs getDataHome "XDG_DATA_DIRS" ["/usr/local/share/", "/usr/share/"]

readDataFile :: FilePath -> ReadAction a a
readDataFile = readFileFromDirs getDataDirs

readData :: Monoid b => (a -> b) -> FilePath -> ReadAction a b
readData = appendEnvFiles getDataDirs

getConfigDirs :: EnvAction [Path Abs Dir]
getConfigDirs = getEnvDirs getConfigHome "XDG_CONFIG_DIRS" ["/etc/xdg"]

readConfigFile :: FilePath -> ReadAction a a
readConfigFile = readFileFromDirs getConfigDirs

readConfig :: Monoid b => (a -> b) -> FilePath -> ReadAction a b
readConfig = appendEnvFiles getConfigDirs

readStateFile :: FilePath -> ReadAction a a
readStateFile = readFileFromDir getStateHome

readCacheFile :: FilePath -> ReadAction a a
readCacheFile = readFileFromDir getCacheHome

readRuntimeFile :: FilePath -> ReadAction a a
readRuntimeFile = readFileFromDir getRuntimeDir

writeConfigFile :: FilePath -> a -> WriteAction a ()
writeConfigFile = writeFileToDir getConfigHome

writeDataFile :: FilePath -> a -> WriteAction a ()
writeDataFile = writeFileToDir getDataHome

writeCacheFile :: FilePath -> a -> WriteAction a ()
writeCacheFile = writeFileToDir getCacheHome

writeStateFile :: FilePath -> a -> WriteAction a ()
writeStateFile = writeFileToDir getStateHome

writeRuntimeFile :: FilePath -> a -> WriteAction a ()
writeRuntimeFile = writeFileToDir getRuntimeDir

-- Helpers

requireEnv :: String -> EnvAction String
requireEnv env = maybe (throw $ MissingEnv env) pure =<< getEnv env

requireAbsDir :: FilePath -> (Error XDGError) -@> Path Abs Dir
requireAbsDir path = maybe (throw $ InvalidPath path) pure $ parseAbsDir path

requireRelFile :: FilePath -> (Error XDGError) -@> Path Rel File
requireRelFile path = maybe (throw $ InvalidPath path) pure $ parseRelFile path

getEnvHome :: String -> Path Rel Dir -> EnvAction (Path Abs Dir)
getEnvHome env defaultDir = do
  dir <- (>>= parseAbsDir) <$> getEnv env
  maybe getDefault pure dir
 where
  getDefault = do
    home <- requireEnv "HOME"
    home' <- requireAbsDir home
    pure $ home' </> defaultDir

getEnvDirs ::
  (EnvAction (Path Abs Dir)) -> String -> [String] -> EnvAction [Path Abs Dir]
getEnvDirs getUserDir env defaultDirs = do
  userDir <- catch (Just <$> getUserDir) (const $ pure Nothing)
  dirs <- fromMaybe defaultDirs . noEmpty . fmap (endBy ":") <$> getEnv env
  pure $ catMaybes $ userDir : (map parseAbsDir dirs)
 where
  noEmpty (Just []) = Nothing
  noEmpty x = x

readFileFromDir :: EnvAction (Path Abs Dir) -> FilePath -> ReadAction a a
readFileFromDir getDir subPath = do
  subFile <- requireRelFile subPath
  dir <- getDir
  readFile $ dir </> subFile

readFileFromDirs :: EnvAction [Path Abs Dir] -> FilePath -> ReadAction a a
readFileFromDirs getDirs subPath = do
  subFile <- requireRelFile subPath
  let tryOne dir next = catch (readFile $ dir </> subFile) (const next)
  dirs <- getDirs
  foldr tryOne (throw NoReadableFile) dirs

appendEnvFiles ::
  Monoid b => EnvAction [Path Abs Dir] -> (a -> b) -> FilePath -> ReadAction a b
appendEnvFiles getDirs parse subPath = do
  subFile <- requireRelFile subPath
  files <- map (</> subFile) <$> getDirs
  fold
    <$> traverse (\path -> catch (parse <$> readFile path) (pure mempty)) files

maybeRead :: ReadAction a a -> ReadAction a (Maybe a)
maybeRead action =
  catch
    (Just <$> action)
    ( \case
        NoReadableFile -> pure Nothing
        err -> throw err
    )

writeFileToDir :: EnvAction (Path Abs Dir) -> FilePath -> a -> WriteAction a ()
writeFileToDir getDir subPath value = do
  subFile <- requireRelFile subPath
  dir <- getDir
  writeFile (dir </> subFile) value

runXDGIO :: WriteAction ByteString a -> IO a
runXDGIO action = do
  result <- runM $ runError $ runReadWriteFileIO $ runEnvIO action
  either IO.throwIO pure result

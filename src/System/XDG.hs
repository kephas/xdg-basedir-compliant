{-# LANGUAGE DataKinds #-}

module System.XDG where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Either                    ( fromRight )
import           Polysemy
import           Polysemy.Error                 ( runError )
import           Polysemy.Operators
import           System.XDG.Env
import           System.XDG.Error
import           System.XDG.FileSystem
import qualified System.XDG.Internal           as In


getDataHome :: IO FilePath
getDataHome = In.runXDGIO In.getDataHome

getConfigHome :: IO FilePath
getConfigHome = In.runXDGIO In.getConfigHome

getStateHome :: IO FilePath
getStateHome = In.runXDGIO In.getStateHome

getCacheHome :: IO FilePath
getCacheHome = In.runXDGIO In.getCacheHome

getRuntimeDir :: IO FilePath
getRuntimeDir = In.runXDGIO In.getRuntimeDir

getDataDirs :: IO [FilePath]
getDataDirs = In.runXDGIO In.getDataDirs

readDataFile :: FilePath -> IO (Maybe ByteString)
readDataFile file = In.runXDGIO $ In.maybeRead $ In.readDataFile file

readData :: Monoid b => (ByteString -> b) -> FilePath -> IO b
readData parse file = In.runXDGIO $ In.readData parse file

getConfigDirs :: IO [FilePath]
getConfigDirs = In.runXDGIO In.getConfigDirs

readConfigFile :: FilePath -> IO (Maybe ByteString)
readConfigFile file = In.runXDGIO $ In.maybeRead $ In.readConfigFile file

readCacheFile :: FilePath -> IO (Maybe ByteString)
readCacheFile file = In.runXDGIO $ In.maybeRead $ In.readCacheFile file

readStateFile :: FilePath -> IO (Maybe ByteString)
readStateFile file = In.runXDGIO $ In.maybeRead $ In.readStateFile file

readRuntimeFile :: FilePath -> IO (Maybe ByteString)
readRuntimeFile file = In.runXDGIO $ In.maybeRead $ In.readStateFile file



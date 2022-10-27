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

getDataDirs :: IO [FilePath]
getDataDirs = In.runXDGIO In.getDataDirs

readDataFile :: FilePath -> IO ByteString
readDataFile file = In.runXDGIO $ In.readDataFile file

readData :: Monoid b => (ByteString -> b) -> FilePath -> IO b
readData parse file = In.runXDGIO $ In.readData parse file

getConfigDirs :: IO [FilePath]
getConfigDirs = In.runXDGIO In.getConfigDirs

readConfigFile :: FilePath -> IO ByteString
readConfigFile file = In.runXDGIO $ In.readConfigFile file

readCacheFile :: FilePath -> IO ByteString
readCacheFile file = In.runXDGIO $ In.readCacheFile file

readStateFile :: FilePath -> IO ByteString
readStateFile file = In.runXDGIO $ In.readStateFile file



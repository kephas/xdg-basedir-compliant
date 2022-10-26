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
getDataHome = runM $ runEnvIO $ In.getDataHome

getConfigHome :: IO FilePath
getConfigHome = runM $ runEnvIO $ In.getConfigHome

getStateHome :: IO FilePath
getStateHome = runM $ runEnvIO $ In.getStateHome

getCacheHome :: IO FilePath
getCacheHome = runM $ runEnvIO $ In.getCacheHome

getDataDirs :: IO [FilePath]
getDataDirs = runM $ runEnvIO $ In.getDataDirs

readDataFile :: FilePath -> IO ByteString
readDataFile = In.readFileIO In.readDataFile

readData :: Monoid b => (ByteString -> b) -> FilePath -> IO b
readData parse file = do
  fromRight mempty
    <$> runM (runError $ runReadFileIO $ runEnvIO $ In.readData parse file)

getConfigDirs :: IO [FilePath]
getConfigDirs = runM $ runEnvIO $ In.getConfigDirs

readConfigFile :: FilePath -> IO ByteString
readConfigFile = In.readFileIO In.readConfigFile

readCacheFile :: FilePath -> IO ByteString
readCacheFile = In.readFileIO In.readCacheFile



module System.XDG where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Either                    ( fromRight )
import           Polysemy
import           Polysemy.Error                 ( runError )
import qualified System.IO.Error               as IO
import           System.XDG.Env
import           System.XDG.FileSystem
import qualified System.XDG.Internal           as In


getDataHome :: IO FilePath
getDataHome = runM $ runEnvIO $ In.getDataHome

getConfigHome :: IO FilePath
getConfigHome = runM $ runEnvIO $ In.getConfigHome

getStateHome :: IO FilePath
getStateHome = runM $ runEnvIO $ In.getStateHome

getDataDirs :: IO [FilePath]
getDataDirs = runM $ runEnvIO $ In.getDataDirs

readData :: Monoid b => (ByteString -> b) -> FilePath -> IO b
readData parse file = do
  fromRight mempty
    <$> runM (runError $ runReadFileIO $ runEnvIO $ In.readData parse file)

getConfigDirs :: IO [FilePath]
getConfigDirs = runM $ runEnvIO $ In.getConfigDirs

readConfigFile :: FilePath -> IO ByteString
readConfigFile file = do
  result <- runM $ runError $ runReadFileIO $ runEnvIO $ In.readConfigFile file
  either raiseIO pure result
 where
  raiseIO _error = IO.ioError $ IO.mkIOError IO.doesNotExistErrorType
                                             "System.XDG.readConfigFile"
                                             Nothing
                                             (Just file)

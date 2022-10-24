module System.XDG where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Either                    ( fromRight )
import           Polysemy
import           Polysemy.Error                 ( runError )
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


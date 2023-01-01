{-|
Module      : System.XDG
Description : XDG Basedir functions

These functions implement the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).


When an environment variable is missing that must be defined, they will
raise a `System.XDG.Error.MissingEnv` exception. This applies to @$HOME@ and @$XDG_RUNTIME_DIR@.

As per the specification, these functions will treat any relative path in the
environment variables as invalid. They will be skipped when operating on a list of
paths. Functions that return a single path will raise an `System.XDG.Error.InvalidPath` exception.
-}
module System.XDG
  (
  -- * Data
    getDataHome
  , getDataDirs
  , readDataFile
  , readData
  , writeDataFile
  -- * Config
  , getConfigHome
  , getConfigDirs
  , readConfigFile
  , readConfig
  , writeConfigFile
  -- * Cache
  , getCacheHome
  , readCacheFile
  , writeCacheFile
  -- * State
  , getStateHome
  , readStateFile
  , writeStateFile
  -- * Runtime
  -- | The specification says that when @$XDG_RUNTIME_DIR@ isn't set, an application should fall back to a replacement directory and warn users. To that end, `getRuntimeDir` will raise a `System.XDG.Error.MissingEnv` exception when @$XDG_RUNTIME_DIR@ isn't set. The application can then set it to useful value and then use `readRuntimeFile` and `writeRuntimeFile`.
  , getRuntimeDir
  , readRuntimeFile
  , writeRuntimeFile
  ) where

import           Data.ByteString.Lazy           ( ByteString )
import           Path                           ( fromAbsDir )
import qualified System.XDG.Internal           as In


{-| Returns the content of @$XDG_DATA_HOME@ or its default value. -}
getDataHome :: IO FilePath
getDataHome = fromAbsDir <$> In.runXDGIO In.getDataHome

{-| Returns the content of @$XDG_CONFIG_HOME@ or its default value. -}
getConfigHome :: IO FilePath
getConfigHome = fromAbsDir <$> In.runXDGIO In.getConfigHome

{-| Returns the content of @$XDG_STATE_HOME@ or its default value. -}
getStateHome :: IO FilePath
getStateHome = fromAbsDir <$> In.runXDGIO In.getStateHome

{-| Returns the content of @$XDG_CACHE_HOME@ or its default value. -}
getCacheHome :: IO FilePath
getCacheHome = fromAbsDir <$> In.runXDGIO In.getCacheHome

{-| Returns the content of @$XDG_RUNTIME_DIR@. -}
getRuntimeDir :: IO FilePath
getRuntimeDir = fromAbsDir <$> In.runXDGIO In.getRuntimeDir

{-| Returns the list of data dirs taken from @$XDG_DATA_HOME@ and
@$XDG_DATA_DIRS@ or their default values. -}
getDataDirs :: IO [FilePath]
getDataDirs = map fromAbsDir <$> In.runXDGIO In.getDataDirs

{-| Returns the content of the first readable file in the data dirs if there is one.
It will try the files in order of decreasing imporance.


To read @$XDG_DATA_DIRS\/subdir\/filename@:

@
> readDataFile "subdir/filename"
@
-}
readDataFile :: FilePath -> IO (Maybe ByteString)
readDataFile file = In.runXDGIO $ In.maybeRead $ In.readDataFile file

{-| Parse all readable data files into a monoid and append them.
The append operation will operate left to right in the order of decreasing importance. -}
readData :: Monoid b => (ByteString -> b) -> FilePath -> IO b
readData parse file = In.runXDGIO $ In.readData parse file

{-| Returns the list of config dirs taken from @$XDG_CONFIG_HOME@ and
@$XDG_CONFIG_DIRS@ or their default values. -}
getConfigDirs :: IO [FilePath]
getConfigDirs = map fromAbsDir <$> In.runXDGIO In.getConfigDirs

{-| Returns the content of the first readable file in the config dirs if there is one.
It will try the files in order of decreasing imporance.


To read @$XDG_CONFIG_DIRS\/subdir\/filename@:

@
> readConfigFile "subdir/filename"
@
-}
readConfigFile :: FilePath -> IO (Maybe ByteString)
readConfigFile file = In.runXDGIO $ In.maybeRead $ In.readConfigFile file

{-| Parse all readable config files into a monoid and append them.
The append operation will operate left to right in the order of decreasing importance. -}
readConfig :: Monoid b => (ByteString -> b) -> FilePath -> IO b
readConfig parse file = In.runXDGIO $ In.readConfig parse file

{-| Returns the content of the cache file if it exists.

@
> readCacheFile "subdir/filename"
@
-}
readCacheFile :: FilePath -> IO (Maybe ByteString)
readCacheFile file = In.runXDGIO $ In.maybeRead $ In.readCacheFile file

{-| Returns the content of the state file if it exists.

@
> readStateFile "subdir/filename"
@
-}
readStateFile :: FilePath -> IO (Maybe ByteString)
readStateFile file = In.runXDGIO $ In.maybeRead $ In.readStateFile file

{-| Returns the content of the runtime file if it exists.

@
> readRuntimeFile "subdir/filename"
@
-}
readRuntimeFile :: FilePath -> IO (Maybe ByteString)
readRuntimeFile file = In.runXDGIO $ In.maybeRead $ In.readStateFile file


{-| Writes a config file in the config home if it is writable.

@
> writeConfigFile "subdir/filename" $ BS.pack [1, 2, 3]
@
-}
writeConfigFile :: FilePath -> ByteString -> IO ()
writeConfigFile file content = In.runXDGIO $ In.writeConfigFile file content

{-| Writes a data file in the data home if it is writable.

@
> writeDataFile "subdir/filename" $ BS.pack [1, 2, 3]
@
-}
writeDataFile :: FilePath -> ByteString -> IO ()
writeDataFile file content = In.runXDGIO $ In.writeDataFile file content

{-| Writes a cache file in the cache home if it is writable.

@
> writeCacheFile "subdir/filename" $ BS.pack [1, 2, 3]
@
-}
writeCacheFile :: FilePath -> ByteString -> IO ()
writeCacheFile file content = In.runXDGIO $ In.writeCacheFile file content

{-| Writes a state file in the state home if it is writable.

@
> writeStateFile "subdir/filename" $ BS.pack [1, 2, 3]
@
-}
writeStateFile :: FilePath -> ByteString -> IO ()
writeStateFile file content = In.runXDGIO $ In.writeStateFile file content

{-| Writes a runtime file in the runtime dir if it is writable.

@
> writeRuntimeFile "subdir/filename" $ BS.pack [1, 2, 3]
@
-}
writeRuntimeFile :: FilePath -> ByteString -> IO ()
writeRuntimeFile file content = In.runXDGIO $ In.writeRuntimeFile file content

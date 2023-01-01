{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module System.XDG.FileSystem where

import qualified Control.Exception as IO
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Path
import Polysemy
import Polysemy.Error
import Polysemy.State (
  State,
  evalState,
  get,
  modify,
 )
import qualified System.IO.Error as IO
import System.XDG.Error

data ReadFile f m a where
  ReadFile :: Path Abs File -> ReadFile f m f

data WriteFile f m a where
  WriteFile :: Path Abs File -> f -> WriteFile f m ()

makeSem ''ReadFile
makeSem ''WriteFile

type FileList a = [(Path Abs File, a)]

type FileMap a = M.Map (Path Abs File) a

runReadWriteFileList ::
  Member (Error XDGError) r =>
  FileList a ->
  Sem (ReadFile a ': WriteFile a ': State (FileMap a) ': r) b ->
  Sem r b
runReadWriteFileList files =
  evalState (M.fromList files)
    . interpret (\(WriteFile path content) -> modify (M.insert path content))
    . interpret
      ( \(ReadFile path) -> do
          fileMap <- get
          maybe (throw $ FileNotFound path) pure $ M.lookup path fileMap
      )

runReadWriteFileIO ::
  Members '[Embed IO, Error XDGError] r =>
  Sem (ReadFile BS.ByteString ': WriteFile BS.ByteString ': r) a ->
  Sem r a
runReadWriteFileIO =
  interpret
    ( \(WriteFile path content) -> do
        result <- embed $ IO.tryJust (notFound path) $ BS.writeFile (toFilePath path) content
        either throw pure result
    )
    . interpret
      ( \(ReadFile path) -> do
          result <- embed $ IO.tryJust (notFound path) $ BS.readFile $ toFilePath path
          either throw pure result
      )
 where
  notFound :: Path Abs File -> IO.IOException -> Maybe XDGError
  notFound path e = if IO.isDoesNotExistError e then Just $ FileNotFound path else Nothing

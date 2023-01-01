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

runReadFileList ::
  Member (Error XDGError) r => FileList a -> InterpreterFor (ReadFile a) r
runReadFileList files =
  interpret
    ( \(ReadFile path) ->
        maybe (throw $ FileNotFound path) pure $ lookup path files
    )

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

runReadFileIO ::
  Members '[Embed IO, Error XDGError] r =>
  InterpreterFor (ReadFile BS.ByteString) r
runReadFileIO =
  interpret
    ( \(ReadFile path) -> do
        let notFound :: IO.IOException -> Maybe XDGError
            notFound e =
              if IO.isDoesNotExistError e then Just $ FileNotFound path else Nothing
        result <- embed $ IO.tryJust notFound $ BS.readFile $ toFilePath path
        either throw pure result
    )

{-# LANGUAGE TemplateHaskell, DataKinds #-}

module System.XDG.FileSystem where

import qualified Control.Exception             as IO
import qualified Data.ByteString.Lazy          as BS
import           Path
import           Polysemy
import           Polysemy.Error
import qualified System.IO.Error               as IO
import           System.XDG.Error


data ReadFile f m a where
  ReadFile ::Path Abs File -> ReadFile f m f

makeSem ''ReadFile


type FileList a = [(Path Abs File, a)]


runReadFileList
  :: Member (Error XDGError) r => FileList a -> InterpreterFor (ReadFile a) r
runReadFileList files = interpret
  (\(ReadFile path) ->
    maybe (throw $ FileNotFound path) pure $ lookup path files
  )

runReadFileIO
  :: Members '[Embed IO , Error XDGError] r
  => InterpreterFor (ReadFile BS.ByteString) r
runReadFileIO = interpret
  (\(ReadFile path) -> do
    let notFound :: IO.IOException -> Maybe XDGError
        notFound e =
          if IO.isDoesNotExistError e then Just $ FileNotFound path else Nothing
    result <- embed $ IO.tryJust notFound $ BS.readFile $ toFilePath path
    either throw pure result
  )

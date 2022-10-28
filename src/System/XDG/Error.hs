module System.XDG.Error where

import           Control.Exception
import           Path


data XDGError
  = FileNotFound (Path Abs File)
  | NoReadableFile
  | MissingEnv String
  | InvalidPath FilePath
  deriving (Eq, Show)

instance Exception XDGError


throwIOLeft :: Exception e => Either e a -> IO a
throwIOLeft = either throwIO pure

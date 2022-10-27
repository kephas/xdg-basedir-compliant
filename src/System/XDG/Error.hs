module System.XDG.Error where

import           Control.Exception


data XDGError
  = FileNotFound FilePath
  | NoReadableFile
  | MissingEnv String
  deriving (Eq, Show)

instance Exception XDGError


throwIOLeft :: Exception e => Either e a -> IO a
throwIOLeft = either throwIO pure

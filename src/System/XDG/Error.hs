module System.XDG.Error where

import           Control.Exception
import           Path


{-| The type of exceptions raised by this library. -}
data XDGError
  = FileNotFound (Path Abs File)
  | NoReadableFile
  {-| This exception is raised when an environment variable that should be defined is not. It is not raised when an optional environment variable is not defined. -}
  | MissingEnv String
  {-| This exception is raised when an invalid file path or a relative path is found instead of a valid absolute path. -}
  | InvalidPath FilePath
  deriving (Eq, Show)

instance Exception XDGError


throwIOLeft :: Exception e => Either e a -> IO a
throwIOLeft = either throwIO pure

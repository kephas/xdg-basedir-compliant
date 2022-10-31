module System.XDG.Error where


data XDGError
  = FileNotFound FilePath
  | NoReadableFile
  | MissingEnv String
  deriving (Eq, Show)

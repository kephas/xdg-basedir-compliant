module System.XDG.Error where


data XDGError
  = FileNotFound FilePath
  | NoReadableFile
  deriving (Eq, Show)

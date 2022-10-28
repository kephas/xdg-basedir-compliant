{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module System.XDG.Env where

import Polysemy
import System.Environment (lookupEnv)

data Env m a where
  GetEnv :: String -> Env m (Maybe String)

makeSem ''Env

type EnvList = [(String, String)]

runEnvList :: EnvList -> InterpreterFor Env r
runEnvList envs = interpret (\(GetEnv name) -> pure $ lookup name envs)

runEnvIO :: Member (Embed IO) r => InterpreterFor Env r
runEnvIO = interpret (\(GetEnv name) -> embed $ lookupEnv name)

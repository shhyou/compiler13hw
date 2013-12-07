module Language.BLang.Error (
  CompileError(..)
) where

import Control.Monad.Error

import Language.BLang.Data

class (Show a, Error a) => BLangError a where
  errorAt :: Line -> String -> a

data CompileError = CompileError { errLine :: Line, errMsg :: String }

instance Show CompileError where
  show (CompileError NoLineInfo msg) = show NoLineInfo ++ " " ++ msg
  show (CompileError line msg) =
    "At " ++ show line ++
    replicate (fromInteger $ colNo line - 1) ' ' ++ "^ " ++ msg ++ "\n"

instance Error CompileError where
  noMsg = strMsg "(unknown error)"
  strMsg msg = CompileError { errMsg = msg, errLine = NoLineInfo }

instance BLangError CompileError where
  errorAt line msg = CompileError { errMsg = msg, errLine = line }

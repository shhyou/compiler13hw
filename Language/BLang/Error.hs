module Language.BLang.Error (
  CompileError(..),
  errorAt,
  errorRanged
) where

import Control.Monad.Error

import Language.BLang.Data

class (Show a, Error a) => BLangError a where
  errorRanged :: Integer -> Line -> String -> a
  errorAt :: Line -> String -> a
  errorAt = errorRanged 0

data CompileError = CompileError { errLine :: Line, errStrLen :: Integer, errMsg :: String }

instance Show CompileError where
  show (CompileError NoLineInfo _ msg) = show NoLineInfo ++ " " ++ msg
  show (CompileError line len msg) =
    "At " ++ show line ++
    replicate (fromInteger $ colNo line - 1) ' ' ++
    replicate (fromInteger len) '~' ++ "^ " ++ msg ++ "\n"

instance Error CompileError where
  noMsg = strMsg "(unknown error)"
  strMsg msg = CompileError { errMsg = msg, errStrLen = 0, errLine = NoLineInfo }

instance BLangError CompileError where
  errorRanged len line msg = CompileError { errMsg = msg, errStrLen = len, errLine = line }

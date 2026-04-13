module Lib
    ( interpret
    ) where

import Data.Text (pack)

import Nat (fromNat)
import Parser (parseExpr)
import Eval (eval)

interpret :: String -> String
interpret input = case parseExpr (pack input) of
  Left err   -> "Parse error: " ++ show err
  Right expr -> show (fromNat (eval expr))

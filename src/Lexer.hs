{-# LANGUAGE OverloadedStrings #-}

module Lexer
    ( Parser
    , lexeme
    , symbol
    , skipSpace
    ) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, empty)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

skipSpace :: Parser ()
skipSpace = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: Text -> Parser Text
symbol = L.symbol skipSpace

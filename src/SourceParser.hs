{-# LANGUAGE OverloadedStrings #-}

module SourceParser
    ( moduleName
    ) where

import Data.Text (Text)
import Data.Char (isSpace)
import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT

data Mod a = Mod a [Mod a] deriving (Show, Eq)

moduleParser :: Parser Text
moduleParser = AT.skipSpace *> AT.string "module" *> AT.skipSpace *> AT.takeWhile (\c -> c /= '(' && not (isSpace c))

moduleName :: Text -> Either String Text
moduleName = AT.parseOnly moduleParser
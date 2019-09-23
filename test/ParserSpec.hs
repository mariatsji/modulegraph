{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module  ParserSpec where

import Test.Hspec
import Data.Text (Text)
import Data.Either
import NeatInterpolation
import SourceParser

testSource :: Text
testSource = 
  let modul' = "module" in
  [text|
    ${modul'} SourceParser
      ( someFunc
      ) where

    import Data.Text (Text)
    import Data.Char (isSpace)
    import Control.Applicative hiding ((<|>))
    import Data.Attoparsec.Text (Parser)
    import qualified Data.Attoparsec.Text as AT

    someFunc :: IO ()
    someFunc = putStrLn "someFunc"
  |]

spec :: Spec
spec = do
  describe "SourceParser" $ do
    it "parses a module name" $ do
      moduleName testSource `shouldSatisfy` isRight
      
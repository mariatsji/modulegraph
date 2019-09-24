{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Either
import Data.Text (Text)
import SourceParser
import System.Directory
import Test.Hspec

spec :: Spec
spec =
  describe "SourceParser"
    $ it "parses a module name"
    $ do
      dir <- getCurrentDirectory
      parseres <- analyzeModule (dir <> "/test/ParserSpec.hs")
      parseres `shouldSatisfy` isRight

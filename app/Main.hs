{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.List (isSuffixOf)
import Data.Maybe
import SourceParser
import DotCreator
import System.Directory
import System.Environment
import Language.Dot.Pretty
import Text.PrettyPrint (render)

main :: IO ()
main = do
  args <- getArgs
  let dir = fromMaybe "." (listToMaybe args) <> "/src"
  when (null args) (putStrLn "Please give this program an aboslute directory not ending in slash as an argument")
  fps <- findAllSourceFiles dir -- [Text]
  eithers <- mapM analyzeModule fps -- analyze :: IO (Either String (Mod Text)).. so -> IO [Either String Text]
  case sequence eithers of -- Either String [Text]
    Left l -> putStrLn $ "error when analyzing : " <> l
    Right mods ->
      print . render . prettyPrintDot $ toGraph dir mods
  

-- given an abolute path that does not end in /
findAllSourceFiles :: FilePath -> IO [FilePath]
findAllSourceFiles filepath = do
  l <- listDirectory filepath
  foldM
    ( \acc fp -> do
        isFile <- doesFileExist (filepath <> "/" <> fp)
        if isFile
          then pure (acc <> filter (isSuffixOf ".hs") [filepath <> "/" <> fp])
          else fmap (acc <>) (findAllSourceFiles (filepath <> "/" <> fp))
    )
    []
    l

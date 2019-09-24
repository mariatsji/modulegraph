module Main where

import Control.Monad
import Data.List (isSuffixOf)
import SourceParser
import System.Directory

main :: IO ()
main = putStrLn "yo"

-- given an abolute path that does not end in /
findAllSourceFiles :: FilePath -> IO [FilePath]
findAllSourceFiles filepath = do
  l <- listDirectory filepath
  foldM
    (\acc fp -> do
      isFile <- doesFileExist (filepath <> "/" <> fp)
      if isFile
        then pure (acc <> filter (isSuffixOf ".hs") [filepath <> "/" <> fp])
        else fmap (acc <>) (findAllSourceFiles (filepath <> "/" <> fp)))
    []
    l

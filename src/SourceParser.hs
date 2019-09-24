{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module SourceParser
    ( analyzeModule
    ) where

import Language.Haskell.Exts
import Data.Text (Text)
import qualified Data.Text as T

data Mod a = Mod a [a] deriving (Show, Eq)

analyzeModule :: FilePath -> IO (Either String (Mod Text))
analyzeModule fp = fmap parseMod (parseFile fp)
  
parseMod :: ParseResult (Module SrcSpanInfo) -> Either String (Mod Text)
parseMod (ParseOk (Module _ (Just (ModuleHead _ (ModuleName _ name') _ _)) _ imports _)) = Right (Mod (T.pack name') (parseImports imports))
parseMod (ParseFailed e _) = Left (show e)
parseMod x = Left $ "ParseOk but no module name found " <> show x

parseImports :: [ImportDecl SrcSpanInfo] -> [Text]
parseImports = fmap (modNameToText . importModule)

modNameToText :: ModuleName a -> Text
modNameToText (ModuleName _ name') = T.pack name'
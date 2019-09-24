{-# LANGUAGE OverloadedStrings #-}

module SourceParser
    ( analyzeModule
    ) where

import Language.Haskell.Exts
import Data.Text (Text)
import Types
import qualified Data.Text as T

analyzeModule :: FilePath -> IO (Either String (Mod Text))
analyzeModule fp = fmap parseMod (parseFile fp)
  
parseMod :: ParseResult (Module SrcSpanInfo) -> Either String (Mod Text)
parseMod (ParseOk (Module _ (Just (ModuleHead _ (ModuleName _ name') _ _)) _ imports _)) = Right (Mod (T.pack name') (parseImports imports))
parseMod (ParseOk (Module _ _ _ imports _)) = Right (Mod "<Module Name Missing>" (parseImports imports))
parseMod e = Left (show e)

parseImports :: [ImportDecl SrcSpanInfo] -> [Text]
parseImports = fmap (modNameToText . importModule)

modNameToText :: ModuleName a -> Text
modNameToText (ModuleName _ name') = T.pack name'

{--
ParseOk (Module srcSpans Nothing [] [ImportDecl {importAnn = SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 1 1 1 27, srcInfoPoints = [SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 1 1 1 7]}, importModule = ModuleName (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 1 8 1 27, srcInfoPoints = []}) "Distribution.Simple", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}] [PatBind (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 1 2 19, srcInfoPoints = []}) (PVar (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 1 2 5, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 1 2 5, srcInfoPoints = []}) "main")) (UnGuardedRhs (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 6 2 19, srcInfoPoints = [SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 6 2 7]}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 8 2 19, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 8 2 19, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "/Users/sjumilli/haskell/sjur/Setup.hs" 2 8 2 19, srcInfoPoints = []}) "defaultMain")))) Nothing])

--}
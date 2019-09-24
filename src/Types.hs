module Types where

import Data.Text (Text)
import qualified Data.Text as T

data Mod a = Mod a [a] deriving (Show, Eq)

modName :: Mod Text -> String
modName (Mod name _) = T.unpack name

modEdges :: Mod Text -> [String]
modEdges (Mod _ l) = T.unpack <$> l
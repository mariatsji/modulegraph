module DotCreator where

import Language.Dot.Syntax
import Data.Text (Text)
import Types

toGraph :: String -> [Mod Text] -> Graph
toGraph name mods = Graph StrictGraph DirectedGraph (Just (NameId name)) (mods >>= toStatements)

toStatements :: Mod Text -> [Statement]
toStatements mod' = [toNodeStatement mod'] <>
                   toEdgeStatements mod'

toNodeStatement :: Mod Text -> Statement
toNodeStatement mod' = NodeStatement (NodeId (NameId (modName mod')) Nothing) []

toEdgeStatements :: Mod Text -> [Statement]
toEdgeStatements mod' = fmap toNode (modEdges mod')

toNode :: String -> Statement
toNode name = EdgeStatement [ENodeId DirectedEdge (NodeId (NameId name) Nothing)] []

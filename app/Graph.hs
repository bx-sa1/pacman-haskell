module Graph where

import Constants
import Foreign.C
import SDL hiding (E)

data Node a = Node (V2 a) (Node a) (Node a) (Node a) (Node a) | Void deriving (Eq)

newtype Graph a = Graph [Node a] deriving (Eq)

nodePos :: Node a -> V2 a
nodePos (Node pos' _ _ _ _) = pos'
nodePos Void = undefined

nodeDistance :: (Floating a) => (Node a, Node a) -> a
nodeDistance (x, y) = distance (nodePos x) (nodePos y)

getAdjacent :: Node a -> Direction -> Node a
getAdjacent (Node _ n _ _ _) N = n
getAdjacent (Node _ _ e _ _) E = e
getAdjacent (Node _ _ _ s _) S = s
getAdjacent (Node _ _ _ _ w) W = w
getAdjacent Void _ = Void

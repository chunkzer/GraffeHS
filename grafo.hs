import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set


----------------- GRAPH

type Vertex = Int 
type Edge = (Vertex, Vertex)

newtype Graph = Graph {grNeighs :: Map Vertex (Set Vertex)} deriving (Show)

emptyGraph :: Graph
emptyGraph = Graph Map.empty

addVertex :: Vertex -> Graph -> Graph
addVertex v (Graph neighs) =  
    Graph $ case Map.lookup v neighs of
                Nothing -> Map.insert v Set.empty neighs
                Just _ -> neighs

addEdge :: Edge -> Graph -> Graph
addEdge (v1, v2) gr = Graph neighs
    where
        gr'    = addVertex v1 (addVertex v2 gr)
        neighs = Map.insert v1 (Set.insert v2 (vertexNeighs v1 gr')) $ 
                 Map.insert v2 (Set.insert v1 (vertexNeighs v2 gr')) $
                 grNeighs gr'

vertexNeighs :: Vertex -> Graph -> Set Vertex
vertexNeighs v (Graph neighs) = neighs Map.! v

graphEdges :: Graph -> Set Edge
graphEdges = Map.foldrWithKey' foldNeighs Set.empty . grNeighs
    where
        foldNeighs v1 ns es =
            Set.foldr' (\v2 -> Set.insert (order (v1, v2))) es ns
        order (v1, v2) = if v1 > v2 then (v1, v2) else (v2, v1)

sampleGraph :: [Edge]
sampleGraph = [(1,  30), (1,  40), (8,  46), (8,  16), (10, 25), (10, 19), (10, 33),
      (12, 8 ), (12, 36), (12, 17), (13, 38), (13, 24), (24, 49), (24, 13),
      (24, 47), (24, 12), (25, 27), (25, 12), (27, 12), (27, 14), (29, 10),
      (29, 8 ), (30, 24), (30, 44), (38, 29), (38, 35), (2,  42), (2,  35),
      (2,  11), (14, 18), (14, 24), (14, 38), (18, 49), (18, 47), (26, 41),
      (26, 42), (31, 39), (31, 47), (31, 25), (37, 26), (37, 16), (39, 50),
      (39, 14), (39, 18), (39, 47), (41, 31), (41, 8 ), (42, 44), (42, 29),
      (44, 37), (44, 32), (3,  20), (3,  28), (6,  45), (6,  28), (9,  6 ),
      (9,  16), (15, 16), (15, 48), (16, 50), (16, 32), (16, 39), (20, 33),
      (33, 9 ), (33, 46), (33, 48), (45, 15), (4,  17), (4,  15), (4,  12),
      (17, 21), (19, 35), (19, 15), (19, 43), (21, 19), (21, 50), (23, 36),
      (34, 23), (34, 24), (35, 34), (35, 16), (35, 18), (36, 46), (5,  7 ),
      (5,  36), (7,  32), (7,  11), (7,  14), (11, 40), (11, 50), (22, 46),
      (28, 43), (28, 8 ), (32, 28), (32, 39), (32, 42), (40, 22), (40, 47),
      (43, 11), (43, 17)]

littleGraph :: [Edge]
littleGraph = [(1,  2), (2,  3), (3,  2), (4,  1)]

fromEdges:: [Edge] -> [Graph -> Graph]
fromEdges toAdd = map (addEdge) toAdd

fromCurries :: [Graph -> Graph] -> Graph -> Graph
fromCurries [] gra = gra
fromCurries (x:xs) gra = fromCurries (xs) (x gra)

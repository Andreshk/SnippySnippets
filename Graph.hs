module Graph (Graph,makeGraph,makeGraphE,graphSize,neighbs,graphTests) where
import Data.Function (on)
import Data.List (sort,nub)
import Data.Vector (Vector,fromList,slice,(!))
import qualified Data.Vector as V (length)
import Test.HUnit (Test(TestList),(~:),(~?=))

-- A graph is represented via adjacency lists, laid out consecutively in memory
-- (i.e. in the first vector). The second Vector is used for indexing: the neighbours
-- of each vertex u are in the subrange [idxs!u; idxs!(u+1)) in the first vector.
-- The graph size can then be inferred from the index vector's size.
-- This internal invariant means graphs should always be constructed via one of
-- the functions below (and is the reason the Graph constructor isn't exported).
data Graph = Graph (Vector Int) (Vector Int)

-- Constructs a graph from an adjacency list
makeGraph :: [[Int]] -> Graph
makeGraph adjLst = Graph (fromList $ concat adjLst)
                         (fromList $ scanl (+) 0 (map length adjLst))
-- Constructs a graph from an edge list
makeGraphE :: [(Int,Int)] -> Graph
makeGraphE = makeGraph . toAdjLst

-- Converts an edge list to an adjacency list
toAdjLst :: [(Int,Int)] -> [[Int]]
toAdjLst edges = foldl addEdge (replicate n []) edges
  where -- The number of vertices is 1 greater than the highest vertex index
        n = succ $ maximum [ max u v | (u,v)<-edges ]
        addEdge :: [[Int]] -> (Int, Int) -> [[Int]]
        addEdge lst (u,v) = let (first,vs:rest) = splitAt u lst in first ++ ((v:vs):rest)

-- Vector length is O(1), as expected
graphSize :: Graph -> Int
graphSize (Graph _ idxs) = length idxs - 1
-- Obtaining the subrange for a given vertex is also a O(1) operation
neighbs :: Int -> Graph -> Vector Int
neighbs u g | u >= graphSize g = error "Invalid vertex index!"
neighbs u (Graph adjLst idxs) = slice from (to-from) adjLst
  where from = idxs!u; to = idxs!(u+1)

graphTests :: Test
graphTests = TestList [
    "Correct length" ~: graphSize testG ~?= length adjLst,
    "Correct neighbours lists" ~:
        all (\u -> neighbs u testG == fromList (adjLst !! u)) [0..length adjLst - 1] ~?= True,
    "Correct conversion" ~: toAdjLst edges `sameAs` adjLst ~?= True
    ]
  where adjLst = [[1,2,3],[0],[1,3],[]]
        edges = [(0,1),(0,2),(0,3),(1,0),(2,1),(2,3)]
        testG = makeGraph adjLst
        -- Ordering or duplications do not matter for adjacency lists
        -- in general, only when comparing them
        sameAs = (==) `on` (map $ sort . nub)

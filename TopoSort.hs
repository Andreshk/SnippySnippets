module TopoSort (Graph,topoSort) where
import Control.Monad.State (StateT,execStateT,get,modify,lift)
import Data.Foldable (for_)
import Data.Vector (Vector,fromList,(!),(//),slice)
import qualified Data.Vector as V (length,replicate,elem)
import Test.HUnit (Test(TestList),runTestTT,(~:),(~?=))

-- A graph is represented via adjacency lists, laid out consecutively in memory
-- (i.e. in the first vector). The second Vector is used for indexing: the neighbours
-- of each vertex u are in the subrange [idxs!u; idxs!(u+1)) in the first vector.
-- The graph size can then be inferred from the index vector's size.
data Graph = Graph (Vector Int) (Vector Int)
makeGraph :: [[Int]] -> Graph
makeGraph adjLst = Graph (fromList $ concat adjLst)
                         (fromList $ scanl (\len l -> len + length l) 0 adjLst)

-- A graph with an unique topological ordering: 2 1 5 0 3 4
-- It is unique due to it being a Hamiltonian path, i.e. no two vertices can be swapped.
g :: Graph
g = makeGraph
    [[3,4]
    ,[0,3,5]
    ,[1,5]
    ,[4]
    ,[]
    ,[0,4]]
-- Minimal graph with a cycle
g' :: Graph
g' = makeGraph [[1],[0]]

-- Vector length is O(1), as expected
graphSize :: Graph -> Int
graphSize (Graph _ idxs) = length idxs - 1
-- Obtaining the subrange for a given vertex is also a O(1) operation
neighbs :: Int -> Graph -> Vector Int
neighbs u g | u >= graphSize g = error "Invalid vertex index!"
neighbs u (Graph adjLst idxs) = slice uIdx (vIdx - uIdx) adjLst
  where uIdx = idxs!u; vIdx = idxs!(u+1)

data Color = White | Gray | Black

update :: Int -> a -> Vector a -> Vector a
update idx val v = v // [(idx, val)]

-- The most accurate representation of this process is StateT (<the actual state>) Maybe,
-- i.e. a function that receives its arguments + state & may fail returning the value-new state pair.
-- We also get MonadFail for free - this gives us the ability to pattern-match the result from get
-- and the possibility for this match to fail if an earlier call returned Nothing.
topoSort :: Graph -> Maybe (Vector Int)
topoSort g = getSnd <$> execStateT (for_ [0..n-1] tryDFSVisit)
                                   (V.replicate n White, V.replicate n 0, n-1)
  where n = graphSize g
        getSnd (_,x,_) = x
        tryDFSVisit :: Int -> StateT (Vector Color, Vector Int, Int) Maybe ()
        tryDFSVisit u = do
            (colors, _, _) <- get
            case colors ! u of White -> dfsVisit u
                               Gray -> error "This shouldn't happen"
                               Black -> return ()
        dfsVisit :: Int -> StateT (Vector Color, Vector Int, Int) Maybe ()
        dfsVisit u = do
            modify $ \(colors, res, idx) -> (update u Gray colors, res, idx)
            for_ (neighbs u g) $ \v -> do
                (colors, _, _) <- get
                case colors ! v of White -> dfsVisit v
                                   Gray -> lift Nothing -- cycle found
                                   Black -> return ()
            modify $ \(colors, res, idx) -> (update u Black colors, update idx u res, idx-1)

isValidSortFor :: Maybe (Vector Int) -> Graph -> Bool
isValidSortFor Nothing _ = False
isValidSortFor (Just lst) g = not $ any (\(u,v) -> u `V.elem` (neighbs v g)) pairs
  where pairs = [ (lst!i, lst!j) | let n = graphSize g, i<-[0..n-2], j<-[i+1..n-1] ]

graphTests :: [[Int]] -> Test
graphTests adjLst = TestList [
    "Correct length" ~: graphSize testG ~?= length adjLst,
    "Correct neighbours lists" ~:
        all (\u -> neighbs u testG == fromList (adjLst !! u)) [0..length adjLst - 1] ~?= True
    ]
  where testG = makeGraph adjLst

topoSortTests :: Test
topoSortTests = TestList [
    "Has unique sorting" ~: topoSort g ~?= Just (fromList [2,1,5,0,3,4]),
    "The sorting is valid" ~: topoSort g `isValidSortFor` g ~?= True,
    "No sorting, minimal case" ~: topoSort g' ~?= Nothing
    ]

main :: IO ()
main = do
    runTestTT $ graphTests [[1,2,3],[0],[1,3],[]]
    runTestTT topoSortTests
    return ()

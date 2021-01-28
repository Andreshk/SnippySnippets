module TopoSort (topoSort) where
import Graph
import Control.Monad.State (StateT,execStateT,get,modify,lift)
import Data.Foldable (for_)
import Data.Vector (Vector,fromList,(!),(//))
import qualified Data.Vector as V (length,replicate,elem)
import Test.HUnit (Test(TestList),runTestTT,(~:),(~?=))

-- A graph with an unique topological ordering: 2 1 5 0 3 4
-- It is unique due to it being a Hamiltonian path, i.e. no two vertices can be swapped.
g :: Graph
g = makeGraphE [(0,3),(0,4),(1,0),(1,3),(1,5),(2,1),(2,5),(3,4),(5,0),(5,4)]
-- Minimal graph with a cycle
g' :: Graph
g' = makeGraphE [(0,1),(1,0)]

-- Vertices are colored during DFS to mark their state (unvisited/in process/visited)
data Color = White | Gray | Black

-- Updating immutable vectors is actually as slow as updating lists
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
isValidSortFor (Just lst) g = not $ any (\(u,v) -> u `elem` (neighbs v g)) pairs
  where pairs = [ (lst!i, lst!j) | let n = graphSize g, i<-[0..n-2], j<-[i+1..n-1] ]

topoSortTests :: Test
topoSortTests = TestList [
    "Has unique sorting" ~: topoSort g ~?= Just (fromList [2,1,5,0,3,4]),
    "The sorting is valid" ~: topoSort g `isValidSortFor` g ~?= True,
    "No sorting, minimal case" ~: topoSort g' ~?= Nothing
    ]

main :: IO ()
main = do
    runTestTT graphTests
    runTestTT topoSortTests
    return ()

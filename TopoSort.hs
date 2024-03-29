module TopoSort (topoSort) where
import Graph
import Control.Monad.State (StateT,execStateT,get,modify,lift,unless)
import Data.Foldable (for_)
import Data.Vector (Vector,fromList,(!),(//))
import qualified Data.Vector as V (length,replicate,elem,modify)
import Data.Vector.Mutable (write)
import Test.HUnit (Test(TestList),runTestTT,(~:),(~?=))

-- Vertices are colored during DFS to mark their state (unvisited/in process/visited)
data Color = White | Gray | Black deriving Eq

-- The most accurate representation of this process is StateT (<the actual state>) Maybe,
-- i.e. a function that receives its arguments + state & may fail returning the value-new state pair.
-- We also get MonadFail for free - this gives us the ability to pattern-match the result from get
-- and the possibility for this match to fail if an earlier call returned Nothing.
topoSort :: Graph -> Maybe (Vector Int)
topoSort g = do
    (colors, res, -1) <- execStateT topoSort' (V.replicate n White, V.replicate n 0, n-1)
    unless (all (==Black) colors) $ error "Not all vertices visited..?"
    return res
  where n = graphSize g
        topoSort' :: StateT (Vector Color, Vector Int, Int) Maybe ()
        topoSort' = for_ [0..n-1] $ \u -> do
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
        -- Destructively update a vector (may be performed in-place)
        update :: Int -> a -> Vector a -> Vector a
        update idx val v = V.modify (\mv -> write mv idx val) v

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
  where -- A graph with an unique topological ordering: 2 1 5 0 3 4
        -- It is unique due to it being a Hamiltonian path, i.e. no two vertices can be swapped.
        g, g' :: Graph
        g = makeGraphE [(0,3),(0,4),(1,0),(1,3),(1,5),(2,1),(2,5),(3,4),(5,0),(5,4)]
        -- Minimal graph with a cycle
        g' = makeGraphE [(0,1),(1,0)]

main :: IO ()
main = do
    runTestTT graphTests
    runTestTT topoSortTests
    return ()

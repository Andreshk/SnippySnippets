module TopoSort (Graph,topoSort) where
import Control.Monad.State (StateT,execStateT,get,modify,lift)
import Data.Foldable (for_)

type Graph = [[Int]]

-- A graph with an unique topological ordering: 2 1 5 0 3 4
-- It is unique due to it being a Hamiltonian path, i.e. no two vertices can be swapped.
g :: Graph
g = [[3,4]
    ,[0,3,5]
    ,[1,5]
    ,[4]
    ,[]
    ,[0,4]]
-- Minimal graph with a cycle
g' :: Graph
g' = [[1],[0]]

graphSize :: Graph -> Int
graphSize = length
neighbs :: Int -> Graph -> [Int]
neighbs = flip (!!)

data Color = White | Gray | Black

update :: Int -> a -> [a] -> [a]
update idx val lst = first ++ (val:rest)
  where (first,_:rest) = splitAt idx lst

-- The most accurate representation of this process is StateT (<the actual state>) Maybe,
-- i.e. a function that receives its arguments + state & may fail returning the value-new state pair.
-- We also get MonadFail for free - this gives us the ability to pattern-match the result from get
-- and the possibility for this match to fail if an earlier call returned Nothing.
topoSort :: Graph -> Maybe [Int]
topoSort g = snd <$> execStateT (for_ [0..n-1] tryDFSVisit) (replicate n White, [])
  where n = graphSize g
        tryDFSVisit :: Int -> StateT ([Color],[Int]) Maybe ()
        tryDFSVisit u = do
            (colors, _) <- get
            case colors !! u of White -> dfsVisit u
                                Gray -> error "This shouldn't happen"
                                Black -> return ()
        dfsVisit :: Int -> StateT ([Color],[Int]) Maybe ()
        dfsVisit u = do
            modify $ \(colors, res) -> (update u Gray colors, res)
            for_ (neighbs u g) $ \v -> do
                (colors, _) <- get
                case colors !! v of White -> dfsVisit v
                                    Gray -> lift Nothing -- cycle found
                                    Black -> return ()
            modify $ \(colors, res) -> (update u Black colors, u:res)

topoTest :: Bool
topoTest = topoSort g == Just [2,1,5,0,3,4]
        && topoSort g' == Nothing

{-# LANGUAGE LambdaCase #-}
module TopoSort (Graph,topoSort,topoSort') where
import Control.Monad (foldM,when)
import Control.Monad.State (State,evalState,execState,get,put,modify)
import Data.Foldable (for_,traverse_)
import Data.Maybe (isJust,fromJust)

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

-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

topoSort :: Graph -> Maybe [Int]
topoSort g = evalState (foldM helper (Just []) [0..n-1]) (replicate n White)
  where n = graphSize g
        helper :: Maybe [Int] -> Int -> State [Color] (Maybe [Int])
        helper Nothing _ = return Nothing
        helper (Just res) u = do
            colors <- get
            case colors !! u of White -> dfsVisit u res
                                Gray -> error "Something ain't right"
                                Black -> return (Just res)
        dfsVisit :: Int -> [Int] -> State [Color] (Maybe [Int])
        dfsVisit u res = do
            modify $ update u Gray
            res' <- foldM helper (Just res) (neighbs u g)
            modify $ update u Black
            return $ (u:) <$> res'
          where helper :: Maybe [Int] -> Int -> State [Color] (Maybe [Int])
                helper Nothing _ = return Nothing
                helper (Just res) v = do
                    colors <- get
                    case colors !! v of White -> dfsVisit v res
                                        Gray -> return Nothing -- намерен е цикъл
                                        Black -> return (Just res)

-- for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
-- Maps each element to an action, evaluates actions left to right & discards the results

-- The "get >>= traverse_" combination receives a pure function and applies it to the state only
-- if it's non-empty. This check is obviously required, in one way or another, at every iteration.
-- Ideally, we'd like to do "Just ... <- get" and have it fail silently - but State is not in MonadFail.

topoSort' :: Graph -> Maybe [Int]
topoSort' g = snd <$> execState (for_ [0..n-1] tryDFSVisit) (Just (replicate n White, []))
  where n = graphSize g
        tryDFSVisit :: Int -> State (Maybe ([Color],[Int])) ()
        tryDFSVisit u = 
            get >>= traverse_ (\(colors, _) -> case colors !! u of White -> dfsVisit u
                                                                   Gray -> error "This shouldn't happen"
                                                                   Black -> return ())
        dfsVisit :: Int -> State (Maybe ([Color],[Int])) ()
        dfsVisit u = 
            get >>= traverse_ (const $ do
                modify $ fmap (\(colors, res) -> (update u Gray colors, res))
                for_ (neighbs u g) $ \v ->
                    get >>= traverse_ (\(colors, _) ->
                        case colors !! v of White -> dfsVisit v
                                            Gray -> put Nothing -- намерен е цикъл
                                            Black -> return ())
                modify $ fmap (\(colors, res) -> (update u Black colors, u:res)))

-- The following three functions are equivalent.
-- Note that the do-block serves as a placeholder for a larger one.
foo :: Int -> State (Maybe Int) ()
foo x = do
    st <- get
    when (isJust st) $ do
        put $ Just (fromJust st + x) -- ugly

bar :: Int -> State (Maybe Int) ()
bar x = 
    get >>= \case Nothing -> return ()
                  Just v -> do put $ Just (v + x)

baz :: Int -> State (Maybe Int) ()
baz x = 
    get >>= traverse_ (\v -> do put $ Just (v + x)) -- we traverse the Maybe (!)

testFoo :: Bool
testFoo = execState (foo 2) (Just 3) == (Just 5)
       && execState (bar 2) (Just 3) == (Just 5)
       && execState (baz 2) (Just 3) == (Just 5)
       && topoSort g == Just [2,1,5,0,3,4]
       && topoSort' g == Just [2,1,5,0,3,4]
       && topoSort g' == Nothing
       && topoSort' g' == Nothing

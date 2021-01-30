{-# LANGUAGE RankNTypes #-}
import Control.Monad (replicateM_,when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Random (StdGen,newStdGen,randomR)

-- Pure (!) makes a mutable copy, modifies it in-place, freezes it & returns the result
shuffle :: StdGen -> V.Vector a -> V.Vector a
shuffle gen vector = V.modify (loop gen (V.length vector)) vector
  where -- Invariant: the first n elements remain to be shuffled
        -- V.modify runs in the ST monad, so we can't use randomIO.
        loop :: PrimMonad m => StdGen -> Int -> M.MVector (PrimState m) a -> m ()
        loop gen n v = when (n > 1) $ do
            let (index, gen') = randomR (0, n-1) gen -- Note: index can match (n-1)
            M.swap v (n-1) index
            loop gen' (n-1) v

-- Runs the Fisher-Yates shuffle k times on a vector of length n
testShuffle :: Int -> Int -> IO ()
testShuffle k n = replicateM_ k $ do
    gen <- newStdGen
    print $ shuffle gen $ V.enumFromN 1 n

-- M.IOVector cannot be an instance of Show,
-- since accessing (incl. freezing) it is in the IO monad.
print' :: Show a => M.IOVector a -> IO ()
print' v = print =<< V.freeze v

-- Cycles through all permutations of a vector of length n
testPerm :: Int -> IO ()
testPerm n = V.thaw (V.fromList [1..n]) >>= loop
  where loop :: M.IOVector Int -> IO ()
        loop vec = do
            print' vec
            hasNext <- M.nextPermutation vec
            when hasNext (loop vec)

main :: IO ()
main = do
    putStrLn "Testing shuffle (k=5,n=10):"
    testShuffle 5 10
    putStrLn "Testing permutations (n=3):"
    testPerm 3

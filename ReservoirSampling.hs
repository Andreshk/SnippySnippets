module ReservoirSampling
  (reservoirSample, reservoirSampleIO, testReservoirDistr,
   fastReservoirSample, fastReservoirSampleIO, testFastReservoirDistr) where

import Control.Monad (when,replicateM,replicateM_)
import Control.Monad.Primitive (PrimMonad,PrimState)
import Data.Function (on)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import System.Random (StdGen,random,randomR,newStdGen)

-- Utility function to take care of division between integer values
(%) :: (Integral a, Fractional b) => a -> a -> b
(%) = (/) `on` fromIntegral
infixl 7 % -- Same as (/)

-- A reservoir sampler selects k values from a stream of an initially unknown, possibly
-- huge size n in O(k) space, with a single pass over the stream, so that at any point
-- each "consumed" value has an equal probability of being selected.
-- The functions, implementing reservoir sampling here are pure and receive an
-- initial random number generator to use for intermediate calculations.

-- Implementation of the "slow" O(n) Algorithm R for sampling k distinct
-- values in [0;n). Pure function - creates a mutable vector, modifies
-- its contents in-place, freezes it & returns it as a pure value.
reservoirSample :: StdGen -> Int -> Int -> Vector Int
reservoirSample gen k n = V.modify (loop gen (k+1)) $ V.enumFromN 0 k
  where -- Invariant: i is the 1-based index of the next value to be added
        loop :: PrimMonad m => StdGen -> Int -> MVector (PrimState m) Int -> m ()
        loop gen i samples = when (i <= n) $ do
            let prob = k % i -- Probability of keeping this i-th sample
                (x, gen') = random gen :: (Float, StdGen)
            when (x < prob) $ do
                -- We can reuse the random number to select the index of a
                -- value to overwrite by extrapolating from [0;prob) to [0;k).
                let idx = floor $ x*(fromIntegral k)/prob
                M.write samples idx (i-1)
            loop gen' (i+1) samples

-- Same as above, but using the built-in global generator (split for subsequent usage).
reservoirSampleIO :: Int -> Int -> IO (Vector Int)
reservoirSampleIO k n = do
    gen <- newStdGen
    return $ reservoirSample gen k n

-- Repeated reservoir sampling, mainly for testing purposes
testReservoir :: Int -> Int -> Int -> IO ()
testReservoir reps k n = replicateM_ reps $ print =<< reservoirSampleIO k n

-- Faster reservoir sampling via the O(k*log(n/k)) Algorithm L, which is
-- optimal for the task. It works by calculating the expected gaps between
-- each successfully added value and skipping over the remaining values.
-- It also draws only 3 random numbers for each of the O(k*log(n/k)) values
-- added, instead of 1 for each of the n values that the linear sampler does.
-- Again a pure function (like reservoirSample).
fastReservoirSample :: StdGen -> Int -> Int -> Vector Int
fastReservoirSample gen k n = V.modify (loop $ updateState (gen, k-1, 1)) $ V.enumFromN 0 k
  where -- Important: the random values (and generators) should not be
        -- reused - it leads to a skewed resulting distribution (!)
        updateState :: (StdGen, Int, Float) -> (StdGen, Int, Float)
        updateState (gen, i, w) = (gen'', i', w')
          where (x, gen') = random gen
                w' = w * exp (log x / fromIntegral k)
                (x', gen'') = random gen'
                i' = i + floor (log x' / log (1-w')) + 1
        -- Actually runs in the ST monad (via runST)
        loop :: PrimMonad m => (StdGen, Int, Float) -> MVector (PrimState m) Int -> m ()
        loop (gen, i, w) samples = when (i < n) $ do
            let (idx, gen') = randomR (0, k-1) gen :: (Int, StdGen)
            M.write samples idx i
            loop (updateState (gen', i, w)) samples

-- Same as above, but using the built-in global generator (split for subsequent usage).
fastReservoirSampleIO :: Int -> Int -> IO (Vector Int)
fastReservoirSampleIO k n = do
    gen <- newStdGen
    return $ fastReservoirSample gen k n

-- Evaluates the results from multiple runs of a reservoir sampler by counting
-- how often each value was present in a sample. These counts are normalized
-- to form a probability distribution function, whose variance is calculated
-- and compared to the variance of a fair n-sided die (ideal distribution).
compareToUniformDistr :: Int -> [Vector Int] -> IO ()
compareToUniformDistr n samples = do
    let reps = length samples
        k = V.length $ head samples
        xs = [0..n-1]
        histo = [ length . filter (elem x) $ samples | x<-xs ]
        -- Each x has an equal probability of k/n to be in the sample (making 1/n when normalized)
        probs = [ count % (reps*k) | count<-histo ]
        -- Variance of the results
        var = sum [ (probs !! x) * (fromIntegral x - mean)^2 | x<-xs ]
          where mean = (n-1) % 2 :: Float -- Mean of xs
        -- The expected variance is one of a fair n-sided die
        expVar = (n*n-1) % 12
    when (abs (sum probs - 1) > 1e-6 || any (<0) probs || any (>1) probs) $
        error "Invalid probabilities!"
    putStrLn $ "Histogram of " ++ show reps ++ " samples for k="
                ++ show k ++ " from [0;" ++ show n ++ "):"
    putStrLn $ show histo ++ " (expected ~" ++ show (reps * k % n) ++ ")"
    putStrLn $ "Variance: " ++ show var ++ " (expected ~" ++ show expVar ++ ")"

-- Runs a simple, general case of unweighted reservoir
-- sampling & evaluates the resulting distribution.
testReservoirDistr :: Int -> Int -> Int -> IO ()
testReservoirDistr reps k n =
    compareToUniformDistr n =<< (replicateM reps $ reservoirSampleIO k n)

-- Runs a simple, general case of unweighted reservoir
-- sampling & evaluates the resulting distribution.
testFastReservoirDistr :: Int -> Int -> Int -> IO ()
testFastReservoirDistr reps k n =
    compareToUniformDistr n =<< (replicateM reps $ fastReservoirSampleIO k n)

-- Weighted reservoir sampling - Wikipedia's flawed (!) version of Chao's algorithm
weightedReservoirSample :: StdGen -> Int -> [Float] -> Vector Int
weightedReservoirSample gen k weights
  | length first < k = error "Too few samples"
  | otherwise        = V.modify (loop gen k (sum first) rest) $ V.enumFromN 0 k -- First k are always selected
  where (first,rest) = splitAt k weights
        loop :: PrimMonad m => StdGen -> Int -> Float -> [Float] -> MVector (PrimState m) Int -> m ()
        loop _ _ _ [] _ = return ()
        loop gen i wSum (w:ws) mv = do
            let wSum' = wSum + w
                prob = w / wSum' -- Probability of keeping this sample (!)
                (x, gen') = random gen :: (Float, StdGen)
            when (x < prob) $ do
                -- extrapolate from [0;prob) to [0;k) to select the index of a value to overwrite
                let idx = floor $ x*(fromIntegral k)/prob
                M.write mv idx i
            loop gen' (i+1) wSum' ws mv

-- Analogous to reservoirSampleIO
weightedReservoirSampleIO :: Int -> [Float] -> IO (Vector Int)
weightedReservoirSampleIO k weights = do
    gen <- newStdGen
    return $ weightedReservoirSample gen k weights

-- Chao's algorithm is supposed to match the regular one when all the
-- weights are identical (regardless if they add up to 1 or not).
testWeighted :: Int -> Int -> Int -> IO ()
testWeighted reps k n =
    compareToUniformDistr n =<< (replicateM reps $ weightedReservoirSampleIO k weights)
  where weights = replicate n (1%n)

{- Future reading (& to-do):
- A possible better evaluation of the sampling, f.e. standard or root mean square deviation
- compare Algorithm L w/ the "very fast" algorithm below
- try fixing Chao's algorithm (or implement another for weighted sampling)
- http://had00b.blogspot.com/2013/07/random-subset-in-mapreduce.html for when k is too big
- https://erikerlandson.github.io/blog/2015/11/20/very-fast-reservoir-sampling/
-}

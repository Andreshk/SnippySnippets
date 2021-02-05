{-# LANGUAGE NamedFieldPuns #-}
module ReservoirSampling
  (ReservoirSampler, IOReservoirSampler, STReservoirSampler,
   newSamplerM, addSampleM, getSamplesM, unsafeGetSamplesM,
   reservoirSample, reservoirSampleIO, testReservoirDistr) where

import Control.Monad (when,replicateM,replicateM_)
import Control.Monad.Primitive (PrimMonad,PrimState,stToPrim)
import Control.Monad.ST (runST)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.STRef (STRef,newSTRef,readSTRef,writeSTRef)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import System.Random (StdGen,random,newStdGen)

-- A reservoir sampler selects k values from a stream of an initially unknown, possibly
-- huge size n in O(k) space, with a single pass over the stream, so that at any point
-- each "consumed" value has an equal probability of being selected.

-- This is an optimized implementation of the "slow" O(n) Algorithm R.
-- The currently sampled values are kept in a mutable vector, frozen only
-- when accessed. Also keeps its state via a mutable reference, to
-- achieve the same workflows for the sampler as a mutable vector itself.
data ReservoirSampler s a = RS {  state :: STRef s (StdGen, Int)
                               , values :: MVector s a }
-- User-friendly type aliases
type IOReservoirSampler   = ReservoirSampler (PrimState IO)
type STReservoirSampler s = ReservoirSampler s

-- Initializes a sampler's state & allocates memory for the values to be added
newSamplerM :: PrimMonad m => StdGen -> Int -> m (ReservoirSampler (PrimState m) a)
newSamplerM gen k = do
    -- Invariant: the Int in the state is the 1-based index of the next value to be added.
    st <- stToPrim $ newSTRef (gen, 1)
    values <- M.new k
    return $ RS st values

-- Adds a value to the sampler, modifying in-place both the inner array and the value's state.
addSampleM :: PrimMonad m => ReservoirSampler (PrimState m) a -> a -> m ()
addSampleM RS{ state, values } val = do
    let k = M.length values
    (gen, i) <- stToPrim $ readSTRef state
    if i <= k
    then do -- First k values are always selected
        M.write values (i-1) val
        stToPrim $ writeSTRef state (gen, i+1)
    else do
        let prob = (fromIntegral k) / (fromIntegral i) -- Probability of keeping this i-th sample
            (x, gen') = random gen :: (Float, StdGen)
        when (x < prob) $ do
            -- Select the index of a value to overwrite on random by extrapolating from [0;prob) to [0;k)
            let idx = floor $ x*(fromIntegral k)/prob
            M.write values idx val
        stToPrim $ writeSTRef state (gen', i+1)

-- Freezes the vector of currently selected k values
-- (safely or unsafely) and returns them as an immutable vector.
getSamplesM, unsafeGetSamplesM :: PrimMonad m => ReservoirSampler (PrimState m) a -> m (Vector a)
getSamplesM       = getSamplesImpl V.freeze
unsafeGetSamplesM = getSamplesImpl V.unsafeFreeze

-- Helper function for the two above
getSamplesImpl :: PrimMonad m =>
    (MVector (PrimState m) a -> m (Vector a)) -> ReservoirSampler (PrimState m) a -> m (Vector a)
getSamplesImpl getter RS{ state, values } = do
    let k = M.length values
    (_, i) <- stToPrim $ readSTRef state
    when (i <= k) $ error "Too few samples!"
    return =<< getter values

-- Uses reservoir sampling and a user-supplied random number
-- generator to select k distinct values in [0;n).
-- Pure function - creates a mutable sampler, modifies its contents
-- in-place, freezes the result & returns it as a pure value.
reservoirSample :: StdGen -> Int -> Int -> Vector Int
reservoirSample gen k n = runST $ do
    sampler <- newSamplerM gen k
    forM_ [0..n-1] $ addSampleM sampler
    return =<< unsafeGetSamplesM sampler

-- Same as above, but using the built-in global generator (split for subsequent usage).
reservoirSampleIO :: Int -> Int -> IO (Vector Int)
reservoirSampleIO k n = do
    gen <- newStdGen
    return $ reservoirSample gen k n

-- Repeated reservoir sampling, mainly for testing purposes
testReservoir :: Int -> Int -> Int -> IO ()
testReservoir reps k n = replicateM_ reps $ print =<< reservoirSampleIO k n

-- Utility function to take care of division between integer values
(%) :: (Integral a, Fractional b) => a -> a -> b
(%) = (/) `on` fromIntegral
infixl 7 % -- Same as (/)

-- Evaluate the reservoir sampling algorithm by running it a given number of times
-- and counting how often each value was present in the sample. We normalize these
-- counts to form a probability distribution function, whose variance we calculate
-- and compare to the variance of a fair n-sided die (ideal distribution).
testReservoirDistr :: Int -> Int -> Int -> IO ()
testReservoirDistr reps k n = do
    putStrLn $ "Histogram of " ++ show reps ++ " samples for k="
                ++ show k ++ " from [0;" ++ show n ++ "):"
    samples <- replicateM reps $ reservoirSampleIO k n
    let xs = [0..n-1]
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
    putStrLn $ show histo ++ " (expected ~" ++ show (reps * k % n) ++ ")"
    putStrLn $ "Variance: " ++ show var ++ " (expected ~" ++ show expVar ++ ")"

-- A similar idea for reservoir sampling, with roughly the same execution,
-- but much less generic & with more explicit state keeping.
reservoirSample' :: StdGen -> Int -> Int -> Vector Int
reservoirSample' gen k n
  | k >= n    = V.enumFromN 0 n -- Probably an error?
  | otherwise = V.modify (loop gen k) $ V.enumFromN 0 k -- First k are always selected
  where -- Invariant: i is the next value to be sampled (making it the i+1-th)
        loop :: PrimMonad m => StdGen -> Int -> MVector (PrimState m) Int -> m()
        loop gen i v = when (i < n) $ do
            let prob = (fromIntegral k) / (fromIntegral (i+1)) -- Probability of keeping this sample
                (x, gen') = random gen :: (Float, StdGen)
            when (x < prob) $ do
                -- extrapolate from [0;prob) to [0;k) to select the index of a value to overwrite
                let idx = floor $ x*(fromIntegral k)/prob
                M.write v idx i
            loop gen' (i+1) v

{- Future reading (& to-do):
- A possible better evaluation of the sampling, f.e. standard or root mean square deviation
- Algorithm L (also see if it's the same as the "very fast" algorithm below)
- Weighted sampling (possibly algorithm A-Chao from the Wikipedia article)
- http://had00b.blogspot.com/2013/07/random-subset-in-mapreduce.html for when k is too big
- https://erikerlandson.github.io/blog/2015/11/20/very-fast-reservoir-sampling/
-}

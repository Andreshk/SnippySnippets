{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module ReservoirSampling
  (ReservoirSampler, {-IOReservoirSampler, STReservoirSampler,-}
   newSamplerM, addSampleM, getSamplesM, unsafeGetSamplesM,
   reservoirSample, reservoirSampleIO, testReservoirDistr) where

import Control.Monad (when,replicateM,replicateM_)
import Control.Monad.Primitive (PrimMonad,PrimState,stToPrim)
import Control.Monad.ST (ST,runST)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.STRef (STRef,newSTRef,readSTRef,writeSTRef)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import System.Random (StdGen,random,newStdGen)

-- Utility function to take care of division between integer values
(%) :: (Integral a, Fractional b) => a -> a -> b
(%) = (/) `on` fromIntegral
infixl 7 % -- Same as (/)

-- A reservoir sampler selects k values from a stream of an initially unknown, possibly
-- huge size n in O(k) space, with a single pass over the stream, so that at any point
-- each "consumed" value has an equal probability of being selected.
class ReservoirSampler r where
    -- Initializes a sampler's state with the first k values (as they are always added)
    newSamplerM :: PrimMonad m => StdGen -> [a] -> m (r (PrimState m) a)
    -- Adds a value to the sampler, modifying in-place the sampler's internal state
    addSampleM :: PrimMonad m => r (PrimState m) a -> a -> m ()
    -- Freezes the vector of currently selected k values
    -- (safely or unsafely) and returns them as an immutable vector.
    getSamplesM, unsafeGetSamplesM :: PrimMonad m => r (PrimState m) a -> m (Vector a)

-- This is an optimized implementation of the "slow" O(n) Algorithm R.
-- The currently sampled values are kept in a mutable vector, frozen only
-- when accessed. Also keeps its state via a mutable reference, to
-- achieve the same workflows for the sampler as a mutable vector itself.
-- Invariant: the Int in the state is the 1-based index of the next value to be added.
data LinearReservoirSampler s a = LRS {   state :: STRef s (StdGen, Int)
                                      , samples :: MVector s a }
-- User-friendly type aliases
type IOLinearReservoirSampler   = LinearReservoirSampler (PrimState IO)
type STLinearReservoirSampler s = LinearReservoirSampler s

instance ReservoirSampler LinearReservoirSampler where
    newSamplerM :: PrimMonad m => StdGen -> [a] -> m (LinearReservoirSampler (PrimState m) a)
    newSamplerM gen values = do
        samples <- V.unsafeThaw $ V.fromList values
        state <- stToPrim $ newSTRef (gen, succ $ M.length samples)
        return $ LRS state samples
    addSampleM :: PrimMonad m => LinearReservoirSampler (PrimState m) a -> a -> m ()
    addSampleM LRS{ state, samples } val = do
        (gen, i) <- stToPrim $ readSTRef state
        let k = M.length samples
            prob = k % i -- Probability of keeping this i-th sample
            (x, gen') = random gen :: (Float, StdGen)
        when (x < prob) $ do
            -- Select the index of a value to overwrite on random by extrapolating from [0;prob) to [0;k)
            let idx = floor $ x*(fromIntegral k)/prob
            M.write samples idx val
        stToPrim $ writeSTRef state (gen', i+1)
    getSamplesM, unsafeGetSamplesM :: PrimMonad m => LinearReservoirSampler (PrimState m) a -> m (Vector a)
    getSamplesM       = V.freeze . samples
    unsafeGetSamplesM = V.unsafeFreeze . samples

-- Uses reservoir sampling and a user-supplied random number
-- generator to select k distinct values in [0;n).
-- Pure function - creates a mutable sampler, modifies its contents
-- in-place, freezes the result & returns it as a pure value.
reservoirSample :: StdGen -> Int -> Int -> Vector Int
reservoirSample gen k n = runST $ do
    sampler <- newSamplerM gen [0..k-1] :: ST s (LinearReservoirSampler s Int)
    forM_ [k..n-1] $ addSampleM sampler
    return =<< unsafeGetSamplesM sampler

-- Same as above, but using the built-in global generator (split for subsequent usage).
reservoirSampleIO :: Int -> Int -> IO (Vector Int)
reservoirSampleIO k n = do
    gen <- newStdGen
    return $ reservoirSample gen k n

-- Repeated reservoir sampling, mainly for testing purposes
testReservoir :: Int -> Int -> Int -> IO ()
testReservoir reps k n = replicateM_ reps $ print =<< reservoirSampleIO k n

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
testReservoirDistr reps k n = do
    samples <- replicateM reps $ reservoirSampleIO k n
    compareToUniformDistr n samples

-- The "fast" sampler is supposed to implement the O(k*log(n/k)) Algorithm L,
-- which is optimal for the task. It works by calculating the expected gaps
-- between each successfully added value and discarding the values inbetween.
-- It also draws only 3 random numbers for each of the O(k*log(n/k)) values
-- added, instead of 1 for each of the n values that the linear sampler does.
-- However, the current sequential ReservoirSampler interface forces attempting
-- to add each value, without actually skipping the rest, resulting in O(n) again.
data FastReservoirSampler s a = FRS {   fstate :: STRef s (StdGen, Int, Float)
                                    ,     iRef :: STRef s Int
                                    , fsamples :: MVector s a }
-- User-friendly type aliases
type IOFastReservoirSampler   = FastReservoirSampler (PrimState IO)
type STFastReservoirSampler s = FastReservoirSampler s

-- Helper function to update the fast sampler's internal state.
-- Important: the random values (and generators) should not be
-- reused - it leads to a skewed resulting distribution (!)
updateState :: Int -> (StdGen, Int, Float) -> (StdGen, Int, Float)
updateState k (gen, next, w) = (gen'', next', w')
  where (x, gen') = random gen
        w' = w * exp (log x / fromIntegral k)
        (x', gen'') = random gen'
        next' = next + floor (log x' / log (1-w')) + 1

instance ReservoirSampler FastReservoirSampler where
    newSamplerM :: PrimMonad m => StdGen -> [a] -> m (FastReservoirSampler (PrimState m) a)
    newSamplerM gen values = do
        samples <- V.unsafeThaw $ V.fromList values
        let k = M.length samples
        state <- stToPrim . newSTRef . updateState k $ (gen, k-1, 1)
        iRef <- stToPrim $ newSTRef k
        return $ FRS state iRef samples
    addSampleM :: PrimMonad m => FastReservoirSampler (PrimState m) a -> a -> m ()
    addSampleM FRS{ fstate, iRef, fsamples } val = do
        i <- stToPrim $ readSTRef iRef
        st@(gen, next, w) <- stToPrim $ readSTRef fstate
        -- "Skipping" a value is actually O(n) => going through n values will execute
        -- the code inside only O(k*log(n/k)) times, but the total time will be O(n) :/
        when (i == next) $ do
            let k = M.length fsamples
                (gen', next', w') = updateState k st
                (x, gen'') = random gen' :: (Float, StdGen)
                -- extrapolate from [0;1) to [0;k) to select the index of a value to overwrite
                idx = floor $ fromIntegral k * x
            M.write fsamples idx val
            stToPrim $ writeSTRef fstate (gen'', next', w')
        -- Always bump the internal counter
        stToPrim $ writeSTRef iRef (i+1)
    getSamplesM, unsafeGetSamplesM :: PrimMonad m => FastReservoirSampler (PrimState m) a -> m (Vector a)
    getSamplesM       = V.freeze . fsamples
    unsafeGetSamplesM = V.unsafeFreeze . fsamples

-- Uses reservoir sampling and a user-supplied random number
-- generator to select k distinct values in [0;n).
-- Pure function - creates a mutable sampler, modifies its contents
-- in-place, freezes the result & returns it as a pure value.
fastReservoirSample :: StdGen -> Int -> Int -> Vector Int
fastReservoirSample gen k n = runST $ do
    sampler <- newSamplerM gen [0..k-1] :: ST s (FastReservoirSampler s Int)
    forM_ [k..n-1] $ addSampleM sampler
    return =<< unsafeGetSamplesM sampler

-- Same as above, but using the built-in global generator (split for subsequent usage).
fastReservoirSampleIO :: Int -> Int -> IO (Vector Int)
fastReservoirSampleIO k n = do
    gen <- newStdGen
    return $ fastReservoirSample gen k n

-- Runs a simple, general case of unweighted reservoir
-- sampling & evaluates the resulting distribution.
testFastReservoirDistr :: Int -> Int -> Int -> IO ()
testFastReservoirDistr reps k n = do
    samples <- replicateM reps $ fastReservoirSampleIO k n
    compareToUniformDistr n samples

-- Weighted reservoir sampling - Wikipedia's flawed (!) version of
-- Chao's algorithm. Roughly the same execution as the main algorithm,
-- but much less generic & with more explicit state keeping.
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
testWeighted reps k n = do
    let weights = replicate n (1%n)
    samples <- replicateM reps $ weightedReservoirSampleIO k weights
    compareToUniformDistr n samples

{- Future reading (& to-do):
- A possible better evaluation of the sampling, f.e. standard or root mean square deviation
- Algorithm L (also see if it's the same as the "very fast" algorithm below)
- Weighted sampling (possibly algorithm A-Chao from the Wikipedia article)
- http://had00b.blogspot.com/2013/07/random-subset-in-mapreduce.html for when k is too big
- https://erikerlandson.github.io/blog/2015/11/20/very-fast-reservoir-sampling/
-}

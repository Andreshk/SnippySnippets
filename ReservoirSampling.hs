module ReservoirSampling where
import System.Random (StdGen, mkStdGen, random)
import Data.Vector (Vector, empty, (//), cons)

data ReservoirSampler a = RS StdGen Int Int (Vector a)

mkSampler :: Int -> Int -> ReservoirSampler a
mkSampler seed k = RS (mkStdGen seed) k 1 empty

-- Invariant: s is the nth sample added
add :: a -> ReservoirSampler a -> ReservoirSampler a
add s (RS gen k n values) = RS gen' k (n+1) values'
  where values' | n <= k    = cons s values -- the first k samples are always added
                | x < prob  = values // [(idx,s)]
                | otherwise = values
        (x, gen') = random gen :: (Float,StdGen)
        prob = (fromIntegral k) / (fromIntegral n) -- the probability of keeping the new sample
        x' = x / prob -- extrapolate from [0;prob) to [0;1)
        idx = floor ((fromIntegral k)*x') -- the index of the value replaced

samples :: ReservoirSampler a -> Vector a
samples (RS _ _ _ values) = values

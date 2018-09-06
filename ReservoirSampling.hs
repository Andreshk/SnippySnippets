module ReservoirSampling where
import System.Random (StdGen, mkStdGen, random)

data ReservoirSampler a = RS StdGen Int Int [a]

mkSampler :: Int -> Int -> ReservoirSampler a
mkSampler seed k = RS (mkStdGen seed) k 1 []

-- Invariant: s is the nth sample added
add :: a -> ReservoirSampler a -> ReservoirSampler a
add s (RS gen k n values) = RS gen' k (n+1) values'
  where values' | n <= k    = (s:values) -- the first k samples are always added
                | x < prob  = replaced
                | otherwise = values
        (x, gen') = random gen :: (Float,StdGen)
        prob = (fromIntegral k) / (fromIntegral n) -- the probability of keeping the new sample
        x' = x / prob -- extrapolate from [0;prob) to [0;1)
        idx = floor ((fromIntegral k)*x') -- the index of the value replaced
        replaced = (take idx values) ++ [s] ++ (drop (idx+1) values)

samples :: ReservoirSampler a -> [a]
samples (RS _ _ _ values) = values

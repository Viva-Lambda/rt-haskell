-- random related
module Random where

import System.Random

randomDouble :: RandomGen g => g -> Double -> Double -> Double
randomDouble generator low high = if low > high
                                  then rand high low
                                  else rand low high
    where rand lval hval = let (d, g) = randomR (low, high) generator
                           in d

rand :: RandomGen g => g -> Double
rand g = randomDouble g 0.0 1.0

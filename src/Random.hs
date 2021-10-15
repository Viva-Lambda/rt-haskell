{-# LANGUAGE BangPatterns #-}
-- random related
module Random where

import System.Random
import GHC.Float

randomDouble :: RandomGen g => g -> Double -> Double -> (Double, g)
randomDouble !generator !low !high = if low > high
                                     then rand high low
                                     else rand low high
    where rand lval hval = randomR (low, high) generator

randomInt :: RandomGen g => g -> Int -> Int -> (Int, g)
randomInt gen low high = 
    let (d, g2) = randomDouble gen (int2Double low) (int2Double high)
    in (double2Int d, g2)

randomDoubles :: RandomGen g => g -> Double -> Double -> [Double]

randomGens :: RandomGen g => g -> Int -> [g]

randomGens !gen !size = foldGens [] gen size
    where foldGens es g size = if (length es) == size
                               then es
                               else if (length es) > size
                                    then take size es
                                    else let (g1, g2) = split g
                                         in foldGens (g1:g2:es) g1 size
                                    

randomDoubles !generator !low !high = if low > high
                                      then rand high low
                                      else rand low high
    where rand lval hval = randomRs (low, high) generator

                           

rand :: RandomGen g => g -> (Double, g)
rand g = randomDouble g 0.0 1.0

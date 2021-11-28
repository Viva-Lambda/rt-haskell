{-# LANGUAGE BangPatterns #-}
-- random related
module Random where

import System.Random
import GHC.Float

randomDouble :: RandomGen g => g -> Double -> Double -> (Double, g)
randomDouble !generator !low !high = if low > high
                                     then ranD high low
                                     else ranD low high
    where ranD lval hval = randomR (lval, hval) generator

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
                                         in foldGens (g1:g2:es) g2 size
                                    

randomDoubles !generator !low !high = if low > high
                                      then ranD high low
                                      else ranD low high
    where ranD lval hval = randomRs (lval, hval) generator


randomRPtr :: (Ord a, Random a, RandomGen g) => g -> a -> a -> ([a], Int)

randomRPtr gen !low !high = if low > high
                            then randPtr high low
                            else randPtr low high
    where randPtr lv hv = (randomRs (lv, hv) gen, 0)

getRandVal :: Random a => Int -> ([a], Int) -> ([a], [a], Int)
getRandVal nbRandEls (rvals, ptr) = 
    let nrvs = take nbRandEls (drop ptr rvals)
    in (nrvs, rvals, ptr + nbRandEls)
                           

randval :: RandomGen g => g -> (Double, g)
randval g = randomDouble g 0.0 1.0

-- utility function
module Utility.Utils where

import Data.List

infty :: Double
infty = (read "Infinity") :: Double

m_pi :: Double
m_pi = 3.141592653589793238

degrees_to_radians :: Double -> Double
degrees_to_radians degrees = degrees * m_pi / 180.0

clamp :: Double -> Double -> Double -> Double

clamp x min max = if x < min
                  then min
                  else if x > max
                       then max
                       else x

-- 
eqReduce :: Eq a => [a] -> ((a -> Bool) -> [a] -> Bool) -> Bool
eqReduce lst f = case lst of
                    [] -> True
                    (x:xs) -> f (== x) (x:xs)

allEqual :: Eq a => [a] -> Bool
allEqual lst = eqReduce lst all


anyEqual :: Eq a => [a] -> Bool
anyEqual lst = eqReduce lst any

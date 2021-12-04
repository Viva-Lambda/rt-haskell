-- utility function
module Utility.Utils where

import Data.List
import Debug.Trace

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

-- enumerate
enumerate :: [a] -> [(Int, a)]
enumerate a = zip [0..((length a)-1)] a

-- take between
takeBetween :: Int -> Int -> [a] -> [a]
takeBetween mnv mxv lst =
    let (mn, mx) = if mnv < mxv
                   then (mnv, mxv)  
                   else (mxv, mnv)
    in if mn < 0
       then traceStack "minimum value is smaller than zero in takeBetween" []
       else if mx > (length lst)
            then let lstlen = "list size " ++ show (length lst)
                     mxstr = "maximum value " ++ show mx
                     msg = "maximum value is bigger than list size " 
                 in traceStack (msg ++ lstlen ++ mxstr) []
            else let enums = enumerate lst
                     pred (i, a) = i >= mn && i <= mx
                     subseq = filter pred enums
                     (nms, els) = unzip subseq
                 in els

-- debug utilities
debugTraceStr :: Show a => [a] -> String
debugTraceStr xs = intercalate "\n " (map show xs)

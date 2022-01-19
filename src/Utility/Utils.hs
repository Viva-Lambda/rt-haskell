-- utility function
module Utility.Utils where

import Data.List
import Data.Bits
import Debug.Trace

import GHC.Float hiding (clamp)

infty :: Double
infty = (read "Infinity") :: Double

m_pi :: Double
m_pi = 3.141592653589793238

degrees_to_radians :: Double -> Double
degrees_to_radians degrees = degrees * m_pi / 180.0

clamp :: Ord a => a -> a -> a -> a

clamp x min max = if x < min
                  then min
                  else if x > max
                       then max
                       else x

-- interpolate a value in one range to another range
interp :: (Double, Double) -> (Double, Double) -> Double -> Double
interp (inputStart, inputEnd) (outputStart, outputEnd) value =
    let idiff = (value - inputStart) / (inputEnd - inputStart)
        odiff = outputEnd - outputStart
    in idiff * odiff + outputStart

-- find the corresponding interval given a function and a value
checkMiddle :: Int -> Int -> Int -> Int -> (Int -> Bool) -> (Int, Int)
checkMiddle middle fs lval hlf intervalFn = 
    if intervalFn middle
    then (middle + 1, lval - (hlf + 1))
    else (fs, hlf)

findInterval :: Int -> (Int -> Bool) -> Int
findInterval size intervalFn =
    let first = 0
        len = size
        --
        halfSearch lval fs = let hlf = lval `shiftR` 1
                                 middle = fs + hlf
                                 (nfs, nlval) = checkMiddle middle fs lval hlf intervalFn
                             in if nlval > 0
                                then halfSearch nlval nfs
                                else (nfs, nlval)
        (nfirst, _) = halfSearch len first
    in clamp (nfirst - 1) 0 (size - 2)


mix :: Double -> Double -> Double -> Double
mix t v u = (1.0 - t) * v + t * u
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

-- 
word2Int :: Word -> Int
word2Int a = fromIntegral a

int2Word :: Int -> Word
int2Word a = fromIntegral a

double2Word :: Double -> Word
double2Word a = fromIntegral $ double2Int a

-- debug utilities
debugTraceStr :: Show a => [a] -> String
debugTraceStr xs = intercalate "\n " (map show xs)

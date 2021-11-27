{-# LANGUAGE BangPatterns #-}
-- Basic Matrix implementation
module Math3D.Matrix where

import Math3D.Vector hiding(sizeError)
import Math3D.CommonOps
import GHC.Float
import Data.List
import Data.Foldable
import Debug.Trace

import Utility.Utils

data Matrix = MList {mdata :: [Double], mstride :: Int} deriving (Eq)

instance Show Matrix where
    show m = 
        let msg1 = "<Matix " ++ show (mRowNb m) ++ "x" ++ show (mColNb m)
        in msg1 ++ " >"

mzero :: Int -> Int -> Matrix
mzero rowNb colNb = MList {mdata = replicate (rowNb * colNb) 0.0, 
                           mstride = colNb}

matFromVector :: [Vector] -> Matrix
matFromVector [] = MList {mdata = [], mstride = 0}
matFromVector (v:vs) = 
    -- let myStrList = lines myStr -- \n
    -- in [splitOn ',' myStr | myStr <- myStrList]
    let sizes = [(vsize v_) == (vsize v) | v_ <- vs]
        allSameLength = foldl1 (==) sizes
    in if not allSameLength
       then traceStack "All vectors must have same length" (mzero 1 1)
       else -- foldfn :: (a -> b -> a)
           let foldfn ac v = let VList a = v
                             in ac ++ a
               md = foldl foldfn [] (v:vs)
           in MList {mdata = md, mstride = vsize v}


mrows :: Matrix -> [Vector]
mrows m =
    let d = mdata m
        s = mstride m
        rowNb = mRowNb m
    in [VList $ takeBetween (s*i) (s*i + s-1) d | i <- [0..(rowNb - 1)]]


mcols :: Matrix -> [Vector]
mcols m = 
    let rows = mrows m
        getcol i = [vget row i | row <-rows]
    in [VList $ getcol i | i <- [0..((mColNb m)-1)]]


msize :: Matrix -> Int
msize !m = length (mdata m)

mget :: Matrix -> Int -> Int -> Double
mget mat col row = vget (mgetRow mat row) col

mRowNb :: Matrix -> Int
mRowNb mat = 
    let m1 = int2Double (msize mat)
        m2 = int2Double (mstride mat)
    in double2Int $! m1 / m2

mColNb :: Matrix -> Int
mColNb mat = mstride mat

mgetColumn :: Matrix -> Int -> Vector
mgetColumn mat index = 
    if index >= (mstride mat)
    then let msg1 = "given column index is larger than stride " ++ show (mstride mat)
             msg2 = msg1 ++ " index " ++ show index 
         in traceStack msg2 zeroV3
    else (mcols mat) !! index

mgetRow :: Matrix -> Int -> Vector
mgetRow mat index = 
    if index >= (mRowNb mat)
    then let msg1 = "given row index is larger than number of rows of matrix"
             msg2 = " index " ++ show index
             msg3 = " row number " ++ show (mRowNb mat)
         in traceStack (msg1 ++ msg2 ++ msg3) zeroV3
    else (mrows mat) !! index

msetRow :: Matrix -> Int -> Vector -> Matrix
msetRow mat index v =
    if index >= (mRowNb mat)
    then traceStack "given row index is larger than number of rows of matrix" (mzero 1 1)
    else let md = mdata mat
             colnb = mColNb mat
             VList vs = v
             loc = index * colnb
             rowend = loc + colnb
             (before, rdata) = splitAt loc md
             (berdata, after) = splitAt rowend md
         in MList {mdata = before ++ vs ++ after, mstride = mstride mat}



sizeError :: Matrix -> Matrix -> String -> String
sizeError !v !s m =
    let msg = "matrix sizes: " ++ (show $! msize v) ++ " and " ++ (show $! msize s)
        msg2 = msg ++ " are incorrect for operation " ++ m
    in msg2

matError :: Matrix -> String -> String
matError !v m =
    let msg = "matrix: " ++ show v ++ " " ++ m 
    in msg

matArithmeticOp :: String -> (Double -> Double -> Double) -> Matrix -> Matrix -> Matrix
matArithmeticOp opname f !v !e =
    if (msize v) /= (msize e)
    then error $ sizeError v e opname
    else if (mstride v) /= (mstride e) 
         then error $ sizeError v e opname
         else let ds = mdata v
                  es = mdata e 
                  ndata = zipWith f ds es
              in MList {mdata = ndata, mstride = mstride v}

matScalarOp :: String -> (Double -> Double) -> Matrix -> Matrix
matScalarOp _ f !v = MList {mdata = map f (mdata v), mstride = mstride v}

instance BinaryOps Matrix where
    elementwiseOp = matArithmeticOp
    elementwiseScalarOp = matScalarOp
    divide !v !e =
        let es = mdata e
        in if 0.0 `elem` es
           then error $ matError e "contains zero in a division operation"
           else matArithmeticOp "divide" (/) v e

-- get XbyY times YbyZ -> XbyZ
matmul :: Matrix -> Matrix -> Matrix
matmul !a !b =
    let aColNb = mColNb a
        aRowNb = mRowNb a
        bColNb = mColNb b
        bRowNb = mRowNb b
    in if aColNb /= bRowNb
       then let msg1 = "SizeError :: argument matrices have incompatible size "
                msg2 = msg1 ++ (show aRowNb)
                msg3 = msg2 ++ "x" ++ (show aColNb)
                msg4 = msg3 ++ " and " ++ (show bRowNb) ++ "x" ++ (show bColNb)
            in traceStack msg4 (mzero 1 1)
       else -- real multiplication work begins
            let bcols = mcols b
                arows = mrows a
                fn arow = [dot arow bcol | bcol <- bcols]
                nmatData = concat $ map fn arows
            in MList {mdata = nmatData, mstride = bColNb}
            

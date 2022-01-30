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
import Utility.HelperTypes

data Matrix = MList {mdata :: NonEmptyList Double,
                     mstride :: Int}

instance Eq Matrix where
    a == b = let ma = nl2List $! mdata a
                 mb = nl2List $! mdata b
                 msa = mstride a
                 msb = mstride b
             in (ma == mb) && (msa == msb)


instance Show Matrix where
    show m = 
        let msg1 = "<Matix " ++ show (mRowNb m) ++ "x" ++ show (mColNb m)
        in msg1 ++ " >"

mzero :: Int -> Int -> Matrix
mzero !rowNb !colNb =
    let (m:ms) = replicate (rowNb * colNb) 0.0
    in MList {mdata = fromList2NL m ms,
              mstride = colNb}

matFromVector :: NonEmptyList Vector -> Matrix
matFromVector vvs =
    -- let myStrList = lines myStr -- \n
    -- in [splitOn ',' myStr | myStr <- myStrList]
    let !(v:vs) = nl2List vvs
        sizes = [(vsize v_) == (vsize v) | v_ <- vs]
        allSameLength = foldl1 (==) sizes
    in if not allSameLength
       then traceStack "All vectors must have same length" (mzero 1 1)
       else -- foldfn :: (a -> b -> a)
           let foldfn ac p = let a = vec2List p in ac ++ a
               (m:ms) = foldl foldfn [] (v:vs)
           in MList {mdata = fromList2NL m ms, mstride = vsize v}


mrows :: Matrix -> [Vector]
mrows !m =
    let d = nl2List $! mdata m
        s = mstride m
        rowNb = mRowNb m
        vlst = [takeBetween (s*i) (s*i + s-1) d | i <- [0..(rowNb - 1)]]
        f v = fromList2NL (head v) (tail v)
    in map (VList . f) vlst


mcols :: Matrix -> [Vector]
mcols !m = 
    let rows = mrows m
        getcol i = let (v:vs) = [vget row i | row <-rows] in fromList2NL v vs
    in [VList $ getcol i | i <- [0..((mColNb m)-1)]]


msize :: Matrix -> Int
msize !m = lengthNL (mdata m)

mget :: Matrix -> Int -> Int -> Double
mget !mat col row = vget (mgetRow mat row) col

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
    else let md = nl2List $! mdata mat
             colnb = mColNb mat
             VList vs = v
             loc = index * colnb
             rowend = loc + colnb
             (before, rdata) = splitAt loc md
             (berdata, after) = splitAt rowend md
             (m:ms) = before ++ (nl2List vs) ++ after
         in MList {mdata = fromList2NL m ms, mstride = mstride mat}



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
         else let ds = nl2List $! mdata v
                  es = nl2List $! mdata e 
                  (m:ms) = zipWith f ds es
              in MList {mdata = fromList2NL m ms, mstride = mstride v}

matScalarOp :: String -> (Double -> Double) -> Matrix -> Matrix
matScalarOp _ f !v =
    let (m:ms) = map f (nl2List $! mdata v)
    in MList {mdata = fromList2NL m ms, mstride = mstride v}

instance BinaryOps Matrix where
    elementwiseOp = matArithmeticOp
    elementwiseScalarOp = matScalarOp
    divide !v !e =
        let es = nl2List $! mdata e
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
                (m:ms) = concat $ map fn arows
            in MList {mdata = fromList2NL m ms, mstride = bColNb}

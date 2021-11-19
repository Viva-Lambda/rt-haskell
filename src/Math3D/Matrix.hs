{-# LANGUAGE BangPatterns #-}
-- Basic Matrix implementation
module Math3D.Matrix where

import qualified Math3D.Vector as Vec
import Math3D.CommonOps
import GHC.Float
import Data.List
import Data.Foldable

data Matrix = MList {mdata :: [Double], mstride :: Int} deriving (Eq, Show)

mzero :: Int -> Int -> Matrix
mzero rowNb colNb = MList {mdata = replicate (rowNb * colNb) 0.0, 
                           mstride = colNb}

matFromVector :: [Vec.Vector] -> Matrix
matFromVector [] = MList {mdata = [], mstride = 0}
matFromVector (v:vs) = 
    -- let myStrList = lines myStr -- \n
    -- in [splitOn ',' myStr | myStr <- myStrList]
    let sizes = [(Vec.vsize v_) == (Vec.vsize v) | v_ <- vs]
        allSameLength = foldl1 (==) sizes
    in if not allSameLength
       then error "All vectors must have same length"
       else -- foldfn :: (a -> b -> a)
           let foldfn ac v = let Vec.VList a = v
                             in ac ++ a
               md = foldl foldfn [] vs
           in MList {mdata = md, mstride = Vec.vsize v}

msize :: Matrix -> Int
msize !m = length (mdata m)

mtake :: Matrix -> Int -> Double
mtake mat index = 
    let vs = mdata mat
    in if (msize mat) <= index || index < 0
       then error $ "IndexError: uncorrect index size: " ++ show index
       else vs !! index


mget :: Matrix -> Int -> Int -> Double
mget mat col row =
    let colnb = mstride mat
        loc = row * colnb + col
    in mtake mat loc

mRowNb :: Matrix -> Int
mRowNb mat = 
    let m1 = int2Double (msize mat)
        m2 = int2Double (mstride mat)
    in double2Int $! m1 / m2

mColNb :: Matrix -> Int
mColNb mat = mstride mat

mgetColumn :: Matrix -> Int -> Vec.Vector
mgetColumn mat index = 
    if index >= (mstride mat)
    then error "given column index is larger than stride"
    else let md = mdata mat
             colnb = mColNb mat
         in Vec.VList [mtake mat (i * colnb + index) | i <- [0..(mRowNb mat)]]

mgetRow :: Matrix -> Int -> Vec.Vector
mgetRow mat index = 
    if index >= (mRowNb mat)
    then error "given row index is larger than number of rows of matrix"
    else let md = mdata mat
             colnb = mColNb mat
         in Vec.VList [mtake mat (i + colnb * index) | i <- [0..(mColNb mat)]]

msetRow :: Matrix -> Int -> Vec.Vector -> Matrix
msetRow mat index v =
    if index >= (mRowNb mat)
    then error "given row index is larger than number of rows of matrix"
    else let md = mdata mat
             colnb = mColNb mat
             Vec.VList vs = v
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

saxpy :: Double -> Vec.Vector -> Vec.Vector -> Vec.Vector
saxpy s x y = if (Vec.vsize x) /= (Vec.vsize y)
              then error $! Vec.sizeError x y "saxpy"
              else let Vec.VList xs = x
                       Vec.VList ys = y
                   in Vec.VList [x_ * s + y_ | x_ <- xs, y_ <- ys]

innerOuterProduct :: Int -> Vec.Vector -> Vec.Vector -> Matrix -> Matrix
innerOuterProduct i x y out =
    let a_i = mgetRow out i
        x_i = Vec.vget x i
        sxpy = saxpy x_i y a_i
    in msetRow out i sxpy

outerProduct :: Vec.Vector -> Vec.Vector -> Matrix -> Matrix
outerProduct x y out =
    let foldfn accOut i = innerOuterProduct i x y accOut
    in foldl' foldfn out [0..(mRowNb out)]


innerMatmul :: Int -> Matrix -> Matrix -> Matrix -> Matrix
innerMatmul !k !a !b !out =
    let a_k = mgetColumn a k
        b_k = mgetRow b k
    in outerProduct a_k b_k out

matmul :: Matrix -> Matrix -> Matrix
matmul !a !b =
    let outColNb = mColNb b
        clNb = mRowNb b
        colNb = mColNb a
        rowNb = mRowNb a
        outmat = mzero rowNb outColNb
    in if clNb /= colNb
       then error "column sizes don't match for matrices"
       else let foldfn accOut i = innerMatmul i a b accOut
            in foldl' foldfn outmat [0..colNb]

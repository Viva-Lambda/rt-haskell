{-# LANGUAGE BangPatterns #-}
-- ray module
module Math3D.Ray where

import Math3D.Vector

data Ray = Rd {
        origin :: Vector,
        direction :: Vector,
        rtime :: Double
    } deriving (Show, Eq)

zeroRay :: Int -> Ray
zeroRay !nbDims = Rd {origin = zeroV nbDims, direction = zeroV nbDims, rtime = 0.0}
zeroRay3 :: Ray
zeroRay3 = zeroRay 3

at :: Ray -> Double -> Vector
at !(Rd {origin = a, direction = b, rtime = _}) !c = add a (multiplyS b c)

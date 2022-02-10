{-# LANGUAGE BangPatterns #-}
-- ray module
module Math3D.Ray where

import Math3D.Vector
import Math3D.CommonOps

import Utility.BaseEnum

data Ray = Rd {
        origin :: Vector,
        direction :: Vector,
        rtime :: Double,
        wavelength :: WaveVal
    } deriving (Show, Eq)

zeroRay :: Int -> Ray
zeroRay !nbDims = Rd {origin = zeroV nbDims, 
                      direction = zeroV nbDims, 
                      rtime = 0.0,
                      wavelength = 0.0
                      }
zeroRay3 :: Ray
zeroRay3 = zeroRay 3

at :: Ray -> Double -> Vector
at !a !c = add (origin a) (multiplyS (direction a) c)

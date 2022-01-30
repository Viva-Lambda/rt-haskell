-- color input output, conversion module
module Color.ColorIO where

import GHC.Float hiding(clamp)

import Math3D.Vector
import Math3D.CommonOps

import Color.ColorInterface
import Color.Pixel
import Color.Colorable
import Spectral.SampledSpectrum
import Spectral.SampledDistribution

import Utility.Utils

-- print vector
vecToInt :: Vector -> [Int]
vecToInt v = map double2Int (vec2List v)

pixSpectrum2RGB :: PixelSpectrum -> Int -> Vector
pixSpectrum2RGB pspec sample_nb =
    let scale = 1.0 / (int2Double sample_nb)
        mfn v = multiplyS v scale
    in case pspec of
         PixSpecSampled s -> 
            if (all isNaN (vec2List (powers (sampled s))))
            then zeroV3
            else toRGB $! mfn (sampled s)
         PixSpecTrichroma (r,g,b) ->
            if (all isNaN [r,g,b])
            then zeroV3
            else mfn (fromList2Vec r [g, b])

writeColor :: PixelSpectrum -> Int -> String
writeColor pspec sample_nb =
    let sv = pixSpectrum2RGB pspec sample_nb
        -- now to change to rgb
        svgamma = vecScalarOp sqrt sv
        nsv = clampV svgamma 0.0 0.999
        nv = multiplyS nsv 256.0
        nvints = vecToInt nv
    in unwords $! map show nvints

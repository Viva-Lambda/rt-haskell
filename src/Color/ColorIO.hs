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

import Debug.Trace

-- print vector
vecToInt :: Vector -> [Int]
vecToInt v = map double2Int (vec2List v)

nanError :: String
nanError = let m1 = "NanError :: Pixel spectrum"
               m2 = m1 ++ " contains"
               m3 = m2 ++ " only nan values. This should not be the case."
           in m3

nanCheck :: Bool -> Vector -> Vector
nanCheck isAll v = if isAll 
                   then if all isNaN (vec2List v)
                        then traceStack (nanError ++ show v) v
                        else v
                   else if any isNaN (vec2List v)
                        then traceStack (nanError ++ show v) v
                        else v
                

pixSpectrum2RGB :: PixelSpectrum -> Int -> Vector
pixSpectrum2RGB pspec sample_nb =
    let scale = 1.0 / (int2Double sample_nb)
        mfn v = multiplyS v scale
        cdata = pixelSpectrumData pspec
    in if any isNaN (vec2List cdata)
       then traceStack nanError zeroV3
       else case pspec of
                 PixSpecSampled s -> 
                    let scaledwp = mfn (sampled s)
                        scaledSpectrum = SSpec { 
                                 spectrumType = spectrumType s,
                                 sampled = scaledwp
                                 }
                    in toRGB $! scaledSpectrum
                 PixSpecTrichroma (r,g,b) -> mfn (fromList2Vec r [g, b])

writeColor :: PixelSpectrum -> Int -> String
writeColor pspec sample_nb =
    let sv = pixSpectrum2RGB pspec sample_nb
        -- now to change to rgb
        svgamma = nanCheck False $! vecScalarOp sqrt sv
        nsv = nanCheck False $! clampV svgamma 0.0 0.999
        nv = nanCheck False $! multiplyS nsv 256.0
        nvints = vecToInt nv
    in unwords $! map show nvints

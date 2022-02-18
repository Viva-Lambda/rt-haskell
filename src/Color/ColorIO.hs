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
import Utility.BaseEnum

import Debug.Trace

-- print vector
vecToInt :: Vector -> [Int]
vecToInt v = map double2Int (vec2List v)

nanError :: String
nanError = let m1 = "NanError :: Pixel spectrum"
               m2 = m1 ++ " contains"
               m3 = m2 ++ " only nan values. This should not be the case."
           in m3


nanCheck :: NaNBehavior -> Vector -> Vector
nanCheck (REPLACE_NAN e) v = let repNan d = if isNaN d then e else d
                                 (x:xs) = map repNan (vec2List v)
                             in fromList2Vec x xs

nanCheck nb v
    | nb == ALL_NAN = if all isNaN (vec2List v)
                      then traceStack (nanError ++ show v) v
                      else v
    | nb == ANY_NAN = if any isNaN (vec2List v) 
                      then traceStack (nanError ++ show v) v
                      else v
    | otherwise = v


pixSpectrum2RGB :: PixelSpectrum -> Int -> Vector
pixSpectrum2RGB pspec sample_nb =
    let scale = 1.0 / int2Double sample_nb
        mfn v = multiplyS v scale
        cdata = pixelSpectrumData pspec
    in if any isNaN (vec2List cdata)
       then traceStack nanError zeroV3
       else case pspec of
                 PixSpecSampled s -> 
                    let -- scaledwp = mfn (sampled s)
                        scaledwp = sampled s
                        scaledSpectrum = SSpec { 
                                 spectrumType = spectrumType s,
                                 sampled = scaledwp
                                 }
                    in toRGB scaledSpectrum
                 PixSpecTrichroma (r,g,b) -> mfn (fromList2Vec r [g, b])

writeColor :: PixelSpectrum -> Int -> String
writeColor pspec sample_nb =
    let sv = pixSpectrum2RGB pspec sample_nb
        nvints = case pspec of
            PixSpecTrichroma _ ->
                let svgamma = nanCheck ANY_NAN $! vecScalarOp sqrt sv
                    nsv = nanCheck ANY_NAN $! clampV svgamma 0.0 0.999
                    nv = nanCheck ANY_NAN $! multiplyS nsv 256.0
                in vecToInt nv
            PixSpecSampled _ ->
                let
                    -- svgamma = nanCheck ZERO_NAN $! vecScalarOp sqrt sv
                    svgamma = sv
                    nsv = nanCheck ANY_NAN $! clampV svgamma 0.0 0.999
                    nv = nanCheck ANY_NAN $! multiplyS nsv 256.0
                in vecToInt nv
    in unwords $! map show nvints
    -- in traceStack (show sv) ""

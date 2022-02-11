{-# LANGUAGE BangPatterns #-}
-- sampled spectrum
module Spectral.SampledSpectrum where

import Spectral.SampledDistribution
import Spectral.PbrSpectrum
import Spectral.PbrtSpecdata
import Spectral.SpectrumUtils

-- transform spectrum to a color 
import Color.Colorable

-- vector
import Math3D.Vector
import Math3D.CommonOps

-- utilities
import Utility.Utils hiding (clamp)
import Utility.HelperTypes

-- third party
import GHC.Float hiding (clamp)
import Debug.Trace

data SpectrumType = REFLECTANCE
                  | ILLUMINANT
                  deriving (Show, Eq)

data SampledSpectrum = SSpec {
    -- Spectrum Type
    spectrumType :: SpectrumType,
    -- sampled data
    sampled :: SampledWavePower
    } deriving (Eq, Show)

zeroLikeSpectrum :: SampledSpectrum -> SampledSpectrum
zeroLikeSpectrum a = SSpec {spectrumType = spectrumType a,
                            sampled = zeroLike $! sampled a}

zeroSampledSpectrum :: SampledSpectrum
zeroSampledSpectrum = SSpec {
        spectrumType = REFLECTANCE,
        sampled = SampledWP (fromList2NL (0.0, -1.0) [])
    }

-- create spd from rgb values
fromRGB :: Double -> Double -> Double -> SpectrumType -> SampledWavePower
fromRGB r g b t =
    let zspd = zeroLike spdRGBRefl2SpectWhite
        --
        whiteR = spdRGBRefl2SpectWhite
        cyanR = spdRGBRefl2SpectCyan
        blueR = spdRGBRefl2SpectBlue
        magentaR = spdRGBRefl2SpectMagenta
        greenR = spdRGBRefl2SpectGreen
        yellowR = spdRGBRefl2SpectYellow
        redR = spdRGBRefl2SpectRed

        whiteI = spdRGBIllum2SpectWhite
        cyanI = spdRGBIllum2SpectCyan
        blueI = spdRGBIllum2SpectBlue
        magentaI = spdRGBIllum2SpectMagenta
        greenI = spdRGBIllum2SpectGreen
        yellowI = spdRGBIllum2SpectYellow
        redI = spdRGBIllum2SpectRed

        whiter s = multiplyS whiteR s
        whitei s = multiplyS whiteI s
        cond1 = r <= g && r <= b
        cond2 = g <= r && g <= b

        condfn f s b1 b2 spec1 spec2 spec3 =
            let zspd2 = add zspd (f s)
            in if b1 <= b2
               then let zspd3 = add zspd2 (multiplyS spec1 (b1 - s))
                    in add zspd3 (multiplyS spec2 (b2 - b1))
               else let zspd3 = add zspd2 (multiplyS spec1 (b2 - s))
                    in add zspd3 (multiplyS spec2 (b1 - b2))
        spval = case t of
                    REFLECTANCE ->
                        let reflZspd
                                | cond1 = condfn whiter r g b cyanR blueR greenR
                                | cond2 = condfn whiter g r b magentaR blueR redR
                                | otherwise = condfn whiter b g r yellowR greenR redR
                        in multiplyS reflZspd 0.94
        --
                    ILLUMINANT ->
                        let illumZspd
                                | cond1 = condfn whitei r g b cyanI blueI greenI
                                | cond2 = condfn whitei g r b magentaI blueI redI
                                | otherwise = condfn whitei b g r yellowI greenI redI
                        in multiplyS illumZspd 0.86445

    -- clamp spd value
        clampedSpd = clamp spval (0.0, float_max)
        lambdaStart = word2Float visibleWavelengthStart
        lambdaEnd = word2Float visibleWavelengthEnd
    in resampleFromWaves clampedSpd lambdaStart lambdaEnd spectralSampleStride

-- from a given spd
fromSampledWave :: SampledWavePower -> SpectrumType -> SampledSpectrum
fromSampledWave a b = SSpec {spectrumType = b, sampled = a}

-- from scalar
fromScalar :: Double -> SpectrumType -> SampledSpectrum
fromScalar a b = SSpec {spectrumType = b, sampled = fromRGB a a a b}

-- from rgb values
fromRGBModel :: Double -> Double -> Double -> SpectrumType -> SampledSpectrum
fromRGBModel r g b t = SSpec {spectrumType = t, sampled = fromRGB r g b t}

-- reflectance spectrum from rgb
reflectanceFromRGB :: Double -> Double -> Double -> SampledSpectrum
reflectanceFromRGB r g b = fromRGBModel r g b REFLECTANCE

-- illuminant spectrum from rgb
illuminantFromRGB :: Double -> Double -> Double -> SampledSpectrum
illuminantFromRGB r g b = fromRGBModel r g b ILLUMINANT

-- instances
spectrumTypeCheck :: SampledSpectrum -> SampledSpectrum -> (Bool, String)
spectrumTypeCheck a b = ((spectrumType a) == (spectrumType b),
                         "Spectrum types of sampled spectrums are not the same")

instance BinaryOps SampledSpectrum where
    elementwiseOp str f a b =
        let (isSame, s) = spectrumTypeCheck a b
        in if isSame == False
           then traceStack (s ++ " :: " ++ str) zeroSampledSpectrum
           else let ap = sampled a
                    bp = sampled b
                    ndata = elementwiseOp str f ap bp
                in SSpec { spectrumType = spectrumType a, 
                           sampled = ndata }

    elementwiseScalarOp str f a = let ap = sampled a
                                      ndata = elementwiseScalarOp str f ap
                                  in SSpec { spectrumType = spectrumType a,
                                             sampled = ndata }
    -- division
    divide a b =
        let (isSame, str) = spectrumTypeCheck a b
        in if isSame == False
           then traceStack (str) zeroSampledSpectrum
           else let ap = sampled a
                    bp = sampled b
                    ndata = divide ap bp
                in SSpec {
                    spectrumType = spectrumType a,
                    sampled = ndata
                }

--
instance Colorable SampledSpectrum where
    toXYZ (SSpec {spectrumType = _, 
                  sampled = a}
            ) = let wstart = minWavelength a
                    wend = maxWavelength a
                    waves = wavelengths a
                    wsize = lengthNL waves
                    foldfn acc wave = let (x, y, z) = acc
                                          sx = evaluateWave wave spdX
                                          sy = evaluateWave wave spdY
                                          sz = evaluateWave wave spdZ
                                          p = evaluateWave wave a
                                      in (x + sx * p, y + sy * p, z + sz * p)
                    (x, y, z) = foldlNL foldfn (0.0, 0.0, 0.0) waves
                    ciey = cieYIntegral * (int2Double wsize)
                    lambdaStart = word2Float visibleWavelengthStart
                    lambdaEnd = word2Float visibleWavelengthEnd
                    waveScale = (float2Double (lambdaEnd - lambdaStart)) / ciey
                in fromList2Vec (x * waveScale) [y * waveScale, z * waveScale]
    toRGB a = 
        let xyzval = toXYZ a
            -- rgbval = xyzval
            rgbval = xyz2rgb_pbr xyzval
            -- rgbval = xyz2rgb_cie xyzval
            -- rgbval = xyz2rgb_srgb xyzval
            msg x r = let str = "xyz or rgb value contains nans :: XYZ "
                          str2 = str ++ show x
                          str3 = str2 ++ " RGB " ++ show r
                      in str3
            check vec = any isNaN (vec2List vec)
        in if (check xyzval) || (check rgbval)
           then traceStack (msg xyzval rgbval) zeroV3
           else rgbval

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
        sampled = SampledWP (fromList2NL (0, -1.0) [])
    }

-- create spd from rgb values
fromRGB :: Double -> Double -> Double -> SpectrumType -> SampledWavePower
fromRGB r g b t =
    let zspd = zeroLike spdRGBRefl2SpectWhite
        --
        normalWhiteR = normalize spdRGBRefl2SpectWhite
        normalCyanR = normalize spdRGBRefl2SpectCyan
        normalBlueR = normalize spdRGBRefl2SpectBlue
        normalMagentaR = normalize spdRGBRefl2SpectMagenta
        normalGreenR = normalize spdRGBRefl2SpectGreen
        normalYellowR = normalize spdRGBRefl2SpectYellow
        normalRedR = normalize spdRGBRefl2SpectRed

        normalWhiteI = normalize spdRGBRefl2SpectWhite
        normalCyanI = normalize spdRGBRefl2SpectCyan
        normalBlueI = normalize spdRGBRefl2SpectBlue
        normalMagentaI = normalize spdRGBRefl2SpectMagenta
        normalGreenI = normalize spdRGBRefl2SpectGreen
        normalYellowI = normalize spdRGBRefl2SpectYellow
        normalRedI = normalize spdRGBRefl2SpectRed

        whiter s = multiplyS normalWhiteR s
        whitei s = multiplyS normalWhiteI s
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
                        let reflZspd = if cond1
                                       then condfn whiter r g b normalCyanR normalBlueR normalGreenR
                                       else if cond2
                                            then condfn whiter g r b normalMagentaR normalBlueR normalRedR
                                            else condfn whiter b g r normalYellowR normalGreenR normalRedR
                        in multiplyS reflZspd 0.94
        --
                    ILLUMINANT ->
                        let illumZspd = if cond1
                                        then condfn whitei r g b normalCyanI normalBlueI normalGreenI
                                        else if cond2
                                             then condfn whitei g r b normalMagentaI normalBlueI normalRedI
                                             else condfn whitei b g r normalYellowI normalGreenI normalRedI
                        in multiplyS illumZspd 0.86445

    -- clamp spd value
        clampedSpd = clamp spval (0.0, float_max)
    in resampleFromWaves clampedSpd visible_lambda_start visible_lambda_end spd_nb_sample

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
                    wsize = int2Word $! lengthNL waves
                    sxNorm = normalize $! resampleFromWaves spdX wstart wend wsize
                    syNorm = normalize $! resampleFromWaves spdY wstart wend wsize
                    szNorm = normalize $! resampleFromWaves spdZ wstart wend wsize
                    foldfn acc wave = let (x, y, z) = acc
                                          sx = evaluateWave wave sxNorm
                                          sy = evaluateWave wave syNorm
                                          sz = evaluateWave wave szNorm
                                          p = evaluateWave wave a
                                      in (x + sx * p, y + sy * p, z + sz * p)
                    (x, y, z) = foldlNL foldfn (0.0, 0.0, 0.0) waves
                    ciey = cieYIntegral * (word2Double wsize)
                    waveScale = (word2Double (wend - wstart)) / ciey
                in fromList2Vec (x * waveScale) [y * waveScale, z * waveScale]
    toRGB a = xyz2rgb_pbr $! toXYZ a

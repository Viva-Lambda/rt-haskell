-- pbr spectrum: translated mostly from pbrt book
module Spectral.PbrSpectrum where

import Math3D.Vector

import Spectral.SampledDistribution hiding (clamp)
import Spectral.PbrtSpecdata

import Utility.HelperTypes
import Utility.Utils
import Utility.BaseEnum

-- 
import qualified Data.Map as DMap
import Data.List
import GHC.Float hiding (clamp)

areWavesSorted :: NonEmptyList WaveVal -> Bool
areWavesSorted wavesnl = 
    let (w:ws) = nl2List wavesnl
    in fn (w:ws)
    where fn [] = True
          fn (w:w2:[]) = not (w > w2)
          fn (w:ws) = if w > head ws
                      then False
                      else fn ws


-- average spectrum
averagePowerWaves :: NonEmptyList PowerVal -> NonEmptyList WaveVal -> WaveVal -> WaveVal -> PowerVal
averagePowerWaves powers waves waveLStart waveLEnd =
    let maxWave = lastNL waves
        minWave = headNL waves
        isSmaller = waveLStart == minWave
        isBigger = waveLEnd == maxWave
        nbElements = lengthNL powers
        minWavePower = headNL powers
        maxWavePower = lastNL powers
        average
            | isSmaller = minWavePower
            | isBigger = maxWavePower
            | nbElements == 1 = minWavePower
            | otherwise =
                --
                let psum1 = 0.0
                    psum2 = minWavePower * (float2Double (minWave - waveLStart))
                    psum3 = maxWavePower * (float2Double (waveLEnd - maxWave))
                    psum4 = if waveLStart < minWave
                            then psum1 + psum2
                            else psum1
                    psum5 = if waveLEnd > maxWave
                            then psum4 + psum3
                            else psum4
                    -- split the waves waveStart > waves[i + 1]
                    nwaves = nl2List waves
                    findWaveIndex index = if waveLStart > (getNL waves (index + 1))
                                          then findWaveIndex (index + 1)
                                          else index
                    waveStartIndex = findWaveIndex 0
                    interpSeg w j =
                        let t1 = w - (getNL waves j)
                            t2 = (getNL waves (j + 1)) - (getNL waves j)
                            t3 = (float2Double t1) / (float2Double t2)
                            t4 = getNL powers j
                            t5 = getNL powers (j + 1)
                        in mix t3 t4 t5
                    --
                    forsum ix sval =
                        let cond1 = (ix + 1) < (lengthNL waves)
                            cond2 = waveLEnd >= (getNL waves ix)
                        in if cond1 && cond2
                           then let segLambS = maximum [waveLStart, (getNL waves ix)]
                                    segLambE = minimum [waveLEnd, (getNL waves (ix + 1))]
                                    segS = interpSeg segLambS ix
                                    segE = interpSeg segLambE ix
                                    segLambDiff = float2Double (segLambE - segLambS)
                                    iseg = (segS + segE) * segLambDiff
                                in forsum (ix + 1) (iseg * 0.5 + sval)
                           else sval
                    npsum = forsum waveStartIndex psum5
                in npsum / (float2Double (waveLEnd - waveLStart))
    -- first checks
    in average

--
averageSpectrum :: SampledWavePower -> WaveVal -> WaveVal -> PowerVal
averageSpectrum b waveStart waveEnd =
    let a = sortSampledWavePower b
        (VList ps) = powers a
    in averagePowerWaves ps (wavelengths a) waveStart waveEnd


-- resample wave power
resampleWavePower :: NonEmptyList PowerVal -> NonEmptyList WaveVal -> WaveVal -> WaveVal -> Word -> SampledWavePower
resampleWavePower pwrs wvs lambdaMin lambdaMax outSize =
    let wsize = lengthNL wvs
        wdelta = (lambdaMax - lambdaMin) / (word2Float (outSize - 1))
        -- clamp wave length fn
        wlStartClamp index
            | index == (-1) = lambdaMin - wdelta
            | index == wsize = lambdaMax + wdelta
            | otherwise = getNL wvs index
        -- clamp power fn
        powerClamp index = getNL pwrs (clamp index 0 (wsize - 1))
        -- resampling fn
        resamplingFn waveVal =
            let halfDelta = double2Float $! wdelta / 2.0
                waveValDDiff = waveVal - wdelta
                waveValDSum = waveVal + wdelta
                (v, inRange)
                    | (waveVal + halfDelta) <= (headNL wvs) = (headNL pwrs, True)
                    | (waveVal - halfDelta) >= (lastNL wvs) = (lastNL pwrs, True)
                    | wsize == 1 = (headNL pwrs, True)
                    | otherwise = (0.0, False)
                samplingResult
                    | inRange = v
                    | otherwise =
                        let starti = if waveValDDiff < (headNL wvs)
                                     then -1.0
                                     else let intervalFn i = 
                                                (word2Double (getNL wvs i)) <= waveValDDiff
                                          in findInterval wsize intervalFn
                            endi
                                | waveValDSum > (lastNL wvs) = wsize
                                | otherwise =
                                    let estart = if starti > 0
                                                 then starti
                                                 else 0.0
                                        fix e = let ef = e < wsize
                                                    wf = waveValDSum
                                                    wl = getNL wvs e
                                                    cond = ef && (wf > wl)
                                                in if cond
                                                   then fix (e + 1)
                                                   else e
                                    in fix estart
                                     --
                            cond1 = (endi - starti) == 2
                            cond2 = (wlStartClamp starti) <= (double2Word waveValDDiff)
                            cond3 = (getNL wvs (starti + 1)) == waveVal
                            cond4 = (wlStartClamp endi) >= (double2Word waveValDSum)
                            result 
                               | cond1 && cond2 && cond3 && cond4 = getNL pwrs (starti + 1)
                               | (endi - starti) == 1 = 
                                   let wst = wlStartClamp starti
                                       wnd = wlStartClamp endi
                                       t = (waveVal - wst) / (wnd - wst)
                                       pwrCS = powerClamp starti
                                       pwrCE = powerClamp endi
                                   in mix t pwrCS pwrCE
                               | otherwise =
                                   let wvds = waveVal - halfDelta
                                       wvde = waveVal + halfDelta
                                   in averagePowerWaves pwrs wvs wvds wvde
                        in result

            in samplingResult
        --
        mapfn oset = let firstT = (word2Double oset) / (word2Double (outSize - 1))
                         nwave = mix firstT (word2Double lambdaMin) (word2Double lambdaMax)
                         nw = double2Word nwave
                         npwr = resamplingFn nw
                     in (npwr, nw)
        ((npwr:npwrs), (nwave:nwaves)) = unzip $! map mapfn [0..(outSize - 1)]
    in fromWavesPowers (fromList2NL npwr npwrs) (fromList2NL nwave nwaves)


resampleFromSampledWavePower :: SampledWavePower -> SampledWavePower -> SampledWavePower
resampleFromSampledWavePower a b =
    let waves1 = wavelengths a
        waves2 = wavelengths b
        powers1 = powers a
        powers2 = powers b
        wsize1 = lengthNL waves1
        wsize2 = lengthNL waves2
        wsize = if wsize1 == wsize2
                then wsize1
                else wsize2
        wvs = if wsize == wsize1
              then waves1
              else waves2
        (VList pwrs) = if wsize == wsize1
                       then powers1
                       else powers2
        maxwv1 = maxWavelength a
        maxwv2 = maxWavelength b
        maxwv = if maxwv1 > maxwv2
                then maxwv1
                else maxwv2
        mnwv1 = minWavelength a
        mnwv2 = minWavelength b
        mnwv = if mnwv1 > mnwv2
               then mnwv1
               else mnwv2
        --
    in resampleWavePower pwrs wvs mnwv maxwv (int2Word wsize)

resampleFromWaves :: SampledWavePower -> WaveVal -> WaveVal -> Word -> SampledWavePower
resampleFromWaves a waveStart waveEnd outSize =
    let VList pwrs = powers a
        wvs = wavelengths a
    in resampleWavePower pwrs wvs waveStart waveEnd outSize

resampleFromOutSize :: SampledWavePower -> Word -> SampledWavePower
resampleFromOutSize a outSize =
    let waveStart = minWavelength a
        waveEnd = maxWavelength a
    in resampleFromWaves a waveStart waveEnd outSize

resampleCurrentWavePower :: SampledWavePower -> SampledWavePower
resampleCurrentWavePower a = resampleFromSampledWavePower a a

xyzAverageSpectrum :: CIETrichroma -> SampledWavePower
xyzAverageSpectrum a =
    let mapfn i = let nSpectralSamples = word2Double spectralSampleNb
                      wl0 = mix ((word2Double i) / nSpectralSamples) 
                              visibleWavelengthStart visibleWavelengthEnd
                      wl1 = mix ((word2Double (i + 1)) / nSpectralSamples) 
                              visibleWavelengthStart visibleWavelengthEnd
                      averagefn pwrs = averagePowerWaves pwrs cieLambda wl0 wl1
                  in case a of
                        CIE_X -> averagefn cieX
                        CIE_Y -> averagefn cieY
                        CIE_Z -> averagefn cieZ
        (w:ws) = [0..spectralSampleNb]
        (x:xs) = map mapfn (w:ws)
    in fromWavesPowers (fromList2NL x xs) (fromList2NL w ws)


spdX :: SampledWavePower
spdX = xyzAverageSpectrum CIE_X

spdY :: SampledWavePower
spdY = xyzAverageSpectrum CIE_Y

spdZ :: SampledWavePower
spdZ = xyzAverageSpectrum CIE_Z

rgb2Spect :: Rgb2Spect -> SampledWavePower
rgb2Spect a = 
    let mapfn i = let nSpectralSamples = word2Double spectralSampleNb
                      wl0 = mix ((word2Double i) / nSpectralSamples) 
                              visibleWavelengthStart visibleWavelengthEnd
                      wl1 = mix ((word2Double (i + 1)) / nSpectralSamples) 
                              visibleWavelengthStart visibleWavelengthEnd
                      averagefn pwrs = averagePowerWaves pwrs rgb2SpectLambda wl0 wl1
                  in case a of
                        REFL_WHITE -> averagefn rgbRefl2SpectWhite
                        REFL_CYAN -> averagefn rgbRefl2SpectCyan
                        REFL_MAGENTA -> averagefn rgbRefl2SpectMagenta
                        REFL_YELLOW -> averagefn rgbRefl2SpectYellow
                        REFL_RED -> averagefn rgbRefl2SpectRed
                        REFL_GREEN -> averagefn rgbRefl2SpectGreen
                        REFL_BLUE -> averagefn rgbRefl2SpectBlue
                        ILLUM_WHITE -> averagefn rgbIllum2SpectWhite
                        ILLUM_CYAN -> averagefn rgbIllum2SpectCyan
                        ILLUM_MAGENTA -> averagefn rgbIllum2SpectMagenta
                        ILLUM_YELLOW -> averagefn rgbIllum2SpectYellow
                        ILLUM_RED -> averagefn rgbIllum2SpectRed
                        ILLUM_GREEN -> averagefn rgbIllum2SpectGreen
                        ILLUM_BLUE -> averagefn rgbIllum2SpectBlue
        (w:ws) = [0..spectralSampleNb]
        (x:xs) = map mapfn (w:ws)
    in fromWavesPowers (fromList2NL x xs) (fromList2NL w ws)


spdRGBRefl2SpectWhite :: SampledWavePower
spdRGBRefl2SpectWhite = rgb2Spect REFL_WHITE

spdRGBRefl2SpectCyan :: SampledWavePower
spdRGBRefl2SpectCyan = rgb2Spect REFL_CYAN

spdRGBRefl2SpectMagenta :: SampledWavePower
spdRGBRefl2SpectMagenta = rgb2Spect REFL_MAGENTA

spdRGBRefl2SpectYellow :: SampledWavePower
spdRGBRefl2SpectYellow = rgb2Spect REFL_YELLOW

spdRGBRefl2SpectRed :: SampledWavePower
spdRGBRefl2SpectRed = rgb2Spect REFL_RED

spdRGBRefl2SpectGreen :: SampledWavePower
spdRGBRefl2SpectGreen = rgb2Spect REFL_GREEN

spdRGBRefl2SpectBlue :: SampledWavePower
spdRGBRefl2SpectBlue = rgb2Spect REFL_BLUE

spdRGBIllum2SpectWhite :: SampledWavePower
spdRGBIllum2SpectWhite = rgb2Spect ILLUM_WHITE

spdRGBIllum2SpectCyan :: SampledWavePower
spdRGBIllum2SpectCyan = rgb2Spect ILLUM_CYAN

spdRGBIllum2SpectBlue :: SampledWavePower
spdRGBIllum2SpectBlue = rgb2Spect ILLUM_BLUE

spdRGBIllum2SpectGreen :: SampledWavePower
spdRGBIllum2SpectGreen = rgb2Spect ILLUM_GREEN

spdRGBIllum2SpectRed :: SampledWavePower
spdRGBIllum2SpectRed = rgb2Spect ILLUM_RED

spdRGBIllum2SpectMagenta :: SampledWavePower
spdRGBIllum2SpectMagenta = rgb2Spect ILLUM_MAGENTA

spdRGBIllum2SpectYellow :: SampledWavePower
spdRGBIllum2SpectYellow = rgb2Spect ILLUM_YELLOW

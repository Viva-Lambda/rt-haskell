-- pbr spectrum: translated mostly from pbrt book
module Spectral.PbrSpectrum where

import Math3D.Vector

import Spectral.SampledDistribution hiding (clamp)
import Spectral.PbrtSpecdata

import Utility.HelperTypes
import Utility.Utils

-- 
import qualified Data.Map as DMap
import Data.List
import GHC.Float hiding (clamp)

-- average spectrum
averagePowerWaves :: NonEmptyList Double -> NonEmptyList Word -> Word -> Word -> Double
averagePowerWaves powers waves waveLStart waveLEnd =
    let 
        maxWave = lastNL waves
        minWave = headNL waves
        isSmaller = waveLStart == minWave
        isBigger = waveLEnd == maxWave
        nbElements = lengthNL powers
        minWavePower = headNL powers
        maxWavePower = lastNL powers
    -- first checks
    in if isSmaller
       then minWavePower
       else if isBigger
            then maxWavePower
            else if nbElements == 1
                 then minWavePower
                 else let psum1 = 0.0
                          psum2 = minWavePower * (word2Double (minWave - waveLStart))
                          psum3 = maxWavePower * (word2Double (maxWave - waveLEnd))
                          psum4 = if waveLStart < minWave
                                  then psum1 + psum2
                                  else psum1
                          psum5 = if waveLEnd > maxWave
                                  then psum4 + psum3
                                  else psum4
                          -- split the waves waveStart > waves[i + 1]
                          nwaves = nl2List waves
                          findPred w = if w >= waveLStart
                                       then True
                                       else False
                          waveStartIndex = case findIndex findPred nwaves of
                                                Just i -> i
                                                Nothing -> 0
                          interpSeg w j =
                            let t1 = (w - (getNL waves j))
                                t2 = (getNL waves (j + 1)) - (getNL waves j)
                                t3 = (word2Double t1) / (word2Double t2)
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
                                        segLambDiff = word2Double (segLambE - segLambS)
                                        iseg = (segS + segE) * segLambDiff
                                    in forsum (ix + 1) (iseg * 0.5 + sval)
                               else sval
                          npsum = forsum waveStartIndex psum5
                      in npsum / (word2Double (waveLEnd - waveLStart))

--
averageSpectrum :: SampledWavePower -> Word -> Word -> Double
averageSpectrum a waveStart waveEnd = 
    let (VList ps) = powers a
    in averagePowerWaves ps (wavelengths a) waveStart waveEnd


-- resample wave power
resampleWavePower :: NonEmptyList Double -> NonEmptyList Word -> Word -> Word -> Word -> SampledWavePower
resampleWavePower pwrs wvs waveStart waveEnd outSize =
    let wsize = lengthNL wvs
        wdelta = (word2Double (waveEnd - waveStart)) / (int2Double (wsize - 1))
        -- clamp wave length fn
        wlStartClamp index = if index == (-1)
                             then double2Word ((word2Double waveStart) - wdelta)
                             else if index == wsize
                                  then double2Word ((word2Double waveEnd) + wdelta)
                                  else getNL wvs index
        -- clamp power fn
        powerClamp index = clamp index 0 (wsize - 1)
        -- resampling fn
        sampler waveVal =
            let halfDelta = wdelta / 2.0
                waveValD = word2Double waveVal
                waveValDDiff = waveValD - wdelta
                waveValDSum = waveValD + wdelta
                (v, inRange) = if (double2Word (waveValD + halfDelta)) <= (headNL wvs)
                               then (headNL pwrs, True)
                               else if (double2Word (waveValD - halfDelta)) >= (lastNL wvs)
                                    then (lastNL pwrs, True)
                                    else (0.0, False)
            in if inRange
               then v
               else if wsize == 1
                    then headNL pwrs
                    else let starti = if waveValDDiff < (word2Double (headNL wvs))
                                      then -1
                                      else let intervalFn i =
                                                (word2Double (getNL wvs i)) <= waveValDDiff
                                           in findInterval wsize intervalFn
                             endi = if waveValDSum > (word2Double (lastNL wvs))
                                    then wsize
                                    else let estart = if starti > 0
                                                      then starti
                                                      else 0
                                             fix e = let ef = e < wsize
                                                         wf = waveValDSum
                                                         wl = word2Double $ getNL wvs e
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
                         in if cond1 && cond2 && cond3 && cond4
                            then getNL pwrs (starti + 1)
                            else if (endi - starti) == 1
                                 then let wst = word2Double $! wlStartClamp starti
                                          wnd = word2Double $! wlStartClamp endi
                                          t = (waveValD - wst) / (wnd - wst)
                                          pwrCS = int2Double $ powerClamp starti
                                          pwrCE = int2Double $ powerClamp endi
                                      in mix t pwrCS pwrCE
                                 else let wvds = double2Word $ waveValD - halfDelta
                                          wvde = double2Word $ waveValD + halfDelta
                                      in averagePowerWaves pwrs wvs wvds wvde
        --
        foldfn acc oset = let (opwrs, owvs) = acc
                              ot = (word2Double oset) / (word2Double (outSize - 1))
                              nwave = mix ot (word2Double waveStart) (word2Double waveEnd)
                              nw = double2Word nwave
                              npwr = sampler nw
                          in (opwrs ++ [npwr], owvs ++ [nw])
        ((npwr:npwrs), (nwave:nwaves)) = foldl foldfn ([], []) [0..(outSize - 1)]
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

resampleFromWaves :: SampledWavePower -> Word -> Word -> Word -> SampledWavePower
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

rgb2Spect :: NonEmptyList Double -> NonEmptyList Double -> SampledWavePower
rgb2Spect rgbWavesD rgbSpect =
    let sizeCheck = (lengthNL rgbWavesD) == (lengthNL rgbSpect)
        rgbWaves = mapNL double2Word rgbWavesD
        minWv = minNL rgbWaves
        maxWv = maxNL rgbWaves
        pwrs = rgbSpect
        nbSample = word2Double spectralSampleNb
        visibleStart = word2Double visibleWavelengthStart
        visibleEnd = word2Double visibleWavelengthEnd
        foldfn acc i = let wl0 = mix ((int2Double i) / nbSample) visibleStart visibleEnd
                           wl1 = mix ((int2Double (i + 1)) / nbSample) visibleStart visibleEnd
                           wl0w = double2Word wl0
                           wl1w = double2Word wl1
                           p = averagePowerWaves pwrs rgbWaves wl0w wl1w
                           wl = (wl0 + wl1) / 2.0
                           (ps, ws) = acc
                       in (ps ++ [p], ws ++ [double2Word wl])
        ((p:ps), (w:ws)) = foldl foldfn ([], []) [0..(word2Int spectralSampleNb)]
    in fromWavesPowers (fromList2NL p ps) (fromList2NL w ws)

spdRGBRefl2SpectWhite :: SampledWavePower
spdRGBRefl2SpectWhite = rgb2Spect rgb2SpectLambda rgbRefl2SpectWhite

spdRGBRefl2SpectCyan :: SampledWavePower
spdRGBRefl2SpectCyan = rgb2Spect rgb2SpectLambda rgbRefl2SpectCyan

spdRGBRefl2SpectMagenta :: SampledWavePower
spdRGBRefl2SpectMagenta = rgb2Spect rgb2SpectLambda rgbRefl2SpectMagenta

spdRGBRefl2SpectYellow :: SampledWavePower
spdRGBRefl2SpectYellow = rgb2Spect rgb2SpectLambda rgbRefl2SpectYellow

spdRGBRefl2SpectRed :: SampledWavePower
spdRGBRefl2SpectRed = rgb2Spect rgb2SpectLambda rgbRefl2SpectRed

spdRGBRefl2SpectGreen :: SampledWavePower
spdRGBRefl2SpectGreen = rgb2Spect rgb2SpectLambda rgbRefl2SpectGreen

spdRGBRefl2SpectBlue :: SampledWavePower
spdRGBRefl2SpectBlue = rgb2Spect rgb2SpectLambda rgbRefl2SpectBlue

spdRGBIllum2SpectWhite :: SampledWavePower
spdRGBIllum2SpectWhite = rgb2Spect rgb2SpectLambda rgbIllum2SpectWhite

spdRGBIllum2SpectCyan :: SampledWavePower
spdRGBIllum2SpectCyan = rgb2Spect rgb2SpectLambda rgbIllum2SpectCyan

spdRGBIllum2SpectBlue :: SampledWavePower
spdRGBIllum2SpectBlue = rgb2Spect rgb2SpectLambda rgbIllum2SpectBlue

spdRGBIllum2SpectGreen :: SampledWavePower
spdRGBIllum2SpectGreen = rgb2Spect rgb2SpectLambda rgbIllum2SpectGreen

spdRGBIllum2SpectRed :: SampledWavePower
spdRGBIllum2SpectRed = rgb2Spect rgb2SpectLambda rgbIllum2SpectRed

spdRGBIllum2SpectMagenta :: SampledWavePower
spdRGBIllum2SpectMagenta = rgb2Spect rgb2SpectLambda rgbIllum2SpectMagenta

spdRGBIllum2SpectYellow :: SampledWavePower
spdRGBIllum2SpectYellow = rgb2Spect rgb2SpectLambda rgbIllum2SpectYellow

spdX :: SampledWavePower 
spdX = rgb2Spect cieLambdaReal cieX

spdY :: SampledWavePower
spdY = rgb2Spect cieLambdaReal cieY

spdZ :: SampledWavePower
spdZ = rgb2Spect cieLambdaReal cieZ

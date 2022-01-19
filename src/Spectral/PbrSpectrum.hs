-- pbr spectrum: translated mostly from pbrt book
module Spectral.PbrSpectrum where

import Math3D.Vector

import Spectral.SampledDistribution hiding (clamp)

import Utility.HelperTypes
import Utility.Utils

-- 
import qualified Data.Map as DMap
import Data.List
import GHC.Float hiding (clamp)

-- average spectrum
averagePowerWaves :: NonEmptyList Double -> NonEmptyList Word -> Word -> Word -> Double
averagePowerWaves powers waves waveStart waveEnd =
    let 
        maxWave = lastNL waves
        minWave = headNL waves
        isSmaller = waveStart == minWave
        isBigger = waveEnd == maxWave
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
                          psum2 = minWavePower * (word2Double (minWave - waveStart))
                          psum3 = maxWavePower * (word2Double (maxWave - waveEnd))
                          psum4 = if waveStart < minWave
                                  then psum1 + psum2
                                  else psum1
                          psum5 = if waveEnd > maxWave
                                  then psum4 + psum3
                                  else psum4
                          -- split the waves waveStart > waves[i + 1]
                          nwaves = nl2List waves
                          waveStartIndex = maximum $ findIndices (< waveStart) nwaves
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
                                cond2 = waveEnd >= (getNL waves ix)
                            in if cond1 && cond2
                               then sval
                               else let segLambS = maximum [waveStart,(getNL waves ix)]
                                        segLambE = minimum [waveEnd,(getNL waves (ix + 1))]
                                        segS = interpSeg segLambS ix
                                        segE = interpSeg segLambE ix
                                        segLambDiff = word2Double (segLambE - segLambS)
                                        iseg = (segS + segE) * segLambDiff
                                    in forsum (ix + 1) (iseg * 0.5 + sval)
                          npsum = forsum waveStartIndex psum5
                      in npsum / (word2Double (waveEnd - waveStart))

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
                             cond3 = (getNL wvs (starti)) == waveVal
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

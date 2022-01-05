-- pbr spectrum
module Spectral.PbrSpectrum where

import Math3D.Vector

import Spectral.SampledDistribution

import Utility.HelperTypes
import Utility.Utils

-- 
import qualified Data.Map as DMap
import Data.List
import GHC.Float

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

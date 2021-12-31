{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- sampled spectral distribution
module Spectral.SampledDistribution where

import Math3D.Vector

import Utility.HelperTypes
import Utility.Utils

-- third party
import qualified Data.Map as DMap
import Data.Word

type WavePower = DMap.Map Word32 Double

data SpecPowerDistribution = SPD {
    waveStart :: Double,
    waveEnd :: Double,
    distribution :: WavePower
    } deriving (Eq, Show)

wavelengths :: SpecPowerDistribution -> NonEmptyList Word32
wavelengths a = let dmap = distribution a
                    (w:ws) = DMap.keys dmap
                in fromList2NL w ws

powers :: SpecPowerDistribution -> NonEmptyList Double
powers a = let dmap = distribution a
               (p:ps) = DMap.elems dmap
           in fromList2NL p ps

minmaxPower :: ([Double] -> Double) -> SpecPowerDistribution -> Double
minmaxWavelength :: ([Word32] -> Word32) -> SpecPowerDistribution -> Word32

minmaxPower f a = let dmap = distribution a
                      ps = DMap.elems dmap
                  in f ps

minmaxWavelength f a = let dmap = distribution a
                           ps = DMap.keys dmap
                       in f ps

maxPower :: SpecPowerDistribution -> Double
maxPower a = minmaxPower maximum a
minPower :: SpecPowerDistribution -> Double
minPower a = minmaxPower minimum a

maxWavelength :: SpecPowerDistribution -> Word32
maxWavelength a = minmaxWavelength maximum a

minWavelength :: SpecPowerDistribution -> Word32
minWavelength a = minmaxWavelength minimum a


-- interpolate a spectral power distribution
interpolateSpd :: SpecPowerDistribution -> (Double, Double) -> SpecPowerDistribution
interpolateSpd a (mn, mx) =
    let minmaxer f = f [mn, mx]
        amin = minmaxer minimum
        amax = minmaxer maximum
        pmin = minPower a
        pmax = maxPower a
        dist = distribution a
        ps = DMap.elems dist
        interpolator p = interp (pmin, pmax) (amin, amax) p
        interpolatedPowers = map interpolator ps
        skeys = DMap.keys dist
        ndist = DMap.fromList $ zip skeys interpolatedPowers
    in SPD {waveStart = waveStart a, waveEnd = waveEnd a, distribution = ndist}

-- clamp a spectral power distribution

clampSpd :: SpecPowerDistribution -> (Double, Double) -> SpecPowerDistribution
clampSpd a (mn, mx) =
    let dist = distribution a
        ps = DMap.elems dist
        clamper p = clamp p mn mx
        clampedPowers = map clamper ps
        skeys = DMap.keys dist
        ndist = DMap.fromList $ zip skeys clampedPowers
    in SPD {waveStart = waveStart a, waveEnd = waveEnd a, distribution = ndist}

normalizeSpd :: SpecPowerDistribution -> SpecPowerDistribution
normalizeSpd a = interpolateSpd a (0.0, 1.0)

-- integrateSpd :: SpecPowerDistribution -> SpecPowerDistribution -> SpecPowerDistribution
-- integrateSpd a b =

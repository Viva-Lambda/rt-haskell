{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- sampled spectral distribution
module Spectral.SampledDistribution where

import Math3D.Vector

import Utility.HelperTypes
import Utility.Utils as Ut 

-- third party
import qualified Data.Map as DMap
import Data.Word
import Debug.Trace

type SampledWavePower = DMap.Map Word Double


wavelengths :: SampledWavePower -> NonEmptyList Word
wavelengths dmap = let (w:ws) = DMap.keys dmap in fromList2NL w ws

powers :: SampledWavePower -> Vector
powers dmap = let (p:ps) = DMap.elems dmap in fromList2Vec p ps

minmaxPower :: ([Double] -> Double) -> SampledWavePower -> Double
minmaxWavelength :: ([Word] -> Word) -> SampledWavePower -> Word

minmaxPower f dmap = let ps = DMap.elems dmap in f ps

minmaxWavelength f dmap = let ps = DMap.keys dmap in f ps

maxPower :: SampledWavePower -> Double
maxPower a = minmaxPower maximum a
minPower :: SampledWavePower -> Double
minPower a = minmaxPower minimum a

maxWavelength :: SampledWavePower -> Word
maxWavelength a = minmaxWavelength maximum a

minWavelength :: SampledWavePower -> Word
minWavelength a = minmaxWavelength minimum a


-- interpolate a spectral power distribution
interpolate :: SampledWavePower -> (Double, Double) -> SampledWavePower
interpolate a (mn, mx) =
    let minmaxer f = f [mn, mx]
        amin = minmaxer minimum
        amax = minmaxer maximum
        pmin = minPower a
        pmax = maxPower a
        ps = DMap.elems a
        interpolator p = Ut.interp (pmin, pmax) (amin, amax) p
        interpolatedPowers = map interpolator ps
        skeys = DMap.keys a
        ndist = DMap.fromList $ zip skeys interpolatedPowers
    in ndist

-- clamp a spectral power distribution

clamp :: SampledWavePower -> (Double, Double) -> SampledWavePower
clamp a (mn, mx) =
    let ps = DMap.elems a
        clamper p = Ut.clamp p mn mx
        clampedPowers = map clamper ps
        skeys = DMap.keys a
        ndist = DMap.fromList $ zip skeys clampedPowers
    in ndist

normalize :: SampledWavePower -> SampledWavePower
normalize a = interpolate a (0.0, 1.0)

evaluateWave :: Word -> SampledWavePower -> Double

evaluateWave wave a =
    -- in range
    let inRange = (wave >= (minWavelength a)) && (wave <= (maxWavelength a))
        isMember = DMap.member wave a
        getWave w = (DMap.!) a w
    in if isMember -- wavelength is a member, we can access to its power
       then getWave wave
       else if inRange -- wavelength is in range, we can interpolate power values
            then let (smallerMap, largerMap) = DMap.split wave a
                     (smallMaxWave, smallPower) = DMap.findMax smallerMap
                     (largeMinWave, largePower) = DMap.findMin largerMap
                 in (smallPower + largePower) / 2.0
            else -- it is not in range nor a member this can not be evaluated
                 let msg = "given wavelength " ++ (show wave) ++ " is outside"
                     msg2 = " of sampled spectrum whose limits are "
                     msg3 = show (minWavelength a)
                     msg4 = show (maxWavelength a)
                 in traceStack (msg ++ msg2 ++ msg3 ++ " and " ++ msg4) 0.0

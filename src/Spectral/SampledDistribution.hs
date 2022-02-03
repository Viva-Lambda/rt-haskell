{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- sampled spectral distribution
module Spectral.SampledDistribution where

import Math3D.Vector
import Math3D.CommonOps

import Utility.HelperTypes
import Utility.Utils as Ut 

-- third party
import qualified Data.Map as DMap
import Data.Word
import Data.List
import Debug.Trace

data SampledWavePower = SampledWP (NonEmptyList (Word, Double))

instance Eq SampledWavePower where
    (SampledWP a) == (SampledWP b) = a == b

instance Show SampledWavePower where
    show (SampledWP a) = "<SampledWavePower :: " ++ show a


wavelengths :: SampledWavePower -> NonEmptyList Word
wavelengths (SampledWP dmap) = let ((w:ws), _) = unzip (nl2List dmap)
                               in fromList2NL w ws

powers :: SampledWavePower -> Vector
powers (SampledWP dmap) = let (_, (p:ps)) = unzip (nl2List dmap)
                          in fromList2Vec p ps

fromWavesPowers :: NonEmptyList Double -> NonEmptyList Word -> SampledWavePower
fromWavesPowers pwrs wvs =
    if (lengthNL pwrs) /= (lengthNL wvs)
    then let errmsg = "number of powers do not match to number of waves"
         in traceStack errmsg (SampledWP (fromList2NL (0,0.0) []))
    else let (n:ns) = zip (nl2List wvs) (nl2List pwrs)
         in SampledWP $! fromList2NL n ns

wavesCheck :: SampledWavePower -> SampledWavePower -> (Bool, String, NonEmptyList Word, NonEmptyList Word)
wavesCheck a b =
    let aw = wavelengths a
        bw = wavelengths b
    in (aw /= bw, "wavelengths differ for sampled power distributions", aw, bw)


sortSampledWavePower :: SampledWavePower -> SampledWavePower
sortSampledWavePower a = let SampledWP b = a
                             (m:ms) = sortOn fst (nl2List b)
                         in SampledWP $! fromList2NL m ms

-- common operations
instance BinaryOps SampledWavePower where
    elementwiseOp str f a b =
        let (areWavelengthsNotSame, str2, aw, bw) = wavesCheck a b
        in if areWavelengthsNotSame
           then traceStack (str2 ++ " :: " ++ str) (zeroLike a)
           else let ap = powers a
                    bp = powers b
                    VList npwrs = vecArithmeticOp str f ap bp
                in fromWavesPowers npwrs aw
    --
    elementwiseScalarOp str f a = let ap = powers a
                                      VList npwrs = vecScalarOp f ap
                                  in fromWavesPowers npwrs (wavelengths a)

    -- division
    divide a b =
        let (areWavelengthsNotSame, str, aw, bw) = wavesCheck a b
        in if areWavelengthsNotSame
           then traceStack (str) (zeroLike a)
           else let ap = powers a
                    bp = powers b
                    VList npwrs = divide ap bp
                in fromWavesPowers npwrs aw


minmaxPower :: ([Double] -> Double) -> SampledWavePower -> Double
minmaxWavelength :: ([Word] -> Word) -> SampledWavePower -> Word

minmaxPower f dmap = let ps = vec2List (powers dmap) in f ps

minmaxWavelength f dmap = let ps = nl2List (wavelengths dmap) in f ps

maxPower :: SampledWavePower -> Double
maxPower a = minmaxPower maximum a
minPower :: SampledWavePower -> Double
minPower a = minmaxPower minimum a

maxWavelength :: SampledWavePower -> Word
maxWavelength a = minmaxWavelength maximum a

minWavelength :: SampledWavePower -> Word
minWavelength a = minmaxWavelength minimum a

zeroLike :: SampledWavePower -> SampledWavePower
zeroLike a =
    let wls = wavelengths a
        pwrs = powers a
        VList b = zeroV (vsize pwrs)
    in fromWavesPowers b wls


-- interpolate a spectral power distribution
interpolate :: SampledWavePower -> (Double, Double) -> SampledWavePower
interpolate b (mn, mx) =
    let minmaxer f = f [mn, mx]
        amin = minmaxer minimum
        amax = minmaxer maximum
        pmin = minPower b
        pmax = maxPower b
        ps = powers b
        interpolator p = Ut.interp (pmin, pmax) (amin, amax) p
        interpolatedPowers = map interpolator (vec2List ps)
        skeys = nl2List $! wavelengths b
        (n:ns) = zip skeys interpolatedPowers
    in SampledWP $! fromList2NL n ns

-- clamp a spectral power distribution

clamp :: SampledWavePower -> (Double, Double) -> SampledWavePower
clamp a (mn, mx) =
    let ps = powers a
        clamper p = Ut.clamp p mn mx
        clampedPowers = map clamper (vec2List ps)
        skeys = nl2List $! wavelengths a
        (n:ns) = zip skeys clampedPowers
    in SampledWP (fromList2NL n ns)

normalize :: SampledWavePower -> SampledWavePower
normalize a = interpolate a (0.0, 1.0)

evaluateWave :: Word -> SampledWavePower -> Double

evaluateWave wave b =
    -- in range
    let (SampledWP a) = b
        inRange = (wave >= (minWavelength b)) && (wave <= (maxWavelength b))
        waves = wavelengths b
        isMember = elemNL wave waves
        getPower w = let pred (wp, pp) = wp == w 
                     in case findNL pred a of
                            Just (_, p) -> p
                            Nothing -> -1.0
        wavePred w = let pred (wp, pp) = wp <= w
                     in partitionNL pred a
    in if isMember -- wavelength is a member, we can access to its power
       then getPower wave
       else if inRange -- wavelength is in range, we can interpolate power values
            then let (smallerLst, largerLst) = wavePred wave
                     (smallMaxWave, smallPower) = last $! sortOn fst smallerLst
                     (largeMinWave, largePower) = head $! sortOn fst largerLst
                 in (smallPower + largePower) / 2.0
            else -- it is not in range nor a member this can not be evaluated
                 let msg = "given wavelength " ++ (show wave) ++ " is outside"
                     msg2 = " of sampled spectrum whose limits are "
                     msg3 = show (minWavelength b)
                     msg4 = show (maxWavelength b)
                 in traceStack (msg ++ msg2 ++ msg3 ++ " and " ++ msg4) 0.0



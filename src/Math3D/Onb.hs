{-# LANGUAGE BangPatterns #-}
-- onb in haskell
module Math3D.Onb where

import Math3D.Vector
import Math3D.CommonOps
import Math3D.EulerAngles
import Utility.HelperTypes

data OrthoNormalBase = Onb (NonEmptyList Vector)

onbSize :: OrthoNormalBase -> Int
onbSize (Onb nl) = lengthNL nl

onbSizeCheck :: OrthoNormalBase -> Int -> Either String Bool
onbSizeCheck !onb !size =
    if onbSize onb < size
    then Left $ "ortho normal base have less than requested size " ++ show size
    else Right True

vBasis :: OrthoNormalBase -> Vector
vBasis !onb = case onbSizeCheck onb 3 of
                Left s -> error s
                Right _ -> let Onb olst = onb in getNL olst 1

upDir :: OrthoNormalBase -> Vector
upDir = vBasis

wBasis :: OrthoNormalBase -> Vector
wBasis onb = case onbSizeCheck onb 3 of
                Left s -> error s
                Right _ -> let Onb olst = onb in getNL olst 0

frontDir :: OrthoNormalBase -> Vector
frontDir = wBasis

uBasis :: OrthoNormalBase -> Vector
uBasis onb = case onbSizeCheck onb 3 of
                Left s -> error s
                Right _ -> let Onb olst = onb in getNL olst 2

rightDir :: OrthoNormalBase -> Vector
rightDir = uBasis

onb3 :: Vector -> Vector -> Vector -> OrthoNormalBase
onb3 w v u = Onb $ NList w [v, u]

localVec :: OrthoNormalBase -> Vector -> Vector
localVec ob t =
    let u = uBasis ob
        v = vBasis ob
        w = wBasis ob
        ux = multiplyS u $ vget t 0
        vy = multiplyS v $ vget t 1
        wz = multiplyS w $ vget t 2
    in add wz (add ux vy)

localXyz :: OrthoNormalBase -> Double -> Double -> Double -> Vector
localXyz ob x y z = localVec ob ( VList (fromList2NL x [y, z]) )

fromW2Onb :: Vector -> OrthoNormalBase
fromW2Onb wvec =
    let w = toUnit wvec
        wx = vget w 0
        a = if (abs wx) > 0.9
            then VList $! fromList2NL 0.0 [1.0, 0.0]
            else VList $! fromList2NL 1.0 [0.0, 0.0]
        v = toUnit $! cross3d w a
        u = cross3d w v
    in onb3 w v u


fromWUp2Onb :: Vector -> Vector -> OrthoNormalBase
fromWUp2Onb wvec upvec =
    let w = toUnit wvec
        a = toUnit upvec
        v = toUnit $! cross3d w a
        u = cross3d w v
    in onb3 w v u

fromEuler2Onb :: EulerAngles -> OrthoNormalBase
fromEuler2Onb eangles =
    let w = toFrontVec eangles
    in fromW2Onb w

fromEulerUp2Onb :: EulerAngles -> Vector -> OrthoNormalBase
fromEulerUp2Onb eangles upv = let w = toFrontVec eangles in fromWUp2Onb w upv

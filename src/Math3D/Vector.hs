{-# LANGUAGE BangPatterns #-}
-- vector library
module Math3D.Vector where

import System.Random
import Random
import Prelude hiding(subtract)
import Debug.Trace

import Data.Foldable
import Data.Word
import qualified GHC.Float as GHCFloat

import Utility.Utils
import Utility.HelperTypes

import Math3D.CommonOps

data Vector = VList (NonEmptyList Double)
            | WList (NonEmptyList Word)

instance Eq Vector where
    (VList a) == (VList b) = (nl2List a) == (nl2List b)
    (VList a) == _ = False
    (WList a) == (WList b) = (nl2List a) == (nl2List b)
    (WList a) == _ = False

fromList2Vec :: Double -> [Double] -> Vector
fromList2Vec a b = VList (fromList2NL a b)

vec2List :: Vector -> [Double]
vec2List (VList a) = nl2List a
vec2List (WList a) = map GHCFloat.word2Double (nl2List a)



instance Show Vector where
    show a =
        let msg1 = "<Vector " ++ "size " ++ show (vsize a)
            msg2 = msg1 ++ " data " ++ (unwords $ map show (vec2List a))
            msg3 = msg2 ++ " >"
        in msg3

singularV :: Int -> Double -> Vector
singularV !size v = fromList2Vec v (replicate (size - 1) v)

zeroV :: Int -> Vector
zeroV !size = singularV size 0.0

zeroV3 :: Vector
zeroV3 = zeroV 3

inftyV :: Int -> Vector
inftyV !size = singularV size infty

inftyV3 :: Vector
inftyV3 = inftyV 3

negInftyV :: Int -> Vector
negInftyV !size = singularV size (-infty)

negInftyV3 ::Vector
negInftyV3 = negInftyV 3

vsize :: Vector -> Int
vsize (VList v) = lengthNL v

vget :: Vector -> Int -> Double
vget !(VList v) !index = getNL v index


sizeError :: Vector -> Vector -> String -> String
sizeError !v !s m =
    let msg = "vector sizes: " ++ (show $! vsize v) ++ " and " ++ (show $! vsize s)
        msg2 = msg ++ " are incorrect for operation " ++ m
        msg3 = msg2 ++ " v1: " ++ show v ++ " v2 " ++ show s
    in msg3

vecError :: Vector -> String -> String
vecError !v m =
    let msg = "vector: " ++ show v ++ " " ++ m 
    in msg

vecArithmeticOp :: String -> (Double -> Double -> Double) -> Vector -> Vector -> Vector
vecArithmeticOp opname f !v !e =
    if (vsize v) /= (vsize e)
    then traceStack (sizeError v e opname) zeroV3
    else let (b:bs) = zipWith f (vec2List v) (vec2List e)
         in fromList2Vec b bs

vecScalarOp :: (Double -> Double) -> Vector -> Vector
vecScalarOp f !v = let (b:bs) = map f (vec2List v) in fromList2Vec b bs

nearZeroVec :: Vector -> Bool
nearZeroVec !(VList v) =
    let vs = nl2List v
        nzero = 1e-10
    in foldl1 (&&) $! map (< nzero) (map abs vs)

instance BinaryOps Vector where
    elementwiseOp str f a b = vecArithmeticOp str f a b
    elementwiseScalarOp _ f a = vecScalarOp f a
    divide v e =
        let es = vec2List e
        in if 0.0 `elem` es
           then traceStack (vecError e "contains zero in a division operation") (zeroV3)
           else vecArithmeticOp "divide" (/) v e

dot :: Vector -> Vector -> Double
dot !v !e = let mult = multiply v e
            in foldl1 (+) (vec2List mult)

lengthSquared :: Vector -> Double
lengthSquared !v = dot v v
magnitude :: Vector -> Double
magnitude !v = sqrt $! lengthSquared v

toUnit :: Vector -> Vector
toUnit !v = divideS v (magnitude v)

cross3d :: Vector -> Vector -> Vector
cross3d !v !e =
    if (((vsize v) /= 3) || ((vsize e) /= 3))
    then error $ sizeError v e "cross product"
    else 
            let vs = vec2List v
                es = vec2List e
                us0 = vs !! 0
                us1 = vs !! 1
                us2 = vs !! 2
                vs0 = es !! 0
                vs1 = es !! 1
                vs2 = es !! 2
                r0 = us1 * vs2 - us2 * vs1
                r1 = us2 * vs0 - us0 * vs2
                r2 = us0 * vs1 - us1 * vs0
            in fromList2Vec r0 [r1, r2]


randomVecGen :: RandomGen g => (Double, Double) -> g -> Int -> RandomResult Vector g
randomVecGen !(mn, mx) !gen !size =
    let ranges = NList (mn, mx) (replicate (size - 1) (mn, mx))
        r = randMap gen randomDouble ranges
    in case r of
         RandResult (NList a b, g) -> RandResult (fromList2Vec a b, g)

randomVec :: RandomGen g => (Double, Double) -> g -> RandomResult Vector g
randomVec !a !g = randomVecGen a g 3

-- generate random vectors

randomVector :: RandomGen g => (Vector, Vector) -> g -> RandomResult Vector g
randomVector (minp, maxp) g =
    let lmnp = vsize minp
        lmxp = vsize maxp
    in if lmnp /= lmxp
       then traceStack (sizeError minp maxp "randomVector") (RandResult (zeroV3, g))
       else let mnlst = vec2List minp
                mxlst = vec2List maxp
                (z:zs) = zip mnlst mxlst
                r = randMap g randomDouble $! fromList2NL z zs
            in case r of
                 RandResult (NList a b, g2) -> RandResult (fromList2Vec a b, g2)


randV :: RandomGen g => g -> RandomResult Vector g
randV !g = randomVec (0.0, 1.0) g

-- random functions resulting in vectors
randomUnitSphere :: RandomGen g => g -> RandomResult Vector g

randomUnitSphere !gen = let r = randomVec (-1.0, 1.0) gen
                            lsqr = rfmap lengthSquared r
                            RandResult (more1, g) = rfmap (>= 1.0) lsqr
                        in if more1
                           then randomUnitSphere g
                           else r

randomUnitVector :: RandomGen g => g -> RandomResult Vector g
randomUnitVector !gen = case randomUnitSphere gen of
                          RandResult (v, g) -> RandResult (toUnit v, g)

randomHemisphere :: RandomGen g => g -> Vector -> RandomResult Vector g
randomHemisphere !gen !norm =
    let rus = randomUnitSphere gen
        fn arg = dot arg norm
        rv = rfmap fn rus
        RandResult (isPlus, g) = rfmap (> 0.0) rv
    in if isPlus
       then rus
       else let mfn arg2 = multiplyS arg2 (-1.0)
            in rfmap mfn rus


-- random in unit disk
randomUnitDisk :: RandomGen g => g -> RandomResult Vector g
randomUnitDisk !gen = case randomVec (-1.0, 1.0) gen of
                        RandResult (VList v, g) ->
                            case nl2List v of
                                [a, b, _] ->
                                    let rvec = fromList2Vec a [b, 0.0]
                                    in if (lengthSquared rvec) >= 1.0
                                       then randomUnitDisk g
                                       else RandResult (rvec, g)

-- random cosine direction
randomCosineDir :: RandomGen g => g -> RandomResult Vector g
randomCosineDir g =
    let r1 = randval g
        r2 = randomChain r1 randomDouble (0.0, 1.0)
        min1r2 arg = 1.0 - arg
        r2sub1 = rfmap min1r2 r2
        z = rfmap sqrt r2sub1
        sqr2 = rfmap sqrt r2
        phi = rfmap (* (m_pi * 2.0)) r1
        cosphi = rfmap cos phi
        sinphi = rfmap sin phi
        x = rfmap (* (liftRandVal cosphi)) sqr2
        y = rfmap (* (liftRandVal sinphi)) sqr2
    in RandResult (fromList2Vec (liftRandVal x) [liftRandVal y, liftRandVal z], 
                  liftRandGen y)

-- random to sphere
random2Sphere :: RandomGen g => g -> (Double, Double) -> RandomResult Vector g 
random2Sphere g (radius, sqrDist) =
    let r1 = randval g
        r2 = randomChain r1 randomDouble (0.0, 1.0)
        val = ((sqrt (1.0 - (radius * radius)/sqrDist))- 1)
        r2val = rfmap (* val) r2
        z = rfmap (+ 1.0) r2val
        phi = rfmap (* (m_pi * 2)) r1
        phival = liftRandVal phi
        (cosphi, sinphi) = (cos phival, sin phival)
        z2 = rfmap (* (liftRandVal z)) z
        fn arg = sqrt (1.0 - arg)
        x = liftRandVal $! rfmap (* cosphi) ( rfmap fn z2)
        y = liftRandVal $! rfmap (* sinphi) ( rfmap fn z2)
    in RandResult (fromList2Vec x [y, liftRandVal z], liftRandGen r2)



reflect :: Vector -> Vector -> Vector
reflect !v !norm = subtract v (multiplyS (multiplyS norm (dot v norm) ) 2.0)

refract :: Vector -> Vector -> Double -> Vector
refract !uv !n !etaiOverEta =
    let costheta = min (dot (multiplyS uv (-1.0)) n) 1.0
        outPerp = multiplyS (add uv (multiplyS n costheta)) etaiOverEta
        absOut = sqrt $! abs (1.0 - lengthSquared outPerp)
        outPar = multiplyS n (-1.0 * absOut)
    in add outPerp outPar

clampV :: Vector -> Double -> Double -> Vector
clampV v mn mx =
    let vs = vec2List v
        (n:ns) = clampvals vs
    in fromList2Vec n ns
    where clampvals [] = []
          clampvals (e:es) = clamp e mn mx : clampvals es

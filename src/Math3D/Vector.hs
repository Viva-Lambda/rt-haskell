{-# LANGUAGE BangPatterns #-}
-- vector library
module Math3D.Vector where

import System.Random
import Random
import Prelude hiding(subtract)
import Debug.Trace

import Data.Foldable
import Utility.Utils

import Math3D.CommonOps

data Vector = VList [Double]
            deriving (Eq)

instance Show Vector where
    show (VList a) =
        let msg1 = "<Vector " ++ "size " ++ show (length a) 
            msg2 = msg1 ++ " data " ++ (unwords $ map show a) ++ " >"
        in msg2

singularV :: Int -> Double -> Vector
singularV !size v = VList $ replicate size v

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
vsize !(VList v) = length v

vget :: Vector -> Int -> Double
vget !v !index = 
    let VList vs = v
    in if (vsize v) <= index || index < 0
       then traceStack (
       "IndexError: uncorrect index size: " ++ show index ++
       " vector: " ++ show v) 0.0
       else vs !! index



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
    else let (VList ds) = v
             (VList es) = e 
         in VList $! zipWith f ds es

vecScalarOp :: (Double -> Double) -> Vector -> Vector
vecScalarOp f !v = let (VList vs) = v in VList $! map f vs 

nearZeroVec :: Vector -> Bool
nearZeroVec !v =
    let (VList vs) = v
        nzero = 1e-10
    in foldl1 (&&) $! map (< nzero) (map abs vs)

instance BinaryOps Vector where
    elementwiseOp str f a b = vecArithmeticOp str f a b
    elementwiseScalarOp _ f a = vecScalarOp f a
    divide v e =
        let (VList es) = e
        in if 0.0 `elem` es
           then error $ vecError e "contains zero in a division operation"
           else vecArithmeticOp "divide" (/) v e

dot :: Vector -> Vector -> Double
dot !v !e = let mult = multiply v e
                (VList vs) = mult
            in foldl1 (+) vs

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
            let (VList vs) = v
                (VList es) = e
                us0 = vs !! 0
                us1 = vs !! 1
                us2 = vs !! 2
                vs0 = es !! 0
                vs1 = es !! 1
                vs2 = es !! 2
                r0 = us1 * vs2 - us2 * vs1
                r1 = us2 * vs0 - us0 * vs2
                r2 = us0 * vs1 - us1 * vs0
            in VList [r0, r1, r2]


randomVecGen :: RandomGen g => (Double, Double) -> g -> Int -> (Vector, g)
randomVecGen !(mn, mx) !gen !size =
    let gens = randomGens gen size
        (vdoubles, gs) = unzip [randomDouble g mn mx | g <- gens ]
    in (VList vdoubles, last gs)

randomVec :: RandomGen g => (Double, Double) -> g -> (Vector, g)
randomVec !a !g = randomVecGen a g 3

-- generate random vectors

randomVector :: RandomGen g => (Vector, Vector) -> g -> (Vector, g)
randomVector (minp, maxp) g =
    let lmnp = vsize minp
        lmxp = vsize maxp
    in if lmnp /= lmxp
       then traceStack (sizeError minp maxp "randomVector") (zeroV3, g)
       else let fn acc i = let (g1, vs) = acc
                               (v, g2) = randomDouble g1 (vget minp i) (vget maxp i)
                            in (g2, vs ++ [v])
                indices = [0..(lmnp - 1)]
                (g1, vals) = foldl fn (g, []) indices
            in (VList vals, g1)


randV :: RandomGen g => g -> (Vector, g)
randV !g = randomVec (0.0, 1.0) g

-- random functions resulting in vectors
randomUnitSphere :: RandomGen g => g -> (Vector, g)

randomUnitSphere !gen = let (rvec, g) = randomVec (-1.0, 1.0) gen
                       in if (lengthSquared rvec) >= 1.0
                          then randomUnitSphere g
                          else (rvec, g)

randomUnitVector :: RandomGen g => g -> (Vector, g)
randomUnitVector !gen = let (v, g) = randomUnitSphere gen in (toUnit v, g)

randomHemisphere :: RandomGen g => g -> Vector -> (Vector, g)
randomHemisphere !gen !norm =
    let (rv, g) = randomUnitSphere gen
    in if (dot rv norm) > 0.0
       then (rv, g)
       else (multiplyS rv (-1.0), g)

-- random in unit disk
randomUnitDisk :: RandomGen g => g -> (Vector, g)
randomUnitDisk !gen =
    let (VList [a,b,_], g) = randomVec (-1.0, 1.0) gen
        rvec = VList [a,b,0.0]
    in if (lengthSquared rvec) >= 1.0
       then randomUnitDisk g
       else (rvec, g)

-- random cosine direction
randomCosineDir :: RandomGen g => g -> (Vector, g)
randomCosineDir g = 
    let (r1, g1) = randval g
        (r2, g2) = randval g1
        z = sqrt (1.0 - r2)
        phi = m_pi * 2 * r1
        x = (cos phi) * (sqrt r2)
        y = (sin phi) * (sqrt r2)
    in (VList [x, y, z], g2)

-- random to sphere
random2Sphere :: RandomGen g => g -> Double -> Double -> (Vector, g)
random2Sphere g radius sqrDist =
    let (r1, g1) = randval g
        (r2, g2) = randval g1
        z = 1.0 + r2 * ((sqrt (1.0 - (radius * radius)/sqrDist))- 1)
        phi = m_pi * 2 * r1
        x = (cos phi) * (sqrt (1.0 - z * z))
        y = (sin phi) * (sqrt (1.0 - z * z))
    in (VList [x, y, z], g2)



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
    let (VList vs) = v
        nvs = clampvals vs
    in VList nvs
    where clampvals [] = []
          clampvals (e:es) = clamp e mn mx : clampvals es

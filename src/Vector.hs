-- vector library
module Vector where
import System.Random
import Random
import Prelude hiding(subtract)
import Debug.Trace

data Vector = VList [Double]
            deriving (Eq, Show)

zeroV :: Int -> Vector
zeroV size = VList $ replicate size 0.0

zeroV3 :: Vector
zeroV3 = zeroV 3

vsize :: Vector -> Int
vsize (VList v) = length v

vget :: Vector -> Int -> Double
vget v index = 
    let VList vs = v
    in if (vsize v) <= index || index < 0
       then error $ "IndexError: uncorrect index size: " ++ show index
       else vs !! index



sizeError :: Vector -> Vector -> String -> String
sizeError v s m =
    let msg = "vector sizes: " ++ (show $! vsize v) ++ " and " ++ (show $! vsize s)
        msg2 = msg ++ " are incorrect for operation " ++ m
    in msg2

vecError :: Vector -> String -> String
vecError v m =
    let msg = "vector: " ++ show v ++ " " ++ m 
    in msg

vecArithmeticOp :: String -> (Double -> Double -> Double) -> Vector -> Vector -> Vector
vecArithmeticOp opname f v e =
    if (vsize v) /= (vsize e)
    then error $ sizeError v e opname
    else let (VList ds) = v
             (VList es) = e 
         in VList $! zipWith f ds es

vecScalarOp :: (Double -> Double) -> Vector -> Vector
vecScalarOp f v =
    let 
        (VList vs) = v
    in VList $! map f vs 

nearZeroVec :: Vector -> Bool
nearZeroVec v =
    let (VList vs) = v
        nzero = 1e-10
    in foldl1 (&&) $! map (< nzero) (map abs vs)

add :: Vector -> Vector -> Vector
add v e = vecArithmeticOp "add" (+) v e
addS :: Vector -> Double -> Vector
addS v s = let f = \d -> d + s in vecScalarOp f v

subtract :: Vector -> Vector -> Vector
subtract v e = vecArithmeticOp "subtract" (-) v e

subtractS :: Vector -> Double -> Vector
subtractS v s = let f = \d -> d - s in vecScalarOp f v

multiply :: Vector -> Vector -> Vector
multiply v e = vecArithmeticOp "multiply" (*) v e

multiplyS :: Vector -> Double -> Vector
multiplyS v s = let f = \d -> d * s in vecScalarOp f v

divide :: Vector -> Vector -> Vector
divide v e =
    let 
        (VList es) = e
    in if 0.0 `elem` es
       then error $ vecError e "contains zero in a division operation"
       else vecArithmeticOp "divide" (/) v e

divideS :: Vector -> Double -> Vector
divideS v s =
    if s == 0.0
    then traceStack ("performing zero division: " ++ show v) (zeroV3)
    else let f = \d -> d / s in vecScalarOp f v

dot :: Vector -> Vector -> Double
dot v e = let mult = multiply v e
              (VList vs) = mult
          in foldl1 (+) vs

lengthSquared :: Vector -> Double
lengthSquared v = dot v v
magnitude :: Vector -> Double
magnitude v = sqrt $ lengthSquared v

toUnit :: Vector -> Vector
toUnit v = divideS v (magnitude v)

cross3d :: Vector -> Vector -> Vector
cross3d v e =
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
randomVecGen (mn, mx) gen size =
    let gens = randomGens gen size
        (vdoubles, gs) = unzip [randomDouble g mn mx | g <- gens ]
    in (VList vdoubles, last gs)

randomVec :: RandomGen g => (Double, Double) -> g -> (Vector, g)
randomVec a g = randomVecGen a g 3

randV :: RandomGen g => g -> (Vector, g)
randV g = randomVec (0.0, 1.0) g

-- random functions resulting in vectors
randomUnitSphere :: RandomGen g => g -> (Vector, g)

randomUnitSphere gen = let (rvec, g) = randomVec (-1.0, 1.0) gen
                       in if (lengthSquared rvec) >= 1.0
                          then randomUnitSphere g
                          else (rvec, g)

randomUnitVector :: RandomGen g => g -> (Vector, g)
randomUnitVector gen = let (v, g) = randomUnitSphere gen in (toUnit v, g)

randomHemisphere :: RandomGen g => g -> Vector -> (Vector, g)
randomHemisphere gen norm =
    let (rv, g) = randomUnitSphere gen
    in if (dot rv norm) > 0.0
       then (rv, g)
       else (multiplyS rv (-1.0), g)

-- random in unit disk
randomUnitDisk :: RandomGen g => g -> (Vector, g)
randomUnitDisk gen =
    let (VList [a,b,_], g) = randomVec (-1.0, 1.0) gen
        rvec = VList [a,b,0.0]
    in if (lengthSquared rvec) >= 1.0
       then randomUnitDisk g
       else (rvec, g)

reflect :: Vector -> Vector -> Vector
reflect v norm = subtract v (multiplyS (multiplyS norm (dot v norm) ) 2.0)

refract :: Vector -> Vector -> Double -> Vector
refract uv n etaiOverEta =
    let costheta = min (dot (multiplyS uv (-1.0)) n) 1.0
        outPerp = multiplyS (add uv (multiplyS n costheta)) etaiOverEta
        absOut = sqrt $! abs (1.0 - lengthSquared outPerp)
        outPar = multiplyS n (-1.0 * absOut)
    in add outPerp outPar


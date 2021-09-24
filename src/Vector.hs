-- vector library
module Vector where


data Vector = VecFromList [Double]
            | VecFromScalar Double Int
            deriving (Eq, Show, Ord)

vsize :: Vector -> Int
vsize (VecFromScalar v s) = s
vsize (VecFromList v) = length v

vget :: Vector -> Int -> Double
vget v index = 
    let vlst = fromScalarToList v
        VecFromList vs = vlst
    in if (vsize v) <= index || index < 0
       then error $ "IndexError: uncorrect index size: " ++ show index
       else vs !! index


fromScalarToList :: Vector -> Vector
fromScalarToList (VecFromList v) = (VecFromList v)
fromScalarToList (VecFromScalar d vsize) = 
    let dlist = replicate vsize d
    in VecFromList dlist

sizeError :: Vector -> Vector -> String -> String
sizeError v s m =
    let msg = "vector sizes: " ++ (show $ vsize v) ++ " and " ++ (show $ vsize s)
        msg2 = msg ++ " are incorrect for operation " ++ m
    in msg2

vecError :: Vector -> String -> String
vecError v m =
    let msg = "vector: " ++ show v ++ " " ++ m 
    in msg

vecArithmeticOp :: String -> ((Double, Double) -> Double) -> Vector -> Vector -> Vector
vecArithmeticOp opname f v e =
    let vd = fromScalarToList v
        ve = fromScalarToList e 
        (VecFromList ds) = vd
        (VecFromList es) = ve
    in if (length ds) == (length es)
       then let dsEs = zip ds es
            in VecFromList $ opZip dsEs
       else error $ sizeError vd ve opname
    where opZip [] = []
          opZip (z:zs) = (f z) : opZip zs

vecScalarOp :: (Double -> Double) -> Vector -> Vector
vecScalarOp f v =
    let vd = fromScalarToList v
        (VecFromList vs) = vd
    in VecFromList $ map f vs 

add :: Vector -> Vector -> Vector
add v e = vecArithmeticOp "add" (\tp -> (fst tp) + (snd tp)) v e
addS :: Vector -> Double -> Vector
addS v s = let f = \d -> d + s in vecScalarOp f v

subtract :: Vector -> Vector -> Vector
subtract v e = vecArithmeticOp "subtract" (\tp -> (fst tp) - (snd tp)) v e

subtractS :: Vector -> Double -> Vector
subtractS v s = let f = \d -> d - s in vecScalarOp f v

multiply :: Vector -> Vector -> Vector
multiply v e = vecArithmeticOp "multiply" (\tp -> (fst tp) * (snd tp)) v e

multiplyS :: Vector -> Double -> Vector
multiplyS v s = let f = \d -> d * s in vecScalarOp f v

divide :: Vector -> Vector -> Vector
divide v e =
    let ve = fromScalarToList e 
        (VecFromList es) = ve
    in if 0.0 `elem` es
       then error $ vecError e "contains zero in a division operation"
       else vecArithmeticOp "divide" (\tp -> (fst tp) / (snd tp)) v e

divideS :: Vector -> Double -> Vector
divideS v s =
    if s == 0.0
    then error "performing zero division"
    else let f = \d -> d / s in vecScalarOp f v

dot :: Vector -> Vector -> Double
dot v e = let mult = multiply v e
              (VecFromList vs) = mult
          in foldl1 (+) vs

lengthSquared :: Vector -> Double
lengthSquared v = dot v v
magnitude :: Vector -> Double
magnitude v = sqrt $ lengthSquared v

toUnit :: Vector -> Vector
toUnit v = divide v (VecFromScalar (magnitude v) (vsize v))

cross3d :: Vector -> Vector -> Vector
cross3d v e =
    let vv = fromScalarToList v
        ev = fromScalarToList e 
    in if (((vsize vv) /= 3) || ((vsize ev) /= 3))
       then error $ sizeError v e "cross product"
       else 
            let (VecFromList vs) = vv
                (VecFromList es) = ev
                us0 = vs !! 0
                us1 = vs !! 1
                us2 = vs !! 2
                vs0 = es !! 0
                vs1 = es !! 1
                vs2 = es !! 2
                r0 = us1 * vs2 - us2 * vs1
                r1 = us2 * vs0 - us0 * vs2
                r2 = us0 * vs1 - us1 * vs0
            in VecFromList [r0, r1, r2]



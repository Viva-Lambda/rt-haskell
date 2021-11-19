-- Basic Quaternion implementation
module Math3D.Quaternion where

import qualified Math3D.Vector as V
import Math3D.CommonOps

data Quaternion = Quat {qR :: Double, qX :: Double,
                        qY :: Double, qZ :: Double} deriving(Eq, Show)

instance BinaryOps Quaternion where
    elementwiseOp _ f q1 q2 =
        let {q1r = qR q1; q2r = qR q2; q1x = qX q1; q2x = qX q2;
             q1y = qY q1; q2y = qY q2; q1z = qZ q1; q2z = qZ q2
            }
        in Quat {qR = f q1r q2r, qX = f q1x q2x,
                 qY = f q1y q2y, qZ = f q1z q2z}
    elementwiseScalarOp _ f q =
        let {qr = qR q; qx = qX q;
             qy = qY q; qz = qZ q;
            }
        in Quat {qR = f qr, qX = f qx,
                 qY = f qy, qZ = f qz}
    divide q1 q2 =
        let {q1r = qR q1; q2r = qR q2; q1x = qX q1; q2x = qX q2;
             q1y = qY q1; q2y = qY q2; q1z = qZ q1; q2z = qZ q2;
             q2rC = q2r == 0; q2xC = q2x == 0; q2yC = q2y == 0;
             q2zC = q2z == 0; anyZerosCheck = q2zC || q2xC || q2yC || q2rC;
            }
        in if anyZerosCheck
           then error "ZeroDivisionError :: performing zero division with quaternions"
           else Quat {qR = q1r / q2r, qX = q1x / q2x,
                      qY = q1y / q2y, qZ = q1z / q2z}


qVector :: Quaternion -> V.Vector
qScalar :: Quaternion -> Double
qScalar q = qR q
qVector q = V.VList [qX q, qY q, qZ q]

fromSVec2Quaternion :: Double -> V.Vector -> Quaternion
fromSVec2Quaternion s v =
    if (V.vsize v) /= 3
    then error $ "vector size must be equal to 3: " ++ show (V.vsize v)
    else Quat {qR = s, qX = V.vget v 0, qY = V.vget v 1, qZ = V.vget v 2}

{- From
Nitecki, Z. (2018) Calculus in 3D: geometry, vectors, and multivariate
calculus, section 8.5.2
-}
fromAngleAxis2Quaternion :: Double -> V.Vector -> Quaternion
fromAngleAxis2Quaternion theta normal =
    let vnorm = V.vsize normal
    in if vnorm > 3
       then error "Vector must have 3 dimensions to be used as axis in 3d"
       else let costheta = cos $ theta / 2.0
                sintheta = sin $ theta / 2.0
                nsin = V.multiplyS normal sintheta
            in fromSVec2Quaternion costheta nsin

hamiltonProduct :: Quaternion -> Quaternion -> Quaternion
hamiltonProduct q_a q_b =
    let s_a = qScalar q_a
        s_b = qScalar q_b
        v_a = qVector q_a
        v_b = qVector q_b
        -- s_a * s_b
        sab = s_a * s_b
        -- va_dot_vb
        a_dot_b = V.dot v_a v_b
        -- va_cross_vb
        a_cross_b = V.cross3d v_a v_b
        -- s_a * b + s_b * a + va_cross_vb
        s_a_b = V.multiplyS v_b s_a
        s_b_a = V.multiplyS v_a s_b
        a_plus_b = V.add s_a_b s_b_a
        ab_plus_cross = V.add a_plus_b a_cross_b
        --
        sab_minus_adotb = sab - a_dot_b
        V.VList [x, y, z] = ab_plus_cross
    in Quat {qR = sab_minus_adotb, qX = x, qY = y, qZ = z}


qConjugate :: Quaternion -> Quaternion
qConjugate q = multiplyS q (-1.0)

qDeterminant :: Quaternion -> Double
qDeterminant q =
    let Quat {qR = a2, qX = b2, qY = c2, qZ = d2} = multiply q q
    in a2 + b2 + c2 + d2

qMagnitude :: Quaternion -> Double
qMagnitude o = sqrt $ qDeterminant o

toUnit :: Quaternion -> Quaternion
toUnit q =
    let norm = qMagnitude q
        invmag = 1.0 / norm
    in multiplyS q invmag

qInverse :: Quaternion -> Quaternion
qInverse q =
    let invmag = 1.0 / (qMagnitude q)
        conj = qConjugate q
        sp = qScalar conj * invmag
        qv = V.multiplyS (qVector q) invmag
    in fromSVec2Quaternion sp qv

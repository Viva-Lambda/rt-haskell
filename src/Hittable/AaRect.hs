-- axis aligned rectangles
module Hittable.AaRect where

import Vector
import Hittable.Hittable
import Hittable.HitRecord
import Ray
import Material.Material
import Hittable.Aabb

data AlignmentAxis = AaX
                   | AaY
                   | AaZ deriving(Eq, Show)

aAxis2Int :: AlignmentAxis -> Int
aAxis2Int AaX = 0
aAxis2Int AaY = 1
aAxis2Int AaZ = 2

data QuadInfo = QInfo {aligned1 :: AlignmentAxis,
                       aligned2 :: AlignmentAxis,
                       notAligned :: AlignmentAxis
                       } deriving (Eq, Show)



data AaRect = Quad {
    quadMat :: Material,
    quadNormal :: Vector,
    quadDistance :: Double,
    quadInfo :: QuadInfo,
    quadAlignedAxisA1 :: Double,
    quadAlignedAxisA2 :: Double,
    quadAlignedAxisB1 :: Double,
    quadAlignedAxisB2 :: Double
    }

type AAxisValue = Double
type AADistance = Double

mkAaRect :: AAxisValue -> AAxisValue -> AAxisValue ->
            AAxisValue -> AADistance -> Material -> Vector -> AaRect

mkAaRect a0 a1 b0 b1 dist mat normal =
    let qinfo = if (vget normal 2) == 1.0 -- z value
                then QInfo {aligned1 = AaX, aligned2 = AaY,
                               notAligned = AaZ}
                else if (vget normal 0) == 1.0 -- x value
                     then QInfo {aligned1 = AaY, aligned2 = AaZ,
                                    notAligned = AaX}
                     else if (vget normal 1) == 1.0 -- y value
                          then QInfo {aligned1 = AaX, aligned2 = AaZ,
                                         notAligned = AaY}
                          else error "only 3d vectors are supported for location"
    in Quad {
        quadMat = mat,
        quadNormal = normal,
        quadInfo = qinfo,
        quadDistance = dist,
        quadAlignedAxisA1 = a0,
        quadAlignedAxisA2 = a1,
        quadAlignedAxisB1 = b0,
        quadAlignedAxisB2 = b1
        }

mkXyRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkXyRect x0 x1 y0 y1 dist mat = 
    mkAaRect x0 x1 y0 y1 dist mat (VList [0.0, 0.0, 1.0] )

mkXzRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkXzRect x0 x1 z0 z1 dist mat = 
    mkAaRect x0 x1 z0 z1 dist mat (VList [0.0, 1.0, 0.0] )

mkYzRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkYzRect y0 y1 z0 z1 dist mat = 
    mkAaRect y0 y1 z0 z1 dist mat (VList [1.0, 0.0, 0.0] )


instance Eq AaRect where
    a == b =
        let Quad {quadMat = _,
                  quadNormal = qn,
                  quadDistance = qd,
                  quadInfo = qi,
                  quadAlignedAxisA1 = qa1,
                  quadAlignedAxisA2 = qa2,
                  quadAlignedAxisB1 = qb1,
                  quadAlignedAxisB2 = qb2
                 } = a
            Quad {quadMat = _, quadNormal = bqn,
                  quadDistance = bqd, quadInfo = bqi,
                  quadAlignedAxisA1 = bqa1,
                  quadAlignedAxisA2 = bqa2,
                  quadAlignedAxisB1 = bqb1,
                  quadAlignedAxisB2 = bqb2
                 } = b
            -- mateq = m == bm
            disteq = qd == bqd
            qinfoeq = qi == bqi
            normaleq = qn == bqn
            alignedeq = qa1 == bqa1 && qa2 == bqa2 && qb1 == bqb1 && qb2 == bqb2
        in disteq && qinfoeq && normaleq && alignedeq

instance Show AaRect where
    show (Quad {quadMat = m,
                quadNormal = qn,
                quadDistance = qd,
                quadInfo = qi,
                quadAlignedAxisA1 = qa1,
                quadAlignedAxisA2 = qa2,
                quadAlignedAxisB1 = qb1,
                quadAlignedAxisB2 = qb2
                }) =
        let -- mstr = "Material: " ++ show m
            normstr = " Normal: " ++ show qn
            dstr = " Distance: " ++ show qd
            istr = " Info: " ++ show qi
            astr = " Aligned: " ++ (show qa1) ++ "," ++ (show qa2) ++ ","
            astr2 = astr ++ (show qb1) ++ "," ++ (show qb2)
        in "<Quad :: " ++ normstr ++ dstr ++ istr ++ astr2 ++ ">"

instance Hittable AaRect where
    hit a inray tmin tmax hrec =
        let (Quad {quadMat = m,
                quadNormal = anormal,
                quadDistance = k,
                quadInfo = axinfo,
                quadAlignedAxisA1 = a1,
                quadAlignedAxisA2 = a2,
                quadAlignedAxisB1 = b1,
                quadAlignedAxisB2 = b2
                }) = a
            (Rd {origin = ro,
               direction = rd,
               rtime = rt}) = inray 
            --
            notAlignedOrigDist = k - (vget ro (aAxis2Int $ notAligned axinfo))
            t = notAlignedOrigDist / (vget rd (aAxis2Int $ notAligned axinfo))
            distCheck = t < tmin || t > tmax
        in if distCheck
           then (hrec, False)
           else let adist1 = vget ro (aAxis2Int $ aligned1 axinfo)
                    adist2 = vget rd (aAxis2Int $ aligned1 axinfo)
                    adist = adist1 + t * adist2
                    bdist1 = vget ro (aAxis2Int $ aligned2 axinfo)
                    bdist2 = vget rd (aAxis2Int $ aligned2 axinfo)
                    bdist = bdist1 + t * bdist2
                    acheck = a1 < adist && adist < a2
                    bcheck = b1 < bdist && bdist < b2
                in if not (acheck && bcheck)
                   then (hrec, False)
                   else let ru = (adist - a1) / (a2 - a1)
                            rv = (bdist - b1) / (b2 - b1)
                            rp = at inray t
                            hr = HRec {hdist = t, point = rp, pnormal = anormal,
                                       matPtr = m, hUV_u = ru, hUV_v = rv,
                                       isFront = False}
                        in (setFaceNormal hr inray anormal, True)

    boundingBox a tmn tmx ab =
        let (Quad {quadMat = m,
                   quadNormal = anormal,
                   quadDistance = k,
                   quadInfo = axinfo,
                   quadAlignedAxisA1 = a1,
                   quadAlignedAxisA2 = a2,
                   quadAlignedAxisB1 = b1,
                   quadAlignedAxisB2 = b2
                }) = a
            (p1, p2) = case notAligned axinfo of
                            AaZ -> (VList [a1, b1, k - 0.0001], VList [a2, b2, k + 0.0001])
                            AaY -> (VList [a1, k - 0.0001, b1], VList [a2, k + 0.0001, b2])
                            AaX -> (VList [k - 0.0001, a1, b1], VList [k + 0.0001, a2, b2])
        in (AaBbox {aabbMin = p1, aabbMax = p2}, True)


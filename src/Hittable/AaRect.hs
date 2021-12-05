-- axis aligned rectangles
module Hittable.AaRect where

import Math3D.Vector
import Math3D.Ray
import Math3D.CommonOps

--
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb

import Material.Material

import Utility.Utils

import Random
import Prelude hiding(subtract)
import Data.List

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

corners :: AaRect -> (Vector, Vector, Vector, Vector)
corners a =
    let (Quad {quadMat = _,
                quadNormal = _,
                quadDistance = k,
                quadInfo = qi,
                quadAlignedAxisA1 = a1,
                quadAlignedAxisA2 = a2,
                quadAlignedAxisB1 = b1,
                quadAlignedAxisB2 = b2
                }) = a
    in case notAligned qi of
           AaZ -> (VList [a1, b1, k], VList [a1, b2, k],
                   VList [a2, b2, k], VList [a2, b1, k])
           AaY -> (VList [a1, k, b1], VList [a1, k, b2],
                   VList [a2, k, b2], VList [a2, k, b1])
           AaX -> (VList [k, a1, b1], VList [k, a1, b2],
                   VList [k, a2, b2], VList [k, a2, b1])

minMaxPointsRect :: AaRect -> (Vector, Vector)
minMaxPointsRect a =
    let (c1, c2, c3, c4) = corners a
        (c:cs) = [c1, c2, c3, c4]
        fn acc i = let VList ss = i
                       (compFn, VList acs) = acc
                   in (compFn, VList [compFn [s, a] | (s, a) <- zip ss acs])
        (_, minp) = foldl fn (minimum, c) cs
        (_, maxp) = foldl fn (maximum, c) cs
    in (minp, maxp)


cornerNormal :: AaRect -> (Vector, Vector, Vector, Vector, Vector)
cornerNormal a = let (p1, p2, p3, p4) = corners a in (p1, p2, p3, p4, quadNormal a)


fromCornersNormal :: (Vector, Vector, Vector, Vector, Vector) -> AaRect
fromCornersNormal (p1, p2, p3, p4, n) =
    let getX v = vget v 0
        getY v = vget v 1
        getZ v = vget v 2
        xvals = map getX [p1, p2, p3, p4]
        yvals = map getY [p1, p2, p3, p4]
        zvals = map getZ [p1, p2, p3, p4]
        -- find distance
        k = if allEqual xvals
            then head xvals
            else if allEqual yvals
                 then head yvals
                 else head zvals
        -- filter out distance from x,y,z vals
        nxvals = takeWhile (/= k) xvals
        nyvals = takeWhile (/= k) yvals
        nzvals = takeWhile (/= k) zvals
        -- get a and b values 
        (alst, blst) = if null nxvals
                       then (nyvals, nzvals)
                       else if null nyvals
                            then (nxvals, nzvals)
                            else (nxvals, nyvals)
        -- get min max for values
        (amin, amax) = (minimum alst, maximum alst)
        (bmin, bmax) = (minimum blst, maximum blst)
    in mkAaRect amin amax bmin bmax k NoMat n

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

newtype XyRect = XyR AaRect
newtype XzRect = XzR AaRect
newtype YzRect = YzR AaRect

mkXyRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkXyRect x0 x1 y0 y1 dist mat = 
    mkAaRect x0 x1 y0 y1 dist mat (VList [0.0, 0.0, 1.0] )

mkXzRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkXzRect x0 x1 z0 z1 dist mat = 
    mkAaRect x0 x1 z0 z1 dist mat (VList [0.0, 1.0, 0.0] )

mkYzRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkYzRect y0 y1 z0 z1 dist mat = 
    mkAaRect y0 y1 z0 z1 dist mat (VList [1.0, 0.0, 0.0] )


getRectPdfValue :: Vector -> Vector -> Double -> Double -> Double
getRectPdfValue dir normal dist area =
    let dsqr = dist * dist
        dsqr2 = dsqr * (lengthSquared dir)
        cosine = abs $ ((dot dir normal) / (magnitude dir))
    in dsqr2 / (cosine * area)


isPointInRect :: Vector -> AaRect -> Bool
isPointInRect v a =
    let qdist = quadDistance a
        (minp, maxp) = minMaxPointsRect a
        VList vs = v
    in case findIndex (== qdist) vs of
            Nothing -> False
            Just index -> 
                let indices = [0..((length vs) - 1)]
                    otherIndices = filter (/= index) indices
                    compFn i = let ival = vget v i
                                   mnval = vget minp i
                                   mxval = vget maxp i
                               in (ival >= mnval) && (ival <= mxval)
                in all compFn otherIndices

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
    hit a g inray tmin tmax hrec =
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
           then (hrec, False, g)
           else let adist1 = vget ro (aAxis2Int $ aligned1 axinfo)
                    adist2 = vget rd (aAxis2Int $ aligned1 axinfo)
                    adist = adist1 + t * adist2
                    bdist1 = vget ro (aAxis2Int $ aligned2 axinfo)
                    bdist2 = vget rd (aAxis2Int $ aligned2 axinfo)
                    bdist = bdist1 + t * bdist2
                    acheck = a1 < adist && adist < a2
                    bcheck = b1 < bdist && bdist < b2
                in if not (acheck && bcheck)
                   then (hrec, False, g)
                   else let ru = (adist - a1) / (a2 - a1)
                            rv = (bdist - b1) / (b2 - b1)
                            rp = at inray t
                            hr = HRec {hdist = t, point = rp, pnormal = anormal,
                                       matPtr = m, hUV_u = ru, hUV_v = rv,
                                       isFront = False}
                        in (setFaceNormal hr inray anormal, True, g)

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

    pdf_value a g orig v =
        let hr = emptyRec
            ry = Rd {origin = orig, direction = v, rtime = 0.0}
            (ahit, isHit, g1) = hit a g ry 0.001 (infty) hr
        in if not isHit
           then (0.0, g1)
           else let a1 = quadAlignedAxisA1 a
                    a2 = quadAlignedAxisA2 a
                    a_2 = maximum [a1, a2]
                    a_1 = minimum [a1, a2]
                    b1 = quadAlignedAxisB1 a
                    b2 = quadAlignedAxisB2 a
                    b_2 = maximum [b1, b2]
                    b_1 = minimum [b1, b2]
                    area = (a_2 - a_1) * (b_2 - b_1)
                    dist = hdist hr
                in (getRectPdfValue v (pnormal hr) dist area, g1)

    hrandom a g orig =
        let k = quadDistance a
            qi = quadInfo a
            a1 = quadAlignedAxisA1 a
            a2 = quadAlignedAxisA2 a
            b1 = quadAlignedAxisB1 a
            b2 = quadAlignedAxisB2 a
            mkv g1 x1 x2 y1 y2 = 
                let (x, g2) = randomDouble g1 (minimum [x1, x2]) (maximum [x1, x2])
                    (y, g3) = randomDouble g2 (minimum [y1, y2]) (maximum [y1, y2])
                in (g3, x, y)

        in case notAligned qi of
                AaZ -> let (gz, x, y) = mkv g a1 a2 b1 b2
                       in (subtract (VList [x, y, k]) orig, gz)

                AaY -> let (gy, x, z) = mkv g a1 a2 b1 b2
                       in (subtract (VList [x, k, z]) orig, gy)

                AaX -> let (gx, y, z) = mkv g a1 a2 b1 b2
                       in (subtract (VList [k, y, z]) orig, gx)

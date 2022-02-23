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
import Utility.HelperTypes

import Random
import Prelude hiding(subtract)
import Data.List
import Debug.Trace

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
    let Quad {quadMat = _,
              quadNormal = _,
              quadDistance = k,
              quadInfo = qi,
              quadAlignedAxisA1 = a1,
              quadAlignedAxisA2 = a2,
              quadAlignedAxisB1 = b1,
              quadAlignedAxisB2 = b2
             } = a
    in case notAligned qi of
           AaZ -> (fromList2Vec a1 [b1, k], fromList2Vec a1 [ b2, k],
                   fromList2Vec a2 [b2, k], fromList2Vec a2 [ b1, k])
           AaY -> (fromList2Vec a1 [k, b1], fromList2Vec a1 [ k, b2],
                   fromList2Vec a2 [k, b2], fromList2Vec a2 [ k, b1])
           AaX -> (fromList2Vec k [a1, b1], fromList2Vec k [a1, b2],
                   fromList2Vec k [a2, b2], fromList2Vec k [a2, b1])

minMaxPointsRect :: AaRect -> (Vector, Vector)
minMaxPointsRect a =
    let (c1, c2, c3, c4) = corners a
        (c:cs) = [c1, c2, c3, c4]
        fn acc i = let VList ss = i
                       (compFn, VList acs) = acc
                       (m:ms) = [compFn [s, a] | (s, a) <- nl2List $! zipNL ss acs]
                   in (compFn, fromList2Vec m ms)
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
        k | allEqual xvals = head xvals
          | allEqual yvals = head yvals
          | otherwise = head zvals
        -- filter out distance from x,y,z vals
        nxvals = takeWhile (/= k) xvals
        nyvals = takeWhile (/= k) yvals
        nzvals = takeWhile (/= k) zvals
        -- get a and b values 
        (alst, blst) | null nxvals = (nyvals, nzvals)
                     | null nyvals = (nxvals, nzvals)
                     | otherwise = (nxvals, nyvals)
        -- get min max for values
        (amin, amax) = (minimum alst, maximum alst)
        (bmin, bmax) = (minimum blst, maximum blst)
    in mkAaRect amin amax bmin bmax k NoMat n

type AAxisValue = Double
type AADistance = Double

mkAaRect :: AAxisValue -> AAxisValue -> AAxisValue ->
            AAxisValue -> AADistance -> Material -> Vector -> AaRect

mkAaRect a0 a1 b0 b1 dist mat normal =
         -- z value
    let qinfo | vget normal 2 == 1.0 = QInfo {aligned1 = AaX, aligned2 = AaY,
                                              notAligned = AaZ}
              | vget normal 0 == 1.0 = QInfo {aligned1 = AaY, aligned2 = AaZ,
                                              notAligned = AaX}
              | vget normal 1 == 1.0 = QInfo {aligned1 = AaX, aligned2 = AaZ,
                                              notAligned = AaY}
              | otherwise = traceStack
                                "only 3d vectors are supported for location"
                                QInfo {aligned1 = AaX, aligned2 = AaZ,
                                       notAligned = AaY}
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
    mkAaRect x0 x1 y0 y1 dist mat (fromList2Vec 0.0 [0.0, 1.0])

mkXzRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkXzRect x0 x1 z0 z1 dist mat = 
    mkAaRect x0 x1 z0 z1 dist mat (fromList2Vec 0.0 [1.0, 0.0] )

mkYzRect :: AAxisValue -> AAxisValue -> AAxisValue -> AAxisValue -> AADistance -> Material -> AaRect
mkYzRect y0 y1 z0 z1 dist mat = 
    mkAaRect y0 y1 z0 z1 dist mat (fromList2Vec 1.0 [0.0, 0.0] )


getRectPdfValue :: Vector -> Vector -> Double -> Double -> Double
getRectPdfValue dir normal dist area =
    let dsqr = dist * dist
        dsqr2 = dsqr * (lengthSquared dir)
        cosine = abs (dot dir normal / (magnitude dir))
    in dsqr2 / (cosine * area)


isPointInRect :: Vector -> AaRect -> Bool
isPointInRect v a =
    let qdist = quadDistance a
        (minp, maxp) = minMaxPointsRect a
        vs = vec2List v
    in case elemIndex qdist vs of
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
    show Quad {quadMat = m,
               quadNormal = qn,
               quadDistance = qd,
               quadInfo = qi,
               quadAlignedAxisA1 = qa1,
               quadAlignedAxisA2 = qa2,
               quadAlignedAxisB1 = qb1,
               quadAlignedAxisB2 = qb2
               } =
        let -- mstr = "Material: " ++ show m
            normstr = " Normal: " ++ show qn
            dstr = " Distance: " ++ show qd
            istr = " Info: " ++ show qi
            astr = " Aligned: " ++ (show qa1) ++ "," ++ (show qa2) ++ ","
            astr2 = astr ++ (show qb1) ++ "," ++ (show qb2)
        in "<Quad :: " ++ normstr ++ dstr ++ istr ++ astr2 ++ ">"

instance Hittable AaRect where
    hit a g inray tmin tmax hrec =
        let Quad {quadMat = m,
                  quadNormal = anormal,
                  quadDistance = k,
                  quadInfo = axinfo,
                  quadAlignedAxisA1 = a1,
                  quadAlignedAxisA2 = a2,
                  quadAlignedAxisB1 = b1,
                  quadAlignedAxisB2 = b2
                } = a
            Rd {origin = ro, direction = rd, rtime = rt} = inray 
            --
            notAlignedOrigDist = k - (vget ro (aAxis2Int $ notAligned axinfo))
            t = notAlignedOrigDist / (vget rd (aAxis2Int $ notAligned axinfo))
            distCheck = t < tmin || t > tmax
            result 
                | distCheck = (hrec, False, g)
                | not $ abcheck axinfo t ro rd a1 a2 b1 b2 = (hrec, False, g)
                | otherwise =
                    let ru = ((adist axinfo t ro rd) - a1) / (a2 - a1)
                        rv = ((bdist axinfo t ro rd) - b1) / (b2 - b1)
                        rp = at inray t
                        hr = HRec {hdist = t, point = rp, pnormal = anormal,
                                   matPtr = m, hUVu = ru, hUVv = rv,
                                   isFront = False}
                    in (setFaceNormal hr inray anormal, True, g)
        in result
        where 
            adist axinfo t ro rd = let adist1 = vget ro (aAxis2Int $ aligned1 axinfo)
                                       adist2 = vget rd (aAxis2Int $ aligned1 axinfo)
                                   in adist1 + t * adist2

            bdist axinfo t ro rd = let bdist1 = vget ro (aAxis2Int $ aligned2 axinfo)
                                       bdist2 = vget rd (aAxis2Int $ aligned2 axinfo)
                                   in bdist1 + t * bdist2

            abcheck axinfo t ro rd a1 a2 b1 b2 =
                    let ad = adist axinfo t ro rd
                        bd = bdist axinfo t ro rd
                        acheck = a1 < ad && ad < a2
                        bcheck = b1 < bd && bd < b2
                    in (acheck && bcheck)


    boundingBox a tmn tmx ab =
        let Quad {quadMat = m,
                  quadNormal = anormal,
                  quadDistance = k,
                  quadInfo = axinfo,
                  quadAlignedAxisA1 = a1,
                  quadAlignedAxisA2 = a2,
                  quadAlignedAxisB1 = b1,
                  quadAlignedAxisB2 = b2
                } = a
            (p1, p2) = case notAligned axinfo of
                            AaZ -> (fromList2Vec a1 [b1, k - 0.0001],
                                    fromList2Vec a2 [b2, k + 0.0001])
                            AaY -> (fromList2Vec a1 [k - 0.0001, b1],
                                    fromList2Vec a2 [k + 0.0001, b2])
                            AaX -> (fromList2Vec (k - 0.0001) [a1, b1],
                                    fromList2Vec (k + 0.0001) [a2, b2])
        in (AaBbox {aabbMin = p1, aabbMax = p2}, True)

    pdf_value a g orig v =
        let hr = emptyRec
            ry = Rd {origin = orig, direction = v, 
                     rtime = 0.0, wavelength = 0}
            (ahit, isHit, g1) = hit a g ry 0.001 infty hr
        in if not isHit
           then RandResult (0.0, g1)
           else let a1 = quadAlignedAxisA1 a
                    a2 = quadAlignedAxisA2 a
                    a_2 = maximum [a1, a2]
                    a_1 = minimum [a1, a2]
                    b1 = quadAlignedAxisB1 a
                    b2 = quadAlignedAxisB2 a
                    b_2 = maximum [b1, b2]
                    b_1 = minimum [b1, b2]
                    area = (a_2 - a_1) * (b_2 - b_1)
                    dist = hdist ahit
                in RandResult (getRectPdfValue v (pnormal ahit) dist area, g1)

    hrandom a g orig =
        let k = quadDistance a
            qi = quadInfo a
            a1 = quadAlignedAxisA1 a
            a2 = quadAlignedAxisA2 a
            b1 = quadAlignedAxisB1 a
            b2 = quadAlignedAxisB2 a
            mkv g1 x1 x2 y1 y2 = 
                let RandResult (x, g2) = randomDouble g1 (minimum [x1, x2], maximum [x1, x2])
                    RandResult (y, g3) = randomDouble g2 (minimum [y1, y2], maximum [y1, y2])
                in (g3, x, y)

        in case notAligned qi of
                AaZ -> let (gz, x, y) = mkv g a1 a2 b1 b2
                           v = subtract (fromList2Vec x [y, k]) orig
                       in RandResult (v, gz)

                AaY -> let (gy, x, z) = mkv g a1 a2 b1 b2
                           v = subtract (fromList2Vec x [k, z]) orig
                       in RandResult (v, gy)

                AaX -> let (gx, y, z) = mkv g a1 a2 b1 b2
                           v = subtract (fromList2Vec k [y, z]) orig
                       in RandResult (v, gx)

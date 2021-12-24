-- Aabb axis aligned bounding box
module Hittable.Aabb where

import Math3D.Vector
import Math3D.Ray
import Prelude hiding (subtract)

data Aabb = AaBbox {aabbMin :: Vector, aabbMax :: Vector} deriving (Eq, Show)

zeroAabb :: Int -> Aabb
zeroAabb nbDims = AaBbox {aabbMin = zeroV nbDims, aabbMax = zeroV nbDims}

zeroAabb3 :: Aabb
zeroAabb3 = zeroAabb 3

compHitAabb :: Int -> Aabb -> Ray -> Double -> Double -> Bool
compHitAabb i (AaBbox {aabbMin = a, aabbMax = b}) ray tmin tmax =
    let invD = 1.0 / (vget (direction ray) i)
        t0 = ((vget a i) - (vget (origin ray) i)) * invD
        t1 = ((vget b i) - (vget (origin ray) i)) * invD
        tfirst = if invD < 0.0 then t1 else t0
        tsecond = if tfirst == t1 then t0 else t1
        t_min = if tfirst > tmin then tfirst else tmin
        t_max = if tsecond < tmax then tsecond else tmax
    in if t_max <= t_min
       then False
       else True

aabbHit :: Aabb -> Ray -> Double -> Double -> Bool
aabbHit ab ray t_min t_max =
        let AaBbox {aabbMin = a, aabbMax = b} = ab
            lena = vsize a
            isHit = foldl1 (&&) [compHitAabb i ab ray t_min t_max | i <- [0..(lena - 1)]]
        in isHit

-- surrounding box
ssBox :: Aabb -> Aabb -> Aabb
ssBox (AaBbox {aabbMin = a, aabbMax = b}) 
      (AaBbox {aabbMin = c, aabbMax = d}) =
      let f el = vec2List el
          (mi:mn) = zipWith min (f a) (f c)
          (ma:mx) = zipWith max (f b) (f d)
      in AaBbox {aabbMin = fromList2Vec mi mn, aabbMax = fromList2Vec ma mx}

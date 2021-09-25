-- utility function
module Utils where

import Vector

infty :: Double
infty = (read "Infinity") :: Double

m_pi :: Double
m_pi = 3.141592653589793238

degrees_to_radians :: Double -> Double
degrees_to_radians degrees = degrees * m_pi / 180.0

clamp :: Double -> Double -> Double -> Double

clamp x min max = if x < min
                  then min
                  else if x > max
                       then max
                       else x

clampV :: Vector -> Double -> Double -> Vector
clampV v mn mx =
    let (VecFromList vs) = fromScalarToList v
        nvs = clampvals vs
    in VecFromList nvs
    where clampvals [] = []
          clampvals (e:es) = clamp e mn mx : clampvals es

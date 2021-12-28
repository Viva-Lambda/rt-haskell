module Spectral.Wave where

import Math3D.Vector

-- planar wave as described by E. Hecht, Optics, 2017, p. 121-122; fig. 4.45

data PlaneWave = PWave {
    -- represented with E_0 in the equation
    amplitude :: Double,

    -- represented with r in the equation 4.12, and fig. 4.45
    position :: Vector,

    -- represented with k in the equation 4.12, and fig. 4.45
    direction :: Vector,

    -- represented with w in the equation 4.12
    angularFrequency :: Double,

    -- represented with t in the equation 4.12
    time :: Double
    } deriving (Eq, Show)


evalPlaneWave :: PlaneWave -> Double
-- e0 * cos (k \dot r - w * t) = E
evalPlaneWave pw =
    let e0 = amplitude pw
        r = position pw
        k = direction pw
        w = angularFrequency pw
        t = time pw
    in e0 * (cos ((dot k r) - (w * t)))

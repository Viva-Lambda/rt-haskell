module Physics.Wave where

import Physics.Quantity

import Math3D.Vector
import Math3D.CommonOps


import Debug.Trace

speedOfLight :: PhysicalQuantity 
speedOfLight = ScalarField 299792458 "meters per second"

-- planar wave as described by E. Hecht, Optics, 2017, p. 121-122; fig. 4.45

data PlaneWave = PWave {
    -- represented with E_0 in the equation
    amplitude :: Double,

    -- represented with r in the equation 4.12, and fig. 4.45
    position :: Vector,

    -- represented with k in the equation 4.12, and fig. 4.45
    direction :: Vector,

    -- represented with w in the equation 4.12
    angularFrequency :: PhysicalQuantity, -- in radians per second

    -- represented with t in the equation 4.12
    duration :: Double -- in seconds
    } deriving (Eq, Show)

{- relation between wave number and wavelength

velocity and angular frequency of a wave are related by:
$v = v(\lambda)$

where $\lambda$ is the wave length

velocity, wavelength, and the angular frequency of a plane wave is related by:
$v(\lambda) = \lambda f(\lambda)$

where frequency $f$ is angular frequency divided by $2\pi$:
$f = \frac{w}{2\pi}$

-}

-- waveFrequency :: PlaneWave -> PhysicalQuantity -- in hertz
-- waveFrequency a = let w = angularFrequency a

-- waveEnergy :: PlaneWave -> PhysicalQuantity -- in joule
-- radiantFlux :: PlaneWave -> PhysicalQuantity -- in joule per second = watt
-- spectralFlux :: PlaneWave -> PhysicalQuantity -- in watt per meter


waveElectricField :: PlaneWave -> PhysicalQuantity
-- e0 * cos (d \dot r - w * t) = E
waveElectricField pw =
    let e0 = amplitude pw
        r = position pw
        d = direction pw
        rd = dot d r
        w_rs = angularFrequency pw -- in radians per second
        t = duration pw -- in second
    in case w_rs of
        ScalarField w _ ->
            ScalarField (e0 * (cos (rd - (w * t)))) "field"
        VectorField w _ -> traceStack "not implemented" emptyQuantity
            

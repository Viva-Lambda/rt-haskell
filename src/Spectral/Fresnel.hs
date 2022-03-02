-- fresnel computation module
module Spectral.Fresnel where

import Math3D.Vector
import Math3D.CommonOps

import Physics.Wave


cosTheta :: Vector -> Vector -> Double
cosTheta normalizedRayDirection normalToSurface = 
    let a = dot normalToSurface normalizedRayDirection
    in if a > 0.0
       then a
       else dot (multiplyS normalToSurface (-1.0)) normalizedRayDirection

incidentCosTheta :: Vector -> Vector -> Double
incidentCosTheta normalizedRayDirection normSurface =
    cosTheta normalizedRayDirection normSurface

getReflectedCosTheta :: Bool -> Vector -> Vector -> Double
getReflectedCosTheta fromIncidentRayDirection rayDirection normalToSurface =
    let rfdir = if fromIncidentRayDirection
                then toUnit $ reflect rayDirection normalToSurface
                else rayDirection
    in cosTheta rfdir normalToSurface


transmittedSinTheta :: (Double, Double) -> Vector -> Vector -> Double
transmittedSinTheta (incidentRefIndx, transmittedRefIndx)
                    rayDirection normalToSurface =
    let coeff = incidentRefIndx / transmittedRefIndx
        ctheta = incidentCosTheta rayDirection normalToSurface
    in coeff * (sqrt (1.0 - (ctheta * ctheta)))

transmittedCosTheta :: (Double, Double) -> Vector -> Vector -> Double
transmittedCosTheta n1n2
                    rayDirection normalToSurface =
    sqrt (1.0 - ((transmittedSinTheta n1n2 rayDirection normalToSurface) ** 2))

-- incident ref index * sin( incident angle ) = transmitted ref index * sin(transmitted angle)
-- E. Hecht, Optics, 2017, p. 109, eq. 4.4
-- this version comes from: https://en.wikipedia.org/wiki/Snell%27s_law
transmittedDirectionv1 :: (Double, Double) -> Vector -> Vector -> Vector
transmittedDirectionv1 (n1, n2) rdir nsurface =
    let r = n1 / n2
        c = cosTheta rdir nsurface
        rcsqr = sqrt (1.0 - (r * r) * (1.0 - (c * c)))
        rcmin = r * c - rcsqr
        rl = addS rdir r
    in add rl (multiplyS nsurface rcmin)

transmittedDirectionv2 :: (Double, Double) -> Vector -> Vector -> Vector
transmittedDirectionv2 (n1, n2) rdir nsurface =
    let r = n1 / n2
        cost1 = cosTheta rdir nsurface
        cost2 = transmittedCosTheta (n1, n2) rdir nsurface
        nterm = addS nsurface (r * cost1 - cost2)
        lterm = addS rdir r
    in add lterm nterm


data FresnelAmplitude = REFLECTED_P
                      | REFLECTED_S
                      | TRANSMITTED_P
                      | TRANSMITTED_S

-- compute amplitudes using Fresnel relations. References are given in their
-- respective functions
getFresnelAmplitude :: FresnelAmplitude -> (Double, Double) -> (Double, Double) -> Vector -> Vector -> Double -> Double

getFresnelAmplitude famp (ni, nt) (mi, mt)
                    incidentRayDir surfaceNormal incidentAmplitude =
    let nimi = ni / mi
        ntmt = nt / mt
        costi = cosTheta incidentRayDir surfaceNormal
        costt = transmittedCosTheta (ni, nt) incidentRayDir surfaceNormal
        fcoeff = case famp of
                    -- electric field is perpendicular to plane of incidence
                    REFLECTED_P -> let nicosti = nimi * costi
                                       ntcostt = ntmt * costt
                                   in (nicosti - ntcostt) / (nicosti + ntcostt)
                    -- electric field is parallel to plane of incidence
                    REFLECTED_S -> let ntcosti = ntmt * costi
                                       nicostt = nimi * costt
                                   in (ntcosti - nicostt) / (ntcosti + nicostt)
                    -- electric field is perpendicular to plane of incidence
                    TRANSMITTED_P -> let nicosti = nimi * costi
                                         ntcostt = ntmt * costt
                                         upterm = 2.0 * nimi * costi
                                     in upterm / (nicosti + ntcostt)
                    -- electric field is parallel to plane of incidence
                    TRANSMITTED_S -> let ntcosti = ntmt * costi
                                         nicostt = nimi * costt
                                         upterm = 2.0 * nimi * costi
                                     in upterm / (ntcosti + nicostt) 
    in fcoeff * incidentAmplitude

{-
P polarization: Electric field is perpendicular to the plane of incidence.
The formula is given at Hecht, p. 124, eq. 4.32, 4.33 and it fits to the
figure 4.47.
-}
reflectedAmplitudeP :: (Double, Double) -> (Double, Double) -> Vector -> Vector -> Double -> Double
reflectedAmplitudeP nint mimt
                    incidentRayDir surfaceNormal incidentAmplitude =
    getFresnelAmplitude REFLECTED_P nint 
                        mimt incidentRayDir surfaceNormal incidentAmplitude

{-
S polarization: Electric field is parallel to the plane of incidence.
The formula is given at Hecht, p. 125, eq. 4.38, 4.33 and it fits to the
figure 4.48.
-}
reflectedAmplitudeS :: (Double, Double) -> (Double, Double) -> Vector -> Vector -> Double -> Double
reflectedAmplitudeS nint mimt
                    incidentRayDir surfaceNormal incidentAmplitude =
    getFresnelAmplitude REFLECTED_S nint 
                        mimt incidentRayDir surfaceNormal incidentAmplitude

{-
P polarization: Electric field is perpendicular to the plane of incidence.
The formula is given at Hecht, p. 124, eq. 4.32, 4.33 and it fits to the
figure 4.47.
-}
transmittedAmplitudeP :: (Double, Double) -> (Double, Double) -> Vector -> Vector -> Double -> Double
transmittedAmplitudeP nint mimt
                    incidentRayDir surfaceNormal incidentAmplitude =
    getFresnelAmplitude TRANSMITTED_P nint 
                        mimt incidentRayDir surfaceNormal incidentAmplitude

{-
S polarization: Electric field is parallel to the plane of incidence.
The formula is given at Hecht, p. 125, eq. 4.38, 4.33 and it fits to the
figure 4.48.
-}
transmittedAmplitudeS :: (Double, Double) -> (Double, Double) -> Vector -> Vector -> Double -> Double
transmittedAmplitudeS nint mimt
                    incidentRayDir surfaceNormal incidentAmplitude =
    getFresnelAmplitude TRANSMITTED_S nint 
                        mimt incidentRayDir surfaceNormal incidentAmplitude

getScatteringWave :: FresnelAmplitude -> PlaneWave -> (Double, Double) -> (Double, Double) -> Vector -> PlaneWave

getScatteringWave famp incidentLightWave
                       (incidentRefIndx, transmittedRefIndx)
                       (incidentMuIndx, transmittedMuIndx)
                       surfaceNormal =
    let incidentAmplitude = amplitude incidentLightWave
        incidentRayDir = direction incidentLightWave
        refd = reflect incidentRayDir surfaceNormal
        transd = transmittedDirectionv1 (incidentRefIndx, transmittedRefIndx)
                                         incidentRayDir surfaceNormal
        waveDir = case famp of
                      REFLECTED_P -> refd
                      REFLECTED_S -> refd
                      TRANSMITTED_P -> transd
                      TRANSMITTED_S -> transd
        collisionLocation = position incidentLightWave
        scatteringAmp = getFresnelAmplitude famp
                                            (incidentRefIndx, transmittedRefIndx)
                                            (incidentMuIndx, transmittedMuIndx)
                                            incidentRayDir surfaceNormal
                                            incidentAmplitude
    in PWave {
        amplitude = scatteringAmp,
        position = collisionLocation,
        direction = waveDir,
        -- see Hecht, p. 122 equation 4.18 for angular frequency
        angularFrequency = angularFrequency incidentLightWave,
        duration = duration incidentLightWave
        }



getReflectedWaveP :: PlaneWave -> (Double, Double) -> (Double, Double) -> Vector -> PlaneWave
getReflectedWaveP incidentLightWave 
                  (incidentRefIndx, transmittedRefIndx)
                  (incidentMuIndx, transmittedMuIndx)
                  surfaceNormal =
    getScatteringWave REFLECTED_P
                      incidentLightWave
                      (incidentRefIndx, transmittedRefIndx)
                      (incidentMuIndx, transmittedMuIndx)
                      surfaceNormal

getReflectedWaveS :: PlaneWave -> (Double, Double) -> (Double, Double) -> Vector -> PlaneWave
getReflectedWaveS incidentLightWave 
                  (incidentRefIndx, transmittedRefIndx)
                  (incidentMuIndx, transmittedMuIndx)
                  surfaceNormal =
    getScatteringWave REFLECTED_S
                      incidentLightWave
                      (incidentRefIndx, transmittedRefIndx)
                      (incidentMuIndx, transmittedMuIndx)
                      surfaceNormal

getTransmittedWaveP :: PlaneWave -> (Double, Double) -> (Double, Double) -> Vector -> PlaneWave
getTransmittedWaveP incidentLightWave 
                  (incidentRefIndx, transmittedRefIndx)
                  (incidentMuIndx, transmittedMuIndx)
                  surfaceNormal =
    getScatteringWave TRANSMITTED_P
                      incidentLightWave
                      (incidentRefIndx, transmittedRefIndx)
                      (incidentMuIndx, transmittedMuIndx)
                      surfaceNormal

getTransmittedWaveS :: PlaneWave -> (Double, Double) -> (Double, Double) -> Vector -> PlaneWave
getTransmittedWaveS incidentLightWave 
                  (incidentRefIndx, transmittedRefIndx)
                  (incidentMuIndx, transmittedMuIndx)
                  surfaceNormal =
    getScatteringWave TRANSMITTED_S
                      incidentLightWave
                      (incidentRefIndx, transmittedRefIndx)
                      (incidentMuIndx, transmittedMuIndx)
                      surfaceNormal

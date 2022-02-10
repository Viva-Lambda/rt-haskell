{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- material module
module Material.Scatter where

import Math3D.Ray
import Math3D.Vector
import Math3D.CommonOps

-- color
import Color.ColorInterface
import Spectral.SampledSpectrum

--
import Random
import Utility.Utils
import Utility.BaseEnum

--
import Hittable.HitRecord

-- 
import Material.Material
import Material.ScatterRecord

-- pdf
import Pdf.PdfObj
import Pdf.CosinePdf

-- textures
import Texture.Texture
import Texture.TextureObj
import Texture.SolidColor

-- thirdparty
import System.Random


type Attenuation = ColorRecord
type ScatteredRay = Ray

class Scatterer a where
    scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> ColorFlag -> (g, ScatterRecord, Bool)
    emitted :: a -> Double -> Double -> Vector -> WaveVal -> ColorFlag -> ColorRecord
    scattering_pdf :: a -> Ray -> HitRecord -> Ray -> Double


emptyEmitted :: WaveVal -> ColorFlag -> ColorRecord
emptyEmitted wavelen cflag =
    case cflag of
       RGB -> ColorRec {model = ColorRGB zeroV3}
       Spectral REFLECTANCE -> ColorRec {
            model = ColorSpec (ILLUMINANT, (wavelen, 0.0))
            }
       Spectral ILLUMINANT -> ColorRec {
            model = ColorSpec (ILLUMINANT, (wavelen, 0.0))
            }


instance Scatterer Material where
    scatter gen a r h f = 
        case a of
            NoMat -> (gen, emptySRec emptyPdfObj, False)
            (LambMat la) -> scatter gen la r h f
            (MetalMat m) -> scatter gen m r h f
            (DielMat m) -> scatter gen m r h f
            (LightMat m) -> scatter gen m r h f
            (IsotMat m) -> scatter gen m r h f

    emitted a u v p w cflag =
        case a of
            (LightMat m) -> emitted m u v p w cflag
            _ -> emptyEmitted w cflag

    scattering_pdf a r hrec sr =
        case a of
            NoMat -> 0.0
            LambMat la -> scattering_pdf la r hrec sr
            MetalMat m -> scattering_pdf m r hrec sr
            DielMat d -> scattering_pdf d r hrec sr
            LightMat li -> scattering_pdf li r hrec sr
            IsotMat im -> scattering_pdf im r hrec sr


instance Scatterer Lambertian where
    emitted _ _ _ _ w cflag = emptyEmitted w cflag

    scatter !gen !a !inray !hrec _ =
        case a of
            LambT t ->
                let recp = point hrec
                    recn = pnormal hrec
                    RandResult (uvec, g) = randomUnitVector gen
                    sdir = add recn uvec
                    hu = hUV_u hrec
                    hv = hUV_v hrec
                in if nearZeroVec sdir
                   then (g,
                    mkSRecord 
                        (Rd {origin = recp,
                             direction = recn,
                             rtime = rtime inray,
                             wavelength = wavelength inray
                             })
                        (False)
                        (color t hu hv recp (wavelength inray))
                        (PdfCons $! CosNormalPdf recn), True
                        )
                   else (g, 
                    mkSRecord 
                        (Rd {origin = recp,
                             direction = sdir,
                             rtime = rtime inray,
                             wavelength = wavelength inray
                             })
                        (False)
                        (color t hu hv recp (wavelength inray))
                        (PdfCons $! CosNormalPdf recn), True
                        )


    scattering_pdf _ r hrec sr =
        let n = pnormal hrec
            u = toUnit $! direction sr
            cosine = dot n u
        in if cosine < 0.0
           then 0.0
           else cosine / m_pi


instance Scatterer Metal where
    emitted _ _ _ _ w cflag = emptyEmitted w cflag

    scatter !gen !c !inray !hrec _ =
        case c of
            (MetT a b) -> 
                let recp = point hrec
                    recn = pnormal hrec
                    hu = hUV_u hrec
                    hv = hUV_v hrec
                    indir = toUnit $! direction inray
                    refdir = reflect indir recn
                    RandResult (uvec, g) = randomUnitSphere gen
                    rdir = add refdir (multiplyS uvec b)
                in (g,
                    mkSRecord 
                        (Rd {origin = recp, 
                             direction = rdir, 
                             rtime = rtime inray,
                             wavelength = wavelength inray
                             })
                        (True)
                        (color a hu hv recp (wavelength inray)) 
                        (emptyPdfObj),
                    True)

    scattering_pdf _ _ _ _ = 0.0


instance Scatterer Dielectric where
    scattering_pdf _ _ _ _ = 0.0
    emitted _ _ _ _ w cflag = emptyEmitted w cflag
    scatter !gen !a !inray !hrec _ =
        case a of
            (DielRefIndices rs) ->
                let atten = ColorRec {model = ColorRGB $! fromList2Vec 1.0 [1.0, 1.0]}
                    -- can change with respect to wavelength
                    ir = head rs
                    refratio = if isFront hrec
                               then 1.0 / ir
                               else ir
                    udir = toUnit $! direction inray
                    costheta = min (dot (multiplyS udir (-1.0)) (pnormal hrec)) 1.0
                    sintheta = sqrt (1.0 - costheta * costheta)
                    canNotRefract = refratio * sintheta > 1.0
                    RandResult (rval, g) = randval gen
                    schlickVal = schlickRef costheta refratio
                    rdir = if canNotRefract || (schlickVal > rval)
                           then reflect udir (pnormal hrec)
                           else refract udir (pnormal hrec) refratio
                    outray = Rd { origin = point hrec,
                                  direction = rdir,
                                  rtime = rtime inray,
                                  wavelength = wavelength inray
                                 }
                in (g,
                    mkSRecord 
                        outray
                        True
                        atten
                        emptyPdfObj,
                    True)

instance Scatterer DiffuseLight where
    scattering_pdf _ _ _ _ = 0.0
    scatter !gen !a !inray !hrec _ = (gen, emptySRec emptyPdfObj, False)
    emitted b u v p wave _ =
        case b of
            DLightEmitTextureCons a -> color a u v p wave

instance Scatterer Isotropic where
    scattering_pdf _ _ _ _ = 0.0
    emitted _ _ _ _ w cflag = emptyEmitted w cflag
    scatter !gen !b !inray !hrec _ =
        case b of
            IsotTexture a ->
                let RandResult (uvec, g) = randomUnitSphere gen
                    recp = point hrec
                    hu = hUV_u hrec
                    hv = hUV_v hrec
                    outray = Rd {origin = recp,
                                 direction = uvec,
                                 rtime = rtime inray,
                                 wavelength = wavelength inray
                                 }
                    atten = color a hu hv recp (wavelength inray)
                in (g,
                    mkSRecord
                        outray
                        True
                        atten
                        emptyPdfObj,
                    True)

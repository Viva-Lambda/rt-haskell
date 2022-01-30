{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- material module
module Material.Scatter where

import Math3D.Ray
import Math3D.Vector
import Math3D.CommonOps

-- color
import Color.ColorInterface

--
import Random
import Utility.Utils

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


type Attenuation = ColorInterface
type ScatteredRay = Ray
type SOutput = (Attenuation, ScatteredRay, Bool)

class Scatterer a where
    scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> (g, ScatterRecord, Bool)
    emitted :: a -> Double -> Double -> Vector -> ColorInterface
    scattering_pdf :: a -> Ray -> HitRecord -> Ray -> Double


-- scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> Attenuation -> 
-- ScatteredRay -> (Attenuation, ScatteredRay, Bool)

instance Scatterer Material where
    scatter gen a r h = 
        case a of
            NoMat -> (gen, emptySRec emptyPdfObj, False)
            (LambMat la) -> scatter gen la r h
            (MetalMat m) -> scatter gen m r h
            (DielMat m) -> scatter gen m r h
            (LightMat m) -> scatter gen m r h
            (IsotMat m) -> scatter gen m r h

    emitted a u v p =
        case a of
            (LightMat m) -> emitted m u v p
            _ -> ColorInt {stype = RGB, colorData = zeroV3}

    scattering_pdf a r hrec sr = 
        case a of
            NoMat -> 0.0
            LambMat la -> scattering_pdf la r hrec sr
            MetalMat m -> scattering_pdf m r hrec sr
            DielMat d -> scattering_pdf d r hrec sr
            LightMat li -> scattering_pdf li r hrec sr
            IsotMat im -> scattering_pdf im r hrec sr


instance Scatterer Lambertian where
    emitted _ _ _ _ = ColorInt {stype = RGB, colorData = zeroV3}

    scatter !gen !a !inray !hrec =
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
                        (color t hu hv recp)
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
                        (color t hu hv recp)
                        (PdfCons $! CosNormalPdf recn), True
                        )
            -- Color
            LambC c -> let lambtxt = TextureCons $ SolidV c
                       in scatter gen (LambT lambtxt) inray hrec


    scattering_pdf _ r hrec sr =
        let n = pnormal hrec
            u = toUnit $! direction sr
            cosine = dot n u
        in if cosine < 0.0
           then 0.0
           else cosine / m_pi


instance Scatterer Metal where
    emitted _ _ _ _ = ColorInt {stype = RGB, colorData = zeroV3}
    scatter !gen !c !inray !hrec =
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
                        (color a hu hv recp) 
                        (emptyPdfObj),
                    True)
            (MetC a b) -> let mt = TextureCons $ SolidV a
                          in scatter gen (MetT mt b) inray hrec

    scattering_pdf _ _ _ _ = 0.0


instance Scatterer Dielectric where
    scattering_pdf _ _ _ _ = 0.0
    emitted _ _ _ _ = ColorInt {stype = RGB, colorData = zeroV3}
    scatter !gen !a !inray !hrec =
        case a of
            (DielRefIndices rs) ->
                let atten = ColorInt {stype = RGB,
                                      colorData = fromList2Vec 1.0 [1.0, 1.0]}
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
    scatter !gen !a !inray !hrec = (gen, emptySRec emptyPdfObj, False)
    emitted b u v p = 
        case b of
            DLightEmitTextureCons a -> color a u v p
            DLightColorCons a -> let b = TextureCons $ SolidV a
                                 in color b u v p

instance Scatterer Isotropic where
    scattering_pdf _ _ _ _ = 0.0
    emitted _ _ _ _ = ColorInt {stype = RGB, colorData = zeroV3}
    scatter !gen !b !inray !hrec =
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
                    atten = color a hu hv recp
                in (g,
                    mkSRecord
                        outray
                        True
                        atten
                        emptyPdfObj,
                    True)

            IsotColor c -> let mt = TextureCons $ SolidV c
                           in scatter gen (IsotTexture mt) inray hrec

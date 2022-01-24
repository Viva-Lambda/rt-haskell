{-# LANGUAGE BangPatterns #-}
-- renderer function
module Render where

--
import Color.Pixel
import Scenes
import Camera

-- math
import Math3D.Ray
import Math3D.Vector
import Math3D.CommonOps

-- pdf
import Pdf.MixturePdf
import Pdf.HittablePdf
import Pdf.CosinePdf
import Pdf.PdfObj
import Pdf.Pdf

-- material
import Material.Scatter
import Material.ScatterRecord

-- hittable
import Hittable.HittableList
import Hittable.Hittable
import Hittable.HitRecord

-- utility etc
import Utility.Utils
import Utility.HelperTypes
import Random

-- third party
import GHC.Float
import Data.Foldable
import System.Random
import Debug.Trace
import Prelude hiding(subtract)

-- scene
import Scene.Scene


rayColor :: RandomGen g => RandomResult Ray g -> HittableList -> HittableList -> Vector -> Int -> RandomResult Vector g
rayColor !rayr !world lights !background !depth =
    if depth <= 0
    then RandResult (zeroV3, liftRandGen rayr)
    else let hrec = emptyRecord 3
             ray = liftRandVal rayr
             gen = liftRandGen rayr
             (hithrec, isHit, g1) = hit world gen ray 0.001 infty hrec
             HRec{point = recp,
                  pnormal = recnorm,
                  hUV_u = uu,
                  hUV_v = vv,
                  matPtr = m} = hithrec
         in if isHit
            then let sout = scatter g1 m ray hithrec
                     (g2, srec, isScattering) = sout
                     l_e = emitted m uu vv recp
                 in if not isScattering
                    then RandResult (l_e, g2)
                    else let isSpec = isSpecularSR srec
                         in if isSpec
                            then let outray = RandResult (specularRaySR srec, g2)
                                     resNColor = rayColor outray world lights background (depth-1)
                                     attens = attenuationSR srec
                                     f n = multiply n attens
                                 in rfmap f resNColor
                            else -- start computing pdf values
                                 let {
        natten = attenuationSR srec;
        mptr = matPtr hithrec;
        -- cospdf = CosNormalPdf (pnormal hithrec);
        cospdf = pdfPtrSR srec;
        hpdf = HitPdf lights (point hithrec);
        mpdf = MixPdf (NList cospdf [PdfCons hpdf]);
        RandResult (rdir, g3) = generate hpdf g2;
        rout = Rd {origin = point hithrec, direction = toUnit rdir, rtime = rtime ray};
        RandResult (pval, g4) = pvalue hpdf g3 (direction rout);
        routr = RandResult (rout, g4);
        spdf = scattering_pdf mptr ray hithrec rout;
        multv = if pval == 0.0 || isNaN pval
                then 0.0
                else spdf / pval;
        res = let resNColor = rayColor routr world lights background (depth - 1)
                  f n = add l_e (multiplyS (multiply natten n) multv)
              in rfmap f resNColor;
                                    }
                                in res
                                {- in traceStack 
                                        (debugTraceStr [
                                            show rout,
                                            show spdf, show pval, show rdir,
                                            show rcolor
                                            ])
                                        (zeroV3, g5)
                                -}
            else RandResult (background, g1)


renderScene :: RandomGen g => [(Int, Int)] -> g -> Scene -> [Pixel]
renderScene !cs !g scn =
    let cmfrom = cam_look_from scn
        cmto = cam_look_to scn
        cmvf = cam_vfov scn
        cmvup = cam_vup scn
        cm_fdist = cam_focus_distance scn
        cm_apr = cam_aperture scn
        sample_pixs = nb_samples scn
        aratio = aspect_ratio scn
        bdepth = bounce_depth scn
        cam = mkCam cmfrom cmto cmvup cmvf aratio cm_apr cm_fdist 0.0 0.0
        wrld = scene_obj scn
        imw_ = img_width scn
        imh_ = img_height scn
        bground = back_ground scn
        samplingObj = sample_obj scn
    in pixels g cs sample_pixs cam wrld samplingObj bground bdepth (imw_, imh_)

    where pixels gen ((cy, cx):cc) nb_smpl cMra objs sobjs b depth imWh =
            let (pc, g2) = foldColor gen (cy, cx) nb_smpl cMra objs sobjs b depth imWh
                p = Pix {x = cx, y = cy, color = pc}
            in p : pixels g2 cc nb_smpl cMra objs sobjs b depth imWh
          pixels _ [] _ _ _ _ _ _ _ = []

          foldColor rng coord nsmp cmra sobjs sos b bd iMWh =
            let -- foldfn (a -> b -> a) :: 
                foldfn acc _ = let (pcols_, g_) = acc
                                   RandResult (col, g2) = mkColor coord g_ cmra sobjs sos b bd iMWh
                               in (pcols_ ++ [col], g2)
                (pcols, g3) = foldl' foldfn ([], rng) [0..(nsmp - 1)]
            in (foldl1 add pcols, g3)

          mkColor coord rng cmr sobjs sos b bdepth imwimh =
            let rayr = mkPixelRay imwimh coord rng cmr
            in rayColor rayr sobjs sos b bdepth


mkPixelRay :: RandomGen g => (Int, Int) -> (Int, Int) -> g -> Camera -> RandomResult Ray g
mkPixelRay !(imw, imh) !(j,i) gen !cm =
    let fnlst = fromList2NL randval [randval]
        RandResult (lst, g2) = rfmap nl2List (randFoldlFixedRange2 gen fnlst)
        (udouble:vdouble:_) = lst
        u = (udouble + (int2Double i)) / (int2Double (imw - 1))
        v = (vdouble + (int2Double j)) / (int2Double (imh - 1))
    in getRay g2 cm u v


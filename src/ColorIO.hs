{-# LANGUAGE BangPatterns #-}
-- input output of colors
module ColorIO where

import Pixel
import Scenes
import Camera

-- math
import Math3D.Ray
import Math3D.Vector
import Math3D.CommonOps

-- pdf
import Pdf.MixturePdf
import Pdf.HittablePdf
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


rayColor :: RandomGen g => Ray -> HittableList -> HittableList -> Vector -> Int -> g -> (Vector, g)
rayColor !ray !world lights !background !depth !gen =
    if depth <= 0
    then (zeroV3, gen)
    else let hrec = emptyRecord 3
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
                    then (l_e, g2)
                    else let isSpec = isSpecularSR srec
                         in if isSpec
                            then let outray = specularRaySR srec
                                     (ncolor, g3) = rayColor outray world lights background (depth-1) g2
                                 in (multiply ncolor (attenuationSR srec), g3)
                            else -- start computing pdf values
                                 let {
        natten = attenuationSR srec;
        {- 
        hpdf = HitPdf lights (point hithrec);
        mpdf = MixPdf (NList (PdfCons hpdf) [pdfPtrSR srec]);
        -- (ndir, g3) = generate mpdf g2;
        (ndir, g3) = generate hpdf g2;
        rout = Rd {origin = point hithrec,
                   direction = ndir,
                   rtime = rtime ray};
        (pval, g4) = pvalue mpdf g3 ndir;
        -- (pval, g4) = pvalue hpdf g3 ndir;
        (ncolor, g5) = rayColor rout world lights background (depth-1) g4;
        -- (ncolor, g5) = rayColor (specularRaySR srec) world lights background (depth-1) g4;
        l_r = multiplyS (multiply natten ncolor) spdf;
        -- TODO against zero divisions we do the following hack
        -- needs to be organized in a better way to compute a pdf value
        -- for all sampled directions towards a hittable
        rcolor = if pval == 0.0
                 then VList [1.0, 0.2, 0.2]
                 else add l_e (divideS l_r pval);
        -- l_r = multiply natten ncolor;
        -}
        -- sample lights directly
        (xv, g3) = randomDouble g2 213.0 343.0;
        (zv, g4) = randomDouble g3 227.0 332.0;
        onlight = VList [xv, 554.0, zv];
        tolight = subtract onlight (point hithrec);
        disqr = lengthSquared tolight;
        tulight = toUnit tolight;
        dval = dot tulight (pnormal hithrec);
        larea = (343.0-213.0) * (332.0 - 227.0);
        lcos = abs $ vget tulight 1;
        pval = disqr / (lcos * larea);
        rout = Rd {origin = point hithrec,
                   direction = tulight,
                   rtime = rtime ray};
        spdf = scattering_pdf (matPtr hithrec) ray hithrec rout;
        multv = spdf / pval;
        (rcolor, g5) = if dval < 0.0
                 then (l_e, g4)
                 else if lcos < 0.00001
                      then (l_e, g4)
                      else let (rCol, gv) = rayColor rout world lights background (depth - 1) g4
                           in (multiplyS (multiply natten rCol) multv, gv);
                                    }
                                in (rcolor, g5)
                                {- in traceStack 
                                        (debugTraceStr [
                                            show rout, show ncolor, show l_r,
                                            show spdf, show pval, show ndir,
                                            show rcolor
                                            ])
                                        (zeroV3, g5)
                                -}

            else (background, g1)


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
        cam = mkCam cmfrom cmto  cmvup cmvf aratio cm_apr cm_fdist 0.0 0.0
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
                                   (col, g2) = mkColor coord g_ cmra sobjs sos b bd iMWh
                               in (pcols_ ++ [col], g2)
                (pcols, g3) = foldl' foldfn ([], rng) [0..(nsmp - 1)]
            in (foldl1 add pcols, g3)

          mkColor coord rng cmr sobjs sos b bdepth imwimh =
            let (ray, g2) = mkPixelRay imwimh coord rng cmr
                (rcol, g3) = rayColor ray sobjs sos b bdepth g2
            in (rcol, g3)


mkPixelRay :: RandomGen g => (Int, Int) -> (Int, Int) -> g -> Camera -> (Ray, g)
mkPixelRay !(imw, imh) !(j,i) gen !cm  =
    let (udouble, g1) = randval gen
        (vdouble, g2) = randval g1
        u = (udouble + (int2Double i)) / (int2Double (imw - 1))
        v = (vdouble + (int2Double j)) / (int2Double (imh - 1))
    in getRay g2 cm u v


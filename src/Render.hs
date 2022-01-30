{-# LANGUAGE BangPatterns #-}
-- renderer function
module Render where

--
import Scenes
import Camera

-- math
import Math3D.Ray
import Math3D.Vector
import Math3D.CommonOps

-- spectral handling
import Spectral.SampledDistribution
import Spectral.SampledSpectrum

-- color handling
import Color.Pixel
import Color.ColorInterface

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


rayColor :: RandomGen g => RandomResult Ray g -> HittableList -> HittableList -> ColorInterface -> Int -> RandomResult ColorInterface g
rayColor !rayr !world lights !background !depth =
    if depth <= 0
    then RandResult (emptyModelLike background, liftRandGen rayr)
    else let hrec = emptyRecord 3
             ray = liftRandVal rayr
             gen = liftRandGen rayr
             (hithrec, isHit, g1) = hit world gen ray 0.001 infty hrec
             HRec { point = recp,
                    pnormal = recnorm,
                    hUV_u = uu,
                    hUV_v = vv,
                    matPtr = m } = hithrec
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
        rout = Rd {origin = point hithrec, 
                   direction = toUnit rdir, 
                   rtime = rtime ray,
                   wavelength = wavelength ray
                   };
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


getSceneCamera :: Scene -> Camera
getSceneCamera scn =
    let cmfrom = cam_look_from scn
        cmto = cam_look_to scn
        cmvf = cam_vfov scn
        cmvup = cam_vup scn
        cm_fdist = cam_focus_distance scn
        cm_apr = cam_aperture scn
        aratio = aspect_ratio scn
    in mkCam cmfrom cmto cmvup cmvf aratio cm_apr cm_fdist 0.0 0.0

foldColor :: RandomGen g => g -> (Int, Int) -> Camera -> Scene -> (PixelSpectrum, g)
foldColor rng coord cmra scene =
    let samples = nb_samples scene
                -- foldfn (a -> b -> a) :: 
        foldfn acc _ = let (pcols_, g_) = acc
                           RandResult (col, g2) = mkColor coord g_ cmra scene
                       in (pcols_ ++ [col], g2)
        (pcols, g3) = foldl' foldfn ([], rng) [0..(samples - 1)]
    in (foldl1 add pcols, g3)

mkColor :: RandomGen g => (Int, Int) -> g -> Camera -> Scene -> RandomResult PixelSpectrum g
mkColor coord rng cmr scene =
    let imwimh = (img_width scene, img_height scene)
        rayr = mkPixelRay imwimh coord rng cmr
        RandResult (ray, rgen) = rayr
        sceneObjects = scene_obj scene
        sampleObjects = sample_obj scene
        back = back_ground scene
        depth = bounce_depth scene
        rcolor = case back of
                    PixSpecTrichroma _ ->
                        let backColor = toColorInterface back (wavelength ray)
                            RandResult (sceneColor, g1) = rayColor rayr sceneObjects sampleObjects backColor depth
                        in case stype sceneColor of
                                RGB -> let [r, g, b] = vec2List $! colorData sceneColor
                                       in (PixSpecTrichroma (r, g, b), g1)
                                --
                                _ -> traceStack
                                        "Scene color model had change in evaluation"
                                        (zeroPixelSpectrum, g1)
                    PixSpecSampled s ->
                        let fn acc wave =
                                let (lst, gen) = acc
                                    r = RandResult (ray, gen)
                                    backPower = toColorInterface back (wavelength ray)
                                    RandResult (scenePower, g1) =
                                        rayColor r sceneObjects sampleObjects backPower depth
                                in case stype scenePower of
                                     RGB -> traceStack
                                                "Scene color model had change in evaluation"
                                                ([], g1)
                                     _ -> (lst ++ [(wave, colorData scenePower)], g1)
                            (wavePowers, ngen) = foldl fn ([], rgen) [visible_lambda_start..visible_lambda_end]
                            ((w:ws), powerVecs) = unzip wavePowers
                            --
                            (p:ps) = map sumD powerVecs
                            sampledWPower = fromWavesPowers 
                                                (fromList2NL p ps)
                                                (fromList2NL w ws)
                            spect = fromSampledWave sampledWPower REFLECTANCE
                        in (PixSpecSampled spect, ngen)
    in RandResult rcolor

foldPixels :: RandomGen g => g -> [(Int, Int)] -> Camera -> Scene -> [Pixel]
foldPixels gen lst cMra scne =
    case lst of
        [] -> []
        ((cy, cx):cc) ->
            let (pc, g2) = foldColor gen (cy, cx) cMra scne
                p = Pix {x = cx, y = cy, color = pc}
            in p : foldPixels g2 cc cMra scne


renderScene :: RandomGen g => [(Int, Int)] -> g -> Scene -> [Pixel]
renderScene !cs !g scn =
    let cam = getSceneCamera scn
    in foldPixels g cs cam scn


mkPixelRay :: RandomGen g => (Int, Int) -> (Int, Int) -> g -> Camera -> RandomResult Ray g
mkPixelRay !(imw, imh) !(j,i) gen !cm =
    let fnlst = fromList2NL randval [randval]
        RandResult (lst, g2) = rfmap nl2List (randFoldlFixedRange2 gen fnlst)
        (udouble:vdouble:_) = lst
        u = (udouble + (int2Double i)) / (int2Double (imw - 1))
        v = (vdouble + (int2Double j)) / (int2Double (imh - 1))
    in getRay g2 cm u v


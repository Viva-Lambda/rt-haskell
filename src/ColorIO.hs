{-# LANGUAGE BangPatterns #-}
-- input output of colors
module ColorIO where

import GHC.Float
import Data.Foldable
import Math3D.Vector
import Math3D.CommonOps
import Pixel
import Scenes
import Camera
import Math3D.Ray
import Hittable.HittableList
import Hittable.Hittable
import Hittable.HitRecord
import Material.Scatter
import Utility.Utils
import System.Random
import Random
import Prelude hiding(subtract)

-- scene
import Scene.Scene


rayColor :: RandomGen g => Ray -> HittableList -> Vector -> Int -> g -> (Vector, g)
rayColor !ray !world !background !depth !gen =
    if depth <= 0
    then (zeroV3, gen)
    else let hrec = emptyRecord 3
             (hithrec, isHit) = hit world ray 0.001 infty hrec
             HRec{point = recp,
                  pnormal = recnorm,
                  hUV_u = uu,
                  hUV_v = vv,
                  matPtr = m} = hithrec
         in if isHit
            then let sout = scatter gen m ray hithrec
                     (g, natten, outray, isScattering) = sout
                     semit = emitted m uu vv recp
                 in if isScattering
                    then let (ncolor, g2) = rayColor outray world background (depth-1) g
                         in (multiply ncolor natten, g2)
                    else (semit, g)
            else (background, gen)


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
    in pixels g cs sample_pixs cam wrld bground bdepth (imw_, imh_)

    where pixels gen ((cy, cx):cc) nb_smpl cMra objs b depth imWh =
            let (pc, g2) = foldColor gen (cy, cx) nb_smpl cMra objs b depth imWh
                p = Pix {x = cx, y = cy, color = pc}
            in p : pixels g2 cc nb_smpl cMra objs b depth imWh
          pixels _ [] _ _ _ _ _ _ = []

          foldColor rng coord nsmp cmra sobjs b bd iMWh =
            let -- foldfn (a -> b -> a) :: 
                foldfn acc _ = let (pcols_, g_) = acc
                                   (col, g2) = mkColor coord g_ cmra sobjs b bd iMWh
                               in (pcols_ ++ [col], g2)
                (pcols, g3) = foldl' foldfn ([], rng) [0..(nsmp - 1)]
            in (foldl1 add pcols, g3)

          mkColor coord rng cmr sobjs b bdepth imwimh =
            let (ray, g2) = mkPixelRay imwimh coord rng cmr
            in rayColor ray sobjs b bdepth g2


mkPixelRay :: RandomGen g => (Int, Int) -> (Int, Int) -> g -> Camera -> (Ray, g)
mkPixelRay !(imw, imh) !(j,i) gen !cm  =
    let (udouble, g1) = randval gen
        (vdouble, g2) = randval g1
        u = (udouble + (int2Double i)) / (int2Double (imw - 1))
        v = (vdouble + (int2Double j)) / (int2Double (imh - 1))
    in getRay g2 cm u v


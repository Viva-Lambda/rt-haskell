{-# LANGUAGE BangPatterns #-}
-- next week final scene
module Scene.NextWeekFinal where

-- scene default values
import Scene.Scene

-- 
import System.Random
import Random
import GHC.Float

-- math
import Math3D.Vector
import Math3D.Ray

-- texture
import Texture.SolidColor
import Texture.TextureObj

-- hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Hittable
import Hittable.AaRect
import Hittable.Rotatable
import Hittable.Translatable
import Hittable.Bvh

-- instance
import Instance.Box

--
import Camera
import Material.Material

--
import Utility.HelperTypes

mkBoxes :: RandomGen g => g -> (g, [HittableObj])
mkBoxes g =
    let gmat = LambMat $! LambC (VList [0.48, 0.83, 0.53])
        bcoords = zip (map int2Double [0..20]) (map int2Double [0..20])
        mkbox acc (i, j) = let w = 100.0
                               x0 = -1000.0 + (i * w)
                               z0 = -1000.0 + (j * w)
                               y0 = 0.0
                               x1 = x0 + w
                               (g1, lst) = acc
                               (y1, g2) = randomDouble g1 1.0 101.0
                               z1 = z0 + w
                               mnp = VList [x0, y0, z0]
                               mxp = VList [x1, y1, z1]
                           in (g2, lst ++ [HittableCons $ mkBox mnp mxp gmat])
        (g2, boxes) = foldl mkbox (g, []) bcoords
    in (g2, boxes)

nextWeekFinal :: RandomGen g => g -> Scene
nextWeekFinal gen =
    let (g1, boxes) = mkBoxes gen
        mbvh = mkBvh boxes g1 0 (length boxes) 0.0 1.0
        lightMat = LightMat $! DLightColorCons (VList [15.0, 15.0, 15.0])
        light = HittableCons $ mkXzRect 123.0 423.0 147.0 412.0 554.0 lightMat
        hs = HList {objects = NList (HittableCons mbvh) [light]}
    -- in error $ "\nN: " ++ show b2 ++ "\nR: " ++ show b2rot ++ "\nT: " ++ show b2trans
    in SceneVals {
        img_width = 800,
        aspect_ratio = 800.0 / 600.0,
        img_height = 600,
        nb_samples = 10000,
        bounce_depth = 50,
        cam_look_from = VList [478.0, 278.0, -600.0],
        cam_look_to = VList [278.0, 278.0, 0.0],
        cam_vfov = 40.0,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        back_ground = VList [0.0, 0.0, 0.0]
    }

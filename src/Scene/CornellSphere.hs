{-# LANGUAGE BangPatterns #-}
-- cornell box
module Scene.CornellSphere(cornellSphere) where

-- scene default values
import Scene.Scene

import Color.Pixel
-- 
import System.Random
import Random

-- math
import Math3D.Vector
import Math3D.Ray

-- texture
import Texture.SolidColor
import Texture.TextureObj

-- hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Sphere
import Hittable.Hittable
import Hittable.AaRect
import Hittable.Rotatable
import Hittable.Translatable

-- instance
import Instance.Box

--
import Camera
import Material.Material

--
import Utility.HelperTypes

cornellSphere :: RandomGen g => g -> Scene
cornellSphere gen =
    let cfrom = fromList2Vec 278.0 [ 278.0, -800.0]
        cto = fromList2Vec 278.0 [ 278.0, 0.0]
        cvfov = 40.0 
        -- set up camera
        sceneC = mkCamTime cfrom cto camVUp cvfov aspectRatio 0.0 camFocDistance

        whiteMat = LambMat $! LambC (fromList2Vec 0.75 [ 0.75, 0.75])
        redMat = LambMat $! LambC (fromList2Vec 0.65 [ 0.05, 0.05])
        greenMat = LambMat $! LambC (fromList2Vec 0.12 [ 0.45, 0.15])
        lightMat = LightMat $! DLightColorCons (fromList2Vec 15.0 [ 15.0, 15.0])
        metMat = MetalMat $! MetC (fromList2Vec 0.8 [ 0.7, 0.75]) 0.001
        dieMt = DielMat $! DielRefIndices [1.5]
        -- cornell walls
        c1 = 0.0
        c2 = 555.0
        -- yz walls
        yzGreenWall = HittableCons $ mkYzRect c1 c2 c1 c2 c2 greenMat
        yzRedWall = HittableCons $ mkYzRect c1 c2 c1 c2 c1 redMat
        -- xz walls
        xzWhiteWall1 = HittableCons $ mkXzRect c1 c2 c1 c2 c2 whiteMat
        xzWhiteWall2 = HittableCons $ mkXzRect c1 c2 c1 c2 c1 whiteMat
        -- xy wall
        xyWhiteWall = HittableCons $ mkXyRect c1 c2 c1 c2 c2 whiteMat
        -- light
        lightR = HittableCons $ mkXzRect 213.0 343.0 227.0 332.0 554.0 lightMat

        -- boxes
        -- get locating parameters
        loc = getCameraLocatingParams gen sceneC
        (_, _, time) = loc
        b1 = mkBox (zeroV3) (fromList2Vec 165.0 [ 330.0, 165.0]) metMat
        b1rot = mkRotatable b1 (-45.0) RY
        b1trans = HittableCons $! Translate b1rot (fromList2Vec 265.0 [ 0.0, 295.0])
        b2 =  HittableCons $! SphereObj {sphereCenter = fromList2Vec 190.0 [ 90.0, 190.0],
                                         sphereRadius = 90.0, 
                                         sphereMat = dieMt}

        hs = HList {objects = NList (b1trans) [b2, yzGreenWall, yzRedWall,
                                               xzWhiteWall1, xzWhiteWall2,
                                               xyWhiteWall, lightR]}
    -- in error $ "\nN: " ++ show b2 ++ "\nR: " ++ show b2rot ++ "\nT: " ++ show b2trans
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = 100,
        bounce_depth = 20,
        cam_look_from = cfrom,
        cam_look_to = cto,
        cam_vfov = cvfov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        sample_obj = HList {objects = NList lightR [b2, b1trans]},
        back_ground = PixSpecTrichroma (0.0, 0.0, 0.0)
    }

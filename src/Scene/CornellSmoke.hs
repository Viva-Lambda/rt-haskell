-- cornell smoke box
module Scene.CornellSmoke(cornellSmoke) where

-- scene default values
import Scene.Scene

import Color.ColorInterface
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
import Hittable.Hittable
import Hittable.AaRect
import Hittable.Rotatable
import Hittable.Translatable
import Hittable.ConstantMedium

-- instance
import Instance.Box

--
import Camera
import Material.Material

--
import Utility.HelperTypes

cornellSmoke :: RandomGen g => g -> Scene
cornellSmoke gen =
    let cfrom = fromList2Vec 278.0 [ 278.0, -800.0]
        cto = fromList2Vec 278.0 [ 278.0, 0.0]
        cvfov = 40.0 
        -- set up camera
        sceneC = mkCamTime cfrom cto camVUp cvfov aspectRatio 0.0 camFocDistance

        whitishTexture = TextureCons $! SolidD 0.75 0.3 0.6
        whiteTexture = TextureCons $! SolidD 0.75 0.75 0.75
        redTexture = TextureCons $! SolidD 0.65 0.05 0.05
        greenTexture = TextureCons $! SolidD 0.12 0.45 0.15
        highWhiteTexture = TextureCons $! SolidD 15.0 15.0 15.0
        blackTexture = TextureCons $! SolidD 0.0 0.0 0.0

        whiteMat = LambMat $! LambT whiteTexture
        redMat = LambMat $! LambT redTexture
        greenMat = LambMat $! LambT greenTexture
        lightMat = LightMat $! DLightEmitTextureCons highWhiteTexture

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
        b1 = mkBox (zeroV3) (fromList2Vec 165.0 [ 330.0, 165.0]) whiteMat
        b1rot = mkRotatable b1 45.0 RY
        b1trans = Translate b1rot (fromList2Vec 265.0 [ 0.0, 295.0])
        b2 = mkBox (zeroV3) (fromList2Vec 165.0 [ 165.0, 165.0]) whiteMat
        b2rot = mkRotatable b2 (-18.0) RY
        b2trans = Translate b2rot (fromList2Vec 130.0 [0.0, 65.0])
        b1smoke = mkConstantMedium b1trans 0.1  whitishTexture-- white
        b2smoke = mkConstantMedium b2trans 0.01 blackTexture -- black

        hs = HList {objects = NList (HittableCons b1smoke) [HittableCons $ b2smoke,
                                                      yzGreenWall, yzRedWall,
                                                      xzWhiteWall1, xzWhiteWall2,
                                                      xyWhiteWall, lightR]}
    -- in error $ "\nN: " ++ show b2 ++ "\nR: " ++ show b2rot ++ "\nT: " ++ show b2trans
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = 200,
        bounce_depth = 50,
        cam_look_from = cfrom,
        cam_look_to = cto,
        cam_vfov = cvfov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        sample_obj = HList {objects = NList lightR []},
        back_ground = PixSpecTrichroma (0.0, 0.0, 0.0)
    }

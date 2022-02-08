-- Spectral Checker scene
module Scene.SpectralScene(cornellBoxSpectral) where

-- scene default values
import Scene.Scene

import Color.ColorInterface
import Color.Pixel

-- spectral
import Spectral.SampledSpectrum
-- 
import System.Random
import Random

-- math
import Math3D.Vector
import Math3D.Ray

-- texture
import Texture.SolidColor
import Texture.TextureObj
import Texture.Spectral

-- hittable
import Hittable.HittableList
import Hittable.HittableObj
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

cornellBoxSpectral :: RandomGen g => g -> Scene
cornellBoxSpectral gen =
    let cfrom = fromList2Vec 278.0 [ 278.0, -800.0]
        cto = fromList2Vec 278.0 [ 278.0, 0.0]
        cvfov = 40.0 
        -- set up camera
        sceneC = mkCamTime cfrom cto camVUp cvfov aspectRatio 0.0 camFocDistance
        whiteTexture = TextureCons (SpectT $! fromRGBModel 0.75 0.75 0.75 REFLECTANCE)
        redTexture = TextureCons (SpectT $! fromRGBModel 0.65 0.05 0.05 REFLECTANCE)
        greenTexture = TextureCons (SpectT $! fromRGBModel 0.12 0.45 0.15 REFLECTANCE)
        highWhiteTexture = TextureCons (SpectT $! fromRGBModel 15.0 15.0 15.0 ILLUMINANT)

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
        b1rot = mkRotatable b1 45.0 RY (show b1)
        b1off = fromList2Vec 265.0 [0.0, 295.0]
        b1trans = HittableCons $! Translate b1rot b1off (show b1rot)
        b2 = mkBox (zeroV3) (fromList2Vec 165.0 [ 165.0, 165.0]) whiteMat
        b2rot = mkRotatable b2 (-18.0) RY (show b2)
        b2off = fromList2Vec 130.0 [0.0, 65.0]
        b2trans = HittableCons $! Translate b2rot b2off (show b2rot)

        hs = HList {objects = NList (b1trans) [b2trans, yzGreenWall, yzRedWall,
                                               xzWhiteWall1, xzWhiteWall2,
                                               xyWhiteWall, lightR]}
    -- in error $ "\nN: " ++ show b2 ++ "\nR: " ++ show b2rot ++ "\nT: " ++ show b2trans
    in SceneVals {
        img_width = 128,
        aspect_ratio = aspectRatio,
        img_height = getImgHeight 128 aspectRatio,
        nb_samples = 2,
        bounce_depth = 3,
        cam_look_from = cfrom,
        cam_look_to = cto,
        cam_vfov = cvfov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        sample_obj = HList {objects = NList lightR []},
        back_ground = PixSpecSampled $! fromRGBModel 0.0 0.0 0.0 REFLECTANCE
    }

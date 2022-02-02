-- Spectral Checker scene
module Scene.SpectralChecker(twoCheckeredSpheresSpectral) where

import Scene.Scene


import Color.ColorInterface
import Color.Pixel

-- spectral handling
import Spectral.SampledDistribution
import Spectral.SampledSpectrum

-- math3d
import Math3D.Vector
import Math3D.CommonOps

-- hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Sphere

-- texture
import Texture.TextureObj
import Texture.SolidColor
import Texture.Checker
import Texture.Spectral

-- material
import Material.Material

-- utility
import Utility.HelperTypes

twoCheckeredSpheresSpectral :: Scene
twoCheckeredSpheresSpectral =
    let s1 = SpectT $! fromRGBModel 0.2 0.3 0.1 REFLECTANCE
        s2 = SpectT $! fromRGBModel 0.9 0.9 0.8 REFLECTANCE
        backSpectrum = fromRGBModel 0.7 0.8 1.0 REFLECTANCE
        tobj = TextureCons $! CheckT s1 s2
        lmb = LambMat $! LambT tobj
        sp1 = SphereObj {sphereCenter = fromList2Vec 0.0 [-10.0, 0.0],
                         sphereRadius = 10,
                         sphereMat = lmb}
        sp2 = SphereObj {sphereCenter = fromList2Vec 0.0 [10.0, 0.0],
                         sphereRadius = 10,
                         sphereMat = lmb}
        hs = HList {objects = NList (HittableCons sp1) [HittableCons sp2]}
    in SceneVals {
        img_width = 128,
        aspect_ratio = aspectRatio,
        img_height = getImgHeight 128 aspectRatio,
        nb_samples = 3,
        bounce_depth = 2,
        cam_look_from = camLookFrom,
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        sample_obj = HList {objects = NList (HittableCons sp1) []},
        back_ground = PixSpecSampled backSpectrum
    }


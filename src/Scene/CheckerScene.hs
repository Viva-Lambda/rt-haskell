-- Checker scene
module Scene.CheckerScene(twoCheckeredSpheres) where

import Scene.Scene


import Color.ColorInterface
import Color.Pixel

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

-- material
import Material.Material

-- utility
import Utility.HelperTypes

twoCheckeredSpheres :: Scene
twoCheckeredSpheres =
    let s1 = SolidD 0.2 0.3 0.1
        s2 = SolidD 0.9 0.9 0.8
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
        img_width = 256,
        aspect_ratio = aspectRatio,
        img_height = getImgHeight 256 aspectRatio,
        nb_samples = nbSamples,
        bounce_depth = bounceDepth,
        cam_look_from = camLookFrom,
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        sample_obj = hs,
        back_ground = PixSpecTrichroma (0.7, 0.8, 1.0)
    }


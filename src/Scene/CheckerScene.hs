-- Checker scene
module Scene.CheckerScene(twoCheckeredSpheres) where

import Scene.Scene

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

twoCheckeredSpheres :: Scene
twoCheckeredSpheres =
    let s1 = SolidV $! VList [0.2, 0.3, 0.1]
        s2 = SolidV $! VList [0.9, 0.9, 0.8]
        tobj = CheckerTexture $! CheckT s1 s2
        lmb = LambMat $! Lamb {lalbedo = tobj}
        sp1 = SphereObj {sphereCenter = VList [0.0, -10.0, 0.0],
                         sphereRadius = 10,
                         sphereMat = lmb}
        sp2 = SphereObj {sphereCenter = VList [0.0, 10.0, 0.0],
                         sphereRadius = 10,
                         sphereMat = lmb}
        hs = HList [HitSphere sp1, HitSphere sp2]
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = nbSamples,
        bounce_depth = bounceDepth,
        cam_look_from = camLookFrom,
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        back_ground = VList [0.7, 0.8, 1.0]
    }


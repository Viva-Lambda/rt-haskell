-- perlin sphere scene
module Scene.PerlinSphere(twoPerlinSpheres) where

import Scene.Scene

-- math
import Math3D.Vector
import Math3D.CommonOps

-- hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Sphere
import Hittable.MovingSphere

-- texture
import Texture.SolidColor
import Texture.TextureObj
import Texture.Noise

-- material
import Material.Material

-- random
import System.Random
import GHC.Float
import Random

import Utility.HelperTypes


twoPerlinSpheres :: RandomGen g => g -> Scene
twoPerlinSpheres g =
    let ptex = TextureCons $! mkPerlinNoise g 4.0
        lmb = LambMat $! LambT ptex
        sp1 = SphereObj {sphereCenter = VList [0.0, -1000.0, 0.0],
                         sphereRadius = 1000,
                         sphereMat = lmb}
        sp2 = SphereObj {sphereCenter = VList [0.0, 2.0, 0.0],
                         sphereRadius = 2,
                         sphereMat = lmb}
        hs = HList {objects = NList (HittableCons sp1) [HittableCons sp2]}
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


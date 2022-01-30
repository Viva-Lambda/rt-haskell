-- diffuse sphere scene
module Scene.DiffuseSphere(diffuseSphere) where

import Scene.Scene

-- math
import Math3D.Vector
import Math3D.CommonOps

-- color
import Color.Pixel

-- hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Sphere
import Hittable.MovingSphere

-- texture
import Texture.SolidColor
import Texture.TextureObj

-- material
import Material.Material

-- utility
import Utility.HelperTypes

diffuseSphere :: Scene
diffuseSphere =
    let sobj = HList {objects = NList (
            HittableCons $! SphereObj {
                            sphereCenter = fromList2Vec (-4.0) [1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = LambMat $! LambC (fromList2Vec 0.4 [0.2, 0.1])
                        }) [
            HittableCons $ SphereObj {
                            sphereCenter = fromList2Vec 0.0 [-1000.0, 0.0],
                            sphereRadius = 1000.0,
                            sphereMat =  LambMat $! LambC (fromList2Vec 0.5 [0.5, 0.5])
                        } ]}
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
        cam_aperture = 0.1,
        scene_obj = sobj,
        sample_obj = sobj,
        back_ground = PixSpecTrichroma (0.7,0.8,1.0)
         }

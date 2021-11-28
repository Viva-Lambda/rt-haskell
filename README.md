# rt-haskell

A Path Tracer in Haskell

We follow mostly the P. Shirley's architecture with couple of differences.

The branches follow the chapters from [online](raytracing.github.io/)
repository.


## Show case

- Small color gradient in 01-ppm branch:

![ppm-color-gradient](./images/gradient.png)


- Red sphere from 04-sphere branch:

![red-sphere](./images/sphere.png)

- Normals from branch 05-surface:

![surface-normals](./images/05-surface.png)

- Multiple objects from branch 06-multiple:

![multiple-normals](./images/multiple.png)

- Antialiasing from branch 07-antialias:

![antialias-normals](./images/antialias.png)

- Diffuse image from branch 08-diffuse

![diffuse-image](./images/diffuse.png)

- Metal image from branch 09-metal

![metal-image-01](./images/metal.png)

- Fuzzy metal image from branch 09-metal

![metal-image-02](./images/fuzzmetal.png)

- Dielectric from branch 10-dielectric

![dielectric-01](./images/diel01.png)

- Camera focus from branch 11-camera

![focus-image-02](./images/focus.png)

- A version of final scene from branch 12-oneweekend

![oneweekend-final-image-01](./images/final-oneweekend-diffuse.png)

- Another version of final scene branch 12-oneweekend

![oneweekend-final-image-02](./images/final-oneweekend-metallic.png)

- Fixed version of final scene. The fix happens around branch 14-texture

![oneweekend-final-image-03](./images/oneweekendfinal.png)

- Final one weekend final branch 14-texture

![oneweekend-final-image-04](./images/oneweekend.png)

- Motion blur branch 14-texture

![motion-blur-image-01](./images/motionblur.png)

- Checkered texture from branch 14-texture

![checker-image-01](./images/checker.png)

- Perlin Noise with Light from branch 14-texture

![perlin-image-01](./images/light.png)

- Earth image from branch 14-texture

![earth-image-01](./images/earth.png)

- Cornell box image from branch 15-instances

![cornell-box-01](./images/cornell.png)

## Some Notes on Performance

The from branch 08-diffuse an onwards as the usage of random functions become
prominent the performance decreases considerably. However the inverse is also
true, if you can place your random generators efficiently, you can easily
increase your performance. I simply concentrated on getting the images right.
Do not be surprised if you find that some other arrangement of RNGs result in
better performance.

## Planned Features

I hope to make the tracer as minimal but useful as possible.
Here is a list of planned features:

- Loading assets with obj files
- Spectral rendering switch
- BVH acceleration structure: done but not tested.
- Multithreaded rendering: This is as easy as passing -N3 as option now, since
  most of the code is composed of pure functions.

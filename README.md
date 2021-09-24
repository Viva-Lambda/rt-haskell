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

## Planned Features

I hope to make the tracer as minimal but useful as possible.
Here is a list of planned features:

- Loading assets with obj files
- Spectral rendering switch
- BVH acceleration structure
- Multithreaded rendering

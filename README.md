# noise
noise implements several coherent noise generation functions for the GLSL. Each type of noise is implemented as a [glls](http://wiki.call-cc.org/eggref/4/glls) shader. While it is not necessary to use glls in your application, it is recommended since it makes things much easier. It is also entirely possible to use this library to create noise for an application that doesn’t otherwise use OpenGL. In this case, noise can be generated (very quickly!) on the GPU and then saved as a texture for access by the CPU. See the [Examples](#examples) section for an example of this.

noise is indebted to Stefan Gustavson and Ashima Arts who created the highly optimized shaders that this library re-implements. You can see a description of these shaders [in their own words](https://github.com/ashima/webgl-noise/wiki).

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install noise`.

## Requirements
* glls

## Documentation
The following sections describe the shaders that noise exports and the symbols that they, in turn, export.

    [constant] simplex-noise-2d

**Exports**

    (snoise (POSITION #:vec2)) -> #:float

Given a 2D position, return the simplex noise value at that point.

    (fractal-snoise (POSITION #:vec2) (OCTAVES #:int) (FREQUENCY #:float) 
                    (AMPLITUDE #:float) (PERSISTENCE #:float) (LACUNARITY #:float) 
         -> #:float

Given a 2D position, return the fractal simplex noise value at that point. `OCTAVES` is the number of octaves of noise to sample, `FREQUENCY` is the frequency of the first octave, `AMPLITUDE` is the amplitude of the first octave, `PERSISTENCE` is the amount by which the frequency is scaled over each octave, `LACUNARITY` is the amount by which the amplitude is scaled over each octave.


    [constant] simplex-noise-3d

**Exports**

    (snoise (POSITION #:vec3)) -> #:float

Given a 3D position, return the simplex noise value at that point.

    (fractal-snoise (POSITION #:vec3) (OCTAVES #:int) (FREQUENCY #:float) 
                     (AMPLITUDE #:float) (PERSISTENCE #:float) (LACUNARITY #:float) 
         -> #:float

Given a 3D position, return the fractal simplex noise value at that point. `OCTAVES` is the number of octaves of noise to sample, `FREQUENCY` is the frequency of the first octave, `AMPLITUDE` is the amplitude of the first octave, `PERSISTENCE` is the amount by which the frequency is scaled over each octave, `LACUNARITY` is the amount by which the amplitude is scaled over each octave


    [constant] simplex-noise-4d

**Exports**

    (snoise (POSITION #:vec4)) -> #:float

Given a 4D position, return the simplex noise value at that point.

    (fractal-snoise (POSITION #:vec4) (OCTAVES #:int) (FREQUENCY #:float) 
                    (AMPLITUDE #:float) (PERSISTENCE #:float) (LACUNARITY #:float) 
         -> #:float

Given a 4D position, return the fractal simplex noise value at that point. `OCTAVES` is the number of octaves of noise to sample, `FREQUENCY` is the frequency of the first octave, `AMPLITUDE` is the amplitude of the first octave, `PERSISTENCE` is the amount by which the frequency is scaled over each octave, `LACUNARITY` is the amount by which the amplitude is scaled over each octave


    [constant] cell-noise-2d

**Exports**

    jitter -> #:float

A variable that sets the amount of possible “jitter” for each feature point. Should be a value between 0.0 and 1.0, with 0.0 creating a regular grid of points and 1.0 creating the greatest randomness. If there are problems with discontinuities, decrease the jitter. Defaults to 0.9.

    (cell-noise (POSITION #:vec2)) -> #:float

Given a 2D position, return the distance to the nearest feature point. This is fast, but lower-quality than `cell-noise2`

    (cell-noise2 (POSITION #:vec2)) -> #:vec2

Given a 2D position, return the a vector containing the distance to the nearest feature point and second nearest feature point (`(F1 F2)`).


    [constant] cell-noise-3d

**Exports**

    jitter -> #:float

A variable that sets the amount of possible “jitter” for each feature point. Should be a value between 0.0 and 1.0, with 0.0 creating a regular grid of points and 1.0 creating the greatest randomness. If there are problems with discontinuities, decrease the jitter. Defaults to 0.9.

    (cell-noise (POSITION #:vec3)) -> #:float

Given a 3D position, return the distance to the nearest feature point. This is fast, but lower-quality than `cell-noise2`

    (cell-noise2 (POSITION #:vec3)) -> #:vec2

Given a 3D position, return the a vector containing the distance to the nearest feature point and second nearest feature point (`(F1 F2)`).


    [constant] flow-nosie-2d

**Exports**

    isotropic -> #:bool

A variable that controls whether the noise created is faster but less isotropic (set to `#f`) or slower but more isotropic (set to `#t`). Defaults to `#f`.

    (flow-noise (POSITION (in #:vec2)) (ROTATION (in #:float)) (GRADIENT (out #:vec2))
      -> #:float

Given a 2D position and rotation, returns the value of the flow-noise at that point as well as the gradient (via `GRADIENT`).

### Shader source
    [string] simplex-noise-2d-source
    [string] simplex-noise-3d-source
    [string] simplex-noise-4d-source
    [string] cell-noise-2d-source
    [string] cell-noise-3d-source
    [string] flow-noise-2d-source

The source for each shader is provided in these strings, in the case that the user does not want to directly use glls.

## Examples
An example of the use of each type of noise can be seen in [the examples directory](https://github.com/AlexCharlton/noise/tree/master/examples). These examples rely on [opengl-glew](http://wiki.call-cc.org/eggref/4/opengl-glew), [gl-math](http://wiki.call-cc.org/eggref/4/gl-math), [gl-utils](http://wiki.call-cc.org/eggref/4/gl-utils), and [glfw3](http://wiki.call-cc.org/eggref/4/glfw3). They can either be run directly with `csi` or, since they use `glls-render`, they can be compiled by linking with GL (e.g. `csc -lGL 2d-simplex.scm`).

It is important to note how the noise shaders are being imported in these examples: through [glls’ `use` keyword](http://wiki.call-cc.org/eggref/4/glls#shaders-that-export). This makes it so that the prototypes from the noise shaders are automatically added to the examples’ fragment shaders. 

[The render-to-texture example](https://github.com/AlexCharlton/noise/tree/master/examples/render-to-texture.scm) provides an additional example to illustrate how noise can be captured to a texture via a framebuffer. With the noise in a texture, it can then be retrieved to RAM if access to the noise from the CPU is desired.

## Version history
### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/noise).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD

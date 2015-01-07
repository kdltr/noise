;;;; These shaders are a direct port of https://github.com/ashima/webgl-noise

(define-shader simplex-noise-2d
    (#:fragment export: (snoise fractal-snoise))
  (define (mod289 (x #:vec3)) #:vec3
    (- x (* (floor (* x (/ 1.0 289.0)))
            289.0)))

  (define (mod289 (x #:vec2)) #:vec2
    (- x (* (floor (* x (/ 1.0 289.0)))
            289.0)))

  (define (permute (x #:vec3)) #:vec3
    (mod289 (* (+ (* x 34.0)
                  1.0)
               x)))

  (define (snoise (v #:vec2)) #:float
    (let* ((c (const #:vec4) (vec4 0.211324865405187 ; (3.0-sqrt(3.0))/6.0
                                   0.366025403784439 ; 0.5*(sqrt(3.0)-1.0)
                                   -0.577350269189626 ; -1.0 + 2.0 * C.x
                                   0.024390243902439  ; 1.0 / 41.0
                                   ))
           ;;  First corner
           (i #:vec2 (floor (+ v (dot v (~~ c y y)))))
           (x0 #:vec2 (+ v (- i) (dot i (~~ c x x))))
           ;;  Other corners
           (i1 #:vec2 (if (> (.. x0 x) (.. x0 y))
                          (vec2 1 0)
                          (vec2 0 1)))
           (x12 #:vec4 (+ (~~ x0 x y x y)
                          (~~ c x x z z))))
      (-= (~~ x12 x y) i1)
      ;; Permutations
      (set! i (mod289 i))             ; Avoid truncation effects
      (let ((p #:vec3 (permute (+ (permute (+ (.. i y) (vec3 0 (.. i1 y) 1)))
                                  (.. i x)
                                  (vec3 0 (.. i1 x) 1))))
            (m #:vec3 (max (- 0.5 (vec3 (dot x0 x0)
                                        (dot (~~ x12 x y) (~~ x12 x y))
                                        (dot (~~ x12 z w) (~~ x12 z w))))
                           0.0))
            ;; Gradients: 41 points uniformly over a line, mapped onto a diamond.
            ;; The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)
            (x #:vec3 (- (* 2 (fract (* p (~~ c w w w))))
                         1))
            (h #:vec3 (- (abs x) 0.5))
            (ox #:vec3 (floor (+ x 0.5)))
            (a0 #:vec3 (- x ox))
            (g #:vec3))
        (set! m (* m m))
        (set! m (* m m))
        (set! m (* m (- 1.79284291400159 (* 0.85373472095314 (+ (* a0 a0)
                                                                (* h h))))))
        ;; Compute final noise value at P
        (set! (.. g x) (+ (* (.. a0 x) (.. x0 x))
                          (* (.. h x) (.. x0 y))))
        (set! (~~ g y z) (+ (* (~~ a0 y z) (~~ x12 x z))
                            (* (~~ h y z) (~~ x12 y w))))
        (* 130.0 (dot m g)))))

  (define (fractal-snoise (v #:vec2) (octaves #:int)
                          (frequency #:float)
                          (amplitude #:float)
                          (persistence #:float)
                          (lacunarity #:float))
      #:float
    (let ((r #:float 0))
      (do-times (o octaves)
        (+= r (* amplitude (snoise (* v frequency))))
        (*= frequency persistence)
        (*= amplitude lacunarity))
      r))
  ) ;end shader

(define-shader simplex-noise-3d
    (#:fragment export: (snoise fractal-snoise))
  (define (mod289 (x #:vec3)) #:vec3
    (- x (* (floor (* x (/ 1.0 289.0)))
            289.0)))

  (define (mod289 (x #:vec4)) #:vec4
    (- x (* (floor (* x (/ 1.0 289.0)))
            289.0)))

  (define (permute (x #:vec4)) #:vec4
    (mod289 (* (+ (* x 34.0)
                  1.0)
               x)))

  (define (taylor-inv-sqrt (r #:vec4)) #:vec4
    (- 1.79284291400159 (* 0.85373472095314 r)))

  (define (snoise (v #:vec3)) #:float
    (let* ((c (const #:vec2) (vec2 (/ 1.0 6.0) (/ 1.0 3.0)))
           (d (const #:vec4) (vec4 0.0 0.5 1.0 2.0))
           ;;  First corner
           (i #:vec3 (floor (+ v (dot v (~~ c y y y)))))
           (x0 #:vec3 (+ v (- i) (dot i (~~ c x x x))))
           ;;  Other corners
           (g #:vec3 (step (~~ x0 y z x) (~~ x0 x y z)))
           (l #:vec3 (- 1.0 g))
           (i1 #:vec3 (min (~~ g x y z) (~~ l z x y)))
           (i2 #:vec3 (max (~~ g x y z) (~~ l z x y)))
           (x1 #:vec3 (+ x0 (- i1) (~~ c x x x)))
           (x2 #:vec3 (+ x0 (- i2) (~~ c y y y)))
           (x3 #:vec3 (- x0 (~~ d y y y))))
      ;; Permutations
      (set! i (mod289 i)) ; Avoid truncation effects
      (let* ((p #:vec4 (permute
                        (+ (permute
                            (+ (permute (+ (.. i z)
                                           (vec4 0 (.. i1 z) (.. i2 z) 1)))
                               (.. i y)
                               (vec4 0 (.. i1 y) (.. i2 y) 1)))
                           (.. i x)
                           (vec4 0 (.. i1 x) (.. i2 x) 1))))
             ;; Gradients: 7x7 points over a square, mapped onto an octahedron.
             ;; The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
             (ns #:vec3 (- (* 0.142857142857 (~~ d w y z))
                           (~~ d x z x)))
             (j #:vec4 (- p (* 49.0 (floor (* p (.. ns z) (.. ns z))))))
             (x_ #:vec4 (floor (* j (.. ns z))))
             (x #:vec4 (+ (* x_ (.. ns x))
                          (~~ ns y y y y)))
             (y #:vec4 (+ (* (floor (- j (* 7 x_)))
                             (.. ns x))
                          (~~ ns y y y y)))
             (h #:vec4 (- 1.0 (abs x) (abs y)))
             (b0 #:vec4 (vec4 (~~ x xy) (~~ y xy)))
             (b1 #:vec4 (vec4 (~~ x zw) (~~ y zw)))
             (s0 #:vec4 (+ (* (floor b0) 2)
                           1))
             (s1 #:vec4 (+ (* (floor b1) 2)
                           1))
             (sh #:vec4 (- (step h (vec4 0))))
             (a0 #:vec4 (+ (~~ b0 x z y w)
                           (* (~~ s0 x z y w) (~~ sh x x y y))))
             (a1 #:vec4 (+ (~~ b1 x z y w)
                           (* (~~ s1 x z y w) (~~ sh z z w w))))
             (p0 #:vec3 (vec3 (~~ a0 x y) (.. h x)))
             (p1 #:vec3 (vec3 (~~ a0 z w) (.. h y)))
             (p2 #:vec3 (vec3 (~~ a1 x y) (.. h z)))
             (p3 #:vec3 (vec3 (~~ a1 z w) (.. h w)))
             (m #:vec4 (max (- 0.6 (vec4 (dot x0 x0) (dot x1 x1)
                                         (dot x2 x2) (dot x3 x3)))
                            0.0))
             (norm #:vec4 (taylor-inv-sqrt (vec4 (dot p0 p0) (dot p1 p1)
                                                 (dot p2 p2) (dot p3 p3)))))
        ;; Mix final noise value
        (*= p0 (.. norm x))
        (*= p1 (.. norm y))
        (*= p2 (.. norm z))
        (*= p3 (.. norm w))
        (set! m (* m m))
        (* 42.0 (dot (* m m)
                     (vec4 (dot p0 x0) (dot p1 x1)
                           (dot p2 x2) (dot p3 x3)))))))

  (define (fractal-snoise (v #:vec3) (octaves #:int)
                          (frequency #:float)
                          (amplitude #:float)
                          (persistence #:float)
                          (lacunarity #:float))
       #:float
    (let ((r #:float 0))
      (do-times (o octaves)
        (+= r (* amplitude (snoise (* v frequency))))
        (*= frequency persistence)
        (*= amplitude lacunarity))
      r))
  ) ;end shader

(define-shader simplex-noise-4d
    (#:fragment export: (snoise fractal-snoise))
  (define (mod289 (x #:vec4)) #:vec4
    (- x (* (floor (* x (/ 1.0 289.0)))
            289.0)))

  (define (mod289 (x #:float)) #:float
    (- x (* (floor (* x (/ 1.0 289.0)))
            289.0)))

  (define (permute (x #:vec4)) #:vec4
    (mod289 (* (+ (* x 34.0)
                  1.0)
               x)))

  (define (permute (x #:float)) #:float
    (mod289 (* (+ (* x 34.0)
                  1.0)
               x)))

  (define (taylor-inv-sqrt (r #:vec4)) #:vec4
    (- 1.79284291400159 (* 0.85373472095314 r)))

  (define (taylor-inv-sqrt (r #:float)) #:float
    (- 1.79284291400159 (* 0.85373472095314 r)))

  (define (grad4 (j #:float) (ip #:vec4)) #:vec4
    (let ((ones (const #:vec4) (vec4 1.0 1.0 1.0 -1.0))
          (p #:vec4)
          (s #:vec4))
      (set! p.xyz (- (* (floor (* (fract (* (vec3 j)
                                            ip.xyz))
                                  7.0))
                        ip.z)
                     1.0))
      (set! p.w (- 1.5 (dot (abs p.xyz)
                            ones.xyz)))
      (set! s (vec4 (less-than p (vec4 0.0))))
      (set! p.xyz (+ p.xyz (* (- (* s.xyz 2) 1.0)
                              s.www)))
      p))

  (define (snoise (v #:vec4)) #:float
    (let* ((c (const #:vec4) (vec4 0.138196601125011
                                   0.276393202250021
                                   0.414589803375032
                                   -0.447213595499958))
           ;;  First corner
           (i #:vec4 (floor (+ v (dot v (vec4 0.309016994374947451)))))
           (x0 #:vec4 (+ v (- i) (dot i c.xxxx)))
           (is-x #:vec3 (step x0.yzw x0.xxx))
           (is-y-z #:vec3 (step x0.zww x0.yyz))
           (i0 #:vec4))
      ;;  Other corners
      (set! i0.x (+ is-x.x is-x.y is-x.z))
      (set! i0.yzw (- 1.0 is-x))
      (+= i0.y (+ is-y-z.x is-y-z.y))
      (+= i0.zw (- 1.0 is-y-z.xy))
      (+= i0.z is-y-z.z)
      (+= i0.w (- 1.0 is-y-z.z))
      (let* ((i3 #:vec4 (clamp i0 0.0 1.0))
             (i2 #:vec4 (clamp (- i0 1.0) 0.0 1.0))
             (i1 #:vec4 (clamp (- i0 2.0) 0.0 1.0))
             (x1 #:vec4 (+ x0 (- i1) c.xxxx))
             (x2 #:vec4 (+ x0 (- i2) c.yyyy))
             (x3 #:vec4 (+ x0 (- i3) c.zzzz))
             (x4 #:vec4 (+ x0 c.wwww)))
        ;; Permutations
        (set! i (mod289 i)) ; Avoid truncation effects
        (let* ((j0 #:float (permute (+ (permute (+ (permute (+ (permute i.w)
                                                               i.z))
                                                   i.y))
                                       i.x)))
               (j1 #:vec4 (permute
                           (+ (permute
                               (+ (permute
                                   (+ (permute (+ i.w (vec4 i1.w i2.w i3.w 1.0)))
                                      i.z (vec4 i1.z i2.z i3.z 1.0)))
                                  i.y (vec4 i1.y i2.y i3.y 1.0)))
                              i.x (vec4 i1.x i2.x i3.x 1.0))))
               ;; Gradients: 7x7x6 points over a cube, mapped onto a 4-cross polytope
               ;; 7*7*6 = 294, which is close to the ring size 17*17 = 289.
               (ip #:vec4 (vec4 (/ 1.0 294.0) (/ 1.0 49.0) (/ 1.0 7.0) 0.0))
               (p0 #:vec4 (grad4 j0 ip))
               (p1 #:vec4 (grad4 j1.x ip))
               (p2 #:vec4 (grad4 j1.y ip))
               (p3 #:vec4 (grad4 j1.z ip))
               (p4 #:vec4 (grad4 j1.w ip))
               (m0 #:vec3 (max (- 0.6 (vec3 (dot x0 x0) (dot x1 x1) (dot x2 x2)))
                               0.0))
               (m1 #:vec2 (max (- 0.6 (vec2 (dot x3 x3) (dot x4 x4)))
                               0.0))
               (norm #:vec4 (taylor-inv-sqrt (vec4 (dot p0 p0) (dot p1 p1)
                                                   (dot p2 p2) (dot p3 p3)))))
          ;; Mix final noise value
          (*= p0 (.. norm x))
          (*= p1 (.. norm y))
          (*= p2 (.. norm z))
          (*= p3 (.. norm w))
          (*= p4 (taylor-inv-sqrt (dot p4 p4)))
          (set! m0 (* m0 m0))
          (set! m1 (* m1 m1))
          (* 49.0 (+ (dot (* m0 m0)
                          (vec3 (dot p0 x0) (dot p1 x1) (dot p2 x2)))
                     (dot (* m1 m1)
                          (vec2 (dot p3 x3) (dot p4 x4)))))))))

  (define (fractal-snoise (v #:vec4) (octaves #:int)
                          (frequency #:float)
                          (amplitude #:float)
                          (persistence #:float)
                          (lacunarity #:float))
      #:float
    (let ((r #:float 0))
      (do-times (o octaves)
        (+= r (* amplitude (snoise (* v frequency))))
        (*= frequency persistence)
        (*= amplitude lacunarity))
      r))
  ) ;end shader

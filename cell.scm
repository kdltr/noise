;;;; These shaders are a direct port of http://webstaff.itn.liu.se/~stegu/GLSL-cellular/

(define-shader cell-noise-2d
    (#:fragment export: (cell-noise
                         cell-noise2
                         jitter)
                version: 120)

  (define (permute (x #:vec3)) #:vec3
    (mod (* (+ (* x 34.0)
               1.0)
            x)
         289))

  (define (permute (x #:vec4)) #:vec4
    (mod (* (+ (* x 34.0)
               1.0)
            x)
         289))

  (%define k 0.142857142857)  ; 1/7
  (%define k2 0.0714285714285) ; K/2
  (%define ko 0.428571428571) ; 3/7
  (define jitter #:float 0.9)

  (define (cell-noise (pos #:vec2)) #:float
    (let* ((pi #:vec2 (mod (floor pos) 289))
           (pf #:vec2 (fract pos))
           (pfx #:vec4 (+ pf.x (vec4 -0.5 -1.5 -0.5 -1.5)))
           (pfy #:vec4 (+ pf.y (vec4 -0.5 -0.5 -1.5 -1.5)))
           (p #:vec4 (permute (+ (permute (+ pi.x (vec4 0 1 0 1)))
                                 pi.y (vec4 0 0 1 1))))
           (ox #:vec4 (+ (* (mod p 7.0)
                            k)
                         k2))
           (oy #:vec4 (+ (* (mod (floor (* p k)) 7.0)
                            k)
                         k2))
           (dx #:vec4 (+ pfx (* jitter ox)))
           (dy #:vec4 (+ pfy (* jitter oy)))
           (d #:vec4 (+ (* dx dx) (* dy dy))))
      (set! d.xy (min d.xy d.zw))
      (sqrt (min d.x d.y))))

  (define (cell-noise2 (pos #:vec2)) #:vec2
    (let* ((pi #:vec2 (mod (floor pos) 289))
           (pf #:vec2 (fract pos))
           (oi #:vec3 (vec3 -1 0 1))
           (of #:vec3 (vec3 -0.5 0.5 1.5))
           (px #:vec3 (permute (+ pi.x oi)))
           (p #:vec3 (permute (+ px.x pi.y oi))) ; p11, p12, p13
           (ox #:vec3 (- (fract (* p k)) ko))
           (oy #:vec3 (- (* (mod (floor (* p k)) 7)
                            k)
                         ko))
           (dx #:vec3 (+ pf.x 0.5 (* jitter ox)))
           (dy #:vec3 (+ pf.y (- of) (* jitter oy)))
           (d1 #:vec3 (+ (* dx dx) (* dy dy))) ; d11, d12, d13 squared
           (d2 #:vec3)
           (d3 #:vec3)
           (d1a #:vec3))
      (set! p (permute (+ px.y pi.y oi))) ; p21, p22, p23
      (set! ox (- (fract (* p k)) ko))
      (set! oy (- (* (mod (floor (* p k)) 7)
                     k)
                  ko))
      (set! dx (+ pf.x -0.5 (* jitter ox)))
      (set! dy (+ pf.y (- of) (* jitter oy)))
      (set! d2 (+ (* dx dx) (* dy dy))) ; d21, d22, d23 squared
      (set! p (permute (+ px.z pi.y oi))) ; p31, p32, p33
      (set! ox (- (fract (* p k)) ko))
      (set! oy (- (* (mod (floor (* p k)) 7)
                     k)
                  ko))
      (set! dx (+ pf.x -1.5 (* jitter ox)))
      (set! dy (+ pf.y (- of) (* jitter oy)))
      (set! d3 (+ (* dx dx) (* dy dy))) ; d31, d32, d33 squared
      ;; Sort out the two smallest distances (F1, F2)
      (set! d1a (min d1 d2))
      (set! d2 (max d1 d2)) ; Swap to keep candidates for F2
      (set! d2 (min d2 d3)) ; neither F1 nor F2 are now in d3
      (set! d1 (min d1a d2)) ; F1 is not in d1
      (set! d2 (max d1a d2)) ; Swap to keep candidates for F2
      (set! d1.xy (if (< d1.x d1.y)
                      d1.xy
                      d1.yx)) ; Swap if smaller
      (set! d1.xz (if (< d1.x d1.z)
                      d1.xz
                      d1.zx)) ; F1 is in d1.x
      (set! d1.yz (min d1.yz d2.yz)) ; F2 is now not in d2.yz
      (set! d1.y (min d1.y d1.z)) ; nor in d1.z
      (set! d1.y (min d1.y d2.x)) ; F2 is in d1.y, we're done
      (sqrt d1.xy)))

  ) ; end define-shader

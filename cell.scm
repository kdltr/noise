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

(define-shader cell-noise-3d
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
  (%define k2 0.020408163265306) ; k*k
  (%define ko 0.428571428571) ; 3/7
  (%define kz 0.166666666667) ; 1/6
  (%define kzo 0.416666666667) ; 5/12
  (define jitter #:float 0.9)

  (define (cell-noise (pos #:vec3)) #:float
    (let* ((pi #:vec3 (mod (floor pos) 289))
           (pf #:vec3 (fract pos))
           (pfx #:vec4 (+ pf.x (vec4 0 -1 0 -1)))
           (pfy #:vec4 (+ pf.y (vec4 0 0 -1 -1)))
           (p #:vec4 (permute (+ (permute (+ pi.x (vec4 0 1 0 1)))
                                 pi.y (vec4 0 0 1 1))))
           (p1 #:vec4 (permute (+ p pi.z))) ; z+0
           (p2 #:vec4 (permute (+ p pi.z (vec4 1)))) ;z+1
           (ox1 #:vec4 (- (fract (* p1 k)) ko))
           (oy1 #:vec4 (- (* (mod (floor (* p1 k)) 7.0)
                             k)
                          ko))
           (oz1 #:vec4 (- (* (floor (* p1 k2))
                             kz)
                          kzo)) ; p1 < 298
           (ox2 #:vec4 (- (fract (* p2 k)) ko))
           (oy2 #:vec4 (- (* (mod (floor (* p2 k)) 7.0)
                             k)
                          ko))
           (oz2 #:vec4 (- (* (floor (* p2 k2))
                             kz)
                          kzo))
           (dx1 #:vec4 (+ pfx (* jitter ox1)))
           (dy1 #:vec4 (+ pfy (* jitter oy1)))
           (dz1 #:vec4 (+ pf.z (* jitter oz1)))
           (dx2 #:vec4 (+ pfx (* jitter ox2)))
           (dy2 #:vec4 (+ pfy (* jitter oy2)))
           (dz2 #:vec4 (+ pf.z -1 (* jitter oz2)))
           (d1 #:vec4 (+ (* dx1 dx1) (* dy1 dy1) (* dz1 dz1))) ; z+0
           (d2 #:vec4 (+ (* dx2 dx2) (* dy2 dy2) (* dz2 dz2)))) ; z+1
      (set! d1 (min d1 d2))
      (set! d1.xy (min d1.xy d1.wz))
      (sqrt (min d1.x d1.y))))

  (define (cell-noise2 (pos #:vec3)) #:vec2
    (let* ((pi #:vec3 (mod (floor pos) 289))
           (pf #:vec3 (- (fract pos) 0.5))
           (pfx #:vec3 (+ pf.x (vec3 1 0 -1)))
           (pfy #:vec3 (+ pf.y (vec3 1 0 -1)))
           (pfz #:vec3 (+ pf.z (vec3 1 0 -1)))

           (p #:vec3 (permute (+ pi.x (vec3 -1 0 1))))
           (p1 #:vec3 (permute (+ p pi.y -1)))
           (p2 #:vec3 (permute (+ p pi.y)))
           (p3 #:vec3 (permute (+ p pi.y 1)))

           (p11 #:vec3 (permute (+ p1 pi.z -1)))
           (p12 #:vec3 (permute (+ p1 pi.z)))
           (p13 #:vec3 (permute (+ p1 pi.z 1)))

           (p21 #:vec3 (permute (+ p2 pi.z -1)))
           (p22 #:vec3 (permute (+ p2 pi.z)))
           (p23 #:vec3 (permute (+ p2 pi.z 1)))

           (p31 #:vec3 (permute (+ p3 pi.z -1)))
           (p32 #:vec3 (permute (+ p3 pi.z)))
           (p33 #:vec3 (permute (+ p3 pi.z 1)))

           (ox11 #:vec3 (- (fract (* p11 k)) ko))
           (oy11 #:vec3 (- (* (mod (floor (* p11 k)) 7.0) k) ko))
           (oz11 #:vec3 (- (* (floor (* p11 k2)) kz) kzo))

           (ox12 #:vec3 (- (fract (* p12 k)) ko))
           (oy12 #:vec3 (- (* (mod (floor (* p12 k)) 7.0) k) ko))
           (oz12 #:vec3 (- (* (floor (* p12 k2)) kz) kzo))

           (ox13 #:vec3 (- (fract (* p13 k)) ko))
           (oy13 #:vec3 (- (* (mod (floor (* p13 k)) 7.0) k) ko))
           (oz13 #:vec3 (- (* (floor (* p13 k2)) kz) kzo))

           (ox21 #:vec3 (- (fract (* p21 k)) ko))
           (oy21 #:vec3 (- (* (mod (floor (* p21 k)) 7.0) k) ko))
           (oz21 #:vec3 (- (* (floor (* p21 k2)) kz) kzo))

           (ox22 #:vec3 (- (fract (* p22 k)) ko))
           (oy22 #:vec3 (- (* (mod (floor (* p22 k)) 7.0) k) ko))
           (oz22 #:vec3 (- (* (floor (* p22 k2)) kz) kzo))

           (ox23 #:vec3 (- (fract (* p23 k)) ko))
           (oy23 #:vec3 (- (* (mod (floor (* p23 k)) 7.0) k) ko))
           (oz23 #:vec3 (- (* (floor (* p23 k2)) kz) kzo))

           (ox31 #:vec3 (- (fract (* p31 k)) ko))
           (oy31 #:vec3 (- (* (mod (floor (* p31 k)) 7.0) k) ko))
           (oz31 #:vec3 (- (* (floor (* p31 k2)) kz) kzo))

           (ox32 #:vec3 (- (fract (* p32 k)) ko))
           (oy32 #:vec3 (- (* (mod (floor (* p32 k)) 7.0) k) ko))
           (oz32 #:vec3 (- (* (floor (* p32 k2)) kz) kzo))

           (ox33 #:vec3 (- (fract (* p33 k)) ko))
           (oy33 #:vec3 (- (* (mod (floor (* p33 k)) 7.0) k) ko))
           (oz33 #:vec3 (- (* (floor (* p33 k2)) kz) kzo))

           (dx11 #:vec3 (+ pfx (* jitter ox11)))
           (dy11 #:vec3 (+ pfy.x (* jitter oy11)))
           (dz11 #:vec3 (+ pfz.x (* jitter oz11)))

           (dx12 #:vec3 (+ pfx (* jitter ox12)))
           (dy12 #:vec3 (+ pfy.x (* jitter oy12)))
           (dz12 #:vec3 (+ pfz.y (* jitter oz12)))

           (dx13 #:vec3 (+ pfx (* jitter ox13)))
           (dy13 #:vec3 (+ pfy.x (* jitter oy13)))
           (dz13 #:vec3 (+ pfz.z (* jitter oz13)))
           
           (dx21 #:vec3 (+ pfx (* jitter ox21)))
           (dy21 #:vec3 (+ pfy.y (* jitter oy21)))
           (dz21 #:vec3 (+ pfz.x (* jitter oz21)))
           
           (dx22 #:vec3 (+ pfx (* jitter ox22)))
           (dy22 #:vec3 (+ pfy.y (* jitter oy22)))
           (dz22 #:vec3 (+ pfz.y (* jitter oz22)))
             
           (dx23 #:vec3 (+ pfx (* jitter ox23)))
           (dy23 #:vec3 (+ pfy.y (* jitter oy23)))
           (dz23 #:vec3 (+ pfz.z (* jitter oz23)))
           
           (dx31 #:vec3 (+ pfx (* jitter ox31)))
           (dy31 #:vec3 (+ pfy.z (* jitter oy31)))
           (dz31 #:vec3 (+ pfz.x (* jitter oz31)))

           (dx32 #:vec3 (+ pfx (* jitter ox32)))
           (dy32 #:vec3 (+ pfy.z (* jitter oy32)))
           (dz32 #:vec3 (+ pfz.y (* jitter oz32)))
           
           (dx33 #:vec3 (+ pfx (* jitter ox33)))
           (dy33 #:vec3 (+ pfy.z (* jitter oy33)))
           (dz33 #:vec3 (+ pfz.z (* jitter oz33)))
           
           (d11 #:vec3 (+ (* dx11 dx11) (* dy11 dy11) (* dz11 dz11)))
           (d12 #:vec3 (+ (* dx12 dx12) (* dy12 dy12) (* dz12 dz12)))
           (d13 #:vec3 (+ (* dx13 dx13) (* dy13 dy13) (* dz13 dz13)))
           (d21 #:vec3 (+ (* dx21 dx21) (* dy21 dy21) (* dz21 dz21)))
           (d22 #:vec3 (+ (* dx22 dx22) (* dy22 dy22) (* dz22 dz22)))
           (d23 #:vec3 (+ (* dx23 dx23) (* dy23 dy23) (* dz23 dz23)))
           (d31 #:vec3 (+ (* dx31 dx31) (* dy31 dy31) (* dz31 dz31)))
           (d32 #:vec3 (+ (* dx32 dx32) (* dy32 dy32) (* dz32 dz32)))
           (d33 #:vec3 (+ (* dx33 dx33) (* dy33 dy33) (* dz33 dz33)))
           
           (d2a #:vec3)
           (d3a #:vec3)
           (da #:vec3)
           (d1a #:vec3 (min d11 d12)))
      (set! d12 (max d11 d12))
      (set! d11 (min d1a d13))
      (set! d13 (max d1a d13))
      (set! d12 (min d12 d13))

      (set! d2a (min d21 d22))
      (set! d22 (max d21 d22))
      (set! d21 (min d2a d23))
      (set! d23 (max d2a d23))
      (set! d22 (min d22 d23))

      (set! d3a (min d31 d32))
      (set! d32 (max d31 d32))
      (set! d31 (min d3a d33))
      (set! d33 (max d3a d33))
      (set! d32 (min d32 d33))

      (set! da (min d11 d21))
      (set! d21 (max d11 d21))
      (set! d11 (min da d31))
      (set! d31 (max da d31))

      (set! d11.xy (if (< d11.x d11.y)
                       d11.xy
                       d11.yx))
      (set! d11.xz (if (< d11.x d11.z)
                       d11.xz
                       d11.zx))
      (set! d12 (min d12 d21))
      (set! d12 (min d12 d22))
      (set! d12 (min d12 d31))
      (set! d12 (min d12 d32))
      (set! d11.yz (min d11.yz d12.xy))
      (set! d11.y (min d11.y d12.z))
      (set! d11.y (min d11.y d11.z))

      (sqrt d11.xy)))

  ) ; end define-shader

;;;; This shaders are a direct port the flow noise demo from http://webstaff.itn.liu.se/~stegu/gpunoise/

(define-shader flow-noise-2d
    (#:fragment export: (flow-noise
                         isotropic)
                version: 120)

  (%define F2 0.366025403)
  (%define G2 0.211324865)
  (%define K 0.0243902439) ; 1/41
  (%define TWO-PI 6.28318530718)

  (define isotropic #:bool false)

  (define (permute (x #:float)) #:float
    (mod (* (+ (* x 34.0)
               1.0)
            x)
         289))

  (define (grad2 (p #:vec2) (rot #:float)) #:vec2
    (let ((u #:float (+ (* (permute (+ (permute p.x) p.y)) K) rot)))
      (set! u (- (* 4 (fract u)) 2))
      (vec2 (- (abs u) 1) (- (abs (- (abs (+ u 1)) 2)) 1))))

  (define (iso-grad2 (p #:vec2) (rot #:float)) #:vec2
    (let ((u #:float (+ (* (permute (+ (permute p.x) p.y)) K) rot)))
      (set! u (* (fract u) TWO-PI))
      (vec2 (cos u) (sin u))))

  (define (flow-noise (p (in #:vec2)) (rot (in #:float)) (grad (out #:vec2))) #:float
    (let ((ps #:vec2 (+ p (dot p (vec2 F2)))) ;transform input point to skewed simplex grid
          (pi #:vec2 (floor ps)) ; round down to simplex origin
          (p0 #:vec2 (- pi (dot pi (vec2 G2)))) ; transform simplex origin back to x,y
          (v0 #:vec2 (- p p0)) ; find x,y offsets from simplex origin to first corner
          (i1 #:vec2 (if (> v0.x v0.y)
                         (vec2 1 0)
                         (vec2 0 1))) ; pick (+x, +y) or (+y, +x) increment sequence
          ;; determine the offsets from the other two corners
          (v1 #:vec2 (+ v0 (- i1) G2))
          (v2 #:vec2 (+ v0 -1 (* 2 G2)))
          ;; Calculate the circularly symmetric part of each noise wiggle
          (t #:vec3 (max (- 0.5 (vec3 (dot v0 v0) (dot v1 v1) (dot v2 v2))) 0))
          (t2 #:vec3 (* t t))
          (t4 #:vec3 (* t2 t2))
          (g0 #:vec2) (g1 #:vec2) (g2 #:vec2))
      (set! pi (mod pi 289)) ; wrap coordinates at 289 to avoid float precision problems
      ;; calculate the gradients for the three corners
      (if isotropic
          (begin
            (set! g0 (iso-grad2 pi rot))
                 (set! g1 (iso-grad2 (+ pi i1) rot))
                 (set! g2 (iso-grad2 (+ pi 1) rot)))
          (begin
            (set! g0 (grad2 pi rot))
            (set! g1 (grad2 (+ pi i1) rot))
            (set! g2 (grad2 (+ pi 1) rot))))
      ;; compute noise contributions from each corner
      (let ((gv #:vec3 (vec3 (dot g0 v0) (dot g1 v1) (dot g2 v2))); ramp g dot v
            (n #:vec3 (* t4 gv)) ; circular kernel times linear ramp
            ;; compute partial derivatives in x and y
            (temp #:vec3 (* t2 t gv))
            (gradx #:vec3 (* temp (vec3 v0.x v1.x v2.x)))
            (grady #:vec3 (* temp (vec3 v0.y v1.y v2.y))))
        (set! grad.x (* -8 (+ gradx.x gradx.y gradx.z)))
        (set! grad.y (* -8 (+ gradx.x gradx.y gradx.z)))
        (+= grad.x (dot t4 (vec3 g0.x g1.x g2.x)))
        (+= grad.y (dot t4 (vec3 g0.y g1.y g2.y)))
        (*= grad 40)
        (* 40 (+ n.x n.y n.z)))))

  ) ; end define-shader

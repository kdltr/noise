;;;; render-to-texture.scm

;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL render-to-texture.scm

(import scheme (chicken base) (chicken bitwise)
glls-render (prefix glfw3 glfw:) (prefix epoxy gl:) gl-math gl-utils
     noise srfi-4)

(define time (make-f32vector 1 0 #t))

(define rect (make-mesh vertices: '(attributes: ((position #:float 2))
                                    initial-elements: ((position . (-1 -1
                                                                     1 -1
                                                                     1  1
                                                                    -1  1))))
                        indices: '(type: #:ushort
                                   initial-elements: (0 1 2
                                                      0 2 3))))

(define cube (make-mesh vertices: '(attributes: ((position #:float 3)
                                                 (tex-coord #:ushort 2
                                                            normalized: #t))
                                    initial-elements: ((position . (0 0 0
                                                                    1 0 0
                                                                    1 1 0
                                                                    0 1 0
                                                                    0 0 1
                                                                    1 0 1
                                                                    1 1 1
                                                                    0 1 1))
                                                       (tex-coord . (0 0
                                                                     1 0
                                                                     1 1
                                                                     0 1
                                                                     1 0
                                                                     0 0
                                                                     0 1
                                                                     1 1))))
                        indices: '(type: #:ushort
                                   initial-elements: (0 1 2
                                                      2 3 0
                                                      7 6 5
                                                      5 4 7
                                                      0 4 5
                                                      5 1 0
                                                      1 5 6
                                                      6 2 1
                                                      2 6 7
                                                      7 3 2
                                                      3 7 4
                                                      3 4 0))))

;;; Matrices
(define projection-matrix
  (perspective 480 480 0.1 100 70))

(define view-matrix
  (look-at (make-point 1.5 0.5 2)
           (make-point 0.5 0.5 0.5)
           (make-point 0 1 0)))

(define model-matrix (mat4-identity))

(define mvp (m* projection-matrix
                (m* view-matrix model-matrix)
                #t ; Matrix should be in a non-GC'd area
                ))

(define-pipeline noise-shader
  ((#:vertex input: ((position #:vec2))
             output: ((pos #:vec2))) 
   (define (main) #:void
     (set! gl:position (vec4 position 0.0 1.0))
     (set! pos (* position 8))))
  ((#:fragment input: ((pos #:vec2))
               uniform: ((time #:float))
               output: ((frag-color #:vec4))
               use: (flow-noise-2d))
   (define (main) #:void
     (set! isotropic true)
     (let* ((g1 #:vec2) (g2 #:vec2)
            (n1 #:float (flow-noise (* pos 0.5) (* 0.2 time) g1))
            (n2 #:float (flow-noise (+ (* pos 2) (* g1 0.5)) (* 0.51 time) g2))
            (n3 #:float (flow-noise (+ (* pos 4) (* g1 0.5) (* g2 0.5))
                                    (* 0.77 time) g2)))
       (set! isotropic true)
       (set! frag-color (vec4 (+ (vec3 0.6 0.4 0.2)
                                 (vec3 (+ n1 (* n2 0.75) (* n3 0.5))))
                              1.0))))))

(define-pipeline box-shader
  ((#:vertex input: ((position #:vec3)
                     (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((coord #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! coord tex-coord)))
  ((#:fragment input: ((coord #:vec2))
               uniform: ((tex #:sampler-2d))
               output: ((frag-color #:vec4)))
   (begin
     (define (main) #:void
       (set! frag-color (texture tex coord))))))

(glfw:key-callback
 (lambda (window key scancode action mods)
   (if (eq? key glfw:+key-escape+)
    (glfw:set-window-should-close window 1))))

(define (render-noise fbo renderable)
  (with-framebuffer fbo
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (render-noise-shader renderable)))

;;; Initialization and main loop
(glfw:with-window (480 480 "Example" resizable: #f
                   client-api: glfw:+opengl-api+
                   context-version-major: 3
                   context-version-minor: 3)
  (gl:enable gl:+depth-test+)
  (gl:depth-func gl:+less+)
  (compile-pipelines)
  (mesh-make-vao! rect (pipeline-mesh-attributes noise-shader))
  (mesh-make-vao! cube (pipeline-mesh-attributes box-shader))
  (receive (fbo tex _) (create-framebuffer 480 480)
    (let* ((noise-renderable (make-noise-shader-renderable mesh: rect
                                                           time: time))
           (box-renderable (make-box-shader-renderable mesh: cube
                                                       mvp: mvp
                                                       tex: tex)))
      (let loop ()
        (render-noise fbo noise-renderable)
        ;; At this point the texture with the noise (tex) could be transfered to RAM with e.g. gl:get-tex-image
        (glfw:swap-buffers (glfw:window))
        (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
        (render-box-shader box-renderable)
        (check-error)
        (glfw:poll-events)
        (f32vector-set! time 0 (glfw:get-time))
        (unless (glfw:window-should-close (glfw:window))
          (loop))))))

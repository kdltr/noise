;;;; 2d-flow.scm

;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL 2d-flow.scm

(import chicken scheme)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils
     noise)

(define rect (make-mesh vertices: '(attributes: ((position #:float 2))
                                    initial-elements: ((position . (-1 -1
                                                                     1 -1
                                                                     1  1
                                                                    -1  1))))
                        indices: '(type: #:ushort
                                   initial-elements: (0 1 2
                                                      0 2 3))))

(define time (f32vector 0))

(define-pipeline simple-shader
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
     (set! isotropic #t)
     (let* ((g1 #:vec2) (g2 #:vec2)
            (n1 #:float (flow-noise (* pos 0.5) (* 0.2 time) g1))
            (n2 #:float (flow-noise (+ (* pos 2) (* g1 0.5)) (* 0.51 time) g2))
            (n3 #:float (flow-noise (+ (* pos 4) (* g1 0.5) (* g2 0.5))
                                    (* 0.77 time) g2)))
       (set! frag-color (vec4 (+ (vec3 0.4 0.5 0.6)
                                 (vec3 (+ n1 (* n2 0.75) (* n3 0.5))))
                              1.0))))))

(glfw:key-callback
 (lambda (window key scancode action mods)
   (if (eq? key glfw:+key-escape+)
    (glfw:set-window-should-close window 1))))

;;; Initialization and main loop
(glfw:with-window (480 480 "Example" resizable: #f)
  (gl:init)
  (compile-pipelines)
  (mesh-make-vao! rect (pipeline-mesh-attributes simple-shader))
  (let* ((renderable (make-simple-shader-renderable mesh: rect
                                                    time: time)))
    (let loop ()
      (glfw:swap-buffers (glfw:window))
      (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      (render-simple-shader renderable)
      (check-error)
      (glfw:poll-events)
      (f32vector-set! time 0 (/ (glfw:get-time)
                                5))
      (unless (glfw:window-should-close (glfw:window))
        (loop)))))

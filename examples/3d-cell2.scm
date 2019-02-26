;;;; 3d-cell2.scm

;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL 3d-cell2.scm

(import scheme (chicken base) (chicken bitwise)
glls-render (prefix glfw3 glfw:) (prefix epoxy gl:) gl-math gl-utils
     noise srfi-4)

(define rect (make-mesh vertices: '(attributes: ((position #:float 2))
                                    initial-elements: ((position . (-1 -1
                                                                     1 -1
                                                                     1  1
                                                                    -1  1))))
                        indices: '(type: #:ushort
                                   initial-elements: (0 1 2
                                                      0 2 3))))

(define time (make-f32vector 1 0 #t))

(define-pipeline simple-shader
  ((#:vertex input: ((position #:vec2))
             output: ((pos #:vec2))) 
   (define (main) #:void
     (set! gl:position (vec4 position 0.0 1.0))
     (set! pos position)))
  ((#:fragment input: ((pos #:vec2))
               uniform: ((time #:float))
               output: ((frag-color #:vec4))
               use: (cell-noise-3d))
   (define (main) #:void
     (let ((f #:vec2 (cell-noise2 (vec3 (* pos 16) time)))
           (n #:float (- f.y f.x)))
       (set! frag-color (vec4 n n n 1.0))))))

(glfw:key-callback
 (lambda (window key scancode action mods)
   (if (eq? key glfw:+key-escape+)
    (glfw:set-window-should-close window 1))))

;;; Initialization and main loop
(glfw:with-window (480 480 "Example" resizable: #f
                   client-api: glfw:+opengl-api+
                   context-version-major: 3
                   context-version-minor: 3)
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

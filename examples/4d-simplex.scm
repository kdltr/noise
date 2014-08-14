(import chicken scheme)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils
     noise)

;;; VAO data
(define vertex-data (f32vector -1 -1
                               1 -1
                               1  1
                               -1  1))

(define index-data (u16vector 0 1 2
                              0 2 3))

(define time (f32vector 0))

(define-pipeline simple-shader
  ((#:vertex) ((vertex #:vec2))
   (define (main) #:void
     (set! gl:position (vec4 vertex 0.0 1.0))
     (set! pos vertex))
   -> ((pos #:vec2)))
  ((#:fragment use: (simplex-noise-4d)) ((pos #:vec2) uniform: (time #:float))
   (begin
     (define (main) #:void
       (let ((n #:float (+ 0.5 (* 0.5 (fractal-snoise (vec4 pos time 0)
                                                      5 1 0.5 2 0.5
                                              )))))
         (set! frag-color (vec4 n n n 1.0)))))
   -> ((frag-color #:vec4))))

(glfw:key-callback
 (lambda (window key scancode action mods)
   (if (eq? key glfw:+key-escape+)
       (glfw:set-window-should-close window 1))))

;;; Initialization and main loop
(glfw:with-window (480 480 "Example" resizable: #f)
  (gl:init)
  (compile-pipelines)
  (display (shader-source simplex-noise-4d))
  (let* ((vao (make-vao vertex-data index-data
                        `((,(pipeline-attribute 'vertex simple-shader) float: 2))))
         (renderable (make-simple-shader-renderable
                      n-elements: (u16vector-length index-data)
                      element-type: (type->gl-type ushort:)
                      vao: vao
                      time: time)))
    
    (let loop ()
      (glfw:swap-buffers (glfw:window))
      (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      (render-simple-shader renderable)
      (gl:check-error)
      (glfw:poll-events)
      (f32vector-set! time 0 (/ (glfw:get-time)
                                10))
      (unless (glfw:window-should-close (glfw:window))
        (loop)))))

(import chicken scheme)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils
     noise)

(define time (f32vector 0))

;;; VAO data
(define vertex-data (f32vector 0 0 0
                               1 0 0
                               1 1 0
                               0 1 0
                               0 0 1
                               1 0 1
                               1 1 1
                               0 1 1))

(define index-data (u16vector 0 1 2
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
                              3 4 0))

;;; Matrices
(define projection-matrix
  (perspective 640 480 0.1 100 45))

(define view-matrix
  (look-at 1.5 2 2
           0.5 0.5 0.5
           0 1 0))

(define model-matrix (mat4-identity))

(define mvp (m* projection-matrix
                (m* view-matrix model-matrix)
                #t ; Matrix should be in a non-GC'd area
                ))

(define-pipeline simple-shader
  ((#:vertex input: ((vertex #:vec3))
             uniform: ((mvp #:mat4))
             output: ((pos #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 vertex 1.0)))
     (set! pos vertex)))
  ((#:fragment input: ((pos #:vec3))
               uniform: ((time #:float))
               output: ((frag-color #:vec4))
               use: (simplex-noise-4d))
   (begin
     (define (main) #:void
       (let ((n #:float (+ 0.2 (* 0.3 (fractal-snoise (vec4 pos time)
                                                      5 1 0.5 2 0.5)))))
         (set! frag-color (vec4 n n n 1.0)))))))

(glfw:key-callback
 (lambda (window key scancode action mods)
   (if (eq? key glfw:+key-escape+)
       (glfw:set-window-should-close window 1))))

;;; Initialization and main loop
(glfw:with-window (480 480 "Example" resizable: #f)
  (gl:init)
  (gl:enable gl:+depth-test+)
  (gl:depth-func gl:+less+)
  (compile-pipelines)
  (let* ((vao (make-vao vertex-data index-data
                        `((,(pipeline-attribute 'vertex simple-shader) float: 3))))
         (renderable (make-simple-shader-renderable
                      n-elements: (u16vector-length index-data)
                      element-type: (type->gl-type ushort:)
                      vao: vao
                      mvp: mvp
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

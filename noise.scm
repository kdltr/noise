(module noise (simplex-noise-2d
               simplex-noise-3d
               simplex-noise-4d
               cell-noise-2d
               cell-noise-3d
               flow-noise-2d

               simplex-noise-2d-source
               simplex-noise-3d-source
               simplex-noise-4d-source
               cell-noise-2d-source
               cell-noise-3d-source
               flow-noise-2d-source)

(import chicken scheme)
(use glls)
(include "simplex")
(include "cell")
(include "flow")

(define simplex-noise-2d-source (shader-source simplex-noise-2d))
(define simplex-noise-3d-source (shader-source simplex-noise-3d))
(define simplex-noise-4d-source (shader-source simplex-noise-4d))
(define cell-noise-2d-source (shader-source cell-noise-2d))
(define cell-noise-3d-source (shader-source cell-noise-3d))
(define flow-noise-2d-source (shader-source flow-noise-2d))

) ; end module noise

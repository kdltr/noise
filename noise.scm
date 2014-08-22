(module noise (simplex-noise-2d
               simplex-noise-3d
               simplex-noise-4d
               cell-noise-2d
               cell-noise-3d
               flow-noise-2d)

(import chicken scheme)
(use glls)
(include "simplex")
(include "cell")
(include "flow")

) ; end module noise

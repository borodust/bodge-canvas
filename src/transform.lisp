(cl:in-package :bodge-canvas)


(defun translate-canvas (x y)
  (%nanovg:translate *canvas-handle* (f x) (f y)))


(defun rotate-canvas (angle)
  (%nanovg:rotate *canvas-handle* (f angle)))


(defun skew-canvas (x y)
  (%nanovg:skew-x *canvas-handle* (f x))
  (%nanovg:skew-y *canvas-handle* (f y)))


(defun scale-canvas (x y)
  (%nanovg:scale *canvas-handle* (f x) (f y)))


(defun reset-canvas-transform ()
  (%nanovg:reset-transform *canvas-handle*))

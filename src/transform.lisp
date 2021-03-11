(cl:in-package :bodge-canvas)


(defun translate-canvas (x y)
  (%nvg:translate *canvas-handle* (f x) (f y)))


(defun rotate-canvas (angle)
  (%nvg:rotate *canvas-handle* (f angle)))


(defun skew-canvas (x y)
  (%nvg:skew-x *canvas-handle* (f x))
  (%nvg:skew-y *canvas-handle* (f y)))


(defun scale-canvas (x y)
  (%nvg:scale *canvas-handle* (f x) (f y)))


(defun reset-canvas-transform ()
  (%nvg:reset-transform *canvas-handle*))

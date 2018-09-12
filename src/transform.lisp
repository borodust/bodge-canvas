(cl:in-package :bodge-canvas)


(defun translate-canvas (x y)
  (%nvg:translate *handle* (f x) (f y)))


(defun rotate-canvas (angle)
  (%nvg:rotate *handle* (f angle)))


(defun skew-canvas (x y)
  (%nvg:skew-x *handle* (f x))
  (%nvg:skew-y *handle* (f y)))


(defun scale-canvas (x y)
  (%nvg:scale *handle* (f x) (f y)))


(defun reset-canvas-transform ()
  (%nvg:reset-transform *handle*))

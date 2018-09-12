(cl:in-package :bodge-canvas)


(definline fill-path ()
  (%nvg:fill *handle*))


(definline stroke-path ()
  (%nvg:stroke *handle*))


(definline move-to (coords)
  (%nvg:move-to *handle* (x coords) (y coords)))


(definline (setf stroke-width) (width)
  (%nvg:stroke-width *handle* (f width)))


(defmacro path (&body body)
  `(progn
     (%nvg:begin-path *handle*)
     ,@body))


(defun scissors (origin w h)
  (%nvg:scissor *handle*
                (x origin) (y origin)
                (f w) (f h)))


(defun line-to (end)
  (%nvg:line-to *handle* (x end) (y end)))


(defun bezier-to (ctrl0 ctrl1 end)
  (%nvg:bezier-to *handle*
                  (x ctrl0) (y ctrl0)
                  (x ctrl1) (y ctrl1)
                  (x end) (y end)))


(defun rounded-rect (origin width height &optional rounding)
  (%nvg:rounded-rect *handle* (x origin) (y origin)
                     (f width) (f height)
                     (f (or rounding 0))))


(defun circle (center radius)
  (%nvg:circle *handle* (x center) (y center) (f radius)))


(defun ellipse (center x-radius y-radius)
  (%nvg:ellipse *handle*
                (x center) (y center) (f x-radius) (f y-radius)))


(defun arc (center radius a0 a1)
  (%nvg:arc *handle* (x center) (y center) (f radius)
            (f a0) (f a1) %nvg:+cw+))


(defun text (position text)
  (with-retained-canvas
    (%restore-coordinate-system)
    (%nvg:text *handle*
               (x position) (f (- (canvas-height *canvas*) (y position)))
               text (cffi:null-pointer))))

(cl:in-package :bodge-canvas)


(defenum path-winding
  :solid
  :hole)


(defenum line-cap
  :butt
  :round
  :square)


(defenum line-join
  :miter
  :round
  :bevel)


(definline fill-path ()
  (%nvg:fill *handle*))


(definline stroke-path ()
  (%nvg:stroke *handle*))


(defun stroke-width ()
  "Use with setf to set stroke width (line thickness) of the path"
  (error "Only setter is available"))


(definline (setf stroke-width) (width)
  (%nvg:stroke-width *handle* (f width)))


(definline winding->nvg (winding)
  (case winding
    (:solid %nvg:+solid+)
    (:hole %nvg:+hole+)
    (t (error "Unrecognized winding ~A" winding))))


(defun wind-path (winding)
  (%nvg:path-winding *handle* (winding->nvg winding)))


(definline move-to (coords)
  (%nvg:move-to *handle* (x coords) (y coords)))


(defmacro path (&body body)
  `(progn
     (%nvg:begin-path *handle*)
     ,@body))


(defun scissors (origin w h)
  (%nvg:scissor *handle*
                (x origin) (y origin)
                (f w) (f h)))


(definline line-cap->nvg (cap)
  (case cap
    (:butt %nvg:+butt+)
    (:round %nvg:+round+)
    (:square %nvg:+square+)
    (t (error "Unrecognized line cap ~A" cap))))


(definline line-join->nvg (join)
  (case join
    (:round %nvg:+round+)
    (:bevel %nvg:+bevel+)
    (:miter %nvg:+miter+)
    (t (error "Unrecognized line join ~A" join))))


(defun line-cap ()
  (error "Only setter available"))


(defun (setf line-cap) (value)
  (%nvg:line-cap *handle* (line-cap->nvg value)))


(defun line-join ()
  (error "Only setter available"))


(defun (setf line-join) (value)
  (%nvg:line-join *handle* (line-join->nvg value)))


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

(cl:in-package :bodge-canvas)


(define-constant +default-winding+ (cffi:foreign-bitfield-value '%nanovg:winding :cw)
  :test #'=)

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
  (%nanovg:fill *canvas-handle*))


(definline stroke-path ()
  (%nanovg:stroke *canvas-handle*))


(defun stroke-width ()
  "Use with setf to set stroke width (line thickness) of the path"
  (error "Only setter is available"))


(definline (setf stroke-width) (width)
  (%nanovg:stroke-width *canvas-handle* (f width)))


(defun wind-path (winding)
  (%nanovg:path-winding *canvas-handle* (cffi:foreign-bitfield-value '%nanovg:solidity
                                                           winding)))


(definline move-to (coords)
  (%nanovg:move-to *canvas-handle* (x coords) (y coords)))


(defmacro path (&body body)
  `(progn
     (%nanovg:begin-path *canvas-handle*)
     ,@body))


(defun scissors (origin w h)
  (%nanovg:scissor *canvas-handle*
                (x origin) (y origin)
                (f w) (f h)))


(defun reset-scissors ()
  (%nanovg:reset-scissor *canvas-handle*))


(defmacro with-scissors (origin w h &body body)
  `(unwind-protect
        (progn
          (scissors ,origin ,w ,h)
          ,@body)
     (reset-scissors)))


(defun line-cap ()
  (error "Only setter available"))


(defun (setf line-cap) (value)
  (%nanovg:line-cap *canvas-handle* (cffi:foreign-enum-value '%nanovg:line-cap value)))


(defun line-join ()
  (error "Only setter available"))


(defun (setf line-join) (value)
  (%nanovg:line-join *canvas-handle* (cffi:foreign-enum-value '%nanovg:line-cap value)))


(defun line-to (end)
  (%nanovg:line-to *canvas-handle* (x end) (y end)))


(defun bezier-to (ctrl0 ctrl1 end)
  (%nanovg:bezier-to *canvas-handle*
                  (x ctrl0) (y ctrl0)
                  (x ctrl1) (y ctrl1)
                  (x end) (y end)))


(defun rounded-rect (origin width height &optional rounding)
  (%nanovg:rounded-rect *canvas-handle* (x origin) (y origin)
                     (f width) (f height)
                     (f (or rounding 0))))


(defun circle (center radius)
  (%nanovg:circle *canvas-handle* (x center) (y center) (f radius)))


(defun ellipse (center x-radius y-radius)
  (%nanovg:ellipse *canvas-handle*
                (x center) (y center) (f x-radius) (f y-radius)))


(defun arc (center radius a0 a1)
  (%nanovg:arc *canvas-handle* (x center) (y center) (f radius)
            (f a0) (f a1) +default-winding+))


(defun text (position text)
  (with-retained-canvas
    (%restore-coordinate-system)
    (%nanovg:text *canvas-handle*
               (x position) (f (- (canvas-height *canvas*) (y position)))
               text (cffi:null-pointer))))

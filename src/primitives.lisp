(cl:in-package :bodge-canvas)

(defvar *default-clear-color* (vec4 1 1 1 1))

(defun clear-buffers (&optional (clear-color *default-clear-color*))
  (gl:clear-color (x clear-color)
                  (y clear-color)
                  (z clear-color)
                  (w clear-color))
  (gl:clear :color-buffer :stencil-buffer))


(defun reset-viewport (&optional (x 0) (y 0) width height (canvas *canvas*))
  (let* ((pixel-ratio (%pixel-ratio-of canvas))
         (width (* (or width (canvas-width canvas)) pixel-ratio))
         (height (* (or height (canvas-height canvas)) pixel-ratio)))
    (gl:viewport x y width height)))


(defun apply-scissors (origin w h)
  (path
    (scissors origin w h)))


(defun stroke-and-fill (stroke-paint fill-paint thickness)
  (when fill-paint
    (setf (fill-paint) fill-paint)
    (fill-path))
  (when stroke-paint
    (setf (stroke-width) (or thickness 1.0)
          (stroke-paint) stroke-paint)
    (stroke-path)))


(defun draw-line (origin end paint &key (thickness 1.0))
  (path
    (move-to origin)
    (line-to end)
    (setf (stroke-width) (or thickness 1.0)
          (stroke-paint) paint)
    (stroke-path)))


(defun draw-curve (origin end ctrl0 ctrl1 paint &key (thickness 1.0))
  (path
    (move-to origin)
    (bezier-to ctrl0 ctrl1 end)
    (setf (stroke-paint) paint
          (stroke-width) (or thickness 1.0))
    (stroke-path)))


(defun draw-rect (origin w h &key (fill-paint nil) (stroke-paint nil)
                               (thickness 1.0) (rounding 0.0))
  (path
    (rounded-rect origin w h rounding)
    (stroke-and-fill stroke-paint fill-paint thickness)))


(defun draw-image (origin w h image-paint &key (stroke-paint nil)
                                            (thickness 1.0)
                                            (rounding 0.0)
                                            (scale-x 1.0)
                                            (scale-y 1.0)
                                            (translate-x 0.0)
                                            (translate-y 0.0)
                                            (rotate 0.0))
  (path
    (rounded-rect origin w h rounding)
    (with-retained-canvas
      (translate-canvas (+ (x origin) translate-x) (+ (y origin) translate-y))
      (scale-canvas scale-x scale-y)
      (rotate-canvas rotate)
      (stroke-and-fill stroke-paint image-paint thickness))))


(defun draw-circle (center radius &key (fill-paint nil) (stroke-paint nil) (thickness 1.0))
  (path
    (circle center radius)
    (stroke-and-fill stroke-paint fill-paint thickness)))


(defun draw-ellipse (center x-radius y-radius &key (fill-paint nil) (stroke-paint nil)
                                                (thickness 1.0))
  (path
    (ellipse center x-radius y-radius)
    (stroke-and-fill stroke-paint fill-paint thickness)))


(defun draw-arc (center radius a0 a1 &key (fill-paint nil) (stroke-paint nil) (thickness 1.0))
  (path
    (arc center radius a0 a1)
    (stroke-and-fill stroke-paint fill-paint thickness)))


(defun draw-polygon (vertices &key (fill-paint nil) (stroke-paint nil) (thickness 1.0))
  (path
    (let ((first-vertex (car vertices)))
      (move-to first-vertex)
      (loop for vertex in (rest vertices) do (line-to vertex))
      (line-to first-vertex))
    (stroke-and-fill stroke-paint fill-paint thickness)))


(defun draw-polyline (points paint &key (thickness 1.0))
  (path
    (move-to (car points))
    (loop for point in (rest points) do (line-to point))
    (setf (stroke-paint) paint
          (stroke-width) (or thickness 1.0))
    (stroke-path)))

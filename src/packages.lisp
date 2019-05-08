(bodge-util:define-package :bodge-canvas
  (:use :cl :bodge-util :bodge-memory :bodge-math :claw)
  (:export #:make-canvas
           #:canvas-width
           #:canvas-height
           #:destroy-canvas
           #:with-canvas

           #:update-canvas-size
           #:update-canvas-pixel-ratio
           #:flush-canvas
           #:with-retained-canvas
           #:with-preserved-state

           #:path
           #:fill-path
           #:stroke-path
           #:stroke-width
           #:stroke-paint
           #:fill-paint
           #:antialias-shapes
           #:make-image-paint
           #:make-rgba-image-paint
           #:destroy-image-paint
           #:image-paint-width
           #:image-paint-height

           #:scissors
           #:move-to
           #:line-to
           #:bezier-to
           #:rounded-rect
           #:circle
           #:ellipse
           #:arc
           #:text
           #:wind-path
           #:line-cap
           #:line-join

           #:clear-buffers
           #:reset-viewport
           #:apply-scissors
           #:draw-line
           #:draw-curve
           #:draw-rect
           #:draw-image
           #:draw-circle
           #:draw-ellipse
           #:draw-arc
           #:draw-polygon
           #:draw-polyline

           #:translate-canvas
           #:rotate-canvas
           #:skew-canvas
           #:scale-canvas
           #:reset-canvas-transform

           #:draw-text
           #:register-font-face
           #:make-font
           #:make-default-font
           #:with-font
           #:canvas-text-bounds
           #:canvas-text-advance
           #:canvas-font-metrics
           #:canvas-font-line-height
           #:canvas-font-ascender
           #:canvas-font-descender))

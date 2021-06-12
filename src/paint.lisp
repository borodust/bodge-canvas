(cl:in-package :bodge-canvas)


(defgeneric fill-paint ()
  (:documentation "Use with setf to set fill color or paint of the path")
  (:method () (error "Only setter is available")))


(defgeneric stroke-paint ()
  (:documentation "Use with setf to set stroke color or paint of the path")
  (:method () (error "Only setter is available")))


(defgeneric stroke-paint (value))

(defgeneric (setf fill-paint) (value))
(defgeneric (setf stroke-paint) (value))


(defmethod (setf fill-paint) ((color vec4))
  (fill-color color))


(defmethod (setf stroke-paint) ((color vec4))
  (stroke-color color))


(defmethod (setf fill-paint) ((color vec3))
  (fill-color (bodge-math:value->vec4 color :w 1)))


(defmethod (setf stroke-paint) ((color vec3))
  (stroke-color (bodge-math:value->vec4 color :w 1)))


(defclass image-paint ()
  ((handle :initform (cffi:foreign-alloc '(:struct %nanovg:paint)) :reader %handle-of)
   (image :initform nil)))


(defmethod initialize-instance :after ((this image-paint) &key image context)
  (with-slots ((this-image image) handle) this
    (setf this-image image)
    (%nanovg:image-pattern handle (%handle-of context)
                        0.0 0.0 (f (nvg-image-width image)) (f (nvg-image-height image))
                        0.0
                        (nvg-image-id image) 1.0)))


(defun make-image-paint (context image &key flip-vertically use-nearest-interpolation)
  "Image must be an array or list of bytes of encoded .jpg, .png, .psd, .tga, .pic or .gif file"
  (make-instance 'image-paint
                 :context context
                 :image (make-image context image
                                    :flip-vertically flip-vertically
                                    :use-nearest-interpolation use-nearest-interpolation)))


(defun make-rgba-image-paint (context image width height &key flip-vertically use-nearest-interpolation)
  (make-instance 'image-paint
                 :context context
                 :image (make-rgba-image context image width height
                                         :flip-vertically flip-vertically
                                         :use-nearest-interpolation use-nearest-interpolation)))


(defun destroy-image-paint (context image-paint)
  (with-slots (image handle) image-paint
    (destroy-image context image)
    (cffi:foreign-free handle)))


(defun image-paint-height (paint)
  (with-slots (image) paint
    (nvg-image-height image)))


(defun image-paint-width (paint)
  (with-slots (image) paint
    (nvg-image-width image)))


(defmethod (setf fill-paint) ((value image-paint))
  (%nanovg:fill-paint *canvas-handle* (%handle-of value)))


(defmethod (setf stroke-paint) ((value image-paint))
  (%nanovg:fill-paint *canvas-handle* (%handle-of value)))

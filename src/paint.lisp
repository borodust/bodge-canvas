(cl:in-package :bodge-canvas)


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


(defclass image-paint (disposable)
  ((handle :initform (alloc '(:struct (%nvg:paint))) :reader %handle-of)
   (image :initform nil)))


(define-destructor image-paint (handle)
  (free handle))


(defmethod initialize-instance :after ((this image-paint) &key image)
  (with-slots ((this-image image)) this
    (setf this-image image)
    (%nvg:image-pattern (%handle-of this) *handle*
                        0.0 0.0 (f (nvg-image-width image)) (f (nvg-image-height image))
                        0.0
                        (nvg-image-id image) 1.0)))


(defun make-image-paint (image)
  (make-instance 'image-paint :image image))


(defmethod (setf fill-paint) ((value image-paint))
  (%nvg:fill-paint *handle* (%handle-of value)))


(defmethod (setf stroke-paint) ((value image-paint))
  (%nvg:fill-paint *handle* (%handle-of value)))

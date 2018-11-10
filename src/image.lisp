(cl:in-package :bodge-canvas)


(defclass nvg-image ()
  ((id :initarg :id :reader nvg-image-id)
   (width :initarg :width :reader nvg-image-width)
   (height :initarg :height :reader nvg-image-height)))


(defun destroy-image (context image)
  (with-slots (id) image
    (nvg:destroy-image (%handle-of context) id)))


(defun %arrange-opts (flip-vertically use-nearest-interpolation)
  (nconc (list :generate-mipmaps :repeatx :repeaty)
         (when flip-vertically
           (list :flipy))
         (when use-nearest-interpolation
           (list :nearest))))


(defun make-image (context image &key flip-vertically use-nearest-interpolation)
  "Image must be an array or list of bytes of encoded .jpg, .png, .psd, .tga, .pic or .gif file"
  (bodge-util:with-simple-array-pointer (ptr image)
    (let ((id (apply #'nvg:make-image (%handle-of context) ptr
                     (%arrange-opts flip-vertically use-nearest-interpolation))))
      (c-with ((width :int)
               (height :int))
        (%nvg:image-size *handle* id (width &) (height &))
        (make-instance 'nvg-image :id id :width width :height height)))))


(defun make-rgba-image (context image width height &key flip-vertically use-nearest-interpolation)
  (let ((expected-size (* width height 4)))
    (unless (= expected-size (reduce #'* (array-dimensions image)))
      (error "Wrong size of image array: expected ~A, got ~A" expected-size (length image))))
  (bodge-util:with-simple-array-pointer (ptr image)
    (let ((id (apply #'nvg:make-rgba-image (%handle-of context)
                     (floor width) (floor height)
                     ptr
                     (%arrange-opts flip-vertically use-nearest-interpolation))))
      (make-instance 'nvg-image :id id :width width :height height))))

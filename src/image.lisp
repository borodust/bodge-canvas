(cl:in-package :bodge-canvas)


(defclass nvg-image ()
  ((id :initarg :id :reader nvg-image-id)
   (data :initarg :data)
   (width :initarg :width :reader nvg-image-width)
   (height :initarg :height :reader nvg-image-height)))


(defun %arrange-opts (flip-vertically use-nearest-interpolation)
  (nconc (list :generate-mipmaps :repeatx :repeaty)
         (when flip-vertically
           (list :flipy))
         (when use-nearest-interpolation
           (list :nearest))))


(defun make-image (image &key flip-vertically use-nearest-interpolation)
  "Image must be an array or list of bytes of encoded .jpg, .png, .psd, .tga, .pic or .gif file"
  (let* ((data (static-vectors:make-static-vector (length image) :initial-contents image))
         (id (apply #'nvg:make-image *handle* (static-vectors:static-vector-pointer data)
                    (%arrange-opts flip-vertically use-nearest-interpolation))))
    (c-with ((width :int)
             (height :int))
      (%nvg:image-size *handle* id (width &) (height &))
      (make-instance 'nvg-image :id id :data data :width width :height height))))


(defun make-rgba-image (image width height &key flip-vertically use-nearest-interpolation)
  (let ((expected-size (* width height 4)))
    (unless (= expected-size (length image))
      (error "Wrong size of image array: expected ~A, got ~A" expected-size (length image))))
  (let* ((data (static-vectors:make-static-vector (length image) :initial-contents image))
         (id (apply #'nvg:make-rgba-image *handle*
                    (floor width) (floor height)
                    (static-vectors:static-vector-pointer data)
                    (%arrange-opts flip-vertically use-nearest-interpolation))))
    (make-instance 'nvg-image :id id :data data :width width :height height)))


(defun destroy-image (image)
  (with-slots (data id) image
    (static-vectors:free-static-vector data)
    (nvg:destroy-image *handle* id)))

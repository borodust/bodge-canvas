(cl:in-package :bodge-canvas)


(declaim (special *canvas*
                  *canvas-handle*))


(defvar *default-font-name* "NotoMono-Regular")


(let* ((default-font-data
         (read-file-into-byte-vector
          (system-relative-pathname :bodge-canvas
                                    (format nil "../font/~A.ttf" *default-font-name*))))
       (default-font-data-static (mt:make-guarded-reference nil)))
  (defun default-font-static-data ()
    ;; trick to load static-vectorized data upon first usage
    ;; but dump default-font-data as a plain array into an image
    (mt:with-guarded-reference (font-data default-font-data-static)
      (unless font-data
        (setf font-data (static-vectors:make-static-vector (length default-font-data)
                                                           :initial-contents default-font-data)
              default-font-data nil))
      font-data)))


(defclass canvas ()
  ((handle :initarg :handle :initform (error ":handle missing") :reader %handle-of)
   (width :initarg :width :initform (error ":width missing") :reader canvas-width)
   (height :initarg :height :initform (error ":height missing") :reader canvas-height)
   (pixel-ratio :initarg :pixel-ratio :initform 1.0 :reader %pixel-ratio-of)
   (default-font-id :reader %default-font-of)
   (font-map :initform (make-hash-table :test #'equal))))


(defun %register-font (canvas name font-data)
  (with-slots (font-map) canvas
    (setf (gethash (namestring name) font-map) font-data)))


(defun %ensure-font-face (canvas name foreign-data-ptr data-size)
  (let ((font-face-id (%nanovg:find-font (%handle-of canvas) (namestring name))))
    (if (< font-face-id 0)
        (%nanovg:create-font-mem (%handle-of canvas) (namestring name)
                              foreign-data-ptr data-size 0)
        font-face-id)))


(defun %register-font-face (canvas name data)
  (let* ((f-data (static-vectors:make-static-vector (length data) :initial-contents data))
         (id (%ensure-font-face canvas name
                                (static-vectors:static-vector-pointer f-data) (length f-data))))
    (when (< id 0)
      (static-vectors:free-static-vector f-data)
      (error "Failed to register face with name '~A'" name))
    (%register-font canvas name f-data)
    id))


(defun update-canvas-size (canvas width height)
  (with-slots ((w width) (h height)) canvas
    (setf w (round width)
          h (round height))))


(defun update-canvas-pixel-ratio (canvas pixel-ratio)
  (with-slots ((ratio pixel-ratio)) canvas
    (setf ratio pixel-ratio)))


(defmethod initialize-instance :after ((this canvas) &key)
  (with-slots (default-font-id) this
    (let* ((default-font-data (default-font-static-data))
           (font-id (%ensure-font-face this *default-font-name*
                                      (static-vectors:static-vector-pointer default-font-data)
                                      (length default-font-data))))
      (%nanovg:add-fallback-font-id (%handle-of this) font-id font-id)
      (setf default-font-id font-id))))


(defun make-canvas (width height &key (pixel-ratio 1.0) (antialiased t))
  (let ((opts (append (list :stencil-strokes)
                      (when antialiased (list :antialias))
                      (in-development-mode (list :debug)))))
    (make-instance 'canvas
                   :handle (apply #'nanovg:make-context opts)
                   :pixel-ratio (f pixel-ratio)
                   :width width
                   :height height)))


(defun destroy-canvas (canvas)
  (nvg:destroy-context (%handle-of canvas)))


(defun %invert-coordinate-system ()
  (%nanovg:translate *canvas-handle* 0f0 (f (canvas-height *canvas*)))
  (%nanovg:scale *canvas-handle* 1f0 -1f0))


(defun %restore-coordinate-system ()
  (%nanovg:scale *canvas-handle* 1f0 -1f0)
  (%nanovg:translate *canvas-handle* 0f0 (f (- (canvas-height *canvas*)))))


(defun begin-canvas ()
  (%nanovg:begin-frame *canvas-handle* (f (canvas-width *canvas*)) (f (canvas-height *canvas*))
                    (f (%pixel-ratio-of *canvas*)))
  (%invert-coordinate-system))


(defun end-canvas ()
  (%nanovg:end-frame *canvas-handle*))


(defun flush-canvas ()
  (end-canvas)
  (begin-canvas))


(defmacro with-canvas ((canvas) &body body)
  (once-only (canvas)
    `(let ((*canvas* ,canvas)
           (*canvas-handle* (%handle-of ,canvas)))
       (unwind-protect
            (progn
              (begin-canvas)
              ,@body)
         (end-canvas)))))


(defun stroke-color (color)
  (c-with ((color-v %nanovg:|C:@S@NV-GCOLOR@UA@SA|))
    (setf (color-v :r) (x color)
          (color-v :g) (y color)
          (color-v :b) (z color)
          (color-v :a) (w color))
    (%nanovg:stroke-color *canvas-handle* (color-v &))))


(defun fill-color (color)
  (c-with ((color-v %nanovg:|C:@S@NV-GCOLOR@UA@SA|))
    (setf (color-v :r) (x color)
          (color-v :g) (y color)
          (color-v :b) (z color)
          (color-v :a) (w color))
    (%nanovg:fill-color *canvas-handle* (color-v &))))


(defun push-canvas ()
  (%nanovg:save *canvas-handle*))


(defun pop-canvas ()
  (%nanovg:restore *canvas-handle*))


(defun reset-canvas ()
  (%nanovg:reset *canvas-handle*))


(defmacro with-retained-canvas (&body body)
  `(unwind-protect
        (progn
          (push-canvas)
          ,@body)
     (pop-canvas)))


(defun reset-state (program
                    blend-func
                    cull-face-enabled
                    cull-face
                    front-face
                    blend-enabled
                    depth-test-enabled
                    scissor-test-enabled
                    color-mask
                    stencil-mask
                    stencil-op
                    stencil-func
                    active-texture
                    bound-texture-at-0
                    bound-vertex-array
                    unpack-alignment
                    unpack-row-length
                    unpack-skip-pixels
                    unpack-skip-rows)
  (flet ((feature-switch (enum val)
           (if val
               (gl:enable enum)
               (gl:disable enum))))
    (gl:use-program (or program 0))
    (apply #'gl:blend-func (or blend-func '(:one :zero)))
    (feature-switch :cull-face cull-face-enabled)
    (gl:cull-face (or cull-face :back))
    (gl:front-face (or front-face :ccw))
    (feature-switch :blend blend-enabled)
    (feature-switch :depth-test depth-test-enabled)
    (feature-switch :scissor-test scissor-test-enabled)
    (apply #'gl:color-mask (or color-mask '(:true :true :true :true)))
    (gl:stencil-mask (or stencil-mask #xffffffff))
    (apply #'gl:stencil-op (or stencil-op '(:keep :keep :keep)))
    (apply #'gl:stencil-func (or stencil-func '(:always 0 #xffffffff)))
    (gl:active-texture :texture0)
    (apply #'gl:bind-texture (or bound-texture-at-0 '(:texture-2d 0)))
    (when active-texture
      (gl:active-texture active-texture))
    (unless (uiop:featurep :bodge-gl2)
      (gl:bind-vertex-array (or bound-vertex-array 0)))
    (gl:pixel-store :unpack-alignment (or unpack-alignment 4))
    (gl:pixel-store :unpack-row-length (or unpack-row-length 0))
    (gl:pixel-store :unpack-skip-pixels (or unpack-skip-pixels 0))
    (gl:pixel-store :unpack-skip-rows (or unpack-skip-rows 0))))


(defmacro with-preserved-state ((&key program
                                   blend-func
                                   cull-face-enabled
                                   cull-face
                                   front-face
                                   blend-enabled
                                   depth-test-enabled
                                   scissor-test-enabled
                                   color-mask
                                   stencil-mask
                                   stencil-op
                                   stencil-func
                                   active-texture
                                   bound-texture-at-0
                                   bound-vertex-array
                                   unpack-alignment
                                   unpack-row-length
                                   unpack-skip-pixels
                                   unpack-skip-rows)
                                &body body)
  `(unwind-protect
        (progn ,@body)
     (reset-state ,program
                  ,blend-func
                  ,cull-face-enabled
                  ,cull-face
                  ,front-face
                  ,blend-enabled
                  ,depth-test-enabled
                  ,scissor-test-enabled
                  ,color-mask
                  ,stencil-mask
                  ,stencil-op
                  ,stencil-func
                  ,active-texture
                  ,bound-texture-at-0
                  ,bound-vertex-array
                  ,unpack-alignment
                  ,unpack-row-length
                  ,unpack-skip-pixels
                  ,unpack-skip-rows)))


(defun antialias-shapes (value)
  (%nanovg:shape-anti-alias *canvas-handle* (if value 1 0)))

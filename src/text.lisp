(cl:in-package :bodge-canvas)


(defvar *active-font* nil)

(defvar *black* (vec4 0 0 0 1))


(defclass font ()
  ((face :initarg :face :reader %face-of)
   (size :initarg :size :initform nil :reader %size-of)
   (letter-spacing :initarg :letter-spacing :initform nil :reader %letter-spacing-of)
   (line-height :initarg :line-height :initform nil :reader %line-height-of)
   (alignment :initarg :alignment :initform nil :reader %alignment-of)))


(defun register-font-face (canvas name font-container-data)
  (%register-font-face canvas name font-container-data))


(defun draw-text (position text &optional (fill-paint *black*))
  (path
    (setf (fill-paint) fill-paint)
    (text position text)))


(defun make-font (face-id &rest args &key size letter-spacing line-height alignment)
  (declare (ignore letter-spacing line-height alignment size))
  (apply #'make-instance 'font :face face-id args))


(defun make-default-font (&rest args &key size letter-spacing line-height alignment)
  (declare (ignore letter-spacing line-height alignment size))
  (apply #'make-font nil args))


(defun %apply-font (canvas font)
  (let ((context *canvas-handle*))
    (if-let ((face-id (%face-of font)))
      (%nanovg:font-face-id context face-id)
      (%nanovg:font-face-id context (%default-font-of canvas)))
    (when-let ((size (%size-of font)))
      (%nanovg:font-size context (f size)))
    (when-let ((spacing (%letter-spacing-of font)))
      (%nanovg:text-letter-spacing context (f spacing)))
    (when-let ((line-height (%line-height-of font)))
      (%nanovg:text-line-height context (f line-height)))
    (when-let ((alignment (%alignment-of font)))
      (%nanovg:text-align context alignment))))


(defmacro with-font ((font) &body body)
  (once-only (font)
    `(unwind-protect
          (let ((*active-font* ,font))
            (%apply-font *canvas* ,font)
            ,@body)
       (when *active-font*
         (%apply-font *canvas* *active-font*)))))


(defun canvas-text-bounds (string &optional (canvas *canvas*))
  (static-vectors:with-static-vector (bounds 4 :element-type 'single-float)
    (let* ((advance (%nanovg:text-bounds (%handle-of canvas) 0f0 0f0 string (cffi:null-pointer)
                                      (static-vectors:static-vector-pointer bounds)))
           (height (- (aref bounds 3) (aref bounds 1)))
           (width (- (aref bounds 2) (aref bounds 0))))
      (values (vec2 (aref bounds 0) (- (aref bounds 3))) width height advance))))


(defun canvas-text-advance (string &optional (canvas *canvas*))
  (%nanovg:text-bounds (%handle-of canvas) 0f0 0f0 string nil nil))

;;;
;;; Metrics
;;;
(defstruct (font-metrics
            (:conc-name canvas-font-))
  (line-height (f 0) :type single-float :read-only t)
  (ascender (f 0) :type single-float :read-only t)
  (descender (f 0) :type single-float :read-only t))


(defun canvas-font-metrics (&optional (canvas *canvas*))
  (c-with ((ascender :float) (descender :float) (line-height :float))
    (%nanovg:text-metrics (%handle-of canvas) (ascender &) (descender &) (line-height &))
    (make-font-metrics :line-height line-height
                       :ascender ascender
                       :descender descender)))

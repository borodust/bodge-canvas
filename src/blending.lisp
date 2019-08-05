(cl:in-package :bodge-canvas)

(defenum composite-operation
  :source-over
  :source-in
  :source-atop
  :destination-over
  :destination-in
  :destination-out
  :destination-atop
  :lighter
  :copy
  :xor)


(defenum blend-factor
  :zero
  :one
  :source-color
  :one-minus-source-color
  :destination-color
  :one-minus-destination-color
  :source-alpha
  :one-minus-source-alpha
  :destination-alpha
  :one-minus-destination-alpha
  :alpha-saturate)

;;;
;;; COMPOSITE OPERATION
;;;
(defvar *composite-operation* :source-over)
(defvar *source-factor* :one)
(defvar *destination-factor* :zero)
(defvar *source-alpha-factor* nil)
(defvar *destination-alpha-factor* nil)
(defvar *alpha* 1f0)


(defun composite-operation->nvg (op)
  (ecase op
    (:source-over %nvg:+source-over+)
    (:source-in %nvg:+source-in+)
    (:source-atop %nvg:+atop+)
    (:destination-over %nvg:+destination-over+)
    (:destination-in %nvg:+destination-in+)
    (:destination-out %nvg:+destination-out+)
    (:destination-atop %nvg:+destination-atop+)
    (:lighter %nvg:+lighter+)
    (:copy %nvg:+copy+)
    (:xor %nvg:+source-over+)))


(defun switch-composite-operation (canvas)
  (assert (composite-operation-p *composite-operation*))
  (%nvg:global-composite-operation (%handle-of canvas)
                                   (composite-operation->nvg *composite-operation*)))


(defmacro with-composite-operation ((operation &optional (canvas *canvas*)) &body body)
  (once-only (operation canvas)
    `(unwind-protect
          (let ((*composite-operation* ,operation))
            (switch-composite-operation ,canvas)
            ,@body)
       (switch-composite-operation ,canvas))))

;;;
;;; BLENDING
;;;
(defun blend-factor->nvg (op)
  (ecase op
    (:zero %nvg:+zero+)
    (:one %nvg:+one+)
    (:source-color %nvg:+src-color+)
    (:one-minus-source-color %nvg:+one-minus-src-color+)
    (:destination-color %nvg:+dst-color+)
    (:one-minus-destination-color %nvg:+one-minus-dst-color+)
    (:source-alpha %nvg:+src-alpha+)
    (:one-minus-source-alpha %nvg:+one-minus-src-alpha+)
    (:destination-alpha %nvg:+dst-alpha+)
    (:one-minus-destination-alpha %nvg:+one-minus-dst-alpha+)
    (:srouce-alpha-saturate %nvg:+src-alpha-saturate+)))


(defun switch-blend-factors (canvas)
  (%nvg:global-composite-blend-func-separate
   (%handle-of canvas)
   (blend-factor->nvg *source-factor*)
   (blend-factor->nvg *destination-factor*)
   (blend-factor->nvg (or *source-alpha-factor* *source-factor*))
   (blend-factor->nvg (or *destination-alpha-factor* *destination-factor*))))


(defmacro with-blend-factors ((source-factor destination-factor
                               &key source-alpha-factor destination-alpha-factor
                                 (canvas *canvas*)) &body body)
  (once-only (source-factor destination-factor
                            source-alpha-factor destination-alpha-factor
                            canvas)
    `(unwind-protect
          (let* ((*source-factor* ,source-factor)
                 (*destination-factor* ,destination-factor)
                 (*source-alpha-factor* ,source-alpha-factor)
                 (*destination-alpha-factor* ,destination-alpha-factor))
            (switch-blend-factors ,canvas)
            ,@body)
       (switch-blend-factors ,canvas))))


(defmacro with-alpha ((value &key (override nil override-provided-p)
                               (canvas nil canvas-provided-p)) &body body)
  (once-only (value)
    (with-gensyms (cnvs)
      `(let ((,cnvs ,(if canvas-provided-p
                         `(or ,canvas *canvas*)
                         `*canvas*))
             (*alpha* ,(if override-provided-p
                           `(if ,override
                                (float ,value 0f0)
                                (* (float ,value 0f0) *alpha*))
                           `(* (float ,value 0f0) *alpha*))))
         (unwind-protect
              (progn
                (%nvg:global-alpha (%handle-of ,cnvs) *alpha*)
                ,@body)
           (%nvg:global-alpha (%handle-of ,cnvs) *alpha*))))))

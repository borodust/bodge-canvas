(asdf:defsystem :bodge-canvas
  :description "Hardware-accelrated vector graphics library"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :version "1.0.0"
  :license "MIT"
  :depends-on (:bodge-utilities :bodge-math :bodge-memory :bodge-nanovg :nanovg-blob
                                :cl-muth :cl-opengl :static-vectors
                                :cffi :cffi-c-ref)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "canvas")
               (:file "image")
               (:file "paint")
               (:file "path")
               (:file "blending")
               (:file "transform")
               (:file "primitives")
               (:file "text")))

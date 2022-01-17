;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :automaticl
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:fwoar-lisputils
               #:yason
               #:drakma
               #:cl-js)
  :serial t
  :components ((:file "package")
               (:file "matrix")))

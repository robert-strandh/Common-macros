(cl:in-package #:asdf-user)

(defsystem "common-macro-definitions-shared"
  :depends-on ("ecclesia")
  :serial t
  :components
  ((:file "utilities")
   (:file "defmethod")
   (:file "deftype")
   (:file "defpackage")
   (:file "defparameter")
   (:file "defmacro")
   (:file "define-compiler-macro")
   (:file "define-setf-expander")
   (:file "defun")
   (:file "defclass")
   (:file "defgeneric")
   (:file "declaim")))

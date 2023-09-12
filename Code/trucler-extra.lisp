(cl:in-package #:trucler)

(shadow '(#:get-setf-expansion))

(defgeneric get-setf-expansion (client environment place))

(defmethod get-setf-expansion (client environment place)
  (let ((global-environment (global-environment client environment)))
    (get-setf-expansion client global-environment place)))


(cl:in-package #:common-macros-test)

(define-test with-slots)

(define-test with-slots-empty
  :parent with-slots
    (is #'equal
        #1=(with-slots () #'print-object)
        (eval (expand-expression #1#))))

;;; This form fails to compile so it is commented out until
;;; it can be determined whether Parachute has a bug.

;; (define-test with-slots-empty
;;   :parent with-slots
;;   (let ((class-name (gensym))
;;         (slot-name (gensym)))
;;     (is #'equal
;;         (eval `(progn #2=(defclass ,class-name () ((,slot-name)))
;;                       #1=(with-slots ((,slot-name ,slot-name))
;;                              (make-instance ',class-name)
;;                            (setq ,slot-name 234)
;;                            ,slot-name)))
;;         (eval `(progn #2# ,(expand-expression #1#))))))

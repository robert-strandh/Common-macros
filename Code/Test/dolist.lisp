(cl:in-package #:common-macros-test)

(define-test dolist)

(define-test dolist-empty-no-result-form
  :parent dolist
  (is #'equal
      #1=(dolist (var '()))
      (eval (expand-expression '#1#))))

(define-test dolist-empty-with-result-form
  :parent dolist
  (is #'equal
      #1=(dolist (var '() 234))
      (eval (expand-expression '#1#))))

(define-test dolist-one-element-with-result-form
  :parent dolist
  (is #'equal
      (let ((x 0)) #1=(dolist (var '(1) x) (setq x var)))
      (eval `(let ((x 0))
               ,(expand-expression '#1#)))))

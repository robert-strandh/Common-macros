(cl:in-package #:common-macro-definitions)

;;; Based on SBCL's.

(defun expand-do-*-symbols (package-list-form range variable body result)
  ;; package-list-form can be a single package, as per definition of
  ;; with-package-iterator that it takes a list designator.
  (multiple-value-bind (declarations forms) (ecc:separate-ordinary-body body)
    (let ((iterator (gensym "PACKAGE-ITERATOR"))
          (rp (gensym)) (loop (gensym "LOOP")))
      `(block nil
         (with-package-iterator (,iterator ,package-list-form ,@range)
           (tagbody
              ,loop
              (multiple-value-bind (,rp ,variable) (,iterator)
                ,@declarations
                (if ,rp
                    (tagbody ,@forms (go ,loop))
                    (return ,result)))))))))

(defmacro do-symbols ((var &optional (package '*package*) result) &body body)
  (expand-do-*-symbols package '(:internal :external :inherited)
                       var body result))

(defmacro do-external-symbols ((var &optional (package '*package*) result) &body body)
  (expand-do-*-symbols package '(:external) var body result))

(defmacro do-all-symbols ((var &optional result) &body body)
  ;; don't need :inherited since we're already covering all packages.
  (expand-do-*-symbols '(list-all-packages) '(:internal :external) var body result))

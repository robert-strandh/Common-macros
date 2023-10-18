(cl:in-package #:common-macro-definitions)

(defun expand-short-defsetf (access-fn update-fn documentation)
  `(define-setf-expander ,access-fn (&rest args)
     ,documentation ; if NIL, harmless.
     (let ((temps (loop for arg in args collect (gensym)))
           (store (gensym "STORE")))
       (values temps args (list store)
               `(,',update-fn ,@temps ,store)
               `(,',access-fn ,@temps)))))

;;; Note that the long form of DEFSETF is poorly defined for some obscure cases.
;;; See e.g. https://bugs.launchpad.net/sbcl/+bug/1452947
(defun expand-long-defsetf (access-fn lambda-list stores body)
  (let* ((argforms (gensym "ARGFORMS"))
         (temps (gensym "TEMPS"))
         (ll (ecclesia:parse-defsetf-lambda-list lambda-list))
         (eparam1 (ecclesia:environment ll))
         (eparam (if (eq eparam1 :none) nil eparam1)))
    (multiple-value-bind (bindings ignores) (ecc:destructure-lambda-list ll argforms)
      `(define-setf-expander ,access-fn (&rest ,argforms
                                           ,@(when eparam `(&environment ,eparam)))
         (let ((,temps (loop repeat (length ,argforms) collect (gensym))))
           (values ,temps ,argforms ',stores
                   (let* (,@bindings
                          ;; we rebind eparam so that body declarations can apply to it.
                          ,@(when eparam `((,eparam ,eparam))))
                     (declare (ignore ,@ignores))
                     ,@body)
                   ;; avoiding some weird double unquotes
                   (list* ',access-fn ,argforms)))))))

(defmacro defsetf (access-fn &rest rest)
  (check-type access-fn symbol)
  (etypecase (first rest)
    (symbol ; short form
     (destructuring-bind (update-fn &optional (documentation nil docp)) rest
       (when docp (check-type documentation string))
       (expand-short-defsetf access-fn update-fn documentation)))
    (list ; long form
     (destructuring-bind (lambda-list (&rest stores) &body body) rest
       (assert (every #'symbolp stores))
       (expand-long-defsetf access-fn lambda-list stores body)))))

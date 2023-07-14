(cl:in-package #:common-macros)

;;; This definition does not quite work, because it uses a
;;; non-standard function STRING-INPUT-STREAM-POSITION.

(defmethod expand (client (ast ico:with-input-from-string-ast) environment)
  (declare (ignore client environment))
  (let ((make-string-input-stream-ast
          (application
           'make-string-input-stream
           (if (null (ico:start-ast ast))
               (node* (:literal :literal 0))
               (ico:start-ast ast))
           (if (null (ico:end-ast ast))
               (node* (:literal :literal 'nil))
               (ico:end-ast ast)))))
  (if (null (ico:index-ast ast))
      (node* (:with-open-stream)
        (1 :var (ico:var-ast ast))
        (1 :stream make-string-input-stream-ast)
        (* :form (ico:form-asts ast)))
      (node* (:with-open-stream)
        (1 :var (ico:var-ast ast))
        (1 :stream make-string-input-stream-ast)
        (* :declaration (ico:declaration-asts ast))
        (* :form
           (node* (:multiple-value-prog1)
             (1 :values
                (node* (:progn)
                  (* :form (ico:form-asts ast))))
             (1 :form
                (node* (:setf)
                  (1 :place (ico:index-ast ast))
                  (1 :value
                     (application
                      'string-input-stream-position
                      (ico:var-ast ast)))))))))))

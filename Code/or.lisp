(cl:in-package #:common-macros)

(defmethod expand ((ast ico:or-ast))
  (let ((form-asts (ico:form-asts ast))
        (origin (ico:origin ast)))
    (abp:with-builder ((make-instance 'bld:builder))
      (cond ((null form-asts)
             (abp:node* (:literal :literal 'nil :source origin)))
            ((null (rest form-asts))
             (first form-asts))
            (t
             (let ((variable-name (gensym)))
               (flet ((make-variable-name-ast ()
                        (abp:node* (:variable-name
                                    :source origin
                                    :name variable-name))))
                 (abp:node* (:let :source origin)
                   (1 :binding
                      (abp:node* (:value-binding :source origin)
                        (1 :name (make-variable-name-ast))
                        (1 :value (first form-asts))))
                   (1 :form
                      (abp:node* (:if)
                        (1 :test (make-variable-name-ast))
                        (1 :then (make-variable-name-ast))
                        (1 :else (abp:node* (:or :source origin)
                                   (* :form (rest form-asts))))))))))))))

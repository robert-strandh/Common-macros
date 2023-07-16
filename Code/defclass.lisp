(cl:in-package #:common-macros)

;;; FIXME: handle ALLOCATION, TYPE, and implementation-defined
;;; options.
(defun canonicalize-slot-specifier-ast (slot-specifier-ast)
  (node* (:application)
    (1 :function-name (make-function-name-ast 'list))
    (* :argument
       (list (node* (:literal :value ':name))
             (ico:name-ast slot-specifier-ast)))
    (* :argument
       (if (null (ico:initform-ast slot-specifier-ast))
           '()
           (list (node* (:literal :value ':initform))
                 (ico:initform-ast slot-specifier-ast)
                 (node* (:literal :value ':initfunction))
                 (node* (:lambda)
                   (1 :lambda-list (node* (:ordinary-lambda-list)))
                   (1 :form (ico:initform-ast slot-specifier-ast))))))
    (* :argument
       (if (null (ico:initarg-name-asts slot-specifier-ast))
           '()
           (list (node* (:literal :value ':initargs))
                 (node* (:quote
                         :object
                         (mapcar #'unparse
                                 (ico:initarg-name-asts slot-specifier-ast)))))))
    (* :argument
       (list (node* (:literal :value ':readers))
             (node* (:quote
                     :object
                     (append (mapcar #'unparse
                                     (ico:reader-asts slot-specifier-ast))
                             (mapcar #'unparse
                                     (ico:accessor-asts slot-specifier-ast)))))))
    (* :argument
       (list (node* (:literal :value ':writers))
             (node* (:application)
               (1 :function-name (make-function-name-ast 'list))
               (* :argument
                  (loop for writer-ast in (ico:writer-asts slot-specifier-ast)
                        collect (node* (:quote :object (unparse writer-ast)))))
               ;; We cant quote this argument because we must
               ;; construct names such as (SETF FOO) using NODE*.
               (* :argument
                  (loop for accessor-ast in (ico:accessor-asts slot-specifier-ast)
                        collect
                        (node* (:application)
                          (1 :function-name (make-function-name-ast 'list))
                          (1 :argument (node* (:quote :object 'setf)))
                          (1 :argument
                             (node* (:quote :object (unparse accessor-ast))))))))))
    (* :argument
       (if (null (ico:documentation-ast slot-specifier-ast))
           '()
           (list (node* (:literal :value ':documentation))
                 (ico:documentation-ast slot-specifier-ast))))))

(defmethod expand (client (ast ico:defclass-ast) environment)
  (node* (:application)
    (1 :function-name (make-function-name-ast 'ensure-class))
    (1 :argument (ico:name-ast ast))
    (* :argument
       (list (node* (:literal :value ':direct-superclasses))
             (node* (:quote :object
                            (mapcar #'unparse
                                    (ico:superclass-asts ast))))))
    (* :argument
       (list (node* (:literal :value ':direct-slots))
             (canonicalize-slot-specifier-ast (ico:slot-specifier-asts ast))))
    (* :argument
       (if (null (ico:default-initarg-asts ast))
           '()
           (list (node* (:literal :value ':direct-default-initargs))
                 (node* (:application)
                   (1 :function-name (make-function-name-ast 'list))
                   (* :argument
                      (loop for default-initarg-ast in (ico:default-initargs ast)
                            collect
                            (node* (:application)
                              (1 :function-name (make-function-name-ast 'list))
                              (1 :argument (ico:name-ast default-initarg-ast))
                              (1 :argument (ico:initform-ast default-initarg-ast))
                              (1 :argument
                                 (node* (:lambda)
                                   (1 :lambda-list (node* (:ordinary-lambda-list)))
                                   (1 :form
                                      (ico:initform-ast default-initarg-ast)))))))))))
    (* :argument
       (if (null (ico:metaclass-asts ast))
           '()
           (list (node* (:literal :value ':metaclass))
                 (node* (:quote :object (unparse (ico:metaclass-ast ast)))))))
    (* :argument
       (if (null (ico:documentation ast))
           '()
           (list (node* (:literal :value ':documentation))
                 (ico:documentation-ast ast))))))

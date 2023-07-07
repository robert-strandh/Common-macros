(cl:in-package #:common-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate an ordinary body such as a let or let* body that may
;;; contain declarations (but no documentation) into the declarations
;;; and the executable forms.
;;;
;;; If there are declarations after the first executable form (which
;;; is a syntax error), then those declarations will be considered
;;; part of the executable forms.

(defvar *origin*)

(defmacro with-ast-origin (ast &body body)
  `(let ((*origin* (ico:origin ,ast)))
     ,@body))

(defmacro with-builder (&body body)
  `(abp:with-builder ((make-instance 'bld:builder))
     ,@body))

(defgeneric expand (client ast environment))

(defmethod expand :around (client ast environment)
  (with-builder (with-ast-origin ast (call-next-method))))

(defun separate-ordinary-body (body)
  (let ((pos (position-if (lambda (item)
                            (and (consp item)
                                 (eq (car item) 'declare)))
                          body
                          :from-end t)))
    (if (null pos)
        (values '() body)
        (values (subseq body 0 (1+ pos)) (subseq body (1+ pos))))))

(defun extract-bindings (variable-clauses)
  (mapcar
   (lambda (clause)
     (cond ((symbolp clause) clause)
           ((null (cdr clause)) (car clause))
           (t (list (car clause) (cadr clause)))))
   variable-clauses))

(defun extract-updates (variable-clauses)
  (if (null variable-clauses) '()
      (let ((clause (car variable-clauses)))
        (if (and (consp clause)
                 (not (null (cddr clause))))
            (list* (car clause)
                   (caddr clause)
                   (extract-updates (cdr variable-clauses)))
            (extract-updates (cdr variable-clauses))))))

(defmacro node* (initargs &body body)
  `(abp:node* (,@initargs :source *origin*)
     ,@body))

(defun make-let-binding-ast (name-ast value-ast)
  (node* (:value-binding)
    (1 :name name-ast)
    (1 :value value-ast)))

(defun make-eval-when-situation-asts (&rest situations)
  (loop for situation in situations
        collect (node* (:eval-when-situation :situation situation))))

(defun make-quote-ast (object)
  (node* (:quote)
    (1 :object
       (node* (:literal :value object)))))

(defun make-variable-name-ast (name)
  (node* (:variable-name :name name)))

(defun make-unparsed-form-ast (expression)
  (node* (:unparsed :context :form :expression expression)))

(defun make-function-name-ast (function-name)
  (node* (:function-name :name function-name)))

(defun wrap-in-block-ast (block-name form-asts)
  (node* (:block)
    (1 :name
       (node* (:block-name :name block-name)))
    (* :form form-asts)))

(defun expand-place-ast (place-ast environment)
  (multiple-value-bind
        (variables value-forms store-variables store-form read-form)
      (get-setf-expansion (ico:place place-ast) environment)
    (values
     (loop for variable in variables
           for value-form in value-forms
           for variable-name-ast
             = (node* (:variable-name :name variable))
           for value-ast
             = (node* (:unparsed
                       :context :form
                       :expression value-form))
           collect (make-let-binding-ast variable-name-ast value-ast))
     (loop for store-variable in store-variables
           collect
           (node* (:variable-name :name store-variable)))
     (node* (:unparsed
             :context :form
             :expression store-form))
     (node* (:unparsed
             :context :form
             :expression read-form)))))

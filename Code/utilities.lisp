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

(defun atag (name)
  (node* (:tag :name name)))

(defmacro ablock (name-form &body form-asts)
  (let ((form-variable (gensym)))
    `(node* (:block)
       (1 :name (let ((,form-variable ,name-form))
                  (if (symbolp ,form-variable)
                      (node* (:block-name :name ,form-variable))
                      ,form-variable)))
       ,@(loop for form-ast-or-list in form-asts
               collect
               `(* :form
                   (let ((,form-variable ,form-ast-or-list))
                     (if (listp ,form-variable)
                         ,form-variable
                         (list ,form-variable))))))))

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

(defun alet-or-alet* (which-one binding-ast-forms body-ast-forms)
  `(flet ((b (x y)
            (make-let-binding-ast x y)))
     (declare (ignorable (function b)))
     (node* (,which-one)
       ,@(loop for binding-ast-form in binding-ast-forms
               collect
               (let ((form-variable (gensym)))
                 `(* :binding
                     (let ((,form-variable ,binding-ast-form))
                       (if (listp ,form-variable)
                           ,form-variable
                           (list ,form-variable))))))
       ,@(loop for body-ast-form in body-ast-forms
               collect
               (let ((form-variable (gensym)))
                 `(* :form
                     (let ((,form-variable ,body-ast-form))
                       (cond ((null ,form-variable)
                              '())
                             ((consp ,form-variable)
                              (if (typep (car ,form-variable)
                                         'ico:declaration-ast)
                                  '()
                                  ,form-variable))
                             ((typep ,form-variable 'ico:declaration-ast)
                              '())
                             (t
                              (list ,form-variable))))))
               collect
               (let ((form-variable (gensym)))
                 `(* :declaration
                     (let ((,form-variable ,body-ast-form))
                       (cond ((null ,form-variable)
                              '())
                             ((consp ,form-variable)
                              (if (typep (car ,form-variable)
                                         'ico:declaration-ast)
                                  ,form-variable
                                  '()))
                             ((typep ,form-variable 'ico:declaration-ast)
                              (list ,form-variable))
                             (t
                              '())))))))))

(defmacro alet (binding-ast-forms &body body-ast-forms)
  (alet-or-alet* :LET binding-ast-forms body-ast-forms))

(defmacro alet* (binding-ast-forms &body body-ast-forms)
  (alet-or-alet* :LET* binding-ast-forms body-ast-forms))

(defmacro aprogn (&rest body-ast-forms)
  `(node* (:progn)
     ,@(loop for body-ast-form in body-ast-forms
             collect
             (let ((form-variable (gensym)))
               `(* :form
                   (let ((,form-variable ,body-ast-form))
                     (if (listp ,form-variable)
                         ,form-variable
                         (list ,form-variable))))))))

(defmacro awhen (test-ast-form &rest body-ast-forms)
  `(node* (:when)
     (1 :test ,test-ast-form)
     ,@(loop for body-ast-form in body-ast-forms
             collect
             (let ((form-variable (gensym)))
               `(* :form
                   (let ((,form-variable ,body-ast-form))
                     (if (listp ,form-variable)
                         ,form-variable
                         (list ,form-variable))))))))

(defmacro aif (test-ast-form then-ast-form else-ast-form)
  `(node* (:if)
     (1 :test ,test-ast-form)
     (1 :then ,then-ast-form)
     (1 :else ,else-ast-form)))

(defmacro application (function-name-ast-form &rest argument-ast-forms)
  `(node* (:application)
     (1 :function-name
        ,(let ((function-name-variable (gensym)))
           `(let ((,function-name-variable ,function-name-ast-form))
              (if (symbolp ,function-name-variable)
                  (make-function-name-ast ,function-name-variable)
                  ,function-name-variable))))
     ,@(loop for argument-ast-form in argument-ast-forms
             collect `(1 :argument ,argument-ast-form))))

(defun atagbody (&rest items)
  (let ((tagbody-ast (make-instance 'ico:tagbody-ast))
        (current-segment-ast nil))
    (loop for item in items
          do (if (typep item 'ico:tag-ast)
                 (progn
                   ;; We need to start a new segment AST.  So if we
                   ;; have a current segment AST then add it to the
                   ;; TAGBODY AST.
                   (unless (null current-segment-ast)
                     (reinitialize-instance tagbody-ast
                       :segment-asts
                       (append (ico:segment-asts tagbody-ast)
                               (list current-segment-ast))))
                   ;; Then start a new segment AST
                   (setf current-segment-ast
                         (make-instance 'ico:tagbody-segment-ast
                           :tag-ast item)))
                 (progn
                   ;; If the item is not a tag, then we must first
                   ;; make sure that we do have a current segment.  It
                   ;; is possible that we don't have one if the first
                   ;; item in the tagbody is a statement.
                   (when (null current-segment-ast)
                     (setf current-segment-ast
                           (make-instance 'ico:tagbody-segment-ast)))
                   (if (listp item)
                       ;; We have a list of statements, so append them
                       ;; to the current segment AST.
                       (reinitialize-instance current-segment-ast
                         :statement-asts 
                         (append (ico:statement-asts current-segment-ast)
                                 item))
                       ;; We have a single statement.
                       (reinitialize-instance current-segment-ast
                         :statement-asts
                         (append (ico:statement-asts current-segment-ast)
                          (list item)))))))
    ;; We are out of items.  If there is a current segment, it needs
    ;; to be added to the TAGBODY-AST.
    (unless (null current-segment-ast)
      (reinitialize-instance tagbody-ast
        :segment-asts
        (append (ico:segment-asts tagbody-ast)
                (list current-segment-ast))))
    tagbody-ast))

(defun ago (tag-ast)
  (node* (:go) (1 :tag tag-ast)))

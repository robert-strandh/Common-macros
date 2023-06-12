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

;;; Utilities for taking apart Iconoclast lambda lists.


(defun generate-required (required-section-ast)
  (loop for ast in (ico:parameter-asts required-section-ast)
        collect (ico:name (ico:name-ast ast))))

(defun generate-optional (optional-section-ast)
  (if (null optional-section-ast)
      '()
      (cons '&optional
            (loop for ast in (ico:parameter-asts optional-section-ast)
                  for name = (ico:name (ico:name-ast ast))
                  for init-form-ast = (ico:init-form-ast ast)
                  for supplied-p-ast = (ico:supplied-p-parameter-ast ast)
                  collect
                  (append (list name)
                          (if (null init-form-ast)
                              '()
                              (list (ico:form init-form-ast)))
                          (if (null supplied-p-ast)
                              '()
                              (list (ico:name supplied-p-ast))))))))

(defun generate-rest (rest-section-ast)
  (if (null rest-section-ast)
      '()
      (cons '&rest (ico:name (ico:parameter-ast rest-section-ast)))))

(defun generate-key (key-section-ast)
  (if (null key-section-ast)
      '()
      (cons '&key
            (loop for ast in (ico:parameter-asts key-section-ast)
                  for name = (ico:name (ico:name-ast ast))
                  for keyword-name-ast = (ico:keyword-name-ast ast)
                  for init-form-ast = (ico:init-form-ast ast)
                  for supplied-p-ast = (ico:supplied-p-parameter-ast ast)
                  collect
                  (cons (if (null keyword-name-ast)
                            name
                            `(,(ico:name keyword-name-ast) ,name))
                        (if (null supplied-p-ast)
                            (if (null init-form-ast)
                                name
                                `(,name ,(ico:form init-form-ast)))
                            `(,name
                              ,(ico:form init-form-ast)
                              ,(ico:name supplied-p-ast))))))))
          
(defun generate-aux (aux-section-ast)
  (if (null aux-section-ast)
      '()
      (cons '&aux
            (loop for ast in (ico:parameter-asts aux-section-ast)
                  for name = (ico:name (ico:name-ast ast))
                  for init-form-ast = (ico:init-form-ast ast)
                  collect
                  (cons name
                        (if (null init-form-ast)
                            '()
                            (list (ico:form init-form-ast))))))))

(defun generate-ordinary-lambda-list (lambda-list-ast)
  (append (generate-required (ico:required-section-ast lambda-list-ast))
          (generate-optional (ico:optional-section-ast lambda-list-ast))
          (generate-rest (ico:rest-section-ast lambda-list-ast))
          (generate-key (ico:key-section-ast lambda-list-ast))
          (generate-aux (ico:aux-section-ast lambda-list-ast))))

(defun generate-generic-function-lambda-list (lambda-list-ast)
  (append (generate-required (ico:required-section-ast lambda-list-ast))
          (generate-optional (ico:optional-section-ast lambda-list-ast))
          (generate-rest (ico:rest-section-ast lambda-list-ast))
          (generate-key (ico:key-section-ast lambda-list-ast))))


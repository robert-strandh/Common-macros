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

(defun make-let-binding-ast (origin name-ast value-ast)
  (abp:node* (:value-binding :source origin)
    (1 :name name-ast)
    (1 :value value-ast)))

(defun make-eval-when-situation-asts (origin &rest situations)
  (loop for situation in situations
        collect (abp:node* (:eval-when-situation
                            :source origin
                            :situation situation))))

(defun make-quote-ast (origin object)
  (abp:node* (:quote)
    (1 :object
       (abp:node* (:literal :source origin :value object)))))

(defun wrap-in-block-ast (origin block-name form-asts)
  (abp:node* (:block :source origin)
    (1 :name
       (abp:node* (:block-name :source origin :name block-name)))
    (* :form form-asts)))

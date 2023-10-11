(cl:in-package #:common-macro-definitions)

(defparameter *operator-table*
  (make-hash-table :test #'eq))

(defun macro-function-exists-p (operator)
  (nth-value 1 (gethash operator *operator-table*)))

(defun transform-name (name)
  (let ((result (gethash name *operator-table*)))
    (when (null result)
      (setq result (intern (string-downcase (symbol-name name))))
      (setf (gethash name *operator-table* ) result))
    result))

(cl:defmacro defmacro (name lambda-list &body body)
  `(cl:defmacro ,(transform-name name) ,lambda-list ,@body))

(defun macroexpand-1 (form &optional environment)
  (declare (ignore environment))
  (cl:macroexpand-1 (cons (gethash (first form) *operator-table*)
                          (rest form))))

(defun get-setf-expansion (place &optional environment)
  (declare (ignore environment))
  (if (symbolp place)
      (let ((store-variable (gensym)))
        (values '()
                '()
                (list store-variable)
                `(setq ,place ,store-variable)
                place))
      (let ((temporaries (loop repeat (length (rest place))
                               collect (gensym)))
            (store-variable (gensym)))
        (values temporaries
                (rest place)
                (list store-variable)
                `(funcall #'(setf ,(first place))
                          ,store-variable
                          ,@temporaries)
                `(,(first place) ,@temporaries)))))

(defparameter *client* nil)

(defun proper-list-p (object)
  (numberp (ignore-errors (list-length object))))

;;; This definition relies on the fact that LIST-LENGTH returns a
;;; single value when it does not signal an error.  So in that case,
;;; IGNORE-ERRORS returns a single value, and the value of the
;;; variable SECOND is thus NIL.  And if OBJECT is a circular list,
;;; then the only value returned by LIST-LENGTH is NIL, so that is
;;; also the value of FIRST.  If LIST-LENGTH does not signal an error
;;; and does not return NIL, then OBJECT is a proper list, so
;;; CIRCULAR-LIST-P returns NIL.  If LIST-LENGTH is given anything
;;; other than a circular list or a proper list, it signals an error,
;;; so then IGNORE-ERRORS returns two values, NIL and a condition.  In
;;; this case, SECOND is not NIL, so NIL is returned from
;;; CIRCULAR-LIST-P.
(defun circular-list-p (object)
  (and (consp object)
       (multiple-value-bind (first second)
           (ignore-errors (list-length object))
         (and (null first) (null second)))))

(defun dotted-list-p (object)
  (and (consp object)
       (not (proper-list-p object))
       (not (circular-list-p object))))

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
  (let ((pos (position-if-not (lambda (item)
                                (and (consp item)
                                     (eq (car item) 'declare)))
                              body)))
    (if (null pos)
        (values body '())
        (values (subseq body 0 pos) (subseq body pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate a body such as a defun, flet, or lables that may contain
;;; both declarations and a documentation string into the
;;; declarations, the documentation, and the executable forms.
;;;
;;; Return three values.  The first value is a list of declarations.
;;; Each element of the list is a complete declaration, including the
;;; symbol DECLARE.  The second value is a the documentation as a
;;; string, or NIL if no documentation was found.  The last value is a
;;; list of forms.

(defun separate-function-body (body)
  (let ((declarations '())
        (documentation nil)
        (forms '()))
    (loop for (expr . rest) on body
          do (cond ((not (null forms))
                    (push expr forms))
                   ((and (consp expr) (eq (car expr) 'declare))
                    (push expr declarations))
                   ((stringp expr)
                    (if (or (null rest)
                            (not (null documentation))
                            (not (null forms)))
                        (push expr forms)
                        (setf documentation expr)))
                   (t
                    (push expr forms))))
    (values (nreverse declarations) documentation (nreverse forms))))

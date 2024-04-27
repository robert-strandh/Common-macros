(cl:in-package #:common-macro-definitions)

(defun check-defgeneric-options-and-methods (options-and-methods)
  (loop for option-or-method in options-and-methods
        do (unless (consp option-or-method)
             (error 'option-or-method-must-be-non-empty-list
                    :expression option-or-method))
           (unless (member (first option-or-method)
                           '(:argument-precedence-order
                             declare
                             :documentation
                             :method-combination
                             :generic-function-class
                             :method-class
                             :method))
             ;; FIXME: Define signal type.
             (error 'unknown-defgeneric-option
                    :option option-or-method))))

(defun separate-options-and-methods (options-and-methods)
  (values (remove :method options-and-methods
                  :key #'car :test #'eq)
          (remove :method options-and-methods
                  :key #'car :test-not #'eq)))

(defgeneric defgeneric-compile-time-action
    (client
     name
     lambda-list
     argument-precedence-order
     generic-function-class-name
     method-class-name
     method-combination-name
     method-combination-arguments
     documentation-option
     environment))

(defgeneric ensure-generic-function
    (client
     name
     lambda-list
     argument-precedence-order
     generic-function-class-name
     method-class-name
     method-combination-name
     method-combination-arguments
     documentation-option
     environment))

;;; FIXME: We handle the :METHOD option by expanding to a DEFMETHOD,
;;; but that is not quite right.  We need to store these methods in a
;;; slot of the generic function so that we can remove them when the
;;; DEFGENERIC form is reevaluated.

(defun expand-defgeneric
    (environment name lambda-list options-and-methods compile-time-action)
  (check-defgeneric-options-and-methods options-and-methods)
  (multiple-value-bind (options methods)
      (separate-options-and-methods options-and-methods)
    (let* ((method-combination-option
             (assoc :method-combination options))
           (method-combination-name
             (if (null method-combination-option)
                 'standard
                 (second method-combination-option)))
           (method-combination-arguments
             (if (null method-combination-option)
                 '()
                 (rest (rest method-combination-option))))
           (argument-precedence-order
             (assoc :argument-precedence-order options))
           (generic-function-class-option
             (assoc :generic-function-class options))
           (generic-function-class-name
             (if (null generic-function-class-option)
                 'standard-generic-function
                 (second generic-function-class-option)))
           (method-class-option
             (assoc :method-class options))
           (method-class-name
             (if (null method-class-option)
                 'standard-method
                 (second method-class-option)))
           (documentation-option
             (assoc :documentation options)))
      `(progn
         (eval-when (:compile-toplevel)
           ,(funcall compile-time-action
                     name
                     lambda-list
                     argument-precedence-order
                     generic-function-class-name
                     method-class-name
                     method-combination-name
                     method-combination-arguments
                     documentation-option))
         (eval-when (:load-toplevel :execute)
           (let ((result 
                   ,(ensure-generic-function
                     *client*
                     name
                     lambda-list
                     argument-precedence-order
                     generic-function-class-name
                     method-class-name
                     method-combination-name
                     method-combination-arguments
                     documentation-option
                     environment)))
             ,@(loop for method in methods
                     collect `(defmethod ,name ,@(rest method)))
             result))))))

(defmacro defgeneric
    (&environment environment name lambda-list &rest options-and-methods)
  (expand-defgeneric
   environment name lambda-list options-and-methods
   (lambda
       (name
        lambda-list
        argument-precedence-order
        generic-function-class-name
        method-class-name
        method-combination-name
        method-combination-arguments
        documentation-option)
     (defgeneric-compile-time-action
       *client*
       name
       lambda-list
       argument-precedence-order
       generic-function-class-name
       method-class-name
       method-combination-name
       method-combination-arguments
       documentation-option
       environment))))

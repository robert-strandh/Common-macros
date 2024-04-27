(cl:in-package #:common-macro-definitions)

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

(defmacro defgeneric
    (&environment environment name lambda-list &rest options-and-methods)
  (expand-defgeneric
   name lambda-list options-and-methods
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
       environment))
   (lambda
       (name
        lambda-list
        argument-precedence-order
        generic-function-class-name
        method-class-name
        method-combination-name
        method-combination-arguments
        documentation-option)
     (ensure-generic-function
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

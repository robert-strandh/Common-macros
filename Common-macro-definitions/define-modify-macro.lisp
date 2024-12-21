(cl:in-package #:common-macro-definitions)

(defmacro define-modify-macro
    (name lambda-list function &optional documentation)
  (let* ((canonicalized-lambda-list
           (ecclesia:canonicalize-define-modify-macro-lambda-list
            lambda-list))
         (required
           (ecclesia:extract-required
            canonicalized-lambda-list))
         (optionals
           (ecclesia:extract-named-group
            canonicalized-lambda-list '&optional))
         (rest
           (ecclesia:extract-named-group
            canonicalized-lambda-list '&rest))
         (place-var (gensym)))
    `(cl:defmacro ,name (,place-var ,@lambda-list)
       ,@(if (null documentation) '() (list documentation))
       (let ((argument-forms
               (list* ,@required
                      ,@(if (null optionals)
                            '()
                            (mapcar #'first (rest optionals)))
                      ,(if (null rest)
                           '()
                           (second rest)))))
         (multiple-value-bind
               (vars vals store-vars writer-form reader-form)
             (cl:get-setf-expansion ,place-var)
           ;; This last part would look more "natural" if written
           ;; using the backquote facility, but the problem is that
           ;; the DEFINE-MODIFY-MACRO macro code is read by some
           ;; standard reader R, but the expansion might be processed
           ;; by a different compiler, so the expansion cannot contain
           ;; calls to the backquote macros created by R.
           (let ((bindings
                   (loop for var in vars
                         for val in vals
                         collect (list var val)))
                 (binding
                   (list (first store-vars)
                         (list* ',function reader-form argument-forms))))
             (list 'let
                   (append bindings (list binding))
                   writer-form)))))))

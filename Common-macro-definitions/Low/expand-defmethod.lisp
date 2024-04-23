(cl:in-package #:common-macro-definitions)

(defun expand-defmethod
    (function-name rest make-method-lambda-wrapper ensure-method-wrapper)
  (let ((unspecialized-token (gensym)))
    (multiple-value-bind
          (qualifiers required remaining specializers
           declarations documentation forms)
        (parse-defmethod rest unspecialized-token)
      (let ((lambda-list (append required remaining))
            (ignorables (loop for parameter in required
                              for specializer in specializers
                              unless (eq specializer unspecialized-token)
                                collect parameter)))
        (let ((method-lambda
                (funcall make-method-lambda-wrapper
                         `(lambda ,lambda-list
                            ,@declarations
                            (declare (ignorable ,@ignorables))
                            (block ,(if (consp function-name)
                                        (second function-name)
                                        function-name)
                              ,@forms)))))
          (funcall ensure-method-wrapper
                   function-name lambda-list qualifiers
                   (subst 't unspecialized-token specializers)
                   documentation method-lambda))))))

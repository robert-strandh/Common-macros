(cl:in-package #:common-macro-definitions)

;;; Split a specialized lambda list into the required part and the
;;; remaining part.
(defun split-specialized-lambda-list (lambda-list)
  (let ((position (position-if (lambda (item)
                                 (member item lambda-list-keywords))
                               lambda-list)))
    (if (null position)
        (values lambda-list '())
        (values (subseq lambda-list 0 position)
                (subseq lambda-list position)))))

(defun extract-required-parameters (required-part)
  (loop for parameter in required-part
        collect (if (symbolp parameter) parameter (first parameter))))

(defun extract-specializers (required-part unspecialized-token)
  (loop for parameter in required-part
        collect (if (symbolp parameter)
                    unspecialized-token
                    (second parameter))))

(defun parse-defmethod (all-but-name unspecialized-token)
  (let* ((lambda-list-position (position-if #'listp all-but-name))
         (qualifiers (subseq all-but-name 0 lambda-list-position))
         (lambda-list (elt all-but-name lambda-list-position))
         (body (subseq all-but-name (1+ lambda-list-position))))
    (multiple-value-bind (required-part remaining-part)
        (split-specialized-lambda-list lambda-list)
      (multiple-value-bind (declarations documentation forms)
          (ecc:separate-function-body body)
        (values qualifiers
                (extract-required-parameters required-part)
                remaining-part
                (extract-specializers required-part unspecialized-token)
                declarations
                documentation
                forms)))))

(defgeneric make-method-lambda-wrapper (client))

(defgeneric ensure-method-wrapper (client))

(defmacro defmethod (function-name &rest rest)
  (expand-defmethod
   function-name rest
   (make-method-lambda-wrapper *client*)
   (ensure-method-wrapper *client*)))

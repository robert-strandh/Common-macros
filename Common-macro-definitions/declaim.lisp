(cl:in-package #:common-macro-definitions)

;;; It's possible to want something other than simple CL:PROCLAIM in the expansion;
;;; for example SBCL expands into (%proclaim whatever (sb-c:source-location)).
;;; So we have this hook but just use CL:PROCLAIM by default.
(defgeneric proclaim (client declaration-specifier environment)
  (:method (client declaration-specifier environment)
    (declare (ignore client environment))
    `(cl:proclaim ',declaration-specifier)))

(defmacro declaim (&environment environment &rest declaration-specifiers)
  (expand-declaim
   declaration-specifiers
   (lambda (declaration-specifier)
     (proclaim *client* declaration-specifier environment))))

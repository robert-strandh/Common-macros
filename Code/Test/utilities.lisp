(cl:in-package #:common-macros-test)

(defun expand-expression (expression)
  (let* ((builder (make-instance 'bld:builder))
         (ast (ses:parse builder t expression))
         (expanded-ast (cm::expand nil ast nil)))
    (ses:unparse builder t expanded-ast)))


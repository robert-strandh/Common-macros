(cl:in-package #:common-macros)

(defmacro cmd:declaim (&rest declaration-specifiers)
  `(progn ,@(mapcar (lambda (declaration-specifier)
                      `(proclaim ',declaration-specifier))
                    declaration-specifiers)))

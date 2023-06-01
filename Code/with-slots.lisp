(cl:in-package #:common-macros)

(defmacro cmd:with-slots ((slot-entries) instance-form &body forms)
  (let ((instance-var (gensym)))
    `(let ((,instance-var ,instance-form))
       (symbol-macrolet
           ,(loop for entry in slot-entries
                  collect
                  (if (symbolp entry)
                      `(,entry
                        (slot-value ,instance-var ,entry))
                      `(,(first entry)
                        (slot-value ,instance-var ,(second entry)))))
         ,@forms))))

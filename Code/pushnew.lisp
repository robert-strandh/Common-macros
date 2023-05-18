(cl:in-package #:common-macros)

(defmacro cmd:pushnew
    (item place
     &environment environment
     &key
       (key nil key-supplied-p)
       (test nil test-supplied-p)
       (test-not nil test-not-supplied-p))
  (if (and test-supplied-p test-not-supplied-p)
      (progn (warn 'warn-both-test-and-test-not-given)
             `(error 'both-test-and-test-not-given))
      (multiple-value-bind (vars vals store-vars writer-form reader-form)
          (get-setf-expansion place environment)
        `(let (,@(mapcar #'list vars vals))
           (let ((,(car store-vars)
                   (adjoin-core ,item ,reader-form
                                ,key ,key-supplied-p
                                ,test ,test-supplied-p
                                ,test-not ,test-not-supplied-p)))
             ,writer-form)))))

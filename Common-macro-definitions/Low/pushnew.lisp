(cl:in-package #:common-macro-definitions)

;;; Translate from a keyword to a variable name
(defparameter *vars* '((:start . start)
                       (:end . end)
                       (:from-end . from-end)
                       (:key . key)
                       (:test . test)
                       (:test-not . test-not)
                       (:count . count)))

;;; For a list with alternating keywords, and expressions,
;;; generate a list of binding for let.  For instance,
;;; if we have (:key <x> :end <y> :start <z>), we generate
;;; ((key <x>) (end <y>) (start <z>)).  If a keyword occurs
;;; more than once in the list, generate a binding with a
;;; generated symbol instead.
(defun make-bindings (plist)
  (loop with keywords = '()
        for (key value) on plist by #'cddr
        collect (list (if (member key keywords)
                          (gensym)
                          (or (cdr (assoc key *vars*)) (gensym)))
                      value)
        do (push key keywords)))

(defmacro pushnew
    (item place
     &environment environment
     &rest args
     &key
       (key nil key-p)
       (test nil test-p)
       (test-not nil test-not-p))
  (declare (ignorable test test-not))
  (if (and test-p test-not-p)
      (progn (warn 'warn-both-test-and-test-not-given)
             `(error 'both-test-and-test-not-given))
      (let ((item-var (gensym)))
        (multiple-value-bind (vars vals store-vars writer-form reader-form)
            (get-setf-expansion place environment)
          `(let ((,item-var ,item)
                 ,@(mapcar #'list vars vals)
                 ,@(make-bindings args))
             ,@(if key-p `((declare (ignorable key))) `())
             (let ((,(car store-vars) ,reader-form))
               ,(if key
                    (if test-p
                        `(unless (member (funcall key ,item-var) ,(car store-vars)
                                         :test test :key key)
                           (push ,item-var ,(car store-vars)))
                        (if test-not-p
                            `(unless (member (funcall key ,item-var) ,(car store-vars)
                                             :test-not test-not :key key)
                               (push ,item-var ,(car store-vars)))
                            `(unless (member (funcall key ,item-var) ,(car store-vars)
                                             :key key)
                               (push ,item-var ,(car store-vars)))))
                    (if test-p
                        `(unless (member ,item-var ,(car store-vars)
                                         :test test)
                           (push ,item-var ,(car store-vars)))
                        (if test-not-p
                            `(unless (member ,item-var ,(car store-vars)
                                         :test-not test-not)
                               (push ,item-var ,(car store-vars)))
                            `(unless (member ,item-var ,(car store-vars))
                               (push ,item-var ,(car store-vars))))))
               ,writer-form))))))

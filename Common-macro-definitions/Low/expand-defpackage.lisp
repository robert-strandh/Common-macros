(cl:in-package #:common-macro-definitions)

(deftype string-designator ()
  `(or string symbol character))

(deftype package-designator ()
  `(or package string-designator))

(defun check-defpackage-option (option)
  (unless (consp option)
    (error 'defpackage-option-must-be-a-non-empty-list
           :option option))
  (unless (ecc:proper-list-p option)
    (error 'defpackage-option-must-be-a-proper-list
           :option option))
  (unless (member (first option)
                  '(:nicknames :documentation :use :shadow :import :export
                    :shadowing-import-from :import-from :intern :size
                    :local-nicknames))
    (error 'unknown-defpackage-option-name
           :option-name (first option)))
  (destructuring-bind (option-name . arguments)
      option
    (case option-name
      (:nicknames
       (loop for argument in arguments
             unless (typep argument 'string-designator)
               do (error 'package-nickname-must-be-a-string-designator
                         :nickname argument)))
      (:local-nicknames
       (loop for argument in arguments
             unless (consp argument)
               do (error 'package-local-nickname-argument-must-be-cons
                         :argument argument)
             unless (ecc:proper-list-p argument)
               do (error 'package-local-nickname-argument-must-be-proper-list
                         :argument argument)
             unless (= (length argument) 2)
               do (error 'package-local-nickname-argument-must-have-length-2
                         :argument argument)
             unless (typep (first argument) 'string-designator)
               do (error 'package-local-nickname-must-be-a-string-designator
                         :nickname (first argument))
             unless (typep (second argument) 'package-designator)
               do (error 'package-local-nickname-package-must-be-a-package-designator
                         :package-name (second argument))))
      (:documentation
       (when (null arguments)
         (error 'package-documentation-option-requires-an-argument
                :option option))
       (unless (null (rest arguments))
         (error 'package-documentation-option-requres-a-single-argument
                :arguments arguments))
       (unless (stringp (first arguments))
         (error 'package-documentation-must-be-a-string
                :documentation (first arguments))))
      (:use
       (loop for argument in arguments
             unless (typep argument 'package-designator)
               do (error 'package-use-argument-must-be-a-package-designator
                         :package-name argument)))
      (:shadow
       (loop for argument in arguments
             unless (typep argument 'string-designator)
               do (error 'shadowed-symbol-name-must-be-a-string-designator
                         :symbol-name argument)))
      (:shadowing-import-from
       (unless (consp arguments)
         (error 'shadowing-import-from-option-must-have-a-package-argument
                :option option))
       (unless (typep (first arguments) 'package-designator)
         (error 'shadowing-import-from-package-name-must-be-a-package-designator
                :package-name (first arguments)))
       (loop for argument in (rest arguments)
             unless (typep argument 'string-designator)
               do (error 'shadowed-symbol-must-be-a-string-designator
                         :symbol-name argument)))
       (:import-from
       (unless (consp arguments)
         (error 'import-from-option-must-have-a-package-argument
                :option option))
       (unless (typep (first arguments) 'package-designator)
         (error 'import-from-package-name-must-be-a-package-designator
                :package-name (first arguments)))
       (loop for argument in (rest arguments)
             unless (typep argument 'string-designator)
               do (error 'imported-symbol-must-be-a-string-designator
                         :symbol-name argument)))
      (:export
       (loop for argument in arguments
             unless (typep argument 'string-designator)
               do (error 'exported-symbol-name-must-be-a-string-designator
                         :symbol-name argument)))
      (:intern
       (loop for argument in arguments
             unless (typep argument 'string-designator)
               do (error 'interned-symbol-name-must-be-a-string-designator
                         :symbol-name argument)))
      (:size
       (when (null arguments)
         (error 'package-size-option-requires-an-argument
                :option option))
       (unless (null (rest arguments))
         (error 'package-size-option-requres-a-single-argument
                :arguments arguments))
       (unless (integerp (first arguments))
         (error 'package-size-must-be-a-string
                :size (first arguments)))))))

(defun check-defpackage-options (options)
  ;; We start by checking that the contents of each option is valid in
  ;; that it is well formed, that it is a valid option name, and that
  ;; the option arguments are valid for that kind of option type.
  (loop for option in options
        do (check-defpackage-option option))
  ;; Next, we check that the restrictions on the number of options of
  ;; a certain type are respected.
  (when (> (count :documentation options :key #'car) 1)
    (error 'package-documentation-option-may-occur-at-most-once
           :options options))
  (when (> (count :size options :key #'car) 1)
    (error 'package-size-option-may-occur-at-most-once
           :options options)))

;;; For options that can occur more than once, return a list of all
;;; the arguments of all the occurrences.
(defun group-options (name options)
  (loop for (option-name . arguments) in options
        when (eq name option-name)
          append arguments))

(defun gather-nicknames (options)
  (mapcar #'string
          (group-options :nicknames options)))

(defun make-shadowing-imports (options package-var)
  (loop for (option-name . arguments) in options
        when (eq option-name :shadowing-import-from)
          collect (let ((from-package-var (gensym)))
                    `(let ((,from-package-var (find-package ,(string (first arguments)))))
                       (shadowing-import
                        (list ,@(loop for symbol-name in (rest arguments)
                                      collect `(find-symbol ,(string symbol-name)
                                                            ,from-package-var)))
                        ,package-var)))))

(defun make-imports (options package-var)
  (loop for (option-name . arguments) in options
        when (eq option-name :import-from)
          collect (let ((from-package-var (gensym)))
                    `(let ((,from-package-var (find-package ,(string (first arguments)))))
                       (import
                        (list ,@(loop for symbol-name in (rest arguments)
                                      collect `(find-symbol ,(string symbol-name)
                                                            ,from-package-var)))
                        ,package-var)))))

(defun make-shadow (options package-var)
  `(shadow
    (list ,@(loop for symbol-name in (group-options :shadow options)
                  collect `(intern ,(string symbol-name) ,package-var)))
    ,package-var))

(defun make-use (options package-var)
  `(use-package
    (list ,@(loop for name in (group-options :use options)
                  collect `(find-package ,(string name))))
    ,package-var))

(defun make-intern (options package-var)
  (loop for name in (append (group-options :intern options)
                            (group-options :export options))
        collect `(intern ,(string name) ,package-var)))

(defun make-export (options package-var)
  (let ((export-options (group-options :export options)))
    (if (null export-options)
        'nil
        `(export
          (loop for name in ',export-options
                collect (find-symbol (string name) ,package-var))
          ,package-var))))

(defun make-local-nicknames
    (options add-local-nickname package-var)
  (let ((local-nicknames-options (group-options :local-nicknames options)))
    (loop for (nickname package-name) in local-nicknames-options
          collect `(,add-local-nickname
                    ',nickname (find-package ',package-name) ,package-var))))

(defun expand-defpackage (name options add-local-nickname)
  (check-defpackage-options options)
  (let ((package-var (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,package-var (find-package ,(string name)))
              (,package-var
                (if (null ,package-var)
                    (make-package ,(string name)
                      :nicknames ',(gather-nicknames options) :use '())
                    ,package-var)))
         ,@(make-shadowing-imports options package-var)
         ,(make-shadow options package-var)
         ,(make-use options package-var)
         ,@(make-imports options package-var)
         ,@(make-intern options package-var)
         ,(make-export options package-var)
         ,@(make-local-nicknames options add-local-nickname package-var)))))

(cl:in-package #:common-macro-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at macro-expansion time

(define-condition malformed-body (program-error)
  ((%body :initarg :body :reader body))
  (:report (lambda (condition stream)
             (format stream
                     "Expected a proper list of forms,~@
                      but the following was given instead:~@
                      ~s"
                     (body condition)))))

(define-condition malformed-cond-clauses (program-error)
  ((%clauses :initarg :clauses :reader clauses))
  (:report (lambda (condition stream)
             (format stream
                     "Expected a proper list of cond clauses,~@
                      but the following was given instead:~@
                      ~s"
                     (clauses condition)))))

(define-condition malformed-cond-clause (program-error)
  ((%clause :initarg :clause :reader clause)))

(define-condition malformed-case-clauses (program-error)
  ((%clauses :initarg :clauses :reader clauses)))

(define-condition malformed-case-clause (program-error)
  ((%clause :initarg :clause :reader clause)))

(define-condition otherwise-clause-not-last (program-error)
  ((%clauses :initarg :clauses :reader clauses)))

(define-condition malformed-keys (program-error)
  ((%keys :initarg :keys :reader keys)))

(define-condition malformed-typecase-clauses (program-error)
  ((%clauses :initarg :clauses :reader clauses)))

(define-condition malformed-typecase-clause (program-error)
  ((%clause :initarg :clause :reader clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions used at runtime

(define-condition ecase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in ecase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition ccase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in ccase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition etypecase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in etypecasecase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition ctypecase-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "No key matched in ctypecasecase expression.~@
                      Offending datum:~@
                      ~s~@
                      Offending type:~@
                     ~s"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition not-a-package-designator (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A package designator was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type '(or package character string symbol)))

(define-condition nicknames-must-be-proper-list (type-error)
  ()
  (:default-initargs :expected-type 'list)
  (:report (lambda (condition stream)
             (format stream
                     "The list of nicknames must be a proper list,~@
                      but the following was found instead:~@
                      ~s"
                     (type-error-datum condition)))))

(define-condition use-list-must-be-proper-list (type-error)
  ()
  (:default-initargs :expected-type 'list)
  (:report (lambda (condition stream)
             (format stream
                     "The list of used packages must be a proper list,~@
                      but the following was found instead:~@
                      ~s"
                     (type-error-datum condition)))))

(define-condition symbol-conflict (package-error)
  ((%conflicting-symbols
    :initarg :conflicting-symbols
    :reader conflicting-symbols)))

(define-condition symbol-is-not-accessible (package-error)
  ((%symbol :initarg :symbol :reader inaccessible-symbol)))

(define-condition class-name-must-be-non-nil-symbol
    (program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "A class name must be a non-nil symbol, but~@
                      ~s was found."
                     (name condition)))))

(define-condition readers-must-be-proper-list
    (type-error)
  ((%slot-definition :initarg :slot-definition :reader slot-definition)
   (%readers :initarg :readers :reader readers))
  (:report (lambda (condition stream)
             (format stream
                     "The keyword argument :READERS when supplied as~@
                      initialization of a slot definition, must be~@
                      a proper list, but the following was found instead:~@
                      ~s."
                     (readers condition))))
  (:default-initargs :type 'list))

(define-condition malformed-documentation-option
    (program-error)
  ((%documentation-option
    :initarg :documentation-option :reader documentation-option))
  (:report (lambda (condition stream)
             (format stream
                     "A documentation option must have the form~@
                      (:documentation <name>), but~@
                      ~s was found."
                     (documentation-option condition)))))

(define-condition malformed-specializer
    (error)
  ((%specializer :initarg :specializer :reader specializer))
  (:report (lambda (condition stream)
             (format stream
                     "A specializer must be either a class name~@
                      or an EQL specializer, but the following was found:~%
                      ~s"
                     (specializer condition)))))

(define-condition option-or-method-must-be-non-empty-list
    (error)
  ((%expression :initarg :expression :reader expression))
  (:report (lambda (condition stream)
             (format stream
                     "Option or method must be a non-empty list,~@
                      but the following expression was found instead~@
                      ~s"
                     (expression condition)))))

(define-condition direct-default-initargs-must-be-a-proper-list
    (error)
  ((%initargs :initarg :initargs :reader initargs))
  (:report (lambda (condition stream)
             (format stream
                     "The list of direct default initargs must be~@
                      a proper list, but the following was found:~@
                      ~s"
                     (initargs condition)))))

(define-condition direct-default-initarg-must-be-a-proper-list
    (error)
  ((%initarg :initarg :initarg :reader initarg))
  (:report (lambda (condition stream)
             (format stream
                     "A direct default initarg must be a proper~@
                      list, but the following was found:~@
                      ~s"
                     (initarg condition)))))

(define-condition direct-default-initarg-must-be-a-list-of-three-elements
    (error)
  ((%initarg :initarg :initarg :reader initarg))
  (:report (lambda (condition stream)
             (format stream
                     "A direct default initarg must be a list of~@
                      three elements, but the following was found:~@
                      ~s"
                     (initarg condition)))))

(define-condition name-of-direct-default-initarg-must-be-a-symbol
    (error)
  ((%initarg :initarg :initarg :reader initarg)
   (%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "The name of a direct default initarg must be a~@
                      symbol, but the following was found:~@
                      ~s"
                     (name condition)))))

(define-condition direct-superclasses-must-be-proper-list
    (error)
  ((%superclasses :initarg :superclasses :reader superclasses))
  (:report (lambda (condition stream)
             (format stream
                     "The direct superclasses of a class must be a~@
                      proper list, but the following was found instead~@
                      ~s"
                     (superclasses condition)))))

(define-condition direct-slots-must-be-proper-list
    (error)
  ((%direct-slots :initarg :direct-slots :reader direct-slots))
  (:report (lambda (condition stream)
             (format stream
                     "The direct slots must be a proper list,~@
                      but the following was found instead~@
                      ~s"
                     (direct-slots condition)))))

(define-condition qualifier-must-be-non-nil-atom
    (error)
  ((%qualifier :initarg :qualifier :reader qualifier))
  (:report (lambda (condition stream)
             (format stream
                     "A qualifier must be a non-nil atom,~@
                      but the following was found instead:~@
                      ~s"
                     (qualifier condition)))))

(define-condition multiple-allocation-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :ALLOCATION options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition multiple-documentation-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :DOCUMENTATION options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition multiple-initform-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :INITFORM options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition multiple-type-options-not-permitted (error)
  ((%slot-specifier :initarg :slot-specifier :reader slot-specifier))
  (:report (lambda (condition stream)
             (format stream
                     "A slot can not have multiple :TYPE options.~@
                      ~s was found."
                     (slot-specifier condition)))))

(define-condition slot-documentation-option-must-be-string (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The :DOCUMENTATION option of a slot~@
                      must have a string argument, but~@
                      ~s was found."
                     (type-error-datum condition))))
  (:default-initargs :type 'string))

(define-condition superclass-list-must-be-proper-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "The list of superclasses must be a proper list,~@
                      but ~s was found."
                     (type-error-datum condition))))
  (:default-initargs :type 'list))

(define-condition metaclass-option-once (program-error)
  ((%options :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "The metaclass option can appear only once in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition slot-options-must-be-even (program-error)
  ((%options :initarg :options :reader options))
  (:report (lambda (condition stream)
             (format stream
                     "There must be an even number of elements in.~@
                      the list of slot options.~@
                      ~s was found."
                     (options condition)))))

(define-condition malformed-metaclass-option (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed :METACLASS option:~@
                      ~s was found."
                     (option condition)))))

(define-condition malformed-default-initargs-option (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed :DEFAULT-INITARGS option:~@
                      ~s was found."
                     (option condition)))))

(define-condition malformed-slot-spec (program-error)
  ((%slot-spec :initarg :slot-spec :reader slot-spec))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed slot specification.~@
                      ~s was found."
                     (slot-spec condition)))))

(define-condition documentation-option-once (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "The documentation option can appear only once in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition default-initargs-option-once (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "The default-initargs option can appear only once in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition duplicate-class-option-not-allowed (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "This class option can occur at most once, in the~@
                      list of class options, but a second such option:~@
                      ~s was found."
                     (option condition)))))

(define-condition class-option-name-must-be-symbol (program-error)
  ((%option-name :initarg :option-name :reader option-name))
  (:report (lambda (condition stream)
             (format stream
                     "A class option name must be a symbol, but~@
                      ~s was found."
                     (option-name condition)))))

(define-condition :class-option-must-be-non-empty-list (program-error)
  ((%option :initarg :option :reader option))
  (:report (lambda (condition stream)
             (format stream
                     "A class option must be a a non-empty list, but~@
                      ~s was found."
                     (option condition)))))

(define-condition slot-option-name-must-be-symbol (program-error)
  ((%option-name :initarg :option-name :reader option-name))
  (:report (lambda (condition stream)
             (format stream
                     "A slot option name must be a symbol, but~@
                      ~s was found."
                     (option-name condition)))))

(define-condition illegal-slot-name (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name))
  (:report (lambda (condition stream)
             (format stream
                     "Illegal slot name:~@
                      ~s was found."
                     (slot-name condition)))))

(define-condition malformed-slot-list (program-error)
  ((%slot-list :initarg :slot-list :reader slot-list))
  (:report (lambda (condition stream)
             (format stream
                     "The direct-slots must be a proper list of~@
                      slot specs, but~@
                      ~s was found."
                     (slot-list condition)))))

(define-condition illegal-lambda-list-keyword (program-error)
  ((%illegal-keyword
    :initarg :illegal-keyword
    :reader illegal-keyword))
  (:report (lambda (condition stream)
             (format stream
                     "Illegal lambda-list keyword:~@
                      ~s was found."
                     (illegal-keyword condition)))))

(define-condition multiple-lambda-list-keyword (program-error)
  ((%multiple-keyword
    :initarg :multiple-keyword
    :reader multiple-keyword)
   (%number-of-occurrences
    :initarg :number-of-occurrences
    :reader number-of-occurrences))
  (:report (lambda (condition stream)
             (format stream
                     "The lambda-list keyword: ~s~@
                      occurs ~a times."
                     (multiple-keyword condition)
                     (number-of-occurrences condition)))))

;;; This condition is used to indicate that in a (non destructuring)
;;; lambda list, the required parameter must be a variable
(define-condition required-must-be-variable (program-error)
  ((%required-parameter
    :initarg :required-parameter
    :reader required-parameter))
  (:report (lambda (condition stream)
             (format stream
                     "A required parameter in this kind of lambda list~@
                      must be a variable, but the following was found:~@
                      ~s"
                     (required-parameter condition)))))

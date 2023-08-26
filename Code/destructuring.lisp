(cl:in-package #:common-macros)

;;;; This code needs to be adapted to Iconoclast.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function DESTRUCTURE-LAMBDA-LIST.
;;;
;;; Destructuring a tree according to a lambda list.
;;;
;;; The destructuring itself is typically done when a macro function
;;; is run, and the purpose is to take the macro form apart and assign
;;; parts of it to the parameters of the lambda list of the macro.
;;;
;;; The function DESTRUCTURE-LAMBDA-LIST generates the code for doing
;;; the destrucuring.  It is typically run by the expansion of
;;; DEFMACRO.  Recall that DEFMACRO must take the definition of a
;;; macro, in particular its lambda list, and generate a macro
;;; function.  The macro function takes the macro form as input and
;;; generates the expanded form.  Destructuring is done by a LET*
;;; form, and this code generates the bindings of that LET* form.
;;;
;;; It would have been more elegant to generate nested LET
;;; expressions, rather than a single LET*, because there are some
;;; arbitrary forms that need to be evaluated in between bindings, and
;;; those forms would fit more naturally into the body of a LET.  With
;;; a single LET* those forms must be part of the binding form of the
;;; LET*, and there is not always an obvious lexical variable to bind
;;; the result to.  So we must generate new variables and then ignore
;;; them in the LET* body.  But we do it this way because the DEFMACRO
;;; form may contain declarations that mention the variables in the
;;; DEFMACRO lambda list, and with nested LET expressions, some of
;;; those variables would then be introduced in a LET expression that
;;; is not the innermost one.  We could handle some such cases with
;;; LOCALLY, but IGNORE declarations result in warnings in some
;;; implementations.
;;;
;;; So, the bindings that we create will contain generated variables
;;; that are not used in the body of the macro definition, and we want
;;; them to be declared IGNORE.  For that reason,
;;; DESTRUCTURE-LAMBDA-LIST returns two values: the bindings mentioned
;;; above, and a list of variables to declare IGNORE in the beginning
;;; of the body of the macro function.
;;;
;;; The bindings return by DESTRUCTURE-LAMBDA-LIST and its subroutines
;;; are in the reverse order compared to the order it which they
;;; should appear in the expanded expression.  We do it this way in
;;; order to avoid too much consing.
;;;
;;; The lambda list is represented as an Iconoclast AST.

(defun not-enough-arguments-ast ()
  (application 'error (aquote 'too-few-arguments)))

;;; Take a VARIABLE-DEFINITION-AST and create a VARIABLE-REFERENCE-AST
;;; that references the same variable, and link the two up.  Return
;;; the newly created VARIABLE-REFERENCE-AST.
(defun make-variable-reference-ast (variable-definition-ast)
  (let ((result (make-instance 'ico:variable-reference-ast
                  :name (ico:name variable-definition-ast)
                  :variable-definition-ast variable-definition-ast)))
    (reinitialize-instance variable-definition-ast
      :variable-reference-asts
      (append (ico:variable-reference-asts variable-definition-ast)
              (list result)))
    result))

(defun add-binding-asts (variable-ast form-ast let*-ast)
  (reinitialize-instance let*-ast
    :binding-asts
    (append (ico:binding-asts let*-ast)
            (list (make-let-binding-ast variable-ast form-ast)))))

(defgeneric destructure-variable-or-pattern-ast (ast variable-ast let*-ast))

(defmethod destructure-variable-or-pattern-ast
    ((ast ico:variable-name-ast) variable-ast let*-ast)
  (add-binding-asts ast variable-ast let*-ast))

(defmethod destructure-variable-or-pattern-ast
    ((ast ico:pattern-ast) variable-ast let*-ast)
  (destructure-lambda-list ast variable-ast let*-ast))

(defun make-temp-ast ()
  (node* (:variable-name :name (gensym))))

(defgeneric destructure-section (section-ast variable-ast let*-ast))

(defmethod destructure-section
    ((section-ast null) variable-definition-ast let*-ast)
  (declare (ignore section-ast let*-ast))
  variable-definition-ast)

;;; PARAMETER-AST is the AST that represents the parameter to be
;;; destructured.  ARGUMENT-LIST-AST is a VARIABLE-DEFINITION-AST that
;;; represents a variable that refers to the remaining elements of the
;;; argument list.  LET*-AST is the AST to which new bindings should
;;; be added.
(defun destructure-required-parameter
    (parameter-ast argument-list-ast let*-ast)
  (let (;; NAME-AST is either the name of the parameter if the
        ;; parameter is not a pattern.  Otherwise, NAME-AST is an AST
        ;; that represents the pattern, i.e., a nested lambda list.
        (name-ast (ico:name-ast parameter-ast))
        ;; NEW-ARGUMENT-LIST-AST is a new VARIABLE-DEFINITION-AST that
        ;; we introduce and that will refer to the REST of the
        ;; argument list after this parameter has been destructured.
        ;; We return NEW-ARGUMENT-LIST-AST so that further
        ;; restructuring can refer to it.
        (new-argument-list-ast
          (make-instance 'ico:variable-definition-ast
            :name (gensym)))
        ;; TEMP-AST will refer to the FIRST of the argument list (when
        ;; the argument list is not empty, that is).  It is passed as
        ;; a parameter to DESTRUCTURE-VARIABLE-OR-PATTERN-AST and can
        ;; then either be used to destructure further, if the object
        ;; needs to be further destructured when NAME-AST represents a
        ;; pattern.
        (temp-ast
          (make-instance 'ico:variable-definition-ast
            :name (gensym))))
    ;; The first binding we add is is the AST version of:
    ;; (TEMP (IF (NULL ARGUMENT-LIST)
    ;;           (ERROR...)
    ;;           (FIRST ARGUMENT-LIST)))
    (add-binding-asts
     temp-ast
     (aif (application
           'null
           (make-variable-reference-ast argument-list-ast))
          (not-enough-arguments-ast)
          (application
           'first
           (make-variable-reference-ast argument-list-ast)))
     let*-ast)
    ;; Next, we add a binding that is the AST version of:
    ;; (NEW-ARGUMENT-LIST (REST ARGUMENT-LIST))
    (add-binding-asts
     new-argument-list-ast
     (application
      'rest
      (make-variable-reference-ast argument-list-ast))
     let*-ast)
    ;; Finally, we call DESTRUCTURE-VARIABLE-OR-PATTERN-AST with
    ;; TEMP-AST.  Then, if the parameter is just a name, the binding
    ;; becomes (NAME TEMP).  If it is a patter, more bindings will
    ;; be added by a recursive call to DESTRUCTURE-LAMBDA-LIST.
    (destructure-variable-or-pattern-ast name-ast temp-ast let*-ast)
    ;; And we return the new argument-list AST as promised.
    new-argument-list-ast))

(defmethod destructure-section
    ((section-ast ico:required-section-ast) argument-list-ast let*-ast)
  (let ((new-argument-list-ast argument-list-ast))
    (loop for parameter-ast in (ico:parameter-asts section-ast)
          do (setf new-argument-list-ast
                   (destructure-required-parameter
                    parameter-ast new-argument-list-ast let*-ast)))
    new-argument-list-ast))

;;; PARAMETER-AST is the AST that represents the parameter to be
;;; destructured.  ARGUMENT-LIST-AST is a VARIABLE-DEFINITION-AST that
;;; represents a variable that refers to the remaining elements of the
;;; argument list.  LET*-AST is the AST to which new bindings should
;;; be added.
(defun destructure-optional-parameter
    (parameter-ast argument-list-ast let*-ast)
  (let (;; NAME-AST is either the name of the parameter if the
        ;; parameter is not a pattern.  Otherwise, NAME-AST is an AST
        ;; that represents the pattern, i.e., a nested lambda list.
        (name-ast (ico:name-ast parameter-ast))
        ;; NEW-ARGUMENT-LIST-AST is a new VARIABLE-DEFINITION-AST that
        ;; we introduce and that will refer to the REST of the
        ;; argument list after this parameter has been destructured.
        ;; We return NEW-ARGUMENT-LIST-AST so that further
        ;; restructuring can refer to it.
        (new-argument-list-ast
          (make-instance 'ico:variable-definition-ast
            :name (gensym)))
        ;; TEMP-AST will refer to the FIRST of the argument list (when
        ;; the argument list is not empty, that is).  It is passed as
        ;; a parameter to DESTRUCTURE-VARIABLE-OR-PATTERN-AST and can
        ;; then either be used to destructure further, if the object
        ;; needs to be further destructured when NAME-AST represents a
        ;; pattern.
        (temp-ast
          (make-instance 'ico:variable-definition-ast
            :name (gensym))))
    ;; If this parameter has an associated SUPPLIED-P parameter, then
    ;; we add an AST version of the binding:
    ;; (<supplied-p> (NOT (NULL ARGUMENT-LIST)))
    (let ((supplied-p-ast (ico:supplied-p-parameter-ast parameter-ast)))
      (unless (null supplied-p-ast)
        (add-binding-asts
         supplied-p-ast
         (application
          'not
          (application
           'null
           (make-variable-reference-ast argument-list-ast)))
         let*-ast)))
    ;; Next, we add the a binding that is the AST version of
    ;; (TEMP (IF (NULL ARGUMENT-LIST)
    ;;           <init-form>
    ;;           (FIRST ARGUMENT-LIST)))
    (let ((init-form-ast (ico:init-form-ast parameter-ast)))
      (add-binding-asts
       temp-ast
       (aif (application
             'null
             (make-variable-reference-ast argument-list-ast))
            (if (null init-form-ast)
                (aliteral 'nil)
                init-form-ast)
            (application
             'first
             (make-variable-reference-ast argument-list-ast)))
       let*-ast))
    ;; Next, we add a binding that is the AST version of:
    ;; (NEW-ARGUMENT-LIST (REST ARGUMENT-LIST))
    (add-binding-asts
     new-argument-list-ast
     (application
      'rest
      (make-variable-reference-ast argument-list-ast))
     let*-ast)
    ;; Finally, we call DESTRUCTURE-VARIABLE-OR-PATTERN-AST with
    ;; TEMP-AST.  Then, if the parameter is just a name, the binding
    ;; becomes (NAME TEMP).  If it is a patter, more bindings will
    ;; be added by a recursive call to DESTRUCTURE-LAMBDA-LIST.
    (destructure-variable-or-pattern-ast name-ast temp-ast let*-ast)
    ;; And we return the new argument-list AST as promised.
    new-argument-list-ast))

(defmethod destructure-section
    ((section-ast ico:optional-section-ast) argument-list-ast let*-ast)
  (let ((new-argument-list-ast argument-list-ast))
    (loop for parameter-ast in (ico:parameter-asts section-ast)
          do (setf new-argument-list-ast
                   (destructure-optional-parameter
                    parameter-ast new-argument-list-ast let*-ast)))
    new-argument-list-ast))

(defmethod destructure-section
    ((section-ast ico:rest-section-ast) argument-list-ast let*-ast)
  (let (;; NAME-AST is either the name of the parameter if the
        ;; parameter is not a pattern.  Otherwise, NAME-AST is an AST
        ;; that represents the pattern, i.e., a nested lambda list.
        (name-ast (ico:name-ast (ico:parameter-ast section-ast)))
        ;; TEMP-AST will refer to the remaining argument list.  It is
        ;; passed as a parameter to
        ;; DESTRUCTURE-VARIABLE-OR-PATTERN-AST and can then either be
        ;; used to destructure further, if the object needs to be
        ;; further destructured when NAME-AST represents a pattern.
        (temp-ast
          (make-instance 'ico:variable-definition-ast
            :name (gensym))))
    ;; We add the AST version of the binding: (TEMP ARGUMENT-LIST).
    (add-binding-asts temp-ast argument-list-ast let*-ast)
    ;; Finally, we call DESTRUCTURE-VARIABLE-OR-PATTERN-AST with
    ;; TEMP-AST.  Then, if the parameter is just a name, the binding
    ;; becomes (NAME TEMP).  If it is a patter, more bindings will
    ;; be added by a recursive call to DESTRUCTURE-LAMBDA-LIST.
    (destructure-variable-or-pattern-ast name-ast temp-ast let*-ast)
    ;; Since we haven't manipulated the argument list at all, we
    ;; return the AST we were passed as an argument.
    argument-list-ast))

(defun collect-keys (key-parameter-asts)
  (loop for key-parameter-ast in key-parameter-asts
        collect (ico:name (ico:keyword-ast key-parameter-ast))))

;;; Add bindings to LET*-AST that check that the
;;; VARIABLE-DEFINITION-AST ARGUMENT-LIST-AST holds a list of even
;;; length, and signal an error otherwise.
(defun add-bindings-checking-even-length (argument-list-ast let*-ast)
  (let ((reference-ast (make-variable-reference-ast argument-list-ast))
        (ignore-ast (make-instance 'ico:variable-definition-ast
                      :name (gensym))))
    (add-binding-asts
     ignore-ast
     (aif (application 'oddp (application 'length reference-ast))
          (application 'error (aliteral 'odd-number-of-keyword-arguments))
          (aliteral 'nil))
     let*-ast)))

;;; Add a binding that binds a fresh lexical variable to a Boolean
;;; value that true if and only if :ALLOW-OTHER-KEYS is supplied with
;;; a true value.  Return the AST representing the definition of the
;;; fresh lexical variable.
(defun add-binding-for-allow-other-keys (argument-list-ast let*-ast)
  (let ((result-ast (make-instance 'ico:variable-definition-ast
                      :name (gensym))))
    (add-binding-asts
     result-ast
     (application
      'getf
      (aliteral ':allow-other-keys)
      (make-variable-reference-ast argument-list-ast))
     let*-ast)
    result-ast))

(defmethod destructure-section
    ((section-ast ico:key-section-ast) argument-list-ast let*-ast)
  (add-bindings-checking-even-length argument-list-ast let*-ast)
  (unless (null (ico:allow-other-keys-ast section-ast))
    (let ((ignore-ast (make-temp-ast))
          (temp-ast (make-temp-ast)))
      (add-binding-asts
       ignore-ast
       (alet ((b temp-ast variable-ast))
         (atagbody
          (atag 'again)
          (aif (application 'null temp-ast)
               (ago (atag 'out))
               (aliteral 'nil))
          (aif (application
                'not
                (application
                 'member
                 (application 'first temp-ast)
                 (aliteral (cons :allow-other-keys
                                 (collect-keys section-ast)))))
               (application 'error (aliteral 'invalid-keyword))
               (aprogn (node* (:setq)
                         (1 :name temp-ast)
                         (1 :value (application 'cddr temp-ast)))
                       (ago (atag 'again))))
          (atag 'out)))
       let*-ast)))
  (let ((unique-ast (application 'list (aliteral 'nil)))
        (temp-ast-1 (make-temp-ast))
        (temp-ast-2 (make-temp-ast)))
    (add-binding-asts temp-ast-1 unique-ast let*-ast)
    (loop for ast in (ico:parameter-asts section-ast)
          for name-ast = (ico:name-ast ast)
          for init-form-ast = (ico:init-form-ast ast)
          for supplied-p-parameter-ast = (ico:supplied-p-parameter-ast ast)
          for keyword-ast = (ico:keyword-ast ast)
          do (add-binding-asts
              temp-ast-2
              (application 'getf variable-ast keyword-ast temp-ast-1)
              let*-ast)
             (unless (null supplied-p-parameter-ast)
               (add-binding-asts
                supplied-p-parameter-ast
                (application
                 'not
                 (application 'eq temp-ast-2 temp-ast-1))
                let*-ast))
             (let ((temp-ast (make-temp-ast)))
               (add-binding-asts
                temp-ast
                (aif (application 'eq temp-ast-1 temp-ast-2)
                     init-form-ast
                     temp-ast-2)
                let*-ast)
               (destructure-variable-or-pattern-ast
                name-ast temp-ast let*-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DESTRUCTURE-LAMBDA-LIST

(defun destructure-lambda-list
    (lambda-list-ast variable-definition-ast let*-ast)
  (let ((new-variable-definition-ast variable-definition-ast))
    (setq new-variable-definition-ast
          (destructure-section
           (ico:required-section-ast lambda-list-ast)
           new-variable-definition-ast let*-ast))
    (setq new-variable-definition-ast
          (destructure-section
           (ico:optional-section-ast lambda-list-ast)
           new-variable-definition-ast let*-ast))
    (setq new-variable-definition-ast
          (destructure-section
           (ico:rest-section-ast lambda-list-ast)
           new-variable-definition-ast let*-ast))
    (setq new-variable-definition-ast
          (destructure-section
           (ico:key-section-ast lambda-list-ast)
           new-variable-definition-ast let*-ast))
    (destructure-section
     (ico:aux-section-ast lambda-list-ast)
     new-variable-definition-ast let*-ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO

(defun parse-macro (macro-ast)
  (let* ((lambda-list-ast (ico:lambda-list-ast macro-ast))
         (whole-section-ast
           (ico:whole-section-ast lambda-list-ast))
         (whole-parameter-ast
           (if (null whole-section-ast)
               (make-temp-ast)
               (ico:parameter-ast whole-section-ast)))
         (environment-section-ast
           (ico:environment-section-ast lambda-list-ast))
         (environment-parameter-ast
           (if (null environment-section-ast)
               (make-temp-ast)
               (ico:parameter-ast environment-section-ast)))
         (let*-ast (node* (:let*)
                     (1 :declaration
                        (ico:declaration-asts macro-ast))))
         (variable-ast (node* (:variable-name :name (gensym)))))
    (destructure-lambda-list lambda-list-ast variable-ast let*-ast)
    (reinitialize-instance let*-ast
      :form-asts (ico:form-asts macro-ast))
    (node* (:lambda)
      (1 :lambda-list
         (node* (:ordinary-lambda-list)
           (1 :required-section
              (node* (:required-section)
                (* :parameter
                   (list whole-parameter-ast environment-parameter-ast))))))
      (1 :form
         (ablock (ico:name-ast macro-ast)
           let*-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-COMPILER-MACRO
;;;
;;; This function differs from parse-macro only in the code that
;;; destructures the lambda list from the arguments.
;;;
;;; FIXME: Currently there is no difference.  Handle the case where
;;; the form is (FUNCALL (FUNCTION ...) ...)

(defun parse-compiler-macro (macro-ast)
  (let* ((lambda-list-ast (ico:lambda-list-ast macro-ast))
         (whole-section-ast
           (ico:whole-section-ast lambda-list-ast))
         (whole-parameter-ast
           (if (null whole-section-ast)
               (make-temp-ast)
               (ico:parameter-ast whole-section-ast)))
         (environment-section-ast
           (ico:environment-section-ast lambda-list-ast))
         (environment-parameter-ast
           (if (null environment-section-ast)
               (make-temp-ast)
               (ico:parameter-ast environment-section-ast)))
         (let*-ast (node* (:let*)
                     (1 :declaration
                        (ico:declaration-asts macro-ast))))
         (variable-ast (node* (:variable-name :name (gensym)))))
    (destructure-lambda-list lambda-list-ast variable-ast let*-ast)
    (reinitialize-instance let*-ast
      :form-asts (ico:form-asts macro-ast))
    (node* (:lambda)
      (1 :lambda-list
         (node* (:ordinary-lambda-list)
           (1 :required-section
              (node* (:required-section)
                (* :parameter
                   (list whole-parameter-ast environment-parameter-ast))))))
      (1 :form
         (ablock (ico:name-ast macro-ast)
           let*-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-DEFTYPE

(defun parse-deftype (deftype-ast)
  (let* ((lambda-list-ast (ico:lambda-list-ast deftype-ast))
         (whole-section-ast
           (ico:whole-section-ast lambda-list-ast))
         (whole-parameter-ast
           (if (null whole-section-ast)
               (make-temp-ast)
               (ico:parameter-ast whole-section-ast)))
         (environment-section-ast
           (ico:environment-section-ast lambda-list-ast))
         (environment-parameter-ast
           (if (null environment-section-ast)
               (make-temp-ast)
               (ico:parameter-ast environment-section-ast)))
         (let*-ast (node* (:let*)
                     (1 :declaration
                        (ico:declaration-asts deftype-ast))))
         (variable-ast (node* (:variable-name :name (gensym)))))
    (destructure-lambda-list lambda-list-ast variable-ast let*-ast)
    (node* (:lambda)
      (1 :lambda-list
         (node* (:ordinary-lambda-list)
           (1 :required-section
              (node* (:required-section)
                (* :parameter
                   (list whole-parameter-ast environment-parameter-ast))))))
      (1 :form let*-ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-DESTRUCTURING-BIND

(defun parse-destructuring-bind (destructuring-bind-ast)
  (let* ((lambda-list-ast (ico:lambda-list-ast destructuring-bind-ast))
         (whole-section-ast
           (ico:whole-section-ast lambda-list-ast))
         (whole-parameter-ast
           (if (null whole-section-ast)
               (make-temp-ast)
               (ico:parameter-ast whole-section-ast)))
         (let*-ast (node* (:let*)
                     (1 :declaration
                        (ico:declaration-asts destructuring-bind-ast))))
         (variable-ast (node* (:variable-name :name (gensym))))
         (args-ast (make-temp-ast)))
    (add-binding-asts
     whole-parameter-ast (ico:form-ast destructuring-bind-ast) let*-ast)
    (add-binding-asts
     args-ast whole-parameter-ast let*-ast)
    (destructure-lambda-list lambda-list-ast variable-ast let*-ast)
    ;; FIXME: add declarations.
    (reinitialize-instance let*-ast
      :form-asts (ico:form-asts destructuring-bind-ast))
    let*-ast))

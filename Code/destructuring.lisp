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

;;; Given a list of the remaining groups of a lambda list, return true
;;; if and only the list is not empty, and the first group of the list
;;; starts with LAMBDA-LIST-KEYWORD.
(defun first-group-is (remaining lambda-list-keyword)
  (and (not (null remaining))
       (not (null (first remaining)))
       (eq (first (first remaining)) lambda-list-keyword)))

(defun not-enough-arguments-ast ()
  (application 'error (aquote 'too-few-arguments)))

(defun add-binding-asts (variable-ast form-ast let*-ast)
  (reinitialize-instance let*-ast
    :binding-asts
    (append (ico:binding-asts let*-ast)
            (make-let-binding-ast variable-ast form-ast))))

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
    ((section-ast ico:required-section-ast) variable-ast let*-ast)
  (unless (null section-ast)
    (loop with temp-ast = (make-temp-ast)
          for ast in (ico:parameter-asts section-ast)
          for name-ast = (ico:name-ast ast)
          do (add-binding-asts
              temp-ast
              (aif (application 'null variable-ast)
                   (not-enough-arguments-ast)
                   (application 'first variable-ast))
              let*-ast)
             (add-binding-asts
              variable-ast
              (application 'rest variable-ast)
              let*-ast)
             (destructure-variable-or-pattern-ast
              name-ast temp-ast let*-ast))))

(defmethod destructure-section
    ((section-ast ico:optional-section-ast) variable-ast let*-ast)
  (unless (null section-ast)
    (loop with temp-ast = (make-temp-ast)
          for ast in (ico:parameter-asts section-ast)
          for name-ast = (ico:name-ast ast)
          for init-form-ast = (ico:init-form-ast ast)
          for supplied-p-parameter-ast = (ico:supplied-p-parameter-ast ast)
          do (unless (null supplied-p-parameter-ast)
               (add-binding-asts
                supplied-p-parameter-ast
                (application 'not (application 'null variable-ast))
                let*-ast))
             (add-binding-asts
              temp-ast
              (aif (application 'null variable-ast)
                   init-form-ast
                   (application 'first variable-ast))
              let*-ast)
             (add-binding-asts
              variable-ast
              (aif (application 'null variable-ast)
                   variable-ast
                   (application 'rest variable-ast))
              let*-ast)
             (destructure-variable-or-pattern-ast
              name-ast temp-ast let*-ast))))

(defmethod destructure-section
    ((section-ast ico:rest-section-ast) variable-ast let*-ast)
  (unless (null section-ast)
    (let ((temp-ast (make-temp-ast))
          (name-ast (ico:name-ast (ico:parameter-ast section-ast))))
      (add-binding-asts temp-ast variable-ast let*-ast)
      (destructure-variable-or-pattern-ast name-ast temp-ast let*-ast))))

(defun collect-keys (key-parameter-asts)
  (loop for key-parameter-ast in key-parameter-asts
        collect (ico:name (ico:keyword-ast key-parameter-ast))))

(defmethod destructure-section
    ((section-ast ico:key-section-ast) variable-ast let*-ast)
  (let ((ignore-ast (make-temp-ast)))
    (add-binding-asts
     ignore-ast
     (aif (application 'oddp (application 'length variable-ast))
          (application 'error (aliteral 'odd-number-of-keyword-arguments))
          (aliteral 'nil))
     let*-ast))
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

(defun destructure-lambda-list (lambda-list-ast variable-ast let*-ast)
  (destructure-section (ico:required-section-ast lambda-list-ast))
  (destructure-section (ico:optional-section-ast lambda-list-ast))
  (destructure-section (ico:rest-section-ast lambda-list-ast))
  (destructure-section (ico:key-section-ast lambda-list-ast))
  (destructure-section (ico:aux-section-ast lambda-list-ast)))

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
;;; PARSE-COMPILER-MACRO
;;;
;;; This function differs from parse-macro only in the code that
;;; destructures the lambda list from the arguments.

(defun parse-compiler-macro (name lambda-list body &optional environment)
  (declare (ignore name environment)) ; For now.
  (let* ((canonicalized-lambda-list
           (canonicalize-macro-lambda-list lambda-list))
         (environment-group
           (extract-named-group canonicalized-lambda-list '&environment))
         (environment-parameter
           (if (null environment-group) (gensym) (second environment-group)))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&environment
                   (remove '&whole canonicalized-lambda-list
                           :key #'first :test #'eq)
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations documentation forms)
        (separate-function-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-lambda-list remaining args-var whole-parameter)
        `(lambda (,whole-parameter ,environment-parameter)
           ,@(if (null documentation) '() (list documentation))
           ;; If the lambda list does not contain &environment, then
           ;; we IGNORE the GENSYMed parameter to avoid warnings.
           ;; If the lambda list does contain &environment, we do
           ;; not want to make it IGNORABLE because we would want a
           ;; warning if it is not used then.
           ,@(if (null environment-group)
                 `((declare (ignore ,environment-parameter)))
                 `())
           (let ((,args-var (if (and (eq (car ,whole-parameter) 'funcall)
                                   (consp (cdr ,whole-parameter))
                                   (consp (cadr ,whole-parameter))
                                   (eq (car (cadr ,whole-parameter)) 'function))
                              (cddr ,whole-parameter)
                              (cdr ,whole-parameter))))
             (let* ,(reverse bindings)
               (declare (ignore ,@ignored-variables))
               ,@declarations
               ,@forms)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-DEFTYPE

(defun parse-deftype (name lambda-list body)
  (declare (ignore name))
  (let* ((canonicalized-lambda-list
           (canonicalize-deftype-lambda-list lambda-list))
         (environment-group
           (extract-named-group canonicalized-lambda-list '&environment))
         (environment-parameter
           (if (null environment-group) (gensym) (second environment-group)))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&environment
                   (remove '&whole canonicalized-lambda-list
                           :key #'first :test #'eq)
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations documentation forms)
        (separate-function-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-lambda-list remaining args-var whole-parameter)
        `(lambda (,whole-parameter ,environment-parameter)
           ,@(if (null documentation) '() (list documentation))
           ;; If the lambda list does not contain &environment, then
           ;; we IGNORE the GENSYMed parameter to avoid warnings.
           ;; If the lambda list does contain &environment, we do
           ;; not want to make it IGNORABLE because we would want a
           ;; warning if it is not used then.
           ,@(if (null environment-group)
                 `((declare (ignore ,environment-parameter)))
                 `())
           (let ((,args-var (rest ,whole-parameter)))
             (let* ,(reverse bindings)
               (declare (ignore ,@ignored-variables))
               ,@declarations
               ,@forms)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-DESTRUCTURING-BIND

(defun parse-destructuring-bind (lambda-list form body)
  (let* ((canonicalized-lambda-list
           (canonicalize-destructuring-lambda-list lambda-list))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&whole canonicalized-lambda-list
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations forms)
        (separate-ordinary-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-lambda-list remaining args-var whole-parameter)
        `(let* ((,whole-parameter ,form)
                (,args-var ,whole-parameter)
                ,@(reverse bindings))
           (declare (ignore ,@ignored-variables))
           ,@declarations
           ,@forms)))))

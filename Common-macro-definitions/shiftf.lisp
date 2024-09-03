(cl:in-package #:common-macro-definitions)

;;; If there is a single place, i.e. the form looks like this:
;;;
;;;     (SHIFTF <place> <new-value-form>)
;;;
;;; then we generate the following expansion:
;;;
;;;     (LET* <bindings>
;;;       (PROG1 <accessing-form>
;;;         (MULTIPLE-VALUE-BIND <store-variables> <new-value-form>
;;;           <storing-form>)))
;;;
;;; where <bindings> are the bindings of temporary variables to values
;;; of sub-forms of <place>, <accessing-form> is the accessing form of
;;; <place>, <store-variables> are the store variables of <place>, and
;;; <storing-form> is the storing form of <place>.
;;;
;;; If there is more than one place, i.e the form looks like this:
;;;
;;;     (SHIFT <place1> <place2> ... <placen> <new-value-form>)
;;;
;;; then we generate the following expansion:
;;;
;;;     (LET* <bindings1>
;;;       (PROG1 <accessing-form1>
;;;         (LET* <bindings2>
;;;           (MULTIPLE-VALUE-BIND <store-variables1> <accessing-form2>
;;;             ...
;;;               (LET* <bindingsn>
;;;                 (MULTIPLE-VALUE-BIND <store-variablesn-1> <accessing-formn>
;;;                   (MULTIPLE-VALUE-BIND <store-variablesn> <new-value-form>
;;;                     <storing-form1>
;;;                     <storing-form2>
;;;                     ...
;;;                     <storing-formn>))) ...))))

(defmacro shiftf (&environment environment &rest arguments)
  (let* ((places (butlast arguments))
         (new-value-form (first (last arguments)))
         (setf-expansions
           ;; Collect the SETF-EXPANSION of each place as a list of the
           ;; values returned by GET-SETF-EXPANSION. 
           (mapcar (lambda (place)
                     (multiple-value-list
                      (get-setf-expansion *client* place environment)))
                   places)))
    (flet ((make-let*-bindings (temporary-variables value-forms)
             (mapcar #'list temporary-variables value-forms)))
      (if (= (length places) 1)
          (destructuring-bind (temporary-variables
                               value-forms
                               store-variables
                               storing-form
                               accessing-form)
              (first setf-expansions)
            `(let* ,(make-let*-bindings temporary-variables value-forms)
               (prog1 ,accessing-form
                 (multiple-value-bind ,store-variables ,new-value-form
                   ,storing-form))))
          (let ((result
                  ;; We start by creating the body of the result, which
                  ;; contains all the STORE-FORMs, storing the
                  ;; STORE-VARIABLEs in the respective place.
                  `(progn ,@(mapcar #'fourth setf-expansions))))
            ;; Now, we deal with the LAST PLACE.  We wrap the body of
            ;; the result in the binding of NEW-VALUE-FORM to the
            ;; store variables of that place.
            (let ((store-variables (third (first (last setf-expansions)))))
              (setf result
                    `(multiple-value-bind ,store-variables ,new-value-form
                       ,result)))
            ;; Next, for each place P except the last, let Q be the
            ;; place that follows P.  We wrap the current result in
            ;; the LET* form that evaluates the sub-forms of Q and the
            ;; binding of the store variables of P to the accessing
            ;; form of Q.
            (loop for (temporary-variables
                       value-forms
                       nil
                       nil
                       accessing-form)
                    in (reverse (rest setf-expansions))
                  for (nil nil  store-variables)
                    in (reverse (butlast setf-expansions))
                  do (setf result
                           `(let* ,(make-let*-bindings
                                    temporary-variables value-forms)
                              (multiple-value-bind ,store-variables ,accessing-form
                                ,result))))
            ;; Finally, we deal with the first place.  We wrap the
            ;; result in a LET* that evaluates the sub-forms as usual,
            ;; but also in a PROG1 that return the result of the
            ;; accessing form of the first place.
            (destructuring-bind (temporary-variables
                                 value-forms . rest)
                (first setf-expansions)
              (declare (ignore rest))
              (let ((accessing-form (fifth (first setf-expansions))))
                (setf result
                      `(let* ,(make-let*-bindings temporary-variables value-forms)
                         (prog1 ,accessing-form
                           ,result)))))
            result)))))

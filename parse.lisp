;;;; parse.lisp

;;;; parse type specifiers into type objects.

(in-package #:sandalphon.types)

;;; environment hacking: ridiculous
#||
(define-symbol-macro %types nil)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun %types (&optional env)
    (macroexpand-1 '%types env)))

;; "abstraction", I cry to myself
(defun %ctype-binding-form (bindings body env)
  `(symbol-macrolet ((%types ,(append bindings (%types env))))
     ,@body))

(defun typexpander (specname &optional env)
  (assoc-value (%types env) specname))

(defsetf typexpander (specifier &optional env) (new-typexpander)
  (when env (error "wat"))
  `(let ((types (%types)))
     ;; go through this so that we can alter existing entries rather than adding new ones all the time
     (setf (assoc-value types ,specifier) ,new-typexpander)
     ;; FIXME: spurious redefinition warnings? (not on sbcl...)
     (eval `(define-symbol-macro %types ,types)) ; hahahahahahahaha
     ,new-typexpander))
||#

(define-namespace %types typexpander %ctype-bind)

(define-condition unknown-type (error)
  ((specifier :reader unknown-type-specifier :initarg :spec)
   (env :reader unknown-type-env :initarg :env))
  (:report (lambda (condition stream)
	     (format stream "The type specifier ~s is unknown." (unknown-type-specifier condition)))))

(defun specifier-type (specifier &optional env)
  (multiple-value-bind (expansion expanded)
      (typexpand specifier env)
    (cond ((and (symbolp expansion) (find-class expansion nil env)))
	  ((or (symbolp expansion) (listp expansion)) (error 'unknown-type :spec expansion :env env))
	  (expanded expansion)
	  (t (error 'unknown-type :spec specifier :env env)))))

(defun known-type-p (specifier &optional env)
  (handler-case
      (specifier-type specifier env)
    (unknown-type () nil)))

(defun typexpand (specifier &optional env)
  (labels ((mx-loop (spec ever-expanded)
	     (multiple-value-bind (expansion expanded)
		 (typexpand-1 spec env)
	       (if expanded
		   (mx-loop expansion t)
		   (values spec ever-expanded)))))
    (mx-loop specifier nil)))

(defun typexpand-1 (specifier &optional env)
  (multiple-value-bind (expander found)
      (typexpander (if (listp specifier) (first specifier) specifier) env)
    (if found
	(values (funcall *macroexpand-hook* expander specifier env) t)
	(values specifier nil))))

(defsetf specifier-type (specifier &optional env) (new-ctype)
  `(progn
     (setf (typexpander ,specifier ,env)
	   (lambda (form env)
	     (declare (ignore env))
	     (unless (eq form ,specifier)
	       (error "There is no atomic form for specifier ~s" ,specifier))
	     ,new-ctype))
     ,new-ctype))

;;; supposed to be like FUNCTION. my head hurts too much atm to figure out a better way
;;; i guess the problem is that (function some-unknown-name) is compileable (since the ref is runtime)
;;; but (type some-unknown-name) actually shouldn't be, in that type info about variables only has to exist at compile time
;;; i think this extends to types?
;;; agh.
(defmacro ctype (specifier &environment env)
  (specifier-type specifier env))

(define-setf-expander ctype (specifier &environment env)
  ;; FIXME: not sure about the getter
  (get-setf-expansion `(specifier-type ',specifier) env))

(eval-when (:compile-toplevel :execute :load-toplevel)
  ;;; A word on functions versus lambda forms.
  ;;; TYPEMACROLET expands to something that has literal function objects in it,
  ;;; while DEFTYPEMACRO does not. What gives?
  ;;; The answer is dumping. See CL functions, that is the actual objects, are not dumpable into FASLs.
  ;;; That's why DEFTYPEMACRO expands into, essentially, (setf whatever (lambda ...)).
  ;;; (This also allows global macroexpanders to be closures, though then they're not defined during compilation
  ;;;  w/o eval-when of course.)
  ;;; Local type macros, however, only need to exist in the local environment - and there's no reason that can't
  ;;;  be destroyed once compilation is over.
  ;;; (Also, analogously to MACROLET, you can't (conformingly...) have them be closures.)
  ;;; Indeed, all macros must be expanded by the time compilation is completed, and that means our expanders,
  ;;;  stuffed into a symbol macro as they presently are, are gone.
  ;;; (I just checked and it is in fact specified that *MACROLET forms are _replaced_:
  ;;; "macrolet and symbol-macrolet are effectively replaced by forms corresponding to their bodies
  ;;;  in which calls to macros are replaced by their expansions." (from minimal compilation semantics))
  ;;; So the literal functions that TYPEMACROLET has in its expansion are already gone by runtime.
  ;;; Neat, huh? Neat or weird as hell, I guess.
  (defun parse-typemacro-definition (name lambda-list &rest body)
    (multiple-value-bind (munged whole env)
	(munge-deftype-lambda-list lambda-list)
      (let ((env-used env)
	    (whole (or whole (gensym "WHOLE")))
	    (env (or env (gensym "ENV"))))
	;; FIXME (or cease caring): bound declarations on whole and env won't work
	;; the fix i used for that before was des-binding (list env whole thing-actually-being-destructured)
	;; but that's terrible for a variety of reasons, e.g. consing
	`(lambda (,whole ,env)
	   ,@(unless env-used (list `(declare (ignore ,env))))
	   (block ,name
	     (destructuring-bind ,munged (rest (ensure-list ,whole))
	       ,@body))))))

  (defun parse-typemacrolet-definitions (definitions)
    ;; (foo a b c) = (funcall (curry #'apply #'foo) (list a b c))
    (mapcar (lambda (def)
	      (cons (first def)
		    (coerce (apply #'parse-typemacro-definition def) 'function)))
	    definitions))
) ; EVAL-WHEN

(defmacro typemacrolet (definitions &body body)
  `(%ctype-bind ,(parse-typemacrolet-definitions definitions) ,@body))

(defmacro deftypemacro (name lambda-list &body body)
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (setf (typexpander ',name)
	   ,(apply #'parse-typemacro-definition name lambda-list body))
     ',name))

(defmacro %define-unparametric-type (name ctype)
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (setf (ctype ,name) ,ctype)))

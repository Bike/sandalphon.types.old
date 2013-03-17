;;;; envhack.lisp
;;;; bad but conforming mechanism for stuffing arbitrary information into the environment

(in-package #:sandalphon.types)

(defmacro define-namespace (name accessor binder)
  ;; FIXME compile-time aaaaah
  `(progn
     (define-symbol-macro ,name nil)
     (defun ,name (&optional env)
       (macroexpand-1 ',name env))
     (defun ,accessor (name &optional env)
       (assoc-value (,name env) name))
     (defsetf ,accessor (specifier &optional env) (new)
       `(let ((types (,',name)))
	  ;; originally I did this at macroexpansion time, but that's wrong for obscure reasons
	  ;; (defsetf ll variables are bound to temps and sbcl just happened to optimize)
	  ;; and it wouldn't make sense anyway.
	  (when ,env (error "~s with a non-~s environment is undefined." `(setf ,',',accessor) nil))
	  (setf (assoc-value types ,specifier) ,new)
	  ;; You know, I thought this line was bad before, when it just evaluated a define-symbol-macro form.
	  ;; But now. Now, it's a triple backquote.
	  ;; I wrote a DEBACKQUOTE function just to make sure this did what I thought it did (it didn't, I had to fix it).
	  ;; Yeah.
	  ;; At this point I'm kind of hankering for a "named-backquote" that lets me unquote to some other level.
	  ;; Or something. Jesus.
	  (eval `(define-symbol-macro ,',',name ,types))
	  ,new))
     (defmacro ,binder (bindings &body body &environment env)
       `(symbol-macrolet ((,',name ,(append bindings (,name env)))) ,@body))))

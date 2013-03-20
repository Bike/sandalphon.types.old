;;;; values.lisp

(in-package #:sandalphon.types)

(defclass values-ctype (ctype)
  ((required :accessor values-ctype-required :initarg :req)
   (optional :accessor values-ctype-optional :initarg :opt)
   (rest :accessor values-ctype-rest :initarg :rest)
   (aok-p :accessor values-ctype-aok-p :initarg :aok-p)))

(defmethod ctypep (object (ctype values-ctype))
  (error "~s ctype illegal in this context. Try ~s" 'values 'values-ctypep))

(defun values-ctypep (ctype &rest args)
  (let ((req (values-ctype-required ctype))
	(opt (values-ctype-optional ctype))
	(rest (values-ctype-rest ctype))
	(mode :required))
    (if (and (null req) (null opt) (null rest))
	;; (values-ctypep (ctype (values)) nil) => T, (values-ctypep (ctype (values))) => T
	;; (not sure about this)
	(or (null args) (and (null (rest args)) (null (first args))))
	(labels ((next ()
		   (case mode
		     ((:required) (cond (req (pop req)) (t (setf mode '&optional) (next))))
		     ((&optional) (cond (opt (pop opt)) (t (setf mode '&rest) (next))))
		     ((&rest) (if rest rest (return-from values-ctypep nil))))))
	  (dolist (arg args (null req))
	    (unless (ctypep arg (next)) (return nil)))))))

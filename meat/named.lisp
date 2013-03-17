;;;; named.lisp

(in-package #:sandalphon.types)

(defclass named-ctype (ctype)
  ((name :initarg :name :accessor named-ctype-name)))

(defparameter *named-ctypes* (make-hash-table :test #'eq))

(defmethod make-load-form ((object named-ctype) &optional env)
  (declare (ignore env))
  (let ((name (named-ctype-name object)))
    `(or (values (gethash ',name *named-ctypes*))
	 (error "named type ~s not established" ',name))))

(defmacro defsingleton (name &optional reader)
  ;; TODO or not: compiler magic like the old tnil
  `(progn
     (unless (gethash ',name *named-ctypes*)
       (setf (gethash ',name *named-ctypes*) (make-instance 'named-ctype :name ',name)))
     ,(when reader `(defun ,reader () (values (gethash ',name *named-ctypes*))))
     ',name))

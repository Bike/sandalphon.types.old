;;;; class.lisp

(in-package #:sandalphon.types-test)

(def-suite class :in all)

(in-suite class)

;;; no more relationships i guess
(test class-types
  (mapc (lambda (name)
	  (is-true (ctype= (specifier-type name) (find-class name))
		   "~s the type and ~:*~s the class are not type-equivalent"
		   name))
	*types-with-classes*))

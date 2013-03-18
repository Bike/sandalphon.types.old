;;;; util.lisp
;;; stuff to help with testing

(in-package #:sandalphon.types-test)

(defmacro dcsubtypep (t1 t2)
  `(csubtypep (specifier-type ',t1) (specifier-type ',t2)))

(defmacro scsubtypep (t1 t2)
  `(is-true (dcsubtypep ,t1 ,t2)
	    "~s is not a recognizable subtype of ~s" ',t1 ',t2))

(defmacro subtypes (type &rest parents)
  `(progn ,@(mapcar (curry #'list 'scsubtypep type) parents)))

(defmacro sctype= (t1 t2)
  `(is (ctype= (specifier-type ',t1) (specifier-type ',t2))
       "~s is not recognizably type-equal to ~s" ',t1 ',t2))

(defmacro surely-not (form)
  `(is (equal '(nil t) (multiple-value-list ,form))
       "~s was not surely false" ',form))

(defmacro dunno (form)
  `(is (equal '(nil nil) (multiple-value-list ,form))
       "~s was not surely false" ',form))

(defclass mysterious-ctype (ctype)
  ()
  (:documentation "A class with no behavior other than that from CTYPE defined.
Useful for testing."))

(deftypemacro mystery ()
  ;; allocates anew each time
  (make-instance 'mysterious-ctype))

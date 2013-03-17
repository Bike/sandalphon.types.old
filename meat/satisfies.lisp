;;;; satisfies.lisp

(in-package #:sandalphon.types)

(defclass satisfies-ctype (ctype)
  ((predicate :accessor satisfies-ctype-predicate :initarg :predicate
	      :type symbol)))

(defmethod ctypep (object (type satisfies-ctype))
  (funcall (fdefinition (satisfies-ctype-predicate type)) object))

(defmethod csubtypep tri/definite (t1 (t2 satisfies-ctype))
  (declare (ignore t1))
  (values nil nil))
(defmethod csubtypep tri/definite ((t1 satisfies-ctype) t2)
  (declare (ignore t2))
  (values nil nil))

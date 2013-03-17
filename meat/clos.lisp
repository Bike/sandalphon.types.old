;;;; clos.lisp

;;;; are classes mutable types?
;;;; we'll certainly count them as types.

(in-package #:sandalphon.types)

;;; keeping the CL: prefixes to make it obvious they're not their c- versions

(defmethod ctypep (object (type class))
  (cl:typep object type))

(defmethod csubtypep tri/definite ((t1 class) (t2 class))
  (cl:subtypep t1 t2))

;;; if a ctype should be a subtype of a class or vice versa, that should be covered in its definition.
;;; by default we assume nothing.

(defmethod csubtypep tri/definite ((t1 class) (t2 ctype)) (values nil nil))
(defmethod csubtypep tri/definite ((t1 ctype) (t2 class)) (values nil nil))

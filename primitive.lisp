;;;; primitive.lisp

;;;; The type system is unconcerned with the representation of objects; this is left up to the implementation.
;;;; As such, it just delegates to more primitive functions for typep, and defines the simple relations that it can.

;;;; A type being primitive only really implies that it's disjoint from other primitive types.
;;;; For example, take cons.  Cons is primitive, but it's still a subtype of list, which is in turn a subtype of sequence.

;;;; or let me just quote the spec.
;;;; 4.2.2 Type Relationships
;;;;  The types cons, symbol, array, number, character, hash-table, function, readtable, package, pathname, stream,
;;;; random-state, condition, restart, and any single other type created by defstruct, define-condition, or defclass are
;;;; pairwise disjoint, except for type relations explicitly established by specifying superclasses in defclass or
;;;; define-condition or the :include option of destruct.

;;;; "disjoint" means they have no elements in common (meaning their intersection is nil), not that they have no relation
;;;; in the type hierarchy.  for example, cons and array could both be subtypes of structure-object.

(in-package #:sandalphon.types)

(defclass primitive-ctype (ctype) ())

(defmethod intersection/2 ((t1 primitive-ctype) (t2 primitive-ctype))
  (bottom))
(defmethod csubtypep tri/definite ((t1 primitive-ctype) (t2 primitive-ctype))
  (values nil t))
(defmethod negate-ctype ((type primitive-ctype))
  (make-instance 'negation-ctype :neg type))

(defmethod csubtypep tri/definite ((t1 primitive-ctype) (t2 class)) (values nil t))
(defmethod csubtypep tri/definite ((t1 class) (t2 primitive-ctype)) (values nil t))
;(defmethod csubtypep tri/definite ((t1 primitive-ctype) (t2 (eql (find-class 't t)))) (values t t)) ; FIXME ugh

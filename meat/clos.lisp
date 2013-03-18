;;;; clos.lisp

;;;; are classes mutable types?
;;;; we'll certainly count them as types.

(in-package #:sandalphon.types)

;;; keeping the CL: prefixes to make it obvious they're not their c- versions

(defmethod ctypep (object (type class))
  (cl:typep object type))

;;; Honestly, I should probably scrap all this and compute subclasses myself.

(defmethod csubtypep tri/definite ((t1 class) (t2 class))
  (cl:subtypep t1 t2))

;;; What's the deal with this shit? This lengthy comment is an attempt to justify these two bizarre methods.
;;; CLHS 4.2.2 says:
#||
The types cons, symbol, array, number, character, hash-table, function, readtable, package, pathname, stream, random-state, condition, restart, and any single other type created by defstruct, define-condition, or defclass are pairwise disjoint, except for type relations explicitly established by specifying superclasses in defclass or define-condition or the :include option of destruct [sic].
||#
;;; so: for any two types A and B, if A is a subtype of one of the above types and B is a subtype of another,
;;;  the intersection of A and B is bottom, A does not subtype B, and B does not subtype A.
;;; That's nice to support. It means simplifying (and array cons) to nil, and such.
;;; Your first thought may be to programmatically define
;;; (defmethod intersection/2 ((t1 (eql (find-class 'array))) (t2 (eql (find-class 'cons)))) (values nil t))
;;; and so on. But this isn't enough.
;;; Consider (and broadcast-stream cons).
;;; OK, so why not just define methods on all the standard classes that are subtypes of all those?
;;; They're probably BUILT-IN-CLASSes, and you can't subtype those.
;;; But that doesn't sit right with me, especially since subclassing them isn't unknown.
;;; crhodes' sequences and gray-streams are near-examples.

;;; So this exhaustive search every time two classes are intersected is the best I came up with. Sorry.

;; wow, what was I thinking, this is already handled by the cl:subtypep...
#+(or)
(defmethod csubtypep :around ((t1 class) (t2 class))
  (let* ((prims (load-time-value (mapcar #'find-class
					 '(cons symbol array number character hash-table function readtable
					   package pathname stream random-state condition restart))
				 t))
	 (prim1 (find-if (curry #'subtypep t1) prims))
	 (prim2 (find-if (curry #'subtypep t2) prims)))
    (cond ((eql t2 (find-class 't)) (values t t)) ; kludge
	  ((or prim1 prim2) (values (eql prim1 prim2) t))
	  (t (call-next-method)))))

(defmethod intersection/2 :around ((t1 class) (t2 class))
  (let* (;; sure hope this is coalesced
	 (prims (load-time-value (mapcar #'find-class
					 '(cons symbol array number character hash-table function readtable
					   package pathname stream random-state condition restart))
				 t))
	 (prim1 (find-if (curry #'subtypep t1) prims))
	 (prim2 (find-if (curry #'subtypep t2) prims)))
    (cond ((cl:subtypep t1 t2) t1)
	  ((cl:subtypep t2 t1) t2)
	  ((or prim1 prim2) (bottom))
	  (t (call-next-method)))))

;;; if a ctype should be a subtype of a class or vice versa, that should be covered in its definition.
;;; by default we assume nothing.

(defmethod csubtypep tri/definite ((t1 class) (t2 ctype)) (values nil nil))
(defmethod csubtypep tri/definite ((t1 ctype) (t2 class)) (values nil nil))

;;;; basic.lisp

(in-package #:sandalphon.types-test)

(def-suite all)

(def-suite misc :in all)

(in-suite misc)

(test exhaustive-partitions
  ;;; found by regexing through the CLHS
  (sctype= list (or cons null))
  (sctype= integer (or fixnum bignum))
  (sctype= character (or base-char extended-char)))

;;; I'm thinking this test isn't needed. The types being pairwise disjoint doesn't have to mean that
;;;  the intersection of the ctypes /has/ to be bottom.
;;; It would be kind of nice if they were, but right now I think it might be more trouble than it's worth.
;;; or not. nasty :around on class-class but I'm just that anal. see clos.lisp, and disjoin.lisp.

(test primitive-disjointness
  (let ((prims (mapcar #'specifier-type
		       '(cons symbol array number character hash-table function readtable
			 package pathname stream random-state condition restart))))
    (map-permutations (curry #'apply
			     (lambda (t1 t2)
			       (is (csubtypep (ctype-intersection t1 t2) (ctype nil))
				   "~s and ~s are not disjoint" (type-specifier t1) (type-specifier t2))))
		      prims
		      :length 2
		      :copy nil)))

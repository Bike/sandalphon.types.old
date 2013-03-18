;;;; basic.lisp

(in-package #:sandalphon.types-test)

(def-suite :sandalphon.types)

(in-suite :sandalphon.types)

(test all-atomics
  (dolist (spec *standard-atomic-type-specifiers*)
    (handler-case
	(pass "~s was parsed as ~s" spec (specifier-type spec))
      (unknown-type ()
	(fail "~s couldn't be parsed" spec)))))

(defmacro dcsubtypep (t1 t2)
  `(csubtypep (specifier-type ',t1) (specifier-type ',t2)))

(defmacro scsubtypep (t1 t2)
  `(is-true (dcsubtypep ,t1 ,t2)
	    "~s is not a recognizable subtype of ~s" ',t1 ',t2))

(defmacro subtypes (type &rest parents)
  `(progn ,@(mapcar (curry #'list 'scsubtypep type) parents)))

(test standard-atomic-subtypery/a
  (subtypes arithmetic-error error serious-condition condition t)
  (subtypes array t)
  (subtypes atom t))

(test standard-atomic-subtypery/b
  (subtypes base-char character t)
  (subtypes base-string string vector array sequence t)
  (subtypes bignum integer rational real number t)
  (subtypes bit unsigned-byte signed-byte integer rational real number t)
  (subtypes bit-vector vector array sequence t)
  (subtypes broadcast-stream stream t)
  (subtypes built-in-class class standard-object t))

(test standard-atomic-subtypery/c
  (subtypes cell-error error serious-condition condition t)
  (subtypes character t)
  (subtypes class standard-object t)
  (subtypes compiled-function function t)
  (subtypes complex number t)
  (subtypes concatenated-stream stream t)
  (subtypes condition t)
  (subtypes cons list sequence t)
  (subtypes control-error error serious-condition condition t))

(test standard-atomic-subtypery/def
  (subtypes division-by-zero arithmetic-error error serious-condition condition t)
  (subtypes double-float float real number t)
  (subtypes floating-point-inexact arithmetic-error error serious-condition condition t)
  (subtypes floating-point-invalid-operation arithmetic-error error serious-condition condition t)
  (subtypes floating-point-overflow arithmetic-error error serious-condition condition t)
  (subtypes floating-point-underflow arithmetic-error error serious-condition condition t)
  (subtypes function t))

(test standard-atomic-subtypery/ghiklmn
  (subtypes generic-function function t)
  (subtypes hash-table t)
  (subtypes integer rational real number t)
  (subtypes keyword symbol t)
  (subtypes list sequence t)
  (subtypes logical-pathname pathname t)
  (subtypes long-float float real number t)
  (subtypes method t)
  (subtypes method-combination t)
  ;; NIL would go here
  (subtypes null symbol list sequence t)
  (subtypes number t))

(test standard-atomic-subtypery/p
  (subtypes package t)
  (subtypes package-error error serious-condition condition t)
  (subtypes parse-error error serious-condition condition t)
  (subtypes pathname t)
  (subtypes print-not-readable error serious-condition condition t)
  (subtypes program-error error serious-condition condition t))

(test standard-atomic-subtypery/r
  (subtypes random-state t)
  (subtypes ratio rational real t)
  (subtypes reader-error parse-error error serious-condition condition t)
  (subtypes readtable t)
  (subtypes real number t)
  (subtypes restart t))

(test standard-atomic-subtypery/s1
  (subtypes sequence t)
  (subtypes serious-condition condition t)
  (subtypes short-float float real number t)
  (subtypes signed-byte integer rational real number t))

(test standard-atomic-subtypery/simple
  (subtypes simple-array array t)
  (subtypes simple-base-string base-string string vector array sequence t)
  (subtypes simple-bit-vector bit-vector vector array sequence t)
  (subtypes simple-condition condition t)
  (subtypes simple-error simple-condition error serious-condition condition t)
  (subtypes simple-string string vector array sequence t)
  (subtypes simple-type-error simple-condition type-error error serious-condition condition t)
  (subtypes simple-vector vector simple-array array sequence t)
  (subtypes simple-warning simple-condition warning condition t))

(test standard-atomic-subtypery/s2
  (subtypes single-float float real number t)
  (subtypes standard-char base-char character t)
  (subtypes standard-class class standard-object t)
  (subtypes standard-generic-function generic-function function t)
  (subtypes standard-method method standard-object t)
  (subtypes standard-object t)
  (subtypes storage-condition serious-condition condition t)
  (subtypes stream t)
  (subtypes stream-error error serious-condition condition t)
  (subtypes string vector array sequence t)
  (subtypes string-stream stream t)
  (subtypes structure-class class standard-object t)
  (subtypes structure-object t)
  (subtypes style-warning warning condition t)
  (subtypes symbol t)
  (subtypes synonym-stream stream t))

(test standard-atomic-subtypery/tuvwxyz
  ;; T would go here
  (subtypes two-way-stream stream t)
  (subtypes type-error error serious-condition condition t)
  (subtypes unbound-slot cell-error error serious-condition condition t)
  (subtypes unbound-variable cell-error error serious-condition condition t)
  (subtypes undefined-function cell-error error serious-condition condition t)
  (subtypes unsigned-byte signed-byte integer rational real number t)
  (subtypes vector array sequence t)
  (subtypes warning condition t))

;;; harder relationships

(defmacro sctype= (t1 t2)
  `(is (ctype= (specifier-type ',t1) (specifier-type ',t2))
       "~s is not recognizably type-equal to ~s" ',t1 ',t2))  

(test exhaustive-partitions
  ;;; found by regexing through the CLHS
  (sctype= list (or cons null))
  (sctype= integer (or fixnum bignum))
  (sctype= character (or base-char extended-char)))

(defclass mysterious-ctype (ctype)
  ()
  (:documentation "A class with no behavior other than that from CTYPE defined.
Useful for testing."))

(deftypemacro mystery ()
  ;; allocates anew each time
  (make-instance 'mysterious-ctype))

(test identities
  (let ((mystery (ctype mystery)))
    ;;; note that these (should) work on one particular ctype, rather than just two instances of the same class.
    ;; eqlity implies type equality
    (is (ctype= mystery mystery))
    ;; the intersection of a type and its negation is bottom
    (is (ctype= (ctype-intersection mystery (negate-ctype mystery)) (ctype nil)))
    ;; the union of a type and its negation is top
    (is (ctype= (ctype t) (ctype-union mystery (negate-ctype mystery))))
    ;;; these however work more arbitrarily
    ;; bottom is a subtype of everything
    (is (csubtypep (ctype nil) mystery))
    ;; everything is a subtype of top
    (is (csubtypep mystery (ctype t)))
    (is (csubtypep (ctype-intersection mystery (ctype real)) mystery))
    (is (csubtypep mystery (ctype-union mystery (ctype string))))))

(defmacro surely-not (form)
  `(is (equal '(nil t) (multiple-value-list ,form))
       "~s was not surely false" ',form))

(test negations
  ;; neg and neg
  (scsubtypep (not real) (not real)) ; reflexiveness implies negareflexiveness
  (scsubtypep (not t) (not t)) ; for weirdos
  (scsubtypep (not nil) (not nil))
  (scsubtypep (not real) (not integer)) ; contraposition
  (scsubtypep (not t) (not nil)) ; for weirdos

  ;; pos and neg
  (surely-not (dcsubtypep integer (not real)))
  (scsubtypep nil (not mystery))

  (surely-not (dcsubtypep real (not integer)))
  (scsubtypep real (not string))
  (scsubtypep real (not nil))
  (scsubtypep t (not nil))

  ;; neg and pos
  (surely-not (dcsubtypep (not integer) real))
  (surely-not (dcsubtypep (not integer) integer))
  (surely-not (dcsubtypep (not nil) real))
  (scsubtypep (not nil) t)
  (scsubtypep (not t) t)
  (surely-not (dcsubtypep (not nil) nil))

  (surely-not (dcsubtypep (not real) integer))
  (surely-not (dcsubtypep (not string) integer))
  (scsubtypep (not t) nil)
  (scsubtypep (not t) mystery)
  (surely-not (dcsubtypep (not real) nil)))

(defmacro dunno (form)
  `(is (equal '(nil nil) (multiple-value-list ,form))
       "~s was not surely false" ',form))

(test disidentities
  ;; you might think that for all types X, X surely doesn't subtype ¬X, and ¬X doesn't subtype X.
  ;; but you'd be wrong - ¬top subtypes top, and bottom subtypes ¬bottom.
  ;; while those two are unique in this respect, it means that for an unknown type (like mystery),
  ;; we can't say ¬mystery doesn't subtype mystery, or the converse, because mystery might turn out to be T or NIL.
  ;; (the case of a type definitely not being T or NIL is handled in negations)
  (let ((mystery (ctype mystery)))
    (dunno (csubtypep mystery (negate-ctype mystery)))
    (dunno (csubtypep (negate-ctype mystery) mystery))))

;;; I'm thinking this test isn't needed. The types being pairwise disjoint doesn't have to mean that
;;;  the intersection of the ctypes /has/ to be bottom.
;;; It would be kind of nice if they were, but right now I think it might be more trouble than it's worth.
#+(or)
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

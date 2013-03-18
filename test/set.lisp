;;;; set.lisp

(in-package #:sandalphon.types-test)

(def-suite set :in all)

(in-suite set)

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

(test disidentities
  ;; you might think that for all types X, X surely doesn't subtype ¬X, and ¬X doesn't subtype X.
  ;; but you'd be wrong - ¬top subtypes top, and bottom subtypes ¬bottom.
  ;; while those two are unique in this respect, it means that for an unknown type (like mystery),
  ;; we can't say ¬mystery doesn't subtype mystery, or the converse, because mystery might turn out to be T or NIL.
  ;; (the case of a type definitely not being T or NIL is handled in negations)
  (let ((mystery (ctype mystery)))
    (dunno (csubtypep mystery (negate-ctype mystery)))
    (dunno (csubtypep (negate-ctype mystery) mystery))))

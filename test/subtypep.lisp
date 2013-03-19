;;;; subtypep.lisp

(in-package #:sandalphon.types-test)

(def-suite subtypep :in all)

(in-suite subtypep)

(test ccl-hair
  ;; CCL ticket #227 (http://trac.clozure.com/ccl/ticket/277)
  (is-true (csubtypep (ctype (not (cons float t)))
		      (ctype (or (not (cons (eql 0) (real 1.0 1.0)))
				 (not (cons t (eql 0))))))))

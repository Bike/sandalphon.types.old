;;;; typep.lisp

(in-package #:sandalphon.types-test)

(def-suite typep :in all)

(in-suite typep)

(defmacro svalues-ctypep (ll &rest clauses)
  `(progn ,@(mapcar (lambda (clause)
		      `(is-true (values-ctypep (specifier-type '(values ,@ll)) ,@clause)))
		    clauses)))

(test values-ctypep-positive
  (macrolet ((svalues-ctypep (ll &rest clauses)
	       `(progn ,@(mapcar (lambda (clause)
				   `(is-true (values-ctypep (specifier-type '(values ,@ll)) ,@clause)))
				 clauses))))
    (svalues-ctypep () () (nil))
    (svalues-ctypep (integer) (4))
    (svalues-ctypep (integer array) (9 #(b)))
    (svalues-ctypep (&optional integer) () (12))
    (svalues-ctypep (array &optional integer) (#(c) 8))
    (svalues-ctypep (&rest integer) () (8 19 01 891))
    (svalues-ctypep (&optional array &rest integer) () (#(d)) (#(a) 9 1 91))
    (svalues-ctypep (cons &optional array symbol &rest integer)
		    ('(hey))
		    ('(4) #(9))
		    ('("doot") #(#(4)) 'coo)
		    ('(hi) #(1) 'hey 8 91 1))))

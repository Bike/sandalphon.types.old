;;;; disjoin.lisp
;;;; deal with 4.2.2

(in-package #:sandalphon.types)

;; alexandria's map-permutations isn't quite what I want it to be.
(eval-when (:compile-toplevel :execute)
  (defun collect-perms (function sequence &rest keys &key (start 0) end length (copy t))
    (declare (ignore start end length copy))
    (let (res)
      (apply #'map-permutations
	     (lambda (perm) (push (apply function perm) res))
	     sequence
	     keys)
      ;(nreverse res) ; they're not in any real order anyway
      res)))
  
(macrolet ((dis (&rest kinds)
	     `(progn ,@(collect-perms
			(lambda (k1 k2)
			  `(progn
			     (defcomm intersection/2 ((t1 ,k1) (t2 ,k2)) (bottom))
			     (defmethod csubtypep tri/definite ((t1 ,k1) (t2 ,k2)) (values nil t))
			     (defmethod csubtypep tri/definite ((t1 ,k2) (t2 ,k1)) (values nil t))))
			kinds
			:copy nil
			:length 2))))
  (dis array-ctype cons-ctype
       complex-ctype real-ctype))

#+(or)
(macrolet ((dis (&rest specs)
	     `(progn
		,@(collect-perms (lambda (t1 t2)
				   `(defcomm intersection/2 ((t1 (eql ,t1)) (t2 (eql ,t2))) (bottom)))
				 (mapcar #'find-class specs)
				 :copy nil
				 :length 2))))
  (dis cons symbol array number character hash-table function readtable
       package pathname stream random-state condition restart))

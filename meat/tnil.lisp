;;;; tnil.lisp

;;;; Top and bottom types - T and NIL - top-type and bottom-type.
;;;; And now full-type-p and empty-type-p for convenience.

(in-package #:sandalphon.types)

(defsingleton nil bottom)
(defun top () (find-class 't))

(defcomm union/2 ((t1 (eql (bottom))) t2) t2)
(defcomm intersection/2 ((t1 (eql (bottom))) t2) (declare (ignore t2)) (bottom))
;; arg precedence order means the first method here should be chosen first in (csubtypep nil nil)
(defmethod csubtypep tri/definite ((t1 (eql (bottom))) t2) (declare (ignore t2)) (values t t))
(defmethod ctypep (object (type (eql (bottom)))) (declare (ignore object)) nil)
(defmethod negate-ctype ((type (eql (bottom)))) (top))

(defcomm union/2 ((t1 (eql (top))) t2) (declare (ignore t2)) (top))
(defcomm intersection/2 ((t1 (eql (top))) t2) t2)
(defmethod csubtypep tri/definite (t1 (t2 (eql (top)))) (declare (ignore t1)) (values t t)) 
(defmethod ctypep (object (type (eql (top)))) (declare (ignore object)) t)
(defmethod negate-ctype ((type (eql (top)))) (bottom))

;;; these are named -type-p instead of -ctype-p because they aren't properties of the type objects themselves per se
;;; that is, i think (eql (top))-p would imply (typep x '(eql (top))) which is not the case.

(defun top-type-p (ctype) (csubtypep (top) ctype))
(defun bottom-type-p (ctype) (csubtypep ctype (bottom)))

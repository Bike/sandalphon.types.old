;;;; cons.lisp

;;;; cons type.

(in-package #:sandalphon.types)

(defclass cons-ctype (#+s.t.prim primitive-ctype #-s.t.prim ctype)
  ((car-type :accessor cons-ctype-car-type :initarg :car)
   (cdr-type :accessor cons-ctype-cdr-type :initarg :cdr)))

(defmethod union/2 ((t1 cons-ctype) (t2 cons-ctype))
   (let ((car-union
          (union/2 (cons-ctype-car-type t1) (cons-ctype-car-type t2)))
         (cdr-union
          (union/2 (cons-ctype-cdr-type T1) (cons-ctype-cdr-type t2))))
     (if (or (bottom-type-p car-union)
             (bottom-type-p cdr-union))
         (bottom)
         (make-instance 'cons-ctype :car car-union :cdr cdr-union))))

(defmethod intersection/2 ((t1 cons-ctype) (t2 cons-ctype))
  (let ((car-intersection
	 (intersection/2 (cons-ctype-car-type t1) (cons-ctype-car-type t1)))
	(cdr-intersection
	 (intersection/2 (cons-ctype-cdr-type t1) (cons-ctype-cdr-type t2))))
     (if (or (bottom-type-p car-intersection)
             (bottom-type-p cdr-intersection))
         (bottom)
         (make-instance 'cons-ctype :car car-intersection :cdr cdr-intersection))))

(defmethod csubtypep tri/definite ((t1 cons-ctype) (t2 cons-ctype))
  (tri/and (csubtypep (cons-ctype-car-type t1) (cons-ctype-car-type t2))
	   (csubtypep (cons-ctype-cdr-type t1) (cons-ctype-cdr-type t2))))
(defmethod csubtypep tri/definite ((t1 cons-ctype) (t2 (eql (find-class 'cons)))) (values t t))
(defmethod csubtypep tri/definite ((t1 cons-ctype) (t2 (eql (find-class 'sequence)))) (values t t))

(defmethod ctypep (object (ctype cons-ctype))
  (and (consp object)
       (ctypep (car object) (cons-ctype-car-type ctype))
       (ctypep (cdr object) (cons-ctype-cdr-type ctype))))

;;;; number.lisp

;;; Intervals (the meat!) are dealt with separately: see interval.lisp.

(in-package #:sandalphon.types)

;; NUMBER is just the cl class
(defun the-ctype-number () (find-class 'number))

(defclass real-ctype (#+s.t.prim primitive-ctype #-s.t.prim ctype)
  ((format :accessor real-ctype-format :initarg :format)
   (interval :accessor real-ctype-interval :initarg :interval)))

(defmethod csubtypep tri/definite ((t1 (eql (top))) (t2 real-ctype))
  (values nil t))
(defmethod csubtypep tri/definite ((t1 real-ctype) (t2 (eql (bottom))))
  (values nil t))

(defmethod csubtypep tri/definite ((t1 real-ctype) (t2 class))
  (cl:subtypep (real-ctype-format t1) t2))
(defcomm intersection/2 ((t1 real-ctype) (t2 class))
  ;; the case of one being a subtype of the other was already taken care of by the :around
  (bottom))

(defmethod csubtypep tri/definite ((t1 real-ctype) (t2 (eql (the-ctype-number))))
  (values t t))
(defmethod csubtypep tri/definite ((t1 (eql (the-ctype-number))) (t2 real-ctype))
  (values nil t))

;;; real numeric classes form a sublattice of the type system that's pretty easy to reason with
;;; ratio <: rational, integer <: rational, rational <: integer ∨ ratio
;;; single-float, ..., long-float <: float, and implementation-defined relations of the four
;;; rational <: real, float <: real
;;; All these relations are recognizable subtypes, implementation-wise. And since we'd like impl. float types,
;;;  we just reuse CL:SUBTYPEP.

(defun format-union (f1 f2)
  (cond ((cl:subtypep f1 f2) f2)
	((cl:subtypep f2 f1) f1)
	((and (cl:subtypep f1 'rational) (cl:subtypep f2 'rational)) 'rational)
	((and (cl:subtypep f1 'float) (cl:subtypep f2 'float)) 'float)
	;; FIXME: more general than necessary (e.g. (or integer float) is a /proper/ subtype of (or rational float)
	(t 'real)))

;; What do you call this kind of lattice...
(defun format-intersection (f1 f2)
  (cond ((cl:subtypep f1 f2) f1)
	((cl:subtypep f2 f1) f2)
	(t nil)))

(defun subformatp (f1 f2)
  (cl:subtypep f1 f2))

(defmethod union/2 ((t1 real-ctype) (t2 real-ctype))
  (let ((new-interval (interval-union/2 (real-ctype-interval t1) (real-ctype-interval t2)))
	(new-format (format-union (real-ctype-format t1) (real-ctype-format t2))))
    ;; FIXME: (or (integer 1 5) (rational 3 7)) could be reduced to (or (integer 1 3) (rational 3 7))
    ;; instead of (rational 1 7) as currently.
    (if new-interval
	(make-instance 'real-ctype :format new-format :interval new-interval)
	(call-next-method))))

(defmethod intersection/2 ((t1 real-ctype) (t2 real-ctype))
  (let ((new-interval (interval-intersection/2 (real-ctype-interval t1) (real-ctype-interval t2)))
	(new-format (format-intersection (real-ctype-format t1) (real-ctype-format t2))))
    (if (and new-format new-interval)
	(make-instance 'real-ctype :format new-format :interval new-interval)
	;; primitive, so (bottom)
	(call-next-method))))

(defmethod csubtypep tri/definite ((t1 real-ctype) (t2 real-ctype))
  (tri/if (subformatp (real-ctype-format t1) (real-ctype-format t2))
	  (values (subintervalp (real-ctype-interval t1) (real-ctype-interval t2)) t)
	  (values nil t)
	  (values nil nil)))

(defmethod ctypep (object (ctype real-ctype))
  (and (cl:typep object (real-ctype-format ctype))
       (in-interval-p object (real-ctype-interval ctype))))

;;; FIXME: unspecialized complex types are icky

(defclass complex-ctype (#+s.t.prim primitive-ctype #-s.t.prim ctype)
  ((part-type :accessor complex-ctype-part-type :initarg :part)))

(defmethod csubtypep tri/definite ((t1 (eql (top))) (t2 complex-ctype))
  (values nil t))
(defmethod csubtypep tri/definite ((t1 complex-ctype) (t2 (eql (bottom))))
  (values nil t))

(defmethod csubtypep tri/definite ((t1 complex-ctype) (t2 (eql (the-ctype-number))))
  (values t t))
(defmethod csubtypep tri/definite ((t1 (eql (the-ctype-number))) (t2 complex-ctype))
  (values nil t))

(defmethod union/2 ((t1 complex-ctype) (t2 complex-ctype))
  (cond ((not (slot-boundp t1 'part-type)) t1)
	((not (slot-boundp t2 'part-type)) t2)
	((ctype= (complex-ctype-part-type t1) (complex-ctype-part-type t2)) t1)
	(t (call-next-method))))

(defmethod intersection/2 ((t1 complex-ctype) (t2 complex-ctype))
  (cond ((not (slot-boundp t1 'part-type)) t2)
	((not (slot-boundp t2 'part-type)) t1)
	((ctype= (complex-ctype-part-type t1) (complex-ctype-part-type t2)) t1)
	(t (bottom))))

(defmethod ctypep (object (ctype complex-ctype))
  ;; FIXME: not 100% sure this is right. complexes are weird.
  (and (complexp object)
       (or (not (slot-boundp ctype 'part-type))
	   (and (ctypep (realpart object) (complex-ctype-part-type ctype))
		(ctypep (imagpart object) (complex-ctype-part-type ctype))))))

(defmethod csubtypep tri/definite ((t1 complex-ctype) (t2 complex-ctype))
  ;; not sure about this either
  (cond ((not (slot-boundp t1 'part-type))
	 (values (slot-boundp t2 'part-type) t))
	((not (slot-boundp t2 'part-type))
	 (values t t))
	(t (ctype= (complex-ctype-part-type t1) (complex-ctype-part-type t2)))))

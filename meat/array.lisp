;;;; array.lisp

;;;; array is, I think, strictly a representation type.
;;;; or maybe a specialized kind of structural type...?
;;;; it's a bit confusing, due to specialized arrays.
;;;; arrays are... exact, I suppose.

;;; take this code:
;;; (when (< 4 x 7) (let ((y (make-array x :initial-element 23))) ...))
;;; The type of y can be reasonably inferred there to be *(array (eql 23) ((integer 4 (7))))
;;; but this is not really expressible in vanilla CL (though of course allowed in compiler internals):
;;; (eql 23) is only the "expressed" type, not "actual" type,
;;; and the dimension spec has to be made up of integers, not types.
;;; I think it would be sensible to do the following:
;;; 1) Define a notion of "representation type", for types corresponding to "actual" values; e.g. the usual ARRAY type.
;;; 2) typep, subtypep, etc. work as normal on these.
;;; 3) "Types" are separate (or rather a superclass), still just sets (and possibly ones with uncomputable membership!)
;;; 4) type-of returns a representation type, not just any type.
;;;    (otherwise (type-of anything) => '(member anything), assuming type-of is supposed to get the "smallest" set)
;;;    (unlike CL, which defines a variety of "inelegant" (practical) restrictions on type-of's results to avoid this)
;;; 5) representation types can be mostly implementation-defined, but also including useful general ones like ARRAY.

;;; also, about function types, and other things you can't discriminate on, ... I dunno

(in-package #:sandalphon.types)

;;;; FIXME: simplicity is fucked

(deftype array-rank () '(integer 0 (#.array-rank-limit)))
(defun array-rank-p (n)
  (and (positive-integer-p n) (< n #.array-rank-limit)))
;; these are exported by CL, so we have to do some shadowmagic.  ew.
(declaim (inline array-rank))
(defun array-rank (array) (cl:array-rank array))

(defclass array-ctype (#+s.t.prim primitive-ctype #-s.t.prim ctype)
  ((element-type :accessor array-ctype-element-type :initarg :element-type
		 :initform '*)
   (simplicity :accessor array-ctype-simplicity :initarg :simple
	       :initform nil)
   (dimensions :accessor array-ctype-dimensions :initarg :dimensions
	       :initform '*)))

(defmethod csubtypep tri/definite ((t1 (eql (top))) (t2 array-ctype))
  (values nil t))
(defmethod csubtypep tri/definite ((t1 array-ctype) (t2 (eql (bottom))))
  (values nil t))

;;; ads = array-dimensions-spec

(defun canonicalize-ads (spec)
  ;; really more of a check-type...
  (etypecase spec
    ((eql *) spec)
    (array-rank spec) ; original idea was to make a list of *'s, but array-rank-limit can be pretty high.
    (list (mapcar (lambda (dim)
		    (check-type dim (or alexandria:array-length (eql *)) "an array dimension specifier")
		    dim)
		  spec))))

;;; this used to be <= but that's not really right.
;;; <: on the other hand is common notation for subtype, for whatever perverse reason.

(defun ads-<\: (spec1 spec2)
  (flet ((dim-<\: (dim1 dim2)
	   (or (eq dim2 '*)
	       (and (not (eq dim1 '*))
		    ;; (subtypep '(array * (1)) '(array * (2))) => NIL
		    (= dim1 dim2)))))
    (etypecase spec2
      ((eql *) t)
      (array-rank
       (etypecase spec1
	 ((eql *) nil)
	 (array-rank (<= spec1 spec2))
	 (list (= (length spec1) spec2))))
      (list (and (listp spec1)
		 (= (length spec1) (length spec2))
		 (every #'dim-<\: spec1 spec2))))))

(defun ads-mix (spec1 spec2 which)
  "Given two array dimensions specifiers and an operation (:union or :intersection), 
returns either a specifier for the result of the operation, or NIL if there isn't one (e.g. 1 and 2)."
  ;; check-type evaluates its type-string argument.  Wacky!
  (check-type which (member :union :intersection) (format nil "either ~s or ~s" :union :intersection))
  (macrolet ((which (v1 v2) `(case which ((:union) ,v1) ((:intersection) ,v2))))
    (cond ((eql spec1 '*)
	   (which spec1 spec2))
	  ((eql spec2 '*)
	   (which spec2 spec1))
	  ((typep spec1 'array-rank)
	   (etypecase spec2
	     (list (if (= (length spec2) spec1)
		       (which spec1 spec2)
		       nil))
	     (array-rank (if (= spec1 spec2)
			   spec1
			   nil))))
	  ((array-rank-p spec2)
	   ;; we already know t1 isn't an array-rank, that would have triggered the above.
	   (if (= (length spec1) spec2)
	       (which spec2 spec1)
	       nil))
	  ;; they're both lists, so
	  ((= (length spec1) (length spec2))
	   (mapcar (lambda (d1 d2)
		     (etypecase d1
		       ((eql *) (which d1 d2))
		       (array-length
			(etypecase d2
			  ((eql *) (which d2 d1))
			  (array-length (if (= d1 d2)
					  d1 ; doesn't matter which
					  (return-from ads-mix nil)))))))
		 spec1
		 spec2))
	(t nil))))

(defgeneric sub-element-type-p (t1 t2))

(defmethod sub-element-type-p (t1 (t2 (eql '*)))
  (declare (ignore t1))
  t)
(defmethod sub-element-type-p ((t1 (eql '*)) t2)
  (declare (ignore t2))
  nil)
(defmethod sub-element-type-p ((t1 (eql '*)) (t2 (eql '*)))
  t)

(defmethod sub-element-type-p ((t1 ctype) (t2 ctype))
  ;; see below comment in a-e-u's method
  (values (ctype= t1 t2)))
(defmethod sub-element-type-p ((t1 class) (t2 class))
  (values (and (cl:subtypep t1 t2) (cl:subtypep t2 t1))))

(defclass array-element-union () ; subclass ctype?  this is kind of a magical mark rather than a type...
  ((type :accessor array-element-union-type :initarg :type))
  (:documentation "Denotes the union of a type's subtypes for the purposes of array element types.
For example, STRING is an array type whose element type is an ARRAY-ELEMENT-UNION of CHARACTER."))

(defmethod sub-element-type-p (t1 (t2 array-element-union))
  ;; not sure what to do with unrecognizable subtypes here.
  ;; Probably not a big deal, however, since everything's array-upgraded and so probably easy.
  (values (csubtypep t1 (array-element-union-type t2))))
(defmethod sub-element-type-p ((t1 array-element-union) t2) (declare (ignore t2)) nil)
(defmethod sub-element-type-p ((t1 array-element-union) (t2 (eql '*))) t)
(defmethod sub-element-type-p ((t1 array-element-union) (t2 array-element-union))
  (values (csubtypep (array-element-union-type t1) (array-element-union-type t2))))

(defmethod ctypep (object (type array-ctype))
  (and (arrayp object)
       (sub-element-type-p (array-element-type object) (array-ctype-element-type type))
       (ads-<\: (array-dimensions object) (array-ctype-dimensions type))))

(defmethod csubtypep tri/definite ((t1 array-ctype) (t2 array-ctype))
  (with-accessors ((t1s array-ctype-simplicity) (t1et array-ctype-element-type) (t1d array-ctype-dimensions)) t1
    (with-accessors ((t2s array-ctype-simplicity) (t2et array-ctype-element-type) (t2d array-ctype-dimensions)) t2
      (values (and (sub-element-type-p t1et t2et)
		   (ads-<\: t1d t2d)
		   (or (not (sub-element-type-p t2et t1et)) (not t2s) t1s))
	      t))))

(defmethod csubtypep tri/definite ((t1 array-ctype) (t2 (eql (find-class 'sequence))))
  (values t t))

(defmethod union/2 ((t1 array-ctype) (t2 array-ctype))
  (if (ctype= (array-ctype-element-type t1) (array-ctype-element-type t2))
      (make-instance (class-of (if (csubtypep t1 t2) t1 t2)) ; the smaller type
		     :element-type (array-ctype-element-type t1)
		     :dimensions (or (ads-mix (array-ctype-dimensions t1) (array-ctype-dimensions t2) :union)
				     (return-from union/2 (call-next-method))))
      (call-next-method)))

(defmethod intersection/2 ((t1 array-ctype) (t2 array-ctype))
  (if (ctype= (array-ctype-element-type t1) (array-ctype-element-type t2))
      (make-instance (class-of (if (csubtypep t1 t2) t2 t1)) ; the larger type
		     :element-type (array-ctype-element-type t1)
		     :dimensions (or (ads-mix (array-ctype-dimensions t1) (array-ctype-dimensions t2) :intersection)
				     (return-from intersection/2 (call-next-method))))
      ;; next method = default for primitive-type = (bottom-type).  just to reduce dependencies
      (call-next-method)))

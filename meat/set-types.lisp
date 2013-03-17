;;;; set-types.lisp
;;;; types using types as sets: OR (union), AND (intersection), NOT (negation (wrt the universe of types (T)))
;;;; most of these are necessarily only sometimes computable.
;;;; specialized types should define union/2 etc. methods if possible.

(in-package #:sandalphon.types)

(defclass compound-ctype (ctype)
  ((components :accessor compound-ctype-components :initarg :components)))

(defclass union-ctype (compound-ctype) ())

;; FIXME: move this?
(defmethod union/2 (t1 t2)
  (make-instance 'union-ctype :components (list t1 t2)))

(defmethod union/2 ((t1 union-ctype) (t2 union-ctype))
  (apply #'ctype-union (append (compound-ctype-components t1) (compound-ctype-components t2))))

(defcomm intersection/2 ((t1 union-ctype) t2)
  (apply #'ctype-union (mapcar (curry #'ctype-intersection t2) (compound-ctype-components t1))))

(defmethod csubtypep tri/definite ((t1 union-ctype) t2)
  (tri/every (rcurry #'csubtypep t2) (compound-ctype-components t1)))
(defmethod csubtypep tri/definite (t1 (t2 union-ctype))
  ;; if we're not sure it is a subtype we have to say we're not sure - there's no "sure it's not" here.
  ;; consider for example (subtypep '(integer 3 7) '(or (integer 0 5) (integer 6 9)))
  ;; if the integer ranges are left as a dumb union, we will tell that both ranges are surely not subtypes of the t1 range,
  ;; and yet of course the combination subsumes (integer 3 7).
  (tri/or (tri/some (curry #'csubtypep t1) (compound-ctype-components t2))
	  (values nil nil)))

(defmethod ctypep (object (ctype union-ctype))
  (some (curry #'ctypep object) (compound-ctype-components ctype)))

(defmethod negate-ctype ((ctype union-ctype))
  (apply #'ctype-intersection (mapcar #'negate-ctype (compound-ctype-components ctype))))

(defclass intersection-ctype (compound-ctype) ())

;; FIXME: move this?
(defmethod intersection/2 (t1 t2)
  (make-instance 'intersection-ctype :components (list t1 t2)))

(defmethod intersection/2 ((t1 intersection-ctype) (t2 intersection-ctype))
  (apply #'ctype-intersection (append (compound-ctype-components t1) (compound-ctype-components t2))))

(defcomm union/2 ((t1 intersection-ctype) t2)
  (apply #'ctype-intersection (mapcar (curry #'ctype-union t2) (compound-ctype-components t1))))

(defmethod csubtypep tri/definite ((t1 intersection-ctype) t2)
  ;; (subtypep '(and (integer 0 6) (integer 3 9)) '(integer 4 5))
  (tri/or (tri/some (rcurry #'csubtypep t2) (compound-ctype-components t1))
	  (values nil nil)))
(defmethod csubtypep tri/definite (t1 (t2 intersection-ctype))
  (tri/every (curry #'csubtypep t1) (compound-ctype-components t2)))

(defmethod ctypep (object (ctype intersection-ctype))
  (every (curry #'ctypep object) (compound-ctype-components ctype)))

(defmethod negate-ctype ((ctype intersection-ctype))
  (apply #'ctype-union (mapcar #'negate-ctype (compound-ctype-components ctype))))

(defclass negation-ctype (ctype)
  ((negated :accessor negation-ctype-ctype :initarg :neg)))

(defmethod negate-ctype (ctype)
  (make-instance 'negation-ctype :neg ctype))

(defmethod intersection/2 ((t1 negation-ctype) (t2 negation-ctype))
  (make-instance 'negation-ctype :neg (ctype-intersection (negation-ctype-ctype t1) (negation-ctype-ctype t2))))

(defcomm intersection/2 (t1 (t2 negation-ctype))
  (if (csubtypep t1 (negation-ctype-ctype t2))
      (bottom)
      (call-next-method)))

(defmethod union/2 ((t1 negation-ctype) (t2 negation-ctype))
  (make-instance 'negation-ctype :neg (ctype-union (negation-ctype-ctype t1) (negation-ctype-ctype t2))))

(defcomm union/2 ((t1 negation-ctype) t2)
  (if (csubtypep (negation-ctype-ctype t1) t2)
      (top)
      (call-next-method)))

(defmethod negate-ctype ((ctype negation-ctype))
  (negation-ctype-ctype ctype))

(defmethod csubtypep tri/definite ((t1 negation-ctype) (t2 negation-ctype))
  ;; contraposition!
  (csubtypep (negation-ctype-ctype t2) (negation-ctype-ctype t1)))

(defmethod csubtypep tri/definite (t1 (t2 negation-ctype))
  (let ((neg (negation-ctype-ctype t2)))
    (tri/if (csubtypep t1 neg)
	    ;; surely not (subtypep 'integer '(not real)), but (subtypep 'nil '(not anything))
	    (bottom-type-p t1)
	    ;; surely not (subtypep 'real '(not integer)), but surely (subtypep 'real '(not string))
	    ;; but also surely (subtypep 'real '(not nil)) and (subtypep 't '(not nil))
	    (tri/implies (csubtypep neg t1) (bottom-type-p neg))
	    (values nil nil))))
  
(defmethod csubtypep tri/definite ((t1 negation-ctype) t2)
  (let ((neg (negation-ctype-ctype t1)))
    (tri/if (csubtypep neg t2)
	    ;; surely not (subtypep '(not integer) 'real), surely not (subtypep '(not integer) 'integer)
	    ;; surely not (subtypep '(not nil) 'real), surely (subtypep '(not nil) 't), surely (subtypep '(not t) 't)
	    ;; surely not (subtypep '(not nil) 'nil)
	    (tri/or (top-type-p neg)
		    (tri/and (bottom-type-p neg) (top-type-p t2)))
	    ;; surely not (subtypep '(not real) 'integer), surely not (subtypep '(not string) 'integer)
	    ;; surely (subtypep '(not t) 'nil), surely (subtypep '(not t) 'real), surely not (subtypep '(not real) 'nil)
	    (top-type-p neg)
	    (values nil nil))))

(defmethod ctypep (object (ctype negation-ctype))
  (not (ctypep object (negation-ctype-ctype ctype))))

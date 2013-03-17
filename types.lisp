;;;; types.lisp

(in-package #:sandalphon.types)

;;;; Notes: Types are (often infinite) sets.
;;;; Necessary operations on them are union, intersection, subtypep (subset), typep (membership).
;;;; Negation (wrt T) too, though that's a bit tricky for a variety of reasons.

;;;; I've decided to rename everything "ctypes" to avoid as much as possible confusion with type specifiers,
;;;;  and CL types.

(defclass ctype () ())

(defgeneric union/2 (t1 t2)
  (:documentation "Given two types, return a new type for their union.")
  (:method :around (t1 t2)
     (cond ((csubtypep t1 t2) t2)
	   ((csubtypep t2 t1) t1)
	   (t (call-next-method)))))
	 
(defgeneric intersection/2 (t1 t2)
  (:documentation "Given two types, return a new type for their intersection.")
  (:method :around (t1 t2)
     (cond ((csubtypep t1 t2) t1)
	   ((csubtypep t2 t1) t2)
	   (t (call-next-method)))))

(define-method-combination tri/definite :identity-with-one-argument t)

(defgeneric csubtypep (t1 t2)
  (:method-combination tri/definite)
  (:documentation "As cl:subtypep (excepting the lack of environment), but for ctypes."))
(defmethod csubtypep tri/definite ((t1 ctype) (t2 ctype)) ; ctypes instead of unspecialized, for some semblance of safety
  (if (eql t1 t2) ; we at least have reflexivity!
      (values t t)
      (values nil nil)))

(defgeneric ctypep (object ctype)
  (:argument-precedence-order ctype object)
  (:documentation "As cl:typep (excepting the lack of environment), but for ctypes."))
(defgeneric negate-ctype (ctype)
  ;; if at all possible, avoid NOT types, and have this return some usual type
  (:documentation "Return a new type containing all objects not contained by CTYPE."))

(defun ctype-union (&rest types)
  ;; complicated version, to combine more aggressively
  #+fancyass
  (cond ((null types) (bottom-type))
	((null (rest types)) (first types))
	((null (rest (rest types))) (union/2 (first types) (second types)))
	(t
	 (let ((default (load-time-value (list (find-method #'union/2 nil (mapcar #'find-class '(t t)) t))))
	       (testee (first types)))
	   (flet ((good-type-p (type)
		    (not (equal (compute-applicable-methods #'union/2 (list testee type)) default))))
	     #+faster?
	     (let* ((head (or (member-if #'good-type-p types :start 1) (rest types)))
		    (good (car head)))
	       (setf (car head) (union/2 testee good))
	       (apply #'ctype-union (rest types)))
	     (let ((good (or (find-if #'good-type-p types :start 1) (second types))))
	       ;; tail call "for speed"
	       ;; is delete okay? I don't know
	       (apply #'ctype-union (union/2 testee good) (delete good (rest types) :count 1)))))))
  ;; simple version
  (reduce #'union/2 types))
(defun ctype-intersection (&rest types) (reduce #'intersection/2 types))

(defun ctype= (t1 t2)
  "Returns two true values if T1 and T2 are recognizably type equivalent, a true and a false if they're recognizably not,
and two falses if neither can be determined."
  (tri/and (csubtypep t1 t2)
	   (csubtypep t2 t1)))

;; not sure how to properly express commutativity well, bleh.
(defmacro defcomm (name lambda-list &body body)
  `(progn (defmethod ,name ,lambda-list ,@body)
	  (defmethod ,name ,(reverse lambda-list) ,@body)))

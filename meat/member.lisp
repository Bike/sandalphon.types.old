
;;;; member.lisp

;;;; (member ...) and (not (member ...)) types, MEMBER-CTYPE and EXCLUSION-CTYPE respectively.
;;;; TODO: give 'em a parent class analogous to compound-ctype-components, maybe.

(in-package #:sandalphon.types)

;;; Member type.
;;; TODO: Should be faster with a proper finite set data structure.
(defclass member-ctype (ctype)
  ((objects :accessor member-objects :initarg :objects)))

(defmethod union/2 ((t1 member-ctype) (t2 member-ctype))
  (make-instance 'member-ctype :objects (union (member-objects t1) (member-objects t2))))
#+subinfinite
(defcomm union/2 ((t1 member-ctype) t2)
  (let ((outcasts (remove-if-not (rcurry #'ctypep t2) (member-objects t1))))
    (if outcasts
	;; (or (member 2) symbol)
	(make-instance 'outcast-type :main t2 :objects outcasts)
	;; (or (member hello) symbol)
	t2)))
(defmethod intersection/2 ((t1 member-ctype) (t2 member-ctype))
  (make-instance 'member-ctype :objects (intersection (member-objects t1) (member-objects t2))))
(defcomm intersection/2 ((t1 member-ctype) t2)
  (let ((outcasts (remove-if-not (rcurry #'ctypep t2) (member-objects t1))))
    (if outcasts
	;; (and (member 2) symbol)
	(bottom)
	;; (and (member 2) integer)
	t1)))
(defmethod csubtypep tri/definite ((t1 member-ctype) (t2 member-ctype))
  (values (subsetp (member-objects t1) (member-objects t2)) t))
(defmethod csubtypep tri/definite ((t1 member-ctype) t2)
  (values (every (rcurry #'ctypep t2) (member-objects t1)) t))
(defmethod ctypep (object (type member-ctype))
  (member object (member-objects type)))
(defmethod negate-ctype ((type member-ctype))
  (if (null (member-objects type))
      (top)
      (make-instance 'exclusion-ctype :objects (member-objects type))))

;;; (member) and nil are the same type.
(defmethod csubtypep tri/definite ((t1 member-ctype) (t2 (eql (bottom))))
  (values (null (member-objects t1)) t))
(defmethod csubtypep tri/definite ((t1 (eql (bottom))) (t2 member-ctype))
  (values (null (member-objects t2)) t))
;;; no need for an EQL type, that can just expand to a MEMBER without real trouble

(defclass exclusion-ctype (ctype)
  ((objects :accessor exclusion-objects :initarg :objects))
  (:documentation "The type of all objects not in a given finite set, i.e., the opposite of MEMBER."))
;;; The set of all objects not part of some finite set.  Dual to member in enough ways that I should probably automate it
(defmethod union/2 ((t1 exclusion-ctype) (t2 exclusion-ctype))
  (make-instance 'exclusion-ctype :objects (intersection (exclusion-objects t1) (exclusion-objects t2))))
(defcomm union/2 ((t1 exclusion-ctype) t2)
  (let ((incasts (remove-if-not (rcurry #'ctypep t2) (exclusion-objects t1))))
    (if incasts
	;; (or (not (member 2)) symbol)
	t1
	;; (or (not (member 2)) integer)
	(top))))
(defmethod intersection/2 ((t1 exclusion-ctype) (t2 exclusion-ctype))
  (make-instance 'exclusion-ctype :objects (cl:union (exclusion-objects t1) (exclusion-objects t2))))
#+subinfinite
(defcomm intersection/2 ((t1 exclusion-ctype) t2)
  (let ((incasts (remove-if-not (rcurry #'ctypep t2) (exclusion-objects t1))))
    (if incasts
	;; (and (not (member 2)) symbol)
	t2
	;; (and (not (member 2)) integer)
	(make-instance 'incast-type :main t2 :objects incasts))))
(defmethod csubtypep tri/definite ((t1 exclusion-ctype) (t2 exclusion-ctype))
  (values (subsetp (exclusion-objects t2) (exclusion-objects t1)) t))
(defmethod csubtypep tri/definite (t1 (t2 exclusion-ctype)) ; careful about arg precedence
  (values (notany (rcurry #'ctypep t1) (exclusion-objects t2)) t))
(defmethod ctypep (object (type exclusion-ctype))
  (not (member object (exclusion-objects type))))
(defmethod negate-ctype ((type exclusion-ctype))
  (if (null (exclusion-objects type))
      (bottom)
      (make-instance 'member-ctype :objects (exclusion-objects type))))

;; (not (member)) = t
(DEFMETHOD CSUBTYPEP tri/definite ((T1 (EQL (TOP))) (T2 EXCLUSION-CTYPE))
  (VALUES (NULL (EXCLUSION-OBJECTS T2)) T))
;; (subtypep exclusion t) is already handled, of course

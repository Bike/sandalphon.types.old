;;;; unparse.lisp

(in-package #:sandalphon.types)

(defgeneric type-specifier (ctype))

(define-condition unknown-unparse (error) ()) ; TODO add slots

(defmethod no-applicable-method ((gf (eql #'type-specifier)) &rest args)
  (declare (ignore args))
  (restart-case (error 'unknown-unparse)
    (use-value (v) v)))

(defmethod print-object ((object ctype) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (handler-bind ((unknown-unparse (lambda (c) (declare (ignore c)) (use-value "<unknown specifier>"))))
	     (type-specifier object))
	   stream)))

(defmethod type-specifier ((ctype class)) (class-name ctype))

(defmethod type-specifier ((ctype named-ctype)) (named-ctype-name ctype))

(defmethod type-specifier ((ctype (eql (top)))) 't)
(defmethod type-specifier ((ctype (eql (bottom)))) 'nil)

(defmethod type-specifier ((ctype member-ctype))
  `(member ,@(member-objects ctype)))
#+exclusion
(defmethod type-specifier ((ctype exclusion-ctype))
  `(not (member ,@(exclusion-objects ctype))))

(defmethod type-specifier ((ctype intersection-ctype))
  `(and ,@(mapcar #'type-specifier (compound-ctype-components ctype))))

(defmethod type-specifier ((ctype union-ctype))
  `(or ,@(mapcar #'type-specifier (compound-ctype-components ctype))))

(defmethod type-specifier ((ctype negation-ctype))
  `(not ,(type-specifier (negation-ctype-ctype ctype))))

(defmethod type-specifier ((ctype satisfies-ctype))
  `(satisfies ,(satisfies-ctype-predicate ctype)))

(defmethod type-specifier ((ctype (eql (the-ctype-number)))) 'number)
(defmethod type-specifier ((ctype real-ctype))
  (list* (real-ctype-format ctype) (interval-specifier (real-ctype-interval ctype))))
(defmethod type-specifier ((ctype complex-ctype))
  (if (slot-boundp ctype 'part-type)
      `(complex ,(complex-ctype-part-type ctype))
      'complex))

(defmethod type-specifier ((ctype cons-ctype))
  (let ((car (cons-ctype-car-type ctype))
	(cdr (cons-ctype-cdr-type ctype)))
    (cond ((eql car (top))
	   (if (eql cdr (top))
	       'cons
	       `(cons * ,(type-specifier cdr))))
	  ((eql cdr (top))
	   `(cons ,(type-specifier car)))
	  (t `(cons ,(type-specifier car) ,(type-specifier cdr))))))

(defmethod type-specifier ((ctype array-ctype))
  `(,(if (array-ctype-simplicity ctype)
	 'simple-array
	 'array)
     ,(let ((acet (array-ctype-element-type ctype)))
	(if (eq acet '*)
	    acet
	    (type-specifier acet)))
     ,(array-ctype-dimensions ctype)))

;; noooooot sure about this
(defmethod type-specifier ((not-a-ctype array-element-union))
  not-a-ctype)

(defmethod type-specifier ((ctype values-ctype))
  (with-accessors ((req values-ctype-required) (opt values-ctype-optional)
		   (rest values-ctype-rest) (aok-p values-ctype-aok-p))
      ctype
    `(values ,@(mapcar #'type-specifier req)
	     ,@(when opt `(&optional ,@(mapcar #'type-specifier opt)))
	     ,@(when rest `(&rest ,(type-specifier rest)))
	     ,@(when aok-p `(&allow-other-keys)))))

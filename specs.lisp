;;;; specs.lisp
;;;; standard type specifiers

(in-package #:sandalphon.types)

;;; keywords (are weird)

(deftypemacro keyword () '(and symbol (satisfies keywordp)))

;;; conses

(deftypemacro cons (&optional car cdr &environment env)
  (when (eq car '*) (setf car 't))
  (when (eq cdr '*) (setf cdr 't))
  (make-instance 'cons-ctype :car (specifier-type car env) :cdr (specifier-type cdr env)))

(deftypemacro atom () '(not cons))
(deftypemacro null () '(eql nil))
(deftypemacro list () '(or null cons))

;;; characters (has to be before strings)

;; we can't just use the classes here, since standard-char and base-char aren't classes.
(%define-unparametric-type standard-char (the-ctype-standard-char))
(%define-unparametric-type base-char (the-ctype-base-char))

(deftypemacro extended-char () '(and character (not base-char)))

;; use the class for character

;;; arrays

(deftypemacro array (&optional element-type dimension-spec &environment env)
  ;; FIXME: u-a-e-t integration
  (let ((et (if (eq element-type '*)
		'*
		(specifier-type (upgraded-array-element-type element-type env) env))))
    (make-instance 'array-ctype
		   :dimensions (canonicalize-ads dimension-spec)
		   :element-type et)))
(deftypemacro simple-array (&optional element-type dimension-spec &environment env)
  ;; FIXME: u-a-e-t integration
  (let ((et (if (eq element-type '*)
		'*
		(specifier-type (upgraded-array-element-type element-type env) env))))
    (make-instance 'array-ctype
		   :simple t
		   :dimensions (canonicalize-ads dimension-spec)
		   :element-type et)))

(deftypemacro string (&optional size)
  (make-instance 'array-ctype
		 :dimensions (canonicalize-ads (list size))
		 ;; note that we use ctype and ignore the local environment.
		 :element-type (make-instance 'array-element-union :type (ctype character))))
(deftypemacro simple-string (&optional size)
  (make-instance 'array-ctype
		 :simple t
		 :dimensions (canonicalize-ads (list size))
		 ;; note that we use ctype and ignore the local environment.
		 :element-type (make-instance 'array-element-union :type (ctype character))))

(deftypemacro vector (&optional element-type size)
  `(array ,element-type (,size)))
(deftypemacro simple-vector (&optional element-type size)
  `(simple-array ,element-type (,size)))

(deftypemacro bit-vector (&optional size)
  `(array bit (,size)))
(deftypemacro simple-bit-vector (&optional size)
  `(simple-array bit (,size)))

(deftypemacro base-string (&optional size)
  `(array base-char (,size)))
(deftypemacro simple-base-string (&optional size)
  `(simple-array base-char (,size)))

;;; numbers

(%define-unparametric-type number (the-ctype-number))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-standard-real-ctype (format &optional (low '*) (high '*))
    (assert (typep low `(or ,format (cons ,format null) (eql *))))
    (assert (typep high `(or ,format (cons ,format null) (eql *))))
    (make-instance 'real-ctype
		   :format format
		   :interval (make-interval low high))))

(macrolet ((num (format)
	     `(setf (typexpander ',format) (lambda (form env)
					     (declare (ignore env))
					     (apply #'make-standard-real-ctype (ensure-list form)))))
	   (nums (&rest formats)
	     `(eval-when (:compile-toplevel :load-toplevel :execute)
		,@(mapcar (curry #'list 'num) formats))))
  (nums integer rational
	ratio ; having bounds is nonstandard but i think permissible
	short-float single-float double-float long-float float
	real))

(deftypemacro mod (n)
  (check-type n positive-integer)
  `(integer 0 (,n)))

;; from SBCL
(deftypemacro signed-byte (&optional s)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 0))
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(1- bound))))
	(t (error "bad size specified for UNSIGNED-BYTE type specifier: ~S" s))))

;; from SBCL
(deftypemacro unsigned-byte (&optional s)
  (cond ((eq s '*) '(integer 0))
        ((and (integerp s) (> s 0))
         `(integer 0 ,(1- (ash 1 s))))
        (t
         (error "bad size specified for UNSIGNED-BYTE type specifier: ~S" s))))

(deftypemacro bit () '(unsigned-byte 1))

(deftypemacro complex (&optional spec &environment env)
  (if (eq spec '*)
      (make-instance 'complex-ctype)
      ;; FIXME: u-c-p-t integration
      (make-instance 'complex-ctype :part (upgraded-complex-part-type spec env))))

(deftypemacro fixnum () '(integer #.most-negative-fixnum #.most-positive-fixnum))
(deftypemacro bignum () '(and integer (not fixnum)))

;;; tnil

(%define-unparametric-type nil (bottom))

;;; setshit

(deftypemacro and (&whole spec &rest specs &environment env)
  (unless (listp spec) (error "There is no list form for specifier ~s" (first spec)))
  (cond ((null specs) (top))
	((null (rest specs)) (specifier-type (first specs) env))
	(t (apply #'ctype-intersection (mapcar (rcurry #'specifier-type env) specs)))))
(deftypemacro or (&whole spec &rest specs &environment env)
  (unless (listp spec) (error "There is no list form for specifier ~s" (first spec)))
  (cond ((null specs) (bottom))
	((null (rest specs)) (specifier-type (first specs) env))
	(t (apply #'ctype-union (mapcar (rcurry #'specifier-type env) specs)))))

(deftypemacro not (specifier &environment env) (negate-ctype (specifier-type specifier env)))

(deftypemacro satisfies (predname)
  (check-type predname symbol "a possible name for a global predicate function")
  (make-instance 'satisfies-ctype :predicate predname))

;;; member

(deftypemacro member (&whole spec &rest objects)
  (unless (listp spec) (error "There is no list form for specifier ~s" (first spec)))
  (make-instance 'member-ctype :objects objects))

(deftypemacro eql (object)
  (make-instance 'member-ctype :objects (list object)))

;;; functions (oh shit)

(deftypemacro compiled-function () '(and function (satisfies compiled-function-p)))

;;; miscellaneous things, which are actually most of them

#+(or)
(macrolet ((has-class (name)
	     `(%define-unparametric-type ,name (find-class ',name)))
	   (all (&rest names) `(progn ,@(mapcar (curry #'list 'has-class) names))))
  ;; 4-8, in 4.3.7, seems to say that condition types have associated classes.
  (all broadcast-stream concatenated-stream echo-stream file-stream stream
       string-stream synonym-stream two-way-stream)
  (all arithmetic-error cell-error condition control-error division-by-zero end-of-file error
       file-error floating-point-inexact floating-point-invalid-operation floating-point-overflow
       floating-point-underflow package-error parse-error print-not-readable program-error
       reader-error serious-condition simple-condition simple-error simple-type-error simple-warning
       )
  (has-class arithmetic-error)
  (has-class broadcast-stream)
  (has-class built-in-class)
  (has-class cell-error)
  (has-class class)
  (has-class concatenated-stream)
  (has-class condition)
  (has-class control-error)
  (has-class division-by-zero)
  (has-class echo-stream)
  (has-class ))

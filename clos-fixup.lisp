;;;; clos-fixup.lisp
;;;; manufacture appropriate relations with CL classes

(in-package #:sandalphon.types)

(defmacro define-clos-synonym (type-specifier clname)
  (let ((class-spec `(eql (find-class ',clname t)))
	(ctype `(specifier-type ',type-specifier)))
    `(progn
       (defmethod csubtypep tri/definite (t1 (t2 ,class-spec)) (csubtypep t1 ,ctype))
       (defmethod csubtypep tri/definite ((t1 ,class-spec) t2) (csubtypep ,ctype t2))
       ;; dumb KLUDGE to avoid infinite recursion
       (defmethod csubtypep tri/definite ((t1 ,class-spec) (t2 ,class-spec)) (values t t))
       (defmethod union/2 ((t1 ,class-spec) (t2 ,class-spec)) ,ctype)
       (defcomm union/2 (t1 (t2 ,class-spec)) (union/2 t1 ,ctype))
       (defmethod intersection/2 ((t1 ,class-spec) (t2 ,class-spec)) ,ctype)
       (defcomm intersection/2 (t1 (t2 ,class-spec)) (intersection/2 t1 ,ctype))
       (defmethod negate-ctype ((type ,class-spec)) (negate-ctype ,ctype)))))

(define-clos-synonym cons cons)
(define-clos-synonym list list)
(define-clos-synonym null null)

(define-clos-synonym array array)
(define-clos-synonym bit-vector bit-vector)
(define-clos-synonym string string)
(define-clos-synonym vector vector)

(define-clos-synonym real real)
(define-clos-synonym float float)
(define-clos-synonym rational rational)
(define-clos-synonym ratio ratio)
(define-clos-synonym integer integer)

(define-clos-synonym complex complex)
;;; FIXNUM and BIGNUM are nonstandard but widely supported.
#+#.(cl:if (cl:find-class 'cl:fixnum cl:nil) '(and) '(or))
(define-clos-synonym fixnum fixnum)

#+#.(cl:if (cl:find-class 'cl:bignum cl:nil) '(and) '(or))
(define-clos-synonym bignum bignum)

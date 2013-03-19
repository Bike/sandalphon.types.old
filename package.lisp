(defpackage #:sandalphon.types
  (:use #:cl #:alexandria)
  ;; from CL, unexported
  (:shadow #:array-rank)
  ;; trivalogic.lisp
  (:export #:tri/if #:tri/not #:tri/and
	   #:tri/or #:tri/definite #:tri/implies
	   #:tri/every #:tri/some)
  ;; types.lisp
  (:export #:ctype-union #:ctype-intersection #:negate-ctype
	   #:union/2 #:intersection/2
	   #:csubtypep #:ctypep
	   #:ctype=)
  ;; parse.lisp
  (:export #:specifier-type #:ctype #:known-type-p
	   #:typexpander
	   #:typexpand #:typexpand-1
	   #:typemacrolet #:deftypemacro
	   #:unknown-type)
  ;; unparse.lisp
  (:export #:type-specifier #:unknown-unparse)
  ;; interval
  (:export #:interval #:interval-low #:interval-high #:interval-openness
	   #:interval-specifier
	   #:subintervalp #:in-interval-p)
  ;;; meat
  ;; named
  (:export #:named-ctype #:named-ctype-name)
  ;; tnil
  (:export #:top #:bottom #:top-type-p #:bottom-type-p)
  ;; set-types
  (:export #:compound-ctype #:union-ctype #:intersection-ctype
	   #:compound-ctype-components)
  (:export #:negation-ctype #:negation-ctype-ctype)
  ;; satisfies
  (:export #:satisfies-ctype #:satisfies-ctype-predicate)
  ;; member
  (:export #:member-ctype #:member-objects)
  (:export #:exclusion-ctype #:exclusion-objects)
  ;; number
  (:export #:real-ctype #:real-ctype-format #:real-ctype-interval)
  (:export #:complex-ctype #:complex-ctype-part-type)
  ;; nothing from char
  ;; cons
  (:export #:cons-ctype #:cons-ctype-car-type #:cons-ctype-cdr-type)
  ;; nothing from clos
  ;; array
  (:export #:array-ctype
	   #:array-ctype-element-type #:array-ctype-simplicity #:array-ctype-dimensions)
  (:export #:sub-element-type-p
	   #:array-element-union #:array-element-union-type)
  ;; values
  (:export #:values-ctype
	   #:values-ctypep))

;;;; parse.lisp

(in-package #:sandalphon.types-test)

(def-suite parsing :in all)

(in-suite parsing)

(test all-atomics
  (dolist (spec *standard-atomic-type-specifiers*)
    (handler-case
	(pass "~s was parsed as ~s" spec (specifier-type spec))
      (unknown-type ()
	(fail "~s couldn't be parsed" spec)))))

;;;; standard-atomic-subtypery.lisp
;;;; Test every (atomic!) type hierarchy relationship in the spec, essentially.
;;;; Presently a lot of them parse as classes so they're pretty redundant,
;;;;  assuming a conforming implementation.

(in-package #:sandalphon.types-test)

(def-suite standard-atomic-subtypery :in all)

(in-suite standard-atomic-subtypery)

;;; I tried putting this in one test at first, and the resulting compile crashed SBCL.
;;; Heap exhausted during compilation. Great stuff.

(test standard-atomic-subtypery/a
  (subtypes arithmetic-error error serious-condition condition t)
  (subtypes array t)
  (subtypes atom t))

(test standard-atomic-subtypery/b
  (subtypes base-char character t)
  (subtypes base-string string vector array sequence t)
  (subtypes bignum integer rational real number t)
  (subtypes bit unsigned-byte signed-byte integer rational real number t)
  (subtypes bit-vector vector array sequence t)
  (subtypes broadcast-stream stream t)
  (subtypes built-in-class class standard-object t))

(test standard-atomic-subtypery/c
  (subtypes cell-error error serious-condition condition t)
  (subtypes character t)
  (subtypes class standard-object t)
  (subtypes compiled-function function t)
  (subtypes complex number t)
  (subtypes concatenated-stream stream t)
  (subtypes condition t)
  (subtypes cons list sequence t)
  (subtypes control-error error serious-condition condition t))

(test standard-atomic-subtypery/def
  (subtypes division-by-zero arithmetic-error error serious-condition condition t)
  (subtypes double-float float real number t)
  (subtypes floating-point-inexact arithmetic-error error serious-condition condition t)
  (subtypes floating-point-invalid-operation arithmetic-error error serious-condition condition t)
  (subtypes floating-point-overflow arithmetic-error error serious-condition condition t)
  (subtypes floating-point-underflow arithmetic-error error serious-condition condition t)
  (subtypes function t))

(test standard-atomic-subtypery/ghiklmn
  (subtypes generic-function function t)
  (subtypes hash-table t)
  (subtypes integer rational real number t)
  (subtypes keyword symbol t)
  (subtypes list sequence t)
  (subtypes logical-pathname pathname t)
  (subtypes long-float float real number t)
  (subtypes method t)
  (subtypes method-combination t)
  ;; NIL would go here
  (subtypes null symbol list sequence t)
  (subtypes number t))

(test standard-atomic-subtypery/p
  (subtypes package t)
  (subtypes package-error error serious-condition condition t)
  (subtypes parse-error error serious-condition condition t)
  (subtypes pathname t)
  (subtypes print-not-readable error serious-condition condition t)
  (subtypes program-error error serious-condition condition t))

(test standard-atomic-subtypery/r
  (subtypes random-state t)
  (subtypes ratio rational real t)
  (subtypes reader-error parse-error error serious-condition condition t)
  (subtypes readtable t)
  (subtypes real number t)
  (subtypes restart t))

(test standard-atomic-subtypery/s1
  (subtypes sequence t)
  (subtypes serious-condition condition t)
  (subtypes short-float float real number t)
  (subtypes signed-byte integer rational real number t))

(test standard-atomic-subtypery/simple
  (subtypes simple-array array t)
  (subtypes simple-base-string base-string string vector array sequence t)
  (subtypes simple-bit-vector bit-vector vector array sequence t)
  (subtypes simple-condition condition t)
  (subtypes simple-error simple-condition error serious-condition condition t)
  (subtypes simple-string string vector array sequence t)
  (subtypes simple-type-error simple-condition type-error error serious-condition condition t)
  (subtypes simple-vector vector simple-array array sequence t)
  (subtypes simple-warning simple-condition warning condition t))

(test standard-atomic-subtypery/s2
  (subtypes single-float float real number t)
  (subtypes standard-char base-char character t)
  (subtypes standard-class class standard-object t)
  (subtypes standard-generic-function generic-function function t)
  (subtypes standard-method method standard-object t)
  (subtypes standard-object t)
  (subtypes storage-condition serious-condition condition t)
  (subtypes stream t)
  (subtypes stream-error error serious-condition condition t)
  (subtypes string vector array sequence t)
  (subtypes string-stream stream t)
  (subtypes structure-class class standard-object t)
  (subtypes structure-object t)
  (subtypes style-warning warning condition t)
  (subtypes symbol t)
  (subtypes synonym-stream stream t))

(test standard-atomic-subtypery/tuvwxyz
  ;; T would go here
  (subtypes two-way-stream stream t)
  (subtypes type-error error serious-condition condition t)
  (subtypes unbound-slot cell-error error serious-condition condition t)
  (subtypes unbound-variable cell-error error serious-condition condition t)
  (subtypes undefined-function cell-error error serious-condition condition t)
  (subtypes unsigned-byte signed-byte integer rational real number t)
  (subtypes vector array sequence t)
  (subtypes warning condition t))

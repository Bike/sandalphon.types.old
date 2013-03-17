;;;; char.lisp

(in-package #:sandalphon.types)

;;;; standard-char <: base-char
;;;; base-char <: character, extended-char <: character
;;;; character <: base-char ∨ extended-char

(defsingleton standard-char the-ctype-standard-char)

(defmethod ctypep (object (ctype (eql (the-ctype-standard-char))))
  (and (characterp object) (standard-char-p object)))

(defsingleton base-char the-ctype-base-char)

(defmethod ctypep (object (ctype (eql (the-ctype-base-char))))
  (and (characterp object) (cl:typep object 'base-char)))
(defmethod csubtypep tri/definite ((t1 (eql (the-ctype-standard-char))) (t2 (eql (the-ctype-base-char))))
  (values t t))
(defmethod csubtypep tri/definite ((t1 (eql (the-ctype-standard-char))) (t2 (eql (find-class 'character))))
  (values t t))
(defmethod csubtypep tri/definite ((t1 (eql (the-ctype-base-char))) (t2 (eql (find-class 'character))))
  (values t t))

;; extended-char is just (and character (not base-char))
;; for character we use the built-in-class

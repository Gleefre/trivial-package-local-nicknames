;;; Test creation of PLN through defpackage

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(define-test defpackage-one (#!#:a #!#:b #!#:c)
  ;; uninterned symbol as nickname
  (defpackage #!#:a
    (:use)
    (:local-nicknames (#:nick #!#:test)))
  (assert-local-nicknames #!#:a (#:nick #!#:test))

  ;; string as nickname
  (defpackage #!#:b
    (:use)
    (:local-nicknames ("NICK" #!#:test)))
  (assert-local-nicknames #!#:b (#:nick #!#:test))

  ;; character as nickname
  (defpackage #!#:c
    (:use)
    (:local-nicknames (#\N #!#:test)))
  (assert-local-nicknames #!#:c (#:n #!#:test)))

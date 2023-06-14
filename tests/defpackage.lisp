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

(define-test defpackage-same-nick (#!#:a #!#:b #!#:c #!#:d)
  ;; same nickname for the same package is fine
  ;; should not be repeated in (package-local-nicknames ...) alist
  (defpackage #!#:a
    (:use)
    (:local-nicknames (#:nick #!#:test)
                      (#:nick #!#:test)))
  (assert-local-nicknames #!#:a (#:nick #!#:test))

  ;; nicknames can be repeated in different options as well
  (defpackage #!#:b
    (:use)
    (:local-nicknames (#:nick #!#:test))
    (:local-nicknames (#:nick #!#:test)))
  (assert-local-nicknames #!#:b (#:nick #!#:test))

  ;; #:nick and "NICK" is a same nickname
  (defpackage #!#:c
    (:use)
    (:local-nicknames (#:nick #!#:test)
                      ("NICK" #!#:test)))
  (assert-local-nicknames #!#:c (#:nick #!#:test))

  ;; same nicknames for different packages are no go
  (errors package-error
    (defpackage #!#:d
      (:use)
      (:local-nicknames (#:nick #!#:test)
                        (#:nick #!#:test/2)))))

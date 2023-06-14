;;; Test creation of PLN through defpackage

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(define-test defpackage-one (#!#:a #!#:b #!#:c #!#:d)
  ;; uninterned symbol as nickname
  (defpackage #!#:a
    (:use)
    (:local-nicknames (#:nick #!#:test)))
  (assert-local-nicknames #!#:a (#:nick #!#:test))
  (assert-nicknamed-by-list #!#:test #!#:test-1 #!#:a)

  ;; string as nickname
  (defpackage #!#:b
    (:use)
    (:local-nicknames ("NICK" #!#:test)))
  (assert-local-nicknames #!#:b (#:nick #!#:test))
  (assert-nicknamed-by-list #!#:test #!#:test-1 #!#:a #!#:b)

  ;; character as nickname
  (defpackage #!#:c
    (:use)
    (:local-nicknames (#\N #!#:test)))
  (assert-local-nicknames #!#:c (#:n #!#:test))
  (assert-nicknamed-by-list #!#:test #!#:test-1 #!#:a #!#:b #!#:c)

  ;; local nickname for global nickname
  (defpackage #!#:d
    (:use)
    (:local-nicknames (#:nick #!#:test/global-nick)))
  (assert-local-nicknames #!#:d (#:nick #!#:test))
  (assert-nicknamed-by-list #!#:test #!#:test-1 #!#:a #!#:b #!#:c #!#:d))

(define-test defpackage-nonexistent-target (#!#:a #!#:b #!#:c)
  ;; should signal error of type package-error
  (errors package-error
    (defpackage #!#:a
      (:use)
      (:local-nicknames (#:nick #!#:test/this/package/does/not/exist))))
  (errors package-error
    (defpackage #!#:b
      (:use)
      (:local-nicknames (#:nick #!#:test/this/package/does/not/exist)
                        ("NICK" #!#:test/this/package/does/not/exist))))
  (errors package-error
    (defpackage #!#:c
      (:use)
      (:local-nicknames (#\N #!#:test/this/package/does/not/exist)))))

(define-test defpackage-same-nick (#!#:a #!#:b #!#:c #!#:d #!#:e)
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
                        (#:nick #!#:test/2))))

  ;; same nickname can be defined for different names of the target package
  (defpackage #!#:e
    (:use)
    (:local-nicknames (#:nick #!#:test)
                      (#:nick #!#:test/global-nick)))
  (assert-local-nicknames #!#:e (#:nick #!#:test)))

(define-test defpackage-same-target (#!#:a #!#:b #!#:c)
  ;; It is ok to have nicknames for same packages
  (defpackage #!#:a
    (:use)
    (:local-nicknames (#:nick/1 #!#:test)
                      (#:nick/2 #!#:test)))
  (assert-local-nicknames #!#:a (#:nick/1 #!#:test) (#:nick/2 #!#:test))

  ;; It is ok to have nicknames for same packages and its nicknames
  (defpackage #!#:b
    (:use)
    (:local-nicknames (#:nick/1 #!#:test)
                      (#:nick/2 #!#:test)
                      (#:nick/3 #!#:test/global-nick)))
  (assert-local-nicknames #!#:b
                          (#:nick/1 #!#:test)
                          (#:nick/2 #!#:test)
                          (#:nick/3 #!#:test))

  ;; Just big test with different nicknames in different form
  (defpackage #!#:c
    (:use)
    (:local-nicknames (#:nick/1 #!#:test)
                      (#:nick/2 #!#:test))
    (:local-nicknames (#:nick/1 #!"TEST")
                      (#:nick/3 #!#:test)
                      (#:n      #!#:test))
    (:local-nicknames (#:nick/3 #!#:test)
                      (#:nick/5 #!"TEST")
                      (#\N      #!"TEST")
                      (#:nick/1 #!#:test))
    (:local-nicknames ("NICK/3" #!#:test)
                      ("N"      #!#:test)
                      ("NICK/4" #!"TEST")
                      ("NICK/2" #!#:test)))
  (assert-local-nicknames #!#:c
                          (#:nick/1 #!#:test)
                          (#:nick/2 #!#:test)
                          (#:nick/3 #!#:test)
                          (#:nick/4 #!#:test)
                          (#:nick/5 #!#:test)
                          (#:n      #!#:test)))

(define-test defpackage-shadowing-nicknames-blocklist ()
  ;; Can't shadow "CL", "COMMON-LISP" and "KEYWORD"
  (dolist (bad-nickname '("CL" "COMMON-LISP" "KEYWORD"
                          #:cl #:common-lisp #:keyword
                          :cl  :common-lisp  :keyword))
    (with-packages-cleanup (#!#:a #!#:b)
      (errors package-error
        (eval `(defpackage #!#:a
                 (:use)
                 (:local-nicknames (,bad-nickname #!#:test)))))
      ;; But can create a nickname for that package
      (eval `(defpackage #!#:b
               (:use)
               (:local-nicknames (#:nick ,bad-nickname)))))))

(define-test defpackage-double-execution (#!#:a #!#:b)
  ;; Reevaluation of defpackage that is not at variance with current
  ;; state of package that is being defined should not change that
  ;; package.
  (defpackage #!#:a
    (:use)
    (:local-nicknames (#:nick #!#:test)))
  (assert-local-nicknames #!#:a (#:nick #!#:test))
  (defpackage #!#:a
    (:use)
    (:local-nicknames (#:nick #!#:test)))
  (assert-local-nicknames #!#:a (#:nick #!#:test))
  (defpackage #!#:a
    (:use)
    (:local-nicknames ("NICK" #!#:test)))
  (assert-local-nicknames #!#:a (#:nick #!#:test))
  (defpackage #!#:a
    (:use)
    (:local-nicknames ("NICK" #!#:test)))
  (assert-local-nicknames #!#:a (#:nick #!#:test))

  ;; same as above but with character
  (defpackage #!#:b
    (:use)
    (:local-nicknames (#\N #!#:test)))
  (assert-local-nicknames #!#:b (#:n #!#:test))
  (defpackage #!#:b
    (:use)
    (:local-nicknames (#\N #!#:test)))
  (assert-local-nicknames #!#:b (#:n #!#:test))
  (defpackage #!#:b
    (:use)
    (:local-nicknames ("N" #!#:test)))
  (assert-local-nicknames #!#:b (#:n #!#:test))
  (defpackage #!#:b
    (:use)
    (:local-nicknames (#:n #!#:test)))
  (assert-local-nicknames #!#:b (#:n #!#:test))
  (defpackage #!#:b
    (:use)
    (:local-nicknames ("N" #!#:test)))
  (assert-local-nicknames #!#:b (#:n #!#:test))
  (defpackage #!#:b
    (:use)
    (:local-nicknames (#:n #!#:test)))
  (assert-local-nicknames #!#:b (#:n #!#:test)))

(define-test defpackage-shadow-packages (#!#:a #!#:a/nick
                                         #!#:b #!#:b/nick
                                         #!#:c)
  ;; It is allowed to shadow existing packages names and their nicknames
  (defpackage #!#:a
    (:use)
    (:nicknames #!#:a/nick))
  (defpackage #!#:b
    (:use)
    (:nicknames #!#:b/nick))
  (defpackage #!#:c
    (:use)
    (:local-nicknames (#!#:a #!#:b)
                      (#!#:b #!#:a))
    (:local-nicknames (#!#:a/nick #!#:b/nick)
                      (#!#:b/nick #!#:a/nick)))
  (assert-local-nicknames #!#:c
                          (#!#:a #!#:b)
                          (#!#:b #!#:a)
                          (#!#:a/nick #!#:b/nick)
                          (#!#:b/nick #!#:a/nick)))

;; TODO: test for having local nicknames shadowing own nicknames
;; Will CDR allow them? I hope so

;; TODO: test for local nicknames not affecting defpackage
;; Will they? Idk

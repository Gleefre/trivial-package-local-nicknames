;;; Tests setup

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(defpackage #!#:test
  (:use)
  (:export #:sym))

(defconstant +sym+ (find-symbol "SYM" '#!#:test))

(defun reset-test-packages ()
  (trivial-package-locks:without-package-locks
    (when (find-package #!:1)
      (delete-package #!:1))
    (when (find-package #!:2)
      (delete-package #!:2)))
  (defpackage #!:1
    (:use)
    (:local-nicknames (:l :cl) (#:nick #!#:test)))
  (defpackage #!:2
    (:use)
    (:export "CONS")))

;;; Tests setup

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(defpackage #!#:T
  (:use)
  (:export #:sym))

(defconstant +sym+ (find-symbol "SYM" '#!#:T))

(defun reset-test-packages ()
  (trivial-package-locks:without-package-locks
   (when (find-package #!:1)
     (delete-package #!:1))
   (when (find-package #!:2)
     (delete-package #!:2)))
  (defpackage #!:1
    (:use)
    (:local-nicknames (:l :cl) (#!#:N #!#:T)))
  (defpackage #!:2
    (:use)
    (:export "CONS")))

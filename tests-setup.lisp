;;; Tests setup

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(defpackage #!#:T
  (:use)
  (:export #:sym))

(progn
  (defparameter +sym-fullname+ (concatenate 'string #!"T" ":" "SYM"))
  (defparameter +sym-fullnickname+ (concatenate 'string #!"N" ":" "SYM"))
  (defparameter +sym+ (or (find-symbol "SYM" '#!#:T)
                          (error "Symbol not found while loading tests: check +SYM+ binding."))))

(defun reset-test-packages ()
  (#+sbcl sb-ext:without-package-locks
   #-sbcl progn
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

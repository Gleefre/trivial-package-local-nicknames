;;; Tests setup

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(defun reset-test-packages ()
  (trivial-package-locks:without-package-locks
    (with-packages-cleanup (#!#:test #!#:test-1 #!#:test/2 #!#:test-2)))
  ;; partly for old tests
  (defpackage #!#:test
    (:use)
    (:nicknames #!#:test/global-nick)
    (:export #:sym))
  (defpackage #!#:test/2
    (:use)
    (:export #:sym))

  ;; old tests
  (defpackage #!#:test-1
    (:use)
    (:local-nicknames (#:l #:cl)
                      (#:nick #!#:test)))
  (defpackage #!#:test-2
    (:use)
    (:export "CONS")))

(reset-test-packages)

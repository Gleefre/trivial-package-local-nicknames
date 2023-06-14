;;; Test introspection: package-local-nicknames and package-locally-nicknamed-by

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(define-test test-package-local-nicknames-introspection ()
  (dolist (p '("KEYWORD" "COMMON-LISP" "COMMON-LISP-USER" #!:1 #!:2))
    (let ((*package* (find-package p)))
      (let ((alist (package-local-nicknames #!:1)))
        (assert (equal (cons "L" (find-package "CL")) (assoc "L" alist :test 'string=)))
        (assert (equal (cons "NICK" (find-package #!"TEST"))
                       (assoc "NICK" alist :test 'string=)))
        (assert (eql 2 (length alist)))))))

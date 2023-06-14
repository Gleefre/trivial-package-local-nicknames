;;; Tests

(in-package #:trivial-package-local-nicknames.test)
(named-readtables:in-readtable trivial-package-lockal-nicknames.test)

(define-test test-package-local-nicknames-introspection ()
  (dolist (p '("KEYWORD" "COMMON-LISP" "COMMON-LISP-USER" #! :test-1 #! :test-2))
    (let ((*package* (find-package p)))
      (let ((alist (package-local-nicknames #! :test-1)))
        (assert (equal (cons "L" (find-package "CL")) (assoc "L" alist :test 'string=)))
        (assert (equal (cons "NICK" (find-package #!"TEST"))
                       (assoc "NICK" alist :test 'string=)))
        (assert (eql 2 (length alist)))))))

(define-test test-package-local-nicknames-symbol-equality ()
  (let ((*package* (find-package #! :test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (cons1 (find-symbol "CONS" :l))
          (cons1s (find-symbol "CONS" #\L))
          (exit0 (read-from-string #!"TEST:SYM"))
          (exit1 (find-symbol "SYM" '#:nick)))
      (assert (eq 'cons cons0))
      (assert (eq 'cons cons1))
      (assert (eq 'cons cons1s))
      (assert (eq (find-symbol "SYM" '#!#:test) exit0))
      (assert (eq (find-symbol "SYM" '#!#:test) exit1)))))

(define-test test-package-local-nicknames-package-equality ()
  (let ((*package* (find-package #! :test-1)))
    (let ((cl (find-package :l))
          (cls (find-package #\L))
          (sb (find-package '#:nick)))
      (assert (eq cl (find-package :common-lisp)))
      (assert (eq cls (find-package :common-lisp)))
      (assert (eq sb (find-package '#!#:test))))))

(define-test test-package-local-nicknames-symbol-printing ()
  (let ((*package* (find-package #! :test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (exit0 (read-from-string #!"TEST:SYM")))
      (assert (equal "L:CONS" (prin1-to-string cons0)))
      (assert (equal "NICK:SYM" (prin1-to-string exit0))))))

(define-test test-package-local-nicknames-nickname-collision ()
  ;; Can't add same name twice for different global names.
  (errors package-error
    (add-package-local-nickname :l #! :test-2 #! :test-1))
  ;; ...but same name twice is OK.
  (add-package-local-nickname :l :cl #! :test-1)
  (add-package-local-nickname #\L :cl #! :test-1))

(define-test test-package-local-nicknames-nickname-removal ()
  (assert (= 2 (length (package-local-nicknames #! :test-1))))
  (assert (remove-package-local-nickname :l #! :test-1))
  (assert (= 1 (length (package-local-nicknames #! :test-1))))
  (let ((*package* (find-package #! :test-1)))
    (assert (not (find-package :l)))))

(define-test test-package-local-nicknames-nickname-removal-char ()
  (assert (= 2 (length (package-local-nicknames #! :test-1))))
  (assert (remove-package-local-nickname #\L #! :test-1))
  (assert (= 1 (length (package-local-nicknames #! :test-1))))
  (let ((*package* (find-package #! :test-1)))
    (assert (not (find-package :l)))))

(define-test test-package-local-nicknames-nickname-removal-remaining ()
  (remove-package-local-nickname :l #! :test-1)
  (let ((*package* (find-package #! :test-1)))
    (let ((exit0 (read-from-string #!"TEST:SYM"))
          (exit1 (find-symbol "SYM" '#:nick))
          (sb (find-package '#:nick)))
      (assert (eq (find-symbol "SYM" '#!#:test) exit0))
      (assert (eq (find-symbol "SYM" '#!#:test) exit1))
      (assert (equal "NICK:SYM" (prin1-to-string exit0)))
      (assert (eq sb (find-package '#!#:TEST))))))

(define-test test-package-local-nicknames-nickname-removal-readd-another-symbol-equality ()
  (assert (remove-package-local-nickname :l #! :test-1))
  (assert (eq (find-package #! :test-1)
              (add-package-local-nickname :l #! :test-2 #! :test-1)))
  (let ((*package* (find-package #! :test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (cons1 (find-symbol "CONS" :l))
          (exit0 (read-from-string "NICK:SYM"))
          (exit1 (find-symbol "SYM" '#:nick)))
      (assert (eq cons0 cons1))
      (assert (not (eq 'cons cons0)))
      (assert (eq (find-symbol "CONS" #! :test-2)
                  cons0))
      (assert (eq (find-symbol "SYM" '#!#:test) exit0))
      (assert (eq (find-symbol "SYM" '#!#:test) exit1)))))

(define-test test-package-local-nicknames-nickname-removal-readd-another-package-equality ()
  (assert (remove-package-local-nickname :l #! :test-1))
  (assert (eq (find-package #! :test-1)
              (add-package-local-nickname :l #! :test-2 #! :test-1)))
  (let ((*package* (find-package #! :test-1)))
    (let ((cl (find-package :l))
          (sb (find-package '#:nick)))
      (assert (eq cl (find-package #! :test-2)))
      (assert (eq sb (find-package '#!#:TEST))))))

(define-test test-package-local-nicknames-nickname-removal-readd-another-symbol-printing ()
  (assert (remove-package-local-nickname :l #! :test-1))
  (assert (eq (find-package #! :test-1)
              (add-package-local-nickname :l #! :test-2 #! :test-1)))
  (let ((*package* (find-package #! :test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (exit0 (read-from-string "NICK:SYM")))
      (assert (equal "L:CONS" (prin1-to-string cons0)))
      (assert (equal "NICK:SYM" (prin1-to-string exit0))))))

#+package-locks
(define-test test-package-local-nicknames-package-locks ()
  ;; TODO Support for other implementations with package locks.
  (progn
    (setf (trivial-package-locks:package-locked-p #! :test-1) t)
    (errors #+sbcl sb-ext:package-lock-violation #-sbcl error
      (add-package-local-nickname :c #! :test-2 #! :test-1))
    (errors #+sbcl sb-ext:package-lock-violation #-sbcl error
      (remove-package-local-nickname :l #! :test-1))
    (setf (trivial-package-locks:package-locked-p #! :test-1) nil)
    (add-package-local-nickname :c #! :test-2 #! :test-1)
    (remove-package-local-nickname :l #! :test-1)))

(define-test test-delete-package-locally-nicknames-others ("LOCALLY-NICKNAMES-OTHERS" "LOCALLY-NICKNAMED-BY-OTHERS")
  (let ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
        (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
    (add-package-local-nickname :foo p2 p1)
    (assert (equal (list p1) (package-locally-nicknamed-by-list p2)))
    (delete-package p1)
    (assert (not (package-locally-nicknamed-by-list p2)))))

(define-test test-delete-package-locally-nicknamed-by-others ("LOCALLY-NICKNAMES-OTHERS"
                                                              "LOCALLY-NICKNAMED-BY-OTHERS")
  (let ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
        (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
    (add-package-local-nickname :foo p2 p1)
    (assert (package-local-nicknames p1))
    (delete-package p2)
    (assert (not (package-local-nicknames p1)))))

(define-test test-own-name-as-local-nickname-cerror ("OWN-NAME-AS-NICKNAME1" "OWN-NAME-AS-NICKNAME2")
  (let ((p1 (make-package "OWN-NAME-AS-NICKNAME1"))
        (p2 (make-package "OWN-NAME-AS-NICKNAME2")))
    (errors package-error
      (add-package-local-nickname :own-name-as-nickname1 p2 p1))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-name-as-nickname1 p2 p1))))

(define-test test-own-name-as-local-nickname-intern ("OWN-NAME-AS-NICKNAME1" "OWN-NAME-AS-NICKNAME2")
  (let ((p1 (make-package "OWN-NAME-AS-NICKNAME1"))
        (p2 (make-package "OWN-NAME-AS-NICKNAME2")))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-name-as-nickname1 p2 p1))
    (assert (eq (intern "FOO" p2)
                (let ((*package* p1))
                  (intern "FOO" :own-name-as-nickname1))))
    (let ((sym (intern "BAR" p2))
          (lam '(lambda (x) (intern x :own-name-as-nickname1))))
      (dolist (p '("COMMON-LISP" "KEYWORD" "COMMON-LISP-USER"
                   "OWN-NAME-AS-NICKNAME1"
                   "OWN-NAME-AS-NICKNAME2"))
        (let ((*package* p1))
          (assert (eq sym (funcall
                           (let ((*package* (find-package p))) (compile nil lam))
                           "BAR"))
                  ()
                  "test-own-name-as-local-nickname-intern failed for p = ~s"
                  p))))))

(define-test test-own-nickname-as-local-nickname-cerror ("OWN-NICKNAME-AS-NICKNAME1" "OWN-NICKNAME-AS-NICKNAME2")
  (let ((p1 (make-package "OWN-NICKNAME-AS-NICKNAME1"
                          :nicknames '("OWN-NICKNAME")))
        (p2 (make-package "OWN-NICKNAME-AS-NICKNAME2")))
    (errors package-error
      (add-package-local-nickname :own-nickname p2 p1))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-nickname p2 p1))))

(define-test test-own-nickname-as-local-nickname-intern ("OWN-NICKNAME-AS-NICKNAME1" "OWN-NICKNAME-AS-NICKNAME2")
  (let ((p1 (make-package "OWN-NICKNAME-AS-NICKNAME1"
                          :nicknames '("OWN-NICKNAME")))
        (p2 (make-package "OWN-NICKNAME-AS-NICKNAME2")))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-nickname p2 p1))
    (assert (eq (intern "FOO" p2)
                (let ((*package* p1))
                  (intern "FOO" :own-nickname))))
    (let ((sym (intern "BAR" p2))
          (lam '(lambda (x) (intern x :own-nickname)))
          (*package* p1))
      (dolist (p '("COMMON-LISP" "KEYWORD" "COMMON-LISP-USER"
                   "OWN-NICKNAME-AS-NICKNAME1"
                   "OWN-NICKNAME-AS-NICKNAME2"))
        (assert (eq sym
                    (funcall
                     (let ((*package* (find-package p))) (compile nil lam))
                     "BAR"))
                ()
                "test-own-nickname-as-local-nickname-intern failed on p = ~s"
                p)))))

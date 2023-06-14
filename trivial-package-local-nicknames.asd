;;;; trivial-package-local-nicknames.asd

(asdf:defsystem #:trivial-package-local-nicknames
  :description "Portability library for package-local nicknames"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "Public domain"
  :version "0.2"
  :serial t
  :components ((:file "trivial-package-local-nicknames"))
  :in-order-to ((asdf:test-op (asdf:test-op "trivial-package-local-nicknames/tests"))))

(asdf:defsystem #:trivial-package-local-nicknames/tests
  :description "Tests for package-local nicknames extension"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "Public domain"
  :version "0.2"
  :depends-on ("trivial-package-local-nicknames")
  :serial t
  :components ((:file "tests"))
  :perform (asdf:test-op (op c) (uiop:symbol-call '#:trivial-package-local-nicknames-tests '#:run)))

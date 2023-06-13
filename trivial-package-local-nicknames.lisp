;;;; trivial-package-local-nicknames.lisp

(defpackage #:trivial-package-local-nicknames
  (:use #:cl)
  (:import-from
   #+sbcl      #:sb-ext
   #+ccl       #:ccl
   #+ecl       #:ext
   #+clisp     #:ext
   #+abcl      #:ext
   #+clasp     #:ext
   #+lispworks #:hcl
   #+allegro   #:excl
   #-(or sbcl ccl ecl clisp abcl
         clasp lispworks allegro) #:package-local-nicknames
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname)
  (:export
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname))

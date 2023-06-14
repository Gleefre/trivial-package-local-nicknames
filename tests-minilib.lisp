;;;; Test miniature framework

(defpackage #:trivial-package-local-nicknames.test
  (:use #:cl #:trivial-package-local-nicknames))

(in-package #:trivial-package-local-nicknames.test)

(defun |#!-reader| (stream subchar arg)
    (declare (ignore subchar arg))
    (let ((token (read stream t nil t)))
      (typecase token
        (string (concatenate 'string
                             "TRIVIAL-PACKAGE-LOCAL-NICKNAMES.TEST."
                             token))
        (symbol (if (eq (symbol-package token) (find-package '#:keyword))
                    (nth-value 0 (intern (concatenate 'string
                                                      "TRIVIAL-PACKAGE-LOCAL-NICKNAMES.TEST."
                                                      (symbol-name token))
                                         (symbol-package token)))
                    (make-symbol (concatenate 'string
                                              "TRIVIAL-PACKAGE-LOCAL-NICKNAMES.TEST."
                                              (symbol-name token))))))))

(named-readtables:defreadtable trivial-package-lockal-nicknames.test
  (:merge :standard)
  (:dispatch-macro-char #\# #\! #'|#!-reader|))

(defparameter *tests* ())

(defun add-test (function)
  (pushnew function *tests*))

(defun get-tests ()
  (reverse *tests*))

(defmacro errors (error-type &body body)
  (let ((fail (gensym)))
    `(assert (eq ',fail
                 (handler-case
                     (progn ,@body)
                   (,error-type () ',fail))))))

(defmacro assert-local-nicknames (package-designator &rest local-nicknames)
  `(let ((nicknames (package-local-nicknames (find-package ',package-designator)))
         (needed-nicknames (list ,@(loop for (nick package-designator) in local-nicknames
                                         collect `(cons ,(string nick) (find-package ',package-designator))))))
     (assert (= (length nicknames) ,(length local-nicknames)))
     (loop for (nick . package) in nicknames
           do (check-type nick string)
              (check-type package package))
     (loop for (nick . package) in needed-nicknames
           do (assert (= 1
                         (count nick nicknames :key #'car :test #'string=)
                         (count nick needed-nicknames :key #'car :test #'string=)))
              (assert (eq (cdr (assoc nick nicknames :test #'string=))
                          package)))))

(defmacro define-test (name (&rest packages-to-remove/names) &body body)
  `(progn
     (defun ,name ()
       (declare (optimize (debug 3) (safety 3) (speed 0)))
       (reset-test-packages)
       (unwind-protect
            (progn ,@body)
         (reset-test-packages)
         ,@(loop for package in packages-to-remove/names
                 collect `(when (find-package ',package)
                            (delete-package ',package)))))
     (add-test ',name)
     ',name))

(defun run (&optional (ignore-errors t))
  (let ((errors '()))
    (dolist (test (get-tests))
      (if ignore-errors
          (handler-case (funcall test)
            (error (e)
              (format t ";; ~A:~%;;;; ~A~%" test e)
              (push e errors)))
          (funcall test)))
    (format t ";;~%;; ~D tests run, ~D failures."
            (length *tests*) (length errors))
    (null errors)))

#+(or)
(defmacro with-tmp-packages (bindings &body body)
  `(let ,(mapcar #'car bindings)
     (unwind-protect
          (progn
            (setf ,@(apply #'append bindings))
            ,@body)
       ,@(mapcar (lambda (p)
                   `(when ,p (delete-package ,p)))
                 (mapcar #'car bindings)))))

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

(defmacro define-test (name &body body)
  `(progn
     (defun ,name () ,@body)
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

(defmacro with-tmp-packages (bindings &body body)
  `(let ,(mapcar #'car bindings)
     (unwind-protect
          (progn
            (setf ,@(apply #'append bindings))
            ,@body)
       ,@(mapcar (lambda (p)
                   `(when ,p (delete-package ,p)))
                 (mapcar #'car bindings)))))

(defpackage forge-system
  (:use :common-lisp)
  (:export :execute))
(in-package forge-system)

(defun execute (command-string)
  (fresh-line)
  (uiop:run-program command-string :output t))

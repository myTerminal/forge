#!/usr/bin/sbcl --script

;; Need to get rid of the below two lines
(load "~/quicklisp/setup.lisp")
(ql:quickload "asdf")

(load "package.lisp")
(load "system.lisp")

(in-package start)

(defparameter forge-user-config '())
(defun main ()
  (let ((config-file (or (first (uiop:command-line-arguments))
                         "example/forge-user-config.lisp")))
    (load config-file)
    ;; Echo the user config
    (princ forge-user-config)
    ;; Consume a function from forge-system
    (execute "whoami")))
(main)

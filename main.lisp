(in-package :main)

(defparameter forge-user-config '(Something))
(defun main ()
  (let ((config-file (or (first (uiop:command-line-arguments))
                         "example/forge-user-config.lisp")))
    (load config-file)
    ;; Echo the user config
    (print forge-user-config)
    ;; Consume a function from forge-system
    (execute "whoami")))

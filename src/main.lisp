(in-package :main)

(defvar forge-user-config nil)
(defun main ()
  (let ((config-file-path (or (first (uiop:command-line-arguments))
                              "example/forge-user-config.lisp")))
    (setf forge-user-config
          (file-to-string config-file-path)))
  ;; Echo the loaded user config
  (print forge-user-config)
  ;; Consume functions from system
  (execute-in-system "sudo ls /tmp")
  (print (get-result-from-system "whoami"))
  (print (get-list-from-system "ls"))
  (print (exists-in-system-p "cat")))

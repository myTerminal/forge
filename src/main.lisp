(in-package :main)

(defvar forge-system-config nil)
(defvar forge-user-config nil)
(defun main ()
  (let ((system-config-file-path "config.lisp")
        (user-config-file-path (or (first (uiop:command-line-arguments))
                                   "example/forge-user-config.lisp")))
    (setf forge-system-config
          (file-to-string system-config-file-path))
    (setf forge-user-config
          (file-to-string user-config-file-path)))
  ;; Echo the loaded configs
  (print forge-system-config)
  (print forge-user-config)
  ;; Consume functions from system
  (execute-in-system "sudo ls /tmp")
  (print (get-result-from-system "whoami"))
  (print (get-list-from-system "ls"))
  (print (exists-in-system-p "cat")))

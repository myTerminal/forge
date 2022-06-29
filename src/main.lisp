(in-package :main)

(defvar forge-user-config nil)
(defun main ()
  (let ((config-file-path (or (first (uiop:command-line-arguments))
                              "example/forge-user-config.lisp")))
    (setf forge-user-config
          (with-open-file (file-stream config-file-path)
            (read-from-string (reduce (lambda (a b)
                                        (concatenate 'string a b))
                                      (loop for i from 0
                                            for line = (read-line file-stream nil nil)
                                            while line
                                            collect line))))))
  ;; Echo the loaded user config
  (print forge-user-config)
  ;; Consume functions from system
  (execute-in-system "sudo ls /tmp")
  (print (get-result-from-system "whoami"))
  (print (get-list-from-system "ls"))
  (print (exists-in-system-p "cat")))

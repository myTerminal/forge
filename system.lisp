(in-package system)

(defun execute (command-string)
  (fresh-line)
  (uiop:run-program command-string :output t))

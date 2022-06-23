(in-package :system)

(defun execute-in-system (command-string)
  (fresh-line)
  (uiop:run-program command-string :output t))

(defun get-list-from-system (command-string)
  (string-to-list (uiop:run-program command-string
                                    :output '(:string :stripped t))))

(defun exists-in-system-p (command-string)
  (not (null (uiop:run-program (concatenate 'string
                                            "command -v "
                                            command-string)
                               :output t
                               :ignore-error-status t))))

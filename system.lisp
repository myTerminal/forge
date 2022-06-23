(in-package :system)

(defun get-result-from-system (command-string)
  (uiop:run-program command-string
                    :output '(:string :stripped t)))

(defun get-list-from-system (command-string)
  (string-to-list (get-result-from-system command-string)))

(defun exists-in-system-p (command-string)
  (not (null (uiop:run-program (concatenate 'string
                                            "command -v "
                                            command-string)
                               :output t
                               :ignore-error-status t))))

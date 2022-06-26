(in-package :system)

(defun execute-in-system (command-string)
  (uiop:run-program command-string
                    :input :interactive
                    :output :interactive
                    :error-output t
                    :ignore-error-status t))

(defun get-result-from-system (command-string)
  (uiop:run-program command-string
                    :output '(:string :stripped t)
                    :error-output t
                    :ignore-error-status t))

(defun get-list-from-system (command-string)
  (string-to-list (get-result-from-system command-string)))

(defun exists-in-system-p (command-string)
  (not (null (get-result-from-system (concatenate 'string
                                                  "command -v "
                                                  command-string)))))

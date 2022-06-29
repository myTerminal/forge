(in-package :system)

(defun execute-maybe (command-string)
  (let* ((command-line-arguments (uiop:command-line-arguments))
         (run-mode (first command-line-arguments)))
    (if (or (string-equal run-mode "0")
            (string-equal run-mode "2"))
        (progn
          (princ (concatenate 'string "[forge executing]: "
                              command-string))
          (fresh-line)))
    (if (or (string-equal run-mode "1")
            (string-equal run-mode "2"))
        (uiop:run-program command-string
                          :input :interactive
                          :output *standard-output*
                          :error-output t
                          :ignore-error-status t))))

(defun execute-in-system (command-string)
  (execute-maybe command-string))

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

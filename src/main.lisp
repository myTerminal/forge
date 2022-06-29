(in-package :main)

(defvar forge-system-config nil)
(defvar forge-user-config nil)

(defun install-packages (system-config packages)
  ;; TODO: Implement
  (print system-config)
  (print packages))

(defun get-applicable-steps (steps)
  ;; TODO: Implement
  steps)

(defun execute-steps (steps)
  ;; TODO: Implement
  (print steps))

(defun main ()
  (let* ((command-line-arguments (uiop:command-line-arguments))
         (system-config-file-path "config.lisp")
         (user-config-file-path (or (second command-line-arguments)
                                    "example/forge-user-config.lisp")))

    ;; Print 'help' in case of no arguments
    (unless command-line-arguments
      (progn
        (princ "Please specify a run-mode (0, 1, or 2) and optionally a config!")
        (fresh-line)
        (uiop:quit)))

    ;; Load system and user configs
    (setf forge-system-config
          (file-to-string system-config-file-path))
    (setf forge-user-config
          (file-to-string user-config-file-path))

    ;; Install packages for current platform
    (install-packages forge-system-config
                      (car forge-user-config))

    ;; Execute all applicable steps
    (execute-steps (get-applicable-steps (cdr forge-user-config)))))

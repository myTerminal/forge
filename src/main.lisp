(in-package :main)

(defvar forge-system-config nil)
(defvar forge-user-config nil)

(defun print-help ()
  (princ "No arguments specified!")
  (fresh-line)
  (princ "Please specify a run-mode and optionally a config.")
  (fresh-line)
  (princ "Following run-modes are available:")
  (fresh-line)
  (princ "0 - Log commands to be executed, only simulate a run")
  (fresh-line)
  (princ "1 - Execute commands as usual")
  (fresh-line)
  (princ "2 - Do a combination of both 0 and 1")
  (fresh-line))

(defun get-relevant-package-entries (current-platform packages)
  (labels ((does-omit-package-for-platform (platform package)
             (let ((package-reference (find platform
                                            (cdr package)
                                            :key #'car)))
               (and package-reference
                    (null (cadr package-reference))))))
    (remove-if #'null
               (mapcar (lambda (package)
                         (if (or (null (cdr package))
                                 (not (does-omit-package-for-platform current-platform
                                                                      package)))
                             package))
                       packages))))

(defun get-relevant-packages (current-platform system-config package-entries)
  (let* ((known-package-managers (cdr (find current-platform
                                            (car system-config)
                                            :key #'car)))
         (primary-package-manager (car known-package-managers)))
    (mapcar (lambda (entry)
              (let* ((relevant-sources (remove-if #'null
                                                  (mapcar (lambda (package-manager)
                                                            (find package-manager
                                                                  (cdr entry)
                                                                  :key #'car))
                                                          known-package-managers))))
                (cond ((null relevant-sources) `(,primary-package-manager ,(car entry)))
                      (t (car relevant-sources)))))
            package-entries)))

(defun install-packages (system-config packages)
  ;; TODO: Implement
  (print (cdr system-config))
  (print packages))

(defun get-applicable-steps (current-platform steps)
  (remove-if #'null
             (mapcar (lambda (step)
                       `(,(car step)
                          ,(cadar (remove-if-not (lambda (element)
                                                   (if (or (eql (car element)
                                                                current-platform)
                                                           (eql (car element)
                                                                :all))
                                                       element))
                                                 (cddr step)))
                          ,(cadr step)))
                     steps)))

(defun execute-steps (steps)
  ;; TODO: Implement
  (print steps))

(defun main ()
  (let* ((command-line-arguments (uiop:command-line-arguments))
         (system-config-file-path "config.lisp")
         (user-config-file-path (or (second command-line-arguments)
                                    "example/forge-user-config.lisp"))
         (current-platform (get-current-operating-platform)))

    ;; Print 'help' in case of no arguments
    (unless command-line-arguments
      (progn
        (print-help)
        (uiop:quit)))

    ;; Load system and user configs
    (setf forge-system-config
          (file-to-string system-config-file-path))
    (setf forge-user-config
          (file-to-string user-config-file-path))

    ;; Install packages for current platform
    (install-packages forge-system-config
                      (get-relevant-packages current-platform
                                             forge-system-config
                                             (get-relevant-package-entries current-platform
                                                                           (car forge-user-config))))

    ;; Execute all applicable steps
    (execute-steps (get-applicable-steps current-platform
                                         (cdr forge-user-config)))))

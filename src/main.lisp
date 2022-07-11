;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(defun print-help ()
  "Prints help text for the program."
  (princ "No arguments specified!")
  (fresh-line)
  (princ "Please specify a run-mode and optionally a config.")
  (fresh-line)
  (princ "Following run-modes are available:")
  (fresh-line)
  (princ "simulate - Log commands to be executed, only simulate a run")
  (fresh-line)
  (princ "run - Execute commands as usual")
  (fresh-line)
  (princ "debug - Do a combination of both 0 and 1")
  (fresh-line))

(defun get-relevant-package-entries (current-platform packages)
  "Gets package entries from the configuration that are relevant for the
current operating platform."
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

(defun get-relevant-package-groups (current-platform package-manager-info package-entries)
  "Gets information about packages and their respective package manager sources
from their respective package entries."
  (let* ((known-package-managers (cdr (find current-platform
                                            package-manager-info
                                            :key #'car)))
         (primary-package-manager (car known-package-managers))
         (relevant-packages (mapcar (lambda (entry)
                                      (let* ((relevant-sources (remove-if #'null
                                                                          (mapcar (lambda (package-manager)
                                                                                    (find package-manager
                                                                                          (cdr entry)
                                                                                          :key #'car))
                                                                                  known-package-managers))))
                                        (cond ((null relevant-sources) `(,primary-package-manager ,(car entry)))
                                              (t (car relevant-sources)))))
                                    package-entries)))
    (mapcar (lambda (package-manager)
              `(,package-manager . ,(flatten (mapcar (lambda (package)
                                                       (cdr package))
                                                     (remove-if-not (lambda (package)
                                                                      (eql (car package)
                                                                           package-manager))
                                                                    relevant-packages)))))
            known-package-managers)))

(defun generate-install-command (package-manager packages)
  "Generates installation command for the supplied package-manage for the
supplied packages."
  (let ((command-prefix (concatenate 'string
                                     (if (fourth package-manager)
                                         "sudo ")
                                     (second package-manager))))
    (if (equal (third package-manager)
               :multi)
        (concatenate 'string
                     command-prefix
                     " "
                     (reduce (lambda (a b)
                               (concatenate 'string
                                            a
                                            " "
                                            b))
                             packages))
        (mapcar (lambda (package)
                  (concatenate 'string
                               command-prefix
                               " "
                               package))
                packages))))

(defun install-packages (package-manager-commands package-groups)
  "Installs the supplied package-groups using the supplied system config."
  (mapc #'execute-in-system
        (flatten (mapcar (lambda (package-group)
                           (let ((package-manager (find (car package-group)
                                                        package-manager-commands
                                                        :key #'car)))
                             (generate-install-command package-manager
                                                       (cdr package-group))))
                         package-groups))))

(defun get-applicable-steps (current-platform steps)
  "Gets the applicable steps for the current platform from among the supplied
steps."
  (remove-if (lambda (step)
               (null (second step)))
             (mapcar (lambda (step)
                       `(,(first step)
                          ,(flatten (reduce (lambda (a commands-set)
                                              (append a
                                                      (cdr commands-set)))
                                            (remove-if-not (lambda (element)
                                                             (if (or (eql (car element)
                                                                          current-platform)
                                                                     (eql (car element)
                                                                          :all))
                                                                 element))
                                                           (cddr step))
                                            :initial-value '()))
                          ,(second step)))
                     steps)))

(defun execute-step-commands (commands)
  "Execute commands contained in the steps."
  (mapc (lambda (command)
          (fresh-line)
          (execute-in-system command))
        commands))

(defun execute-step (step)
  "Executes the supplied step."
  (fresh-line)
  (princ (first step))
  (if (third step)
      (progn
        (princ " > Run (Y/n)?")
        (fresh-line)
        (if (prompt-y-or-n)
            (execute-step-commands (second step))
            (progn
              (princ (concatenate 'string
                                  "[Skipped]: "
                                  (first step)))
              (fresh-line))))
      (execute-step-commands (second step))))

(defun execute-steps (steps)
  "Executes the supplied steps."
  (mapc #'execute-step
        steps))

(defun main ()
  "The main entry point to the program."
  ;; Clear the screen before starting the program
  (clear-screen)

  ;; Process command-line arguments and start the execution
  (let ((command-line-arguments (uiop:command-line-arguments)))

    ;; Print 'help' in case of no arguments
    (unless command-line-arguments
      (progn
        (print-help)
        (uiop:quit)))

    ;; Validate run-mode
    (unless (find (first command-line-arguments)
                  '("simulate" "run" "debug")
                  :test #'string-equal)
      (princ "Please specify a valid run-mode!")
      (fresh-line)
      (uiop:quit))

    ;; Load configs and continue
    (let* ((system-config-file-path "config.lisp")
           (forge-system-config (file-to-string system-config-file-path))
           (user-config-file-path (or (second command-line-arguments)
                                      "example/forge-config.lisp"))
           (forge-user-config (file-to-string user-config-file-path))
           (current-platform (get-current-operating-platform)))

      (flet ((translate-package-entries (entries)
               (mapcar (lambda (entry)
                         (if (listp entry)
                             entry
                             `(,entry)))
                       entries)))
        ;; Setup/Add software sources for the current platform
        (execute-steps (get-applicable-steps current-platform
                                             (second forge-system-config)))

        ;; Install packages for current platform
        (install-packages (third forge-system-config)
                          (get-relevant-package-groups current-platform
                                                       (first forge-system-config)
                                                       (get-relevant-package-entries current-platform
                                                                                     (translate-package-entries (car forge-user-config)))))

        ;; Execute all applicable user steps
        (execute-steps (get-applicable-steps current-platform
                                             (cdr forge-user-config))))))
  ;; Notify on completion
  (princ "forge has finished performing the setup!"))

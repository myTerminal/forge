;;;; -------------------------------------------------------------------------
;;;; Access to host system

(in-package :system)

(defun escape-risky-chars (input)
  "Replaces characters in the supplied string that have been known to cause issues on the terminal."
  (let ((pos-in-string (search "~"
                               input)))
    (if pos-in-string
        (replace-char-in-string input
                                #\~
                                "$HOME")
        input)))

(defun print-colored-text (text color)
  "Prints the supplied text with the specified color."
  (format t
          (concatenate 'string
                       "~c["
                       (case color
                         ((:red) "31")
                         ((:cyan) "96")
                         ((:green) "92")
                         ((:yellow) "93")
                         ((:magenta) "95")
                         (otherwise "97"))
                       "m"
                       (escape-risky-chars text)
                       "~c[0m~%")
          #\ESC #\ESC))

(defun print-info (text)
  "Prints the supplied informational text to the terminal."
  (print-colored-text text
                      :yellow))

(defun print-question (text)
  "Prints the supplied question to the terminal."
  (print-colored-text text
                      :cyan))

(defun print-success (text)
  "Prints the supplied success message to the terminal."
  (print-colored-text text
                      :green))

(defun print-warning (text)
  "Prints the supplied warning to the terminal."
  (print-colored-text text
                      :magenta))

(defun print-error (text)
  "Prints the supplied error to the terminal."
  (print-colored-text text
                      :red))

(defun print-new-line ()
  "Prints new-line on the terminal console."
  (princ #\newline))

(defun execute-in-system (command-string)
  "Conditionally chooses between logging the supplied command string, executing
it in the underlying system, or both, depending on the command-line arguments
supplied during the program execution."
  (let* ((command-line-arguments (uiop:command-line-arguments))
         (run-mode (first command-line-arguments)))
    (if (or (string-equal run-mode "simulate")
            (string-equal run-mode "debug"))
        (print-warning (concatenate 'string "[forge executing]: "
                                    command-string)))
    (if (or (string-equal run-mode "run")
            (string-equal run-mode "debug"))
        (uiop:run-program command-string
                          :input :interactive
                          :output *standard-output*
                          :error-output t
                          :ignore-error-status t))))

(defun get-result-from-system (command-string)
  "Gets the result of execution of the supplied command string in the
underlying system."
  (uiop:run-program command-string
                    :output '(:string :stripped t)
                    :error-output t
                    :ignore-error-status t))

(defun prompt-y-or-n ()
  "Gets Y or N from the interface."
  (member (trivial-raw-io:read-char)
          '(#\y #\Y #\Newline)))

(defun get-list-from-system (command-string)
  "Executes the supplied command string in the underlying system and returns
a list."
  (string-to-list (get-result-from-system command-string)))

(defun exists-in-system-p (command-string)
  "Returns whether the supplied command exists in the underlying system."
  (not (string-equal (get-result-from-system (concatenate 'string
                                                          "command -v "
                                                          command-string))
                     "")))

(defun clear-screen ()
  "Clears the terminal screen."
  (if (exists-in-system-p "clear")
      (execute-in-system "clear")
      (execute-in-system "cls")))

(defun get-current-operating-platform ()
  "Determines the operating platform of the underlying system."
  (cond ((exists-in-system-p "xbps-query")
         :void)
        ((exists-in-system-p "pacman")
         :arch)
        ((exists-in-system-p "dnf")
         :fedora)
        ((exists-in-system-p "apt")
         :debian)
        (t nil)))

(defun reboot-system ()
  "Reboots the machine."
  (cond ((exists-in-system-p "systemctl") (execute-in-system "systemctl reboot"))
        ((exists-in-system-p "loginctl") (execute-in-system "loginctl reboot"))
        ((exists-in-system-p "reboot") (execute-in-system "sudo reboot"))
        (t (print-error "Unable to reboot automatically. Please reboot to apply changes."))))

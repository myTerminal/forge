(in-package :system)

(defun execute-in-system (command-string)
  (fresh-line)
  (uiop:run-program command-string :output t))

(defun get-list-from-system (command-string)
  ;; TODO: Implement
  (print command-string)
  (print "Not implemented!"))

(defun exists-in-system-p (command-string)
  ;; TODO: Implement
  (print command-string)
  (print "Not implemented!"))

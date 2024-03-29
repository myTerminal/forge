(defpackage :utils
  (:use :cl)
  (:export :flatten
           :string-to-list
           :read-from-file
           :replace-char-in-string))

(defpackage :shell
  (:use :cl)
  (:import-from :utils
                :string-to-list
                :replace-char-in-string)
  (:export :print-info
           :print-question
           :print-success
           :print-warning
           :print-error
           :print-new-line
           :execute-in-system
           :get-result-from-system
           :prompt-y-or-n
           :get-list-from-system
           :exists-in-system-p
           :clear-screen
           :get-current-operating-platform
           :reboot-system))

(defpackage :main
  (:use :cl)
  (:import-from :utils
                :flatten
                :read-from-file)
  (:import-from :shell
                :print-info
                :print-question
                :print-success
                :print-warning
                :print-error
                :print-new-line
                :clear-screen
                :execute-in-system
                :prompt-y-or-n
                :get-current-operating-platform
                :reboot-system)
  (:export :main))

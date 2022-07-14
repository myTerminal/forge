(defpackage :utils
  (:use :cl)
  (:export :flatten
           :string-to-list
           :file-to-string))

(defpackage :system
  (:use :cl)
  (:import-from :utils
                :string-to-list)
  (:export :execute-in-system
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
                :file-to-string)
  (:import-from :system
                :clear-screen
                :execute-in-system
                :prompt-y-or-n
                :get-current-operating-platform
                :reboot-system)
  (:export :main))

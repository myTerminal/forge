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
           :get-list-from-system
           :exists-in-system-p
           :get-current-operating-platform))

(defpackage :main
  (:use :cl)
  (:import-from :utils
                :flatten
                :file-to-string)
  (:import-from :system
                :execute-in-system
                :get-result-from-system
                :get-list-from-system
                :exists-in-system-p
                :get-current-operating-platform)
  (:export :main))

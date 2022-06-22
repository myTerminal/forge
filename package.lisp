(defpackage :system
  (:use :cl)
  (:export :execute-in-system
           :get-list-from-system
           :exists-in-system-p))

(defpackage :main
  (:use :cl)
  (:import-from :system
                :execute-in-system
                :get-list-from-system
                :exists-in-system-p)
  (:export :main))

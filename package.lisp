(defpackage :utils
  (:use :cl)
  (:export :string-to-list))

(defpackage :system
  (:use :cl)
  (:import-from :utils
                :string-to-list)
  (:export :get-result-from-system
           :get-list-from-system
           :exists-in-system-p))

(defpackage :main
  (:use :cl)
  (:import-from :system
                :get-result-from-system
                :get-list-from-system
                :exists-in-system-p)
  (:export :main))

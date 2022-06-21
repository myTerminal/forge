(defpackage :system
  (:use :cl)
  (:export :execute))

(defpackage :main
  (:use :cl)
  (:import-from :system
                :execute)
  (:export :main))

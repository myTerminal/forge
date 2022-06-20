(defpackage system
  (:use :cl)
  (:export :execute))

(defpackage start
  (:use :cl)
  (:import-from :system
                :execute)
  (:export :main))

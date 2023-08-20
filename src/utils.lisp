;;;; -------------------------------------------------------------------------
;;;; Common utility helpers

(in-package :utils)

(defun flatten (items)
  "Flattens the supplied nested list into a single-dimensional list."
  (reduce (lambda (a item)
            (append a
                    (if (listp item)
                        item
                        `(,item))))
          items
          :initial-value '()))

(defun string-to-list (input-string)
  "Converts a string containing NewLine characters into a list of strings."
  (let* ((temp-chars '())
         (items '()))
    (labels ((to-string (chars)
               (coerce (reverse chars) 'string))
             (collect-item ()
               (push (to-string temp-chars) items)
               (setf temp-chars '())))
      (mapc (lambda (c)
              (cond ((eql c #\Newline) (collect-item))
                    (t (push c temp-chars))))
            (coerce input-string 'list))
      (collect-item)
      (reverse items))))

(defun read-from-file (file-path)
  "Reads a file as string and parses it."
  (with-open-file (file-stream file-path)
    (read-from-string (reduce (lambda (a b)
                                (concatenate 'string a b))
                              (loop for i from 0
                                    for line = (read-line file-stream nil nil)
                                    while line
                                    collect line)))))

(defun replace-char-in-string (input search-char substitute)
  "Replaces the given character in supplied string with a specified string"
  (reduce (lambda (a b)
            (concatenate 'string a b))
          (mapcar (lambda (char)
                    (if (eql char search-char)
                        substitute
                        (string char)))
                  (coerce input
                          'list))))

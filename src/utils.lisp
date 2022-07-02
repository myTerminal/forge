(in-package :utils)

(defun flatten (items)
  (reduce (lambda (a item)
            (append a item))
          items
          :initial-value '()))

(defun string-to-list (input-string)
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

(defun file-to-string (file-path)
  (with-open-file (file-stream file-path)
    (read-from-string (reduce (lambda (a b)
                                (concatenate 'string a b))
                              (loop for i from 0
                                    for line = (read-line file-stream nil nil)
                                    while line
                                    collect line)))))

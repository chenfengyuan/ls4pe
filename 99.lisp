(defpackage :ls4pe.99
  (:use :cl))
(in-package :ls4pe.99)

(defun line->number (string)
  (let ((m (position #\, string)))
    (loop for i in (list (subseq string 0 m) (subseq string (1+ m)))
       collect (parse-integer i))))
(defun value (base exponent)
  (* exponent (log base)))

(defun pe99 (filename)
  (with-open-file (in filename)
    (loop
       with m = 0 and
       j
       for i from 1
       for str = (read-line in nil nil)
       while str
       if (> (apply #'value (line->number str)) m)
       do (setf m (apply #'value (line->number str))
		j i)
       finally (return j))))

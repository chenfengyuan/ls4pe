(declaim (optimize (speed 3)))
(defpackage :ls4pe.35
  (:use :cl)
  (:export :pe35))
(in-package :ls4pe.35)
(defparameter *maximize* 1000000)
(defvar *a* nil)
(declaim (inline primep))
(defun make-prime-table (&optional (end *maximize*))
  (setf *a* (make-array (1+ *maximize*) :element-type '(unsigned-byte 8) :initial-element 1))
  (cond
    ((> end *maximize*)
     (setf end *maximize*)))
  (setf (elt *a* 0) 0 (elt *a* 1) 0)
  (loop for i from 2 to end
     if (eq 1 (elt *a* i))
     do (loop for j from (* 2 i) to end by i
	   do (setf (elt *a* j) 0))))
(defun primep (n)
  (eq 1 (elt *a* n)))
;; (defmacro rotate-num_ (n)
;;   (let* ((str (write-to-string n))
;; 	 (times (floor (log n 10))))
;;     `(loop
;; 	do ,(loop
;; 	       for j from 0 to times
;; 	       collect `(elt ,str ,j) into r
;; 	       finally (return (append (list 'rotatef) r)))
;; 	repeat ,times
;; 	collect (read-from-string ,str))))
;; (defun rotate-num(x)
;;   (eval `(rotate-num_ ,x)))
(defun rotate-num (x)
  (loop
     with bit = (floor (log x 10))
     for i = x then (+ (* (expt 10 bit)
			  (mod i 10))
		       (floor i 10))
     repeat (1+ bit)
     collect i into r
     finally (return (cdr r))))
(defun only-even (n)
  (loop
     for i = n then (floor i 10)
     while (> i 0)
     always (oddp (mod i 10))))
(defun find-all ()
  (loop for i from 0 to *maximize*
     if (and (primep i)
	     (or (< i 10)
		 (and
		  (only-even i)
		  (loop
		     for j in (rotate-num i)
		     always (primep j)))))
     collect i))
(defun pe35 ()
  (progn
    (make-prime-table)
    (length
     (find-all))))
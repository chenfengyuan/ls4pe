(defun fac(n)
	   (loop for i from 1 to n
		for s = 1 then (* s i)
	      finally (return s)))

(defun generate-permutations(elements-in-vector)
  (block nil
    (let* ((a elements-in-vector)
	   (n (length a))
	   (b (make-array n))
	   (c (make-array (1+ n) :initial-element 0))
	   (r (make-array (fac n)))
	   (i 0)
	   k j)
      (loop for i from 0 upto (1- n)
	 do (setf (aref b i) i))
      (tagbody
       E2
	 (setf (aref r i) (make-array n :initial-contents a))
	 (incf i)
       E3
	 (setf k 1)
       E3-R
	 (if (= (aref c k) k)
	     (progn 
	       (setf (aref c k) 0)
	       (incf k)
	       (if (>= (aref c k) k)
		   (go E3-R))))
	 (if (= k n)
	     (return r)
	     (incf (aref c k)))
       E4
	 (rotatef (aref a 0) (aref a (aref b k)))
       E5
	 (setf j 1)
	 (decf k)
       E5-R
	 (if (< j k)
	     (progn 
	       (rotatef (aref b j) (aref b k))
	       (incf j)
	       (decf k)
	       (if (< j k)
		   (go E5-R))))
	 (go E2)))))

(defun product-mod-10-is (n)
  (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
  (loop for i from 2 to 8
     if (/= i n)
     collect (loop for j from (1+ i) to 9
	     if (and (/= j n) (= (mod (* i j) 10) n))
		do (vector-push-extend (vector i j) v)))
  v))

(defun make-factor-vector()
  (loop with v = (make-array 0 :adjustable t :fill-pointer 0) for i from 0 upto 9
     do (vector-push-extend (product-mod-10-is i) v)
       finally (return v)))

(defun build-number-from-vector(vector start end)
  (loop 
     for i from start upto end
     for s = (aref vector i) then (+ (* 10 s) (aref vector i))
     finally (return s)))

(defparameter *factors* (make-factor-vector))

(defun euler32-if (vector)
  (let ((p (aref vector (1- (length vector)))))
    (loop with n1 and n2 and n3 for i across (aref *factors* p)
       for a = (position (aref i 0) vector)
       for b = (position (aref i 1) vector)
       if (> a b)
       do (rotatef a b)
       end
       do (progn
	    (setf n1 (build-number-from-vector vector 0 a))
	    (setf n2 (build-number-from-vector vector (1+ a) b))
	    (setf n3 (build-number-from-vector vector (1+ b) (1- (length vector)))))
       if (= (* n1 n2) n3)
       do (return n3))))

(defun euler32()
  (let ((all (remove-duplicates
	      (loop
		 for i across (generate-permutations #(1 2 3 4 5 6 7 8 9))
		 for j = (euler32-if i)
		 if (not (eq nil j))
		 collect j
		 repeat 1000000))))
    (loop for i in all
	 sum i)))
;; (time (euler32))

(defun if-has-0 (n)
  (position #\0 (princ-to-string n)))

(defun euler32-2()
  (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
    (loop with a and z and b and c and d for x from 1 upto 98
       do (and
	   (setf a (princ-to-string x))
	   (not (position #\0 a))
	   (= (length a) (length (remove-duplicates a)))
	   (loop for y from 123 upto (/ 5000 (sqrt x))
	      do (and
		  (setf z (* x y))
		  (< z 9877)
		  (setf b (princ-to-string y))
		  (not (position #\0 b))
		  (setf c (princ-to-string z))
		  (not (position #\0 c))
		  (setf d (concatenate 'string a b c))
		  (= 9 (length d))
		  (= 9 (length (remove-duplicates d)))
		  (vector-push-extend z v)))))
    (loop for i across (remove-duplicates v)
       sum i)))

(defun euler32-3()
  (let ((v (make-array 0 :adjustable t :fill-pointer 0))
	(a "")(b "")(c "")(d "")(z 0))
    (declare (type string a b c d)(type fixnum z)
	     (optimize (safety 0)(speed 3)(debug 0)(space 0)))
    (do ((x 1 (1+ x)))
	((> x 98))
      (declare (type fixnum x))
      (setf a (princ-to-string x))
      (and 
       (not (position #\0 a))
       (= (length a) (length (remove-duplicates a)))
       (do ((y 123 (1+ y)))
	   ((> y (/ 5000 (sqrt x))))
	 (declare (type fixnum y))
	 (setf z (* x y))
	 (and
	  (< z 9877)
	  (setf b (princ-to-string y))
	  (not (position #\0 b))
	  (setf c (princ-to-string z))
	  (not (position #\0 c))
	  (setf d (concatenate 'string a b c))
	  (= 9 (length d))
	  (= 9 (length (remove-duplicates d)))
	  (vector-push-extend z v)))))
    (format t "~a~%"
	    (loop for i across (remove-duplicates v)
	       sum i))))
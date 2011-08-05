(defun reverse-num(num &optional (base 10))
	   (declare (type fixnum num base))
	   (loop
	      for n = num then (truncate (/ n base))
	      while (> n 0)
	      for i = (mod n base) then (+ (mod n base) (* base i))
	      finally (return i)))
(defun euler36()
	   (loop for i from 1 upto 1000000
	      if (and (/= (mod i 10) 0)(/= (mod i 2) 0) (= i (reverse-num i 10))(= i  (reverse-num i 2)))
	      sum i))
(time (euler36))
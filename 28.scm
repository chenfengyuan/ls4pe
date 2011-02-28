(define *+
  (lambda (a b c)
    (+ (* a b) c)
    ))
(define pe-28
  (lambda (r)
    (define n (/ (- r 1) 2))
    (/  (*+ n (*+ n (*+ 16 n 30) 26) 3) 3)))
(display (pe-28 1001))
(newline)
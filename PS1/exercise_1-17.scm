;Kelvin Mei (kelvin@brandeis.edu) 2015-01-29

;given usable methods
(define (double a)
  (+ a a))
(define (halve b)
  (/ b 2))
;doubles a if b is even and halves b
;increments answer by a and decrement b by 1 if b is odd
;if input is both negative, make them positive and solve recursively
;if b is negative, swap a and b and solve recursively
(define (log a b)
  (if (and (< a 0) (< b 0))
      (log (* -1 a)(* -1 b))
      (if (< b 0)
          (log b a)
          (if (= b 0)
              0
              (if (= (modulo b 2) 0)
                  (log (double a)(halve b))
                  (+ a (log a (- b 1))))))))
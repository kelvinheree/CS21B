;Kelvin Mei (kelvin@brandeis.edu) 2015-01-29
;given from exercise 1.43
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))

(define (repeated fn n)
  (if (= n 1)
      fn
      (compose fn(repeated fn (- n 1)))))
;smooth function
(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
;n-fold
(define (nfold n)
  (lambda (f dx x)
   ((repeated (smooth f dx) n) x)))



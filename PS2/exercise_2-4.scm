;Kelvin Mei (kelvin@brandeis.edu) 2016-02-20
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))
;solution
(define (cdr z)
  (z (lambda (p q) q)))
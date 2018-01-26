;Kelvin Mei (kelvin@brandeis.edu) 2015-01-29
;actually this was all given in class on the powerpoint
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))

(define (repeated fn n)
  (if (= n 1)
      fn
      (compose fn(repeated fn (- n 1)))))


;Kelvin Mei (kelvin@brandeis.edu) 2015-01-29
(define (double fn) (lambda (x) (fn (fn x))))
(define (1+ n)
  (+ n 1))
((double 1+) 0)
(((double double) 1+) 0)
((((double double) double) 1+) 0)
(((((double double) double) double) 1+) 0)

;In the substitution model, it looks like
;((double 1+) 0)
;(1+ (1+ 0))
; = 2
;The next one takes this function and calls the double function on that, which squares the number of times 1+ is performed.

;Scheme cannot calculate ((((((double double) double) double) double) 1+) 0) because of the max integer limit.
;The maximum integer limit is 2147483647 or 2^31 - 1.
;In the pattern above, it is 2^1, 2^2, 2^4, 2^16. The next integer is 2^256, which exceeds the limit by far.
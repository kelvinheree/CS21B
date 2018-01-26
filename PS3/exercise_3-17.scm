;Kelvin Mei (kelvin@brandeis.edu) 2016-02-20
(define (count x)
  (define (loop x y)
    ;if not a pair return 0
    (if (not (pair? x))
      0
      ;if (car x) is not in y, then loop on (car x), else skip it
      (if (not (memq (car x) y))
          (+ 1 (loop (car x) (cons (car x) y)) (loop (cdr x) (cons (car x) y)))
          (+ 1 (loop (cdr x) y)))))
  (loop x '()))

(count 10)
(count '(1 2 3 4))
(pair? (car '(2 3)))

(define x '(a b c))
(define z (list x x '(a b c)))
x
z
(count x)
(count z)
(not (memq x z))
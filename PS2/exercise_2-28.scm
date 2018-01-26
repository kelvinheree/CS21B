;Kelvin Mei (kelvin@brandeis.edu) 2016-02-20
;defines null list
(define nil (list))
;checks if input is a list or an atom
(define (atom? x) (not (or (pair? x) (null? x))))
;iterative solution
;similar to 2-27, except the order is swapped and it uses append instead of cons
(define (fringe items)
  (define (iter things answer)
    (if (null? things)
        answer
        (if (atom? (car things))
            (iter (cdr things) 
                  (append answer
                          (list (car things))))
            (iter (cdr things)
                  (append answer(fringe (car things))
                        )))))
  (iter items nil))

;test cases
(define x (list (list 1 2) (list 3 4)))
(fringe x)
;(1 2 3 4)
(fringe (list x x))
;(1 2 3 4 1 2 3 4)
(fringe (list (list (list 5 5) 6) 5))
;(5 5 6 5)
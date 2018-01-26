;Kelvin Mei (kelvin@brandeis.edu) 2016-02-20
;defines empty list
(define nil (list))
;returns true if input is not a list
;returns false if input is a list
(define (atom? x) (not (or (pair? x) (null? x))))
;deep reverse method
(define (deep-reverse items)
  (define (iter things answer)
    (if (null? things)
        answer
        (if (atom? (car things))
            (iter (cdr things) 
                  (cons (car things)
                        answer))
            (iter (cdr things)
                  (cons (deep-reverse (car things))
                        answer)))))
  (iter items nil))
;test cases
(list 5 6 (list 6 7 8 (list 3 2 1) (list 3 4 1)) 5)
(deep-reverse (list 5 6 (list 6 7 8 (list 3 2 1) (list 3 4 1)) 5))
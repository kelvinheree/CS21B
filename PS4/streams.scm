;; This is the code for Streams and Lazy Evaluation
;Kelvin Mei (kelvin@brandeis.edu) 2016-03-22


(define-syntax cons-stream
 (syntax-rules ()
   ((cons-stream a b)
    (cons a (delay b)))))

(define head car)
(define (tail s) (force (cdr s)))
(define stream-car car)
(define stream-cdr tail)


(define the-empty-stream (delay '()))
(define (empty-stream? s)
 (and (not (pair? s))
      (null? (force s))))

(define (1+ x) (+ 1 x))
(define (write-line x)
 (display x)
 (newline))

(define (divisible? x y) (= (remainder x y) 0))

;; Useful stream utility functions

(define (stream-filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;  Mapping functions

(define (stream-map proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

;;  Iterating along a stream.

(define (stream-for-each proc stream)
  (if (empty-stream? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))

;;  Streams of numbers

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (stream-car s1) (stream-car s2))
                           (add-streams (stream-cdr s1) (stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;  Differs from book by not checking for empty streams
(define (interleave s1 s2)
  (cons-stream (stream-car s1)
               (interleave s2
                           (stream-cdr s1))))

(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((h1 (stream-car s1))
               (h2 (stream-car s2)))
           (cond ((< h1 h2) (cons-stream h1 (merge (stream-cdr s1) s2)))
                 ((> h1 h2) (cons-stream h2 (merge s1 (stream-cdr s2))))
                 (else (cons-stream h1 
                                    (merge (stream-cdr s1) 
                                           (stream-cdr s2)))))))))

;; This next procedure is to be used in forming streams of pairs,
;; once you have defined the procedure MERGE-WEIGHTED
(define (weighted-pairs s t pair-weight)
  (cons-stream (cons (stream-car s) (stream-car t))
               (merge-weighted
                  (stream-map (lambda (x) (cons (stream-car s) x))
                              (stream-cdr t))
                  (weighted-pairs (stream-cdr s) (stream-cdr t) pair-weight)
                  (lambda (p) (pair-weight (car p) (cdr p))))))

;; This procedure forms streams of weighted pairs, where pairs of the
;; same weight have been combined.  In order to use it, you must
;; define an appropriate procedure COMBINE-SAME-WEIGHTS
(define (same-weight-pairs s t pair-weight)
  (combine-same-weights (weighted-pairs s t pair-weight)
                        pair-weight))

(define print-stream
  (let ()
    (define (iter s)
      (if (empty-stream? s)
          (display "]")
          (begin (write (stream-car s))
                 (write " ")
                 (iter (stream-cdr s)))))
    (lambda (s)
      (write "")
      (iter s))))
;; You may wonder why PRINT-STREAM has been written in such an obscure
;; way, when
;; (define (print-stream s)
;;   (write "[")
;;   (stream-for-each (lambda (x) (write x) (write " ")) s)
;;   (write "]"))
;; would have the same effect.  If you think about the "actor model"
;; and tail recursion, however, you may begin to see why.

;;  For exercise 3.43
(define (show x)
  (write-line x)
  x)

(define (nth-stream n s)
  (if (= n 0)
      (stream-car s)
      (nth-stream (- n 1) (stream-cdr s))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (enumerate-interval (1+ low) high))))

;Problem 1

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define no235 (stream-filter(lambda (x) (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5)))) integers))
;(print-stream ones)
;(print-stream integers)
;(print-stream no235)

;Problem 2

(define not7 (stream-filter(lambda (x) (not (divisible? x 7))) integers))
(define not3 (stream-filter(lambda (x) (not (divisible? x 3))) integers))
(define weave (interleave not7 not3))
;(print-stream weave)

;Problem 3

(define alt (cons-stream 0 (interleave integers alt)))
;(print-stream alt)

;Problem 4
(define (interleave-pairs s t)
  (cons-stream (cons (stream-car s) (stream-car t))
               (interleave
                (stream-map (lambda (x) (cons (stream-car s) x))
                            (stream-cdr t))
                (interleave-pairs (stream-cdr s) (stream-cdr t)))))
;(print-stream (interleave-pairs integers integers))


;(define (interleave-pairs s1 s2)
 ; )

;Problem 5
(define (merge-weighted s1 s2 weight)
  (let ((h1 (stream-car s1))
        (h2 (stream-car s2))
        (h3 (weight (stream-car s1)))
        (h4 (weight (stream-car s2))))
    (cond ((<= h3 h4)
           (cons-stream h1 (merge-weighted (stream-cdr s1) s2 weight)))
          ((> h3 h4)
           (cons-stream h2 (merge-weighted s1 (stream-cdr s2) weight))))))

;(print-stream (merge-weighted integers integers (lambda (x) (+ 2 x))))


;Problem 6
(define (weighted-pairs s t pair-weight)
  (cons-stream (cons (stream-car s) (stream-car t))
               (merge-weighted
                (stream-map
                 (lambda (x) (cons (stream-car s) x))
                 (stream-cdr t))
                (weighted-pairs (stream-cdr s) (stream-cdr t) pair-weight)
                (lambda (p) (pair-weight (car p) (cdr p))))))

;
;(print-stream (weighted-pairs integers integers (lambda(x y) (+ x y))))
;(print-stream (weighted-pairs integers integers (lambda(x y) (+ (* x x x) (* y y y)))))
;(define newstream (weighted-pairs integers integers (lambda(x y) (+ (* 2 x) (* 3 y) (* 5 x y)))))
;(define newno235 (stream-filter(lambda (x) (not (or (divisible? (car x) 2) (divisible? (car x) 3) (divisible? (car x) 5) (divisible? (cdr x) 2) (divisible? (cdr x) 3) (divisible? (cdr x) 5)))) newstream))
;(print-stream newno235)

;Problem 7

(define (combine-same-weights s1 pair-weight)
  (let ((h1 (pair-weight (car (stream-car s1)) (cdr (stream-car s1)))))
        (cons-stream (cons h1 (makelist h1 s1 pair-weight)) (combine-same-weights (advstream h1 (stream-cdr s1) pair-weight) pair-weight))))
;creates the merged list
(define (makelist num s pair-weight)
  (if (= (pair-weight (car (stream-car s)) (cdr (stream-car s))) num)
      (if (= (pair-weight (car (stream-car(stream-cdr s)))(cdr (stream-car(stream-cdr s)))) num)
          (cons (stream-car s)(makelist num (stream-cdr s) pair-weight))
          (list (stream-car s)))))
;removes the same weights from list
(define (advstream num s pair-weight)
  (let ((c (stream-car s)))
  (if (= (pair-weight (car c) (cdr c)) num)
      (advstream num (stream-cdr s) pair-weight)
      s)))
                                 


;(define singles (weighted-pairs integers integers (lambda(x y) (+ x y))))
;(define romanujan (combine-same-weights singles (lambda(x y) (+ x y))))
;(define trips (weighted-pairs integers integers (lambda(x y) (+ (* x x x) (* y y y)))))
;(define romanujan (combine-same-weights trips (lambda(x y) (+ (* x x x) (* y y y)))))
;(print-stream romanujan)
;(print-stream (combine-same-weights singles (lambda (x y) (+ x y))))


;Problem 8
(define (cube x) (* x x x))
;(define nums (same-weight-pairs integers
;                   integers
;                   (lambda (i j) (+ (cube i) (cube j)))))
;(define rnums (stream-filter(lambda (x) (> (length x) 2)) nums))
;(print-stream rnums)

;Problem 9a
(define (square x) (* x x))
;(define nums (same-weight-pairs integers
;                   integers
;                   (lambda (i j) (+ (square i) (square j)))))
;(define 3sumsquares (stream-filter (lambda (x) (= (length x) 4)) nums))
;(print-stream 3sumsquares)

;Problem 9b
;(define nums (same-weight-pairs integers
;                   integers
;                   (lambda (i j) (+ (cube i) (square j)))))
;(define twoway (stream-filter (lambda (x) (= (length x) 3)) nums))
;(define icubejsquare (stream-filter (lambda (x) (and (and (= (modulo (caadr x) 2) 1) (= (modulo (cdadr x) 2) 0))(and (= (modulo (caaddr x) 2) 1) (= (modulo (cdaddr x) 2) 0)))) twoway))
;(print-stream icubejsquare)

;; This is the code for ``Computer Psychiatrist'' (Doctor)

; for Racket users...
(#%require (only racket/base random))
; *******************

(define (visit-doctor name)
  (write-line (list 'hello name))
  (write-line '(what seems to be the trouble?))
  (doctor-driver-loop name))

(define (doctor-driver-loop name)
  (newline)
  (write '**)
  (let ((user-response (read)))
    (cond ((equal? user-response '(goodbye))
             (write-line (list 'goodbye name))
             (write-line '(see you next week)))
          (else (write-line (reply user-response))
                (doctor-driver-loop name)))))

(define (reply user-response)
  (cond ((fifty-fifty)
           (append (qualifier)
                   (change-person user-response)))
        (else (hedge))))

(define (fifty-fifty)
  (= (random 2) 0))

(define (qualifier)
  (pick-random '((you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say))))

(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue))))

(define (replace pattern replacement lst)
  (cond ((null? lst) '())
        ((equal? (car lst) pattern)
           (cons replacement
                 (replace pattern replacement (cdr lst))))
        (else (cons (car lst)
              (replace pattern replacement (cdr lst))))))

(define (many-replace replacement-pairs lst)
  (cond ((null? replacement-pairs) lst)
         (else (let ((pat-rep (car replacement-pairs)))
            (replace (car pat-rep)
                     (cadr pat-rep)
                     (many-replace (cdr replacement-pairs)
                     lst))))))

(define (change-person phrase)
  (many-replace '((i you) (me you) (am are) (my your))
                phrase))

(define (pick-random lst)
  (nth (+ 1 (random (length lst))) lst))

;;******

(define (prob n1 n2)
  (< (random n2) n1))

(define (ask-patient-name)
  (write-line '(next!))
  (write-line '(who are you?))
  (car (read)))

(define (nth n lst)
  (if (= n 1) 
      (car lst)
      (nth (- n 1) (cdr lst))))
;;******

(define (atom? a) (not (pair? a)))

(define nil '())

(define (write-line x) (begin (write x) (newline)))

;;******


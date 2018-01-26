;; This is the code for ``Computer Psychiatrist'' (Doctor)

; for Racket users...
(#%require (only racket/base random))
; *******************

(define (fifty-fifty)
  (= (random 2) 0))

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

;commented out for problem 2
;(define (change-person phrase)
;  (many-replace '((i you) (me you) (am are) (my your))
;                phrase))

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

;Problem 1


(define (qualifier)
  (pick-random '((you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say)
                 (do you suppose that)
                 (are you sure that)
                 (you understand that)
                 (it is obvious that)
                 (remember that))))

(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (i understand)
                 (it is simply a stage in life)
                 (that is not true)
                 (you are not the only one)
                 (indeed))))

;Problem 2
;(change-person ’(you are not being very helpful to me)) results in (you are not being very helpful to you).

(define (change-person phrase)
 (many-replace '((t you) (t1 am) (t2 i) (t3 my) (t4 you) (t5 are) (t6 your))
               (many-replace '((i t) (are t1) (you t2) (your t3) (me t4) (am t5) (my t6))
                phrase)))
;Test Case
;(change-person '(you are not being very helpful to me))
;(change-person '(i are you your me am my))

;Problem 3

;commented out for problem 4
;(define (visit-doctor name)
;  (write-line (list 'hello name))
;  (write-line '(what seems to be the trouble?))
;  (doctor-driver-loop name nil))

;(define (doctor-driver-loop name record)
;  (newline)
;  (write '**)
;  (let ((user-response (read)))
;    (cond ((equal? user-response '(goodbye))
;             (write-line (list 'goodbye name))
;             (write-line '(see you next week)))
;          (else (write-line (reply user-response record))
;                (doctor-driver-loop name (append (list record)(list user-response)))))))

(define (reply user-response record)
  (cond ((fifty-fifty)
           (append (qualifier)
                   (change-person user-response)))
        ((and (prob 5 10) (> (length record) 1))
         (append '(earlier you said that)
                 (change-person (list-ref record (+ (random (- (length record) 1)) 1)))))
        (else (hedge))))

;Problem 4

(define (visit-doctor name number)
  (cond ((= number 0)
      (write-line '(closing time)))
        ((equal? name 'suppertime)
         (write-line '(time to go home)))
      (else
       (write-line (list 'hello name))
       (write-line '(what seems to be the trouble?))
       (doctor-driver-loop name nil number))))

(define (doctor-driver-loop name record number)
  (newline)
  (write '**)
  (let ((user-response (read)))
    (cond ((equal? user-response '(goodbye))
           (write-line (list 'goodbye name))
           (write-line '(see you next week))
           (if (= number 1)
               (write-line '(closing time))
               (visit-doctor (ask-patient-name) (- number 1))
               )
           )
          (else (write-line (reply user-response record))
                (doctor-driver-loop name (append (list record)(list user-response)) number)))))
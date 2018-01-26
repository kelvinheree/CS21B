;; This is the code for -- Stable Marriage

(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers proposees)
  (zip-together (send proposers 'name)
                (send (send proposers 'intended) 'name)))

(define (courtship unengaged-proposers proposers proposees) ... )

(define (currently-unengaged list-of-people) ... )

(define (send list-of-people message)  ...)

(define (couple? person1 person2) ...)

(define (zip-together list1 list2)
  (if (null? list1)
      '()
      (cons (list (car list1) (car list2))
            (zip-together (cdr list1) (cdr list2)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (make-person my-name)
  (let ((preference-list '())
        (possible-mates '())
        (current-intended '()))
    (define (me message)
      (cond ((eq? message 'name) my-name)
            ((eq? message 'intended) current-intended)
            ((eq? message 'loves) preference-list)
            ((eq? message 'possible) possible-mates)
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
            ((eq? message 'propose)
               (let ((beloved (car possible-mates)))
                 (set! possible-mates (cdr possible-mates))
                 ;Problem 3
                 (write-line (list (me 'name) 'is 'currently 'proposing 'to (beloved 'name)))
                 (if (eq? ((beloved 'i-love-you) me)
                          'i-love-you-too)
                     ;Problem 3
                     (begin (set! current-intended beloved)
                            (write-line (list (beloved 'name) 'has 'accepted 'the 'proposal 'from (me 'name)))
                            'we-are-engaged)
                     ;Problem 3
                     (begin (write-line (list (beloved 'name) 'has 'rejected 'the 'proposal 'from (me 'name)))
                       'no-one-loves-me))))
            ((eq? message 'i-love-you)
             ;Problem 2
             (lambda (asker)
               (if (null? current-intended)
                   (begin (set! current-intended asker)
                   'i-love-you-too)
                   (if ((i-like-more? asker current-intended) preference-list)
                       (begin ((current-intended 'i-changed-my-mind) me)
                              (set! current-intended asker)  
                              'i-love-you-too)
                       'buzz-off-creep)
                   ))
             )
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                            (set! current-intended '())
                            ;Problem 3
                            (write-line (list (me 'name) 'has 'been 'dumped 'by (lost-love 'name)))
                            'dumped!)
                        (else 
                            'there-must-be-some-misunderstanding))))        
            (else 
              (error "Bad message to a person " (list me my-name message)))))
      me))

;; This is a test file for -- Stable Marriage

(define alan (make-person 'Alan))
(define bob (make-person 'Bob))
(define charles (make-person 'Chuck))
(define david (make-person 'Dave))
(define ernest (make-person 'Ernie))
(define franklin (make-person 'Frank))
(define agnes (make-person 'Agnes))
(define bertha (make-person 'Bertha))
(define carol (make-person 'Carol))
(define deborah (make-person 'Debbie))
(define ellen (make-person 'Ellen))
(define francine (make-person 'Fran))

((alan 'load-preferences) 
   (list agnes carol francine bertha deborah ellen))
((bob 'load-preferences) 
   (list carol francine bertha deborah agnes ellen))
((charles 'load-preferences) 
   (list agnes francine carol deborah bertha ellen))
((david 'load-preferences) 
   (list francine ellen deborah agnes carol bertha))
((ernest 'load-preferences) 
   (list ellen carol francine agnes deborah bertha))
((franklin 'load-preferences) 
   (list ellen carol francine bertha agnes deborah))
((agnes 'load-preferences) 
   (list charles alan bob david ernest franklin))
((bertha 'load-preferences) 
   (list charles alan bob david ernest franklin))
((carol 'load-preferences) 
   (list franklin charles bob alan ernest david))
((deborah 'load-preferences) 
   (list bob alan charles franklin david ernest))
((ellen 'load-preferences) 
   (list franklin charles bob alan ernest david))
((francine 'load-preferences) 
   (list alan bob charles david franklin ernest))

(define men (list alan bob charles david ernest franklin))
(define women (list agnes bertha carol deborah ellen francine))

;problem 1

(define (send list-of-people message) 
  (cond ((null? list-of-people) '())
        (else (cons ((car list-of-people) message)
              (send (cdr list-of-people) message)))))

(define (courtship unengaged-proposers proposers proposees)
  (cond ((null? unengaged-proposers) '())
        (else ((car unengaged-proposers) 'propose)
              (courtship (currently-unengaged proposers) proposers proposees))))

(define (couple? person1 person2)
  (if (and (eq? ((person1 'intended) 'name) (person2 'name))(eq? ((person2 'intended) 'name) (person1 'name)))
      #t
      #f))

(define (unengaged? person)
    (if (null? (person 'intended)) 
        #t
        #f)) 

(define (currently-unengaged list-of-people)   
  (filter unengaged? list-of-people))

(define (i-like-more? person1 person2)
  (lambda (list)
    ;compares the position of the person in the list
    (> (length (memq person1 list)) (length (memq person2 list)))))

;problem 2 above    
;problem 3
(define (write-line x) (begin (write x) (newline)))
;dialogue edits are above

;problem 4,5,6 in writeup
;problem 7
(define bob (make-person 'Bob))
(define carol (make-person 'Carol))
(define ted (make-person 'Ted))
(define alice (make-person 'Alice))
((bob 'load-preferences) (list carol alice))
((ted 'load-preferences) (list alice carol))
((carol 'load-preferences) (list ted bob))
((alice 'load-preferences) (list bob ted))
(define men (list bob ted))
(define women (list carol alice))
(match-make men women)
(match-make women men)




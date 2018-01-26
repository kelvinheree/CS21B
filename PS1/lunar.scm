;;Kelvin Mei kelvin@brandeis.edu 2/3/2016 CS121b

;; this is the code for problem set -- Lunar Lander

(define (update ship-state fuel-burn-rate)
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
   (+ (velocity ship-state)
      (* (- (* engine-strength fuel-burn-rate) gravity)
         dt))                                           ; velocity
   (- (fuel ship-state) (* fuel-burn-rate dt))))        ; fuel
  
;(define (lander-loop ship-state)
;  (show-ship-state ship-state)
;  (if (landed? ship-state)
;      (end-game ship-state)
;      (lander-loop (update ship-state (get-burn-rate)))))

(define (show-ship-state ship-state)
  (write-line 
    (list 'height (height ship-state)
          'velocity (velocity ship-state)
          'fuel (fuel ship-state))))

(define (landed? ship-state)
  (<= (height ship-state) 0))

(define (end-game ship-state)
  (let ((final-velocity (velocity ship-state)))
       (write-line final-velocity)
       (cond ((>= final-velocity safe-velocity)
               (write-line "good landing")
               'game-over)
             (else
               (write-line "you crashed!")
               'game-over))))

(define (get-burn-rate)
  (if (= (player-input) burn-key)
      1
      0))

;(define (play) (lander-loop (initial-ship-state)))

(define (initial-ship-state)
  (make-ship-state 50       ; 50 km high
                   0        ; not moving (0 km/sec)
                   20))     ; 20 kg of fuel left

(define dt 1)               ; 1 second interval of simulation
  
(define gravity 0.5)        ; 0.5 km/sec/sec
  
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash

(define engine-strength 1)  ; 1 kilonewton-second

(define (player-input) 
  (char->integer (prompt-for-command-char " action: "))) 

(define burn-key 32)   ;space key

; You'll learn about the stuff below here in Chapter 2.
; For now, think of make-ship-state, height, velocity, and fuel
; as primitive procedures built in to Scheme.

(define (make-ship-state height velocity fuel)
  (list 'HEIGHT   height
        'VELOCITY velocity
        'FUEL     fuel))

(define (height state) (second state))
(define (velocity state) (fourth state))
(define (fuel state) (sixth state))

(define (second l) (cadr l))
(define (fourth l) (cadr (cddr l)))
(define (sixth l) (cadr (cddr (cddr l))))

; Users of DrScheme or DrRacket: add these for compatibility with MIT Scheme...

; for input and output

(define (write-line x)
  (display x)
  (newline))

(define (get-one-key)
  (let ((x (read-char)))
    (if (eq? x #\newline)
        x
        (empty-buffer x))))

(define (empty-buffer x)
  (if (eq? (read-char) #\newline)
      x
      (empty-buffer x)))

(define (prompt-for-command-char prompt)
  (display prompt)
  (get-one-key)) 

; for random number generation

(#%require (only racket/base random))

; a ridiculous addendum  (you'll need this for the exercises)

(define (1+ x) (+ 1 x))


;1

;(define (update ship-state fuel-burn-rate)
;  (if (> fuel-burn-rate (/ (fuel ship-state) dt))
;      (make-ship-state
;       (+ (height ship-state) (* (velocity ship-state) dt)) ; height 
;       (+ (velocity ship-state)
;          (* (- (* engine-strength (/ (fuel ship-state) dt)) gravity)
;             dt)); velocity
;       (- (fuel ship-state) (* (/ (fuel ship-state) dt) dt))) ;fuel
;      (make-ship-state
;       (+ (height ship-state) (* (velocity ship-state) dt)) ; height 
;       (+ (velocity ship-state)
;          (* (- (* engine-strength fuel-burn-rate) gravity)
;             dt))                                           ; velocity
;        (- (fuel ship-state) (* fuel-burn-rate dt)))))   ; fuel

;2

(define (full-burn ship-state) 1)
(define (no-burn ship-state) 0)
(define (ask-user ship-state) (get-burn-rate))
(define (play procedure)
  (lander-loop (initial-ship-state) procedure))

(define (lander-loop ship-state procedure)
  (show-ship-state ship-state)
  (cond ((landed? ship-state) (end-game ship-state))
      (else (lander-loop (update ship-state (procedure ship-state)) procedure))))

;3

;(define (random-choice a b)
;  (lambda (ship-state)
;    (if (= (random 2) 0)
;        (a ship-state)
;        (b ship-state))))

;4

;(define (height-choice strategy-1 strategy-2 h)
;  (lambda (ship-state)
;    (if (<= (height ship-state) h)
;        (strategy-2 ship-state)
;        (strategy-1 ship-state))))

;5

;given
(define (random-choice strategy-1 strategy-2)
  (choice strategy-1
          strategy-2
          (lambda (ship-state) (= (random 2) 0))))

(define (height-choice strategy-2 strategy-1 h)
  (choice strategy-1
          strategy-2
          (lambda (ship-state) (<= (height ship-state) h))))

(define (choice strategy-1 strategy-2 fn)
  (lambda (ship-state)
    (cond ((fn ship-state)
           (strategy-1 ship-state))
          (else (strategy-2 ship-state)))))

;6
  
;(play (height-choice no-burn (random-choice full-burn ask-user) 40))

;7

;If the ship is to land at a perfect speed of 0, then velocity final is 0.
;Using the kinematics equation vf^2 = vi^2 + 2ah, we can prove that the acceleration needed is v^2/2h
;vf^2 = 0, so 0 = vi^2 + 2ah. Solve for a and you get -vi^2/2h = a. The negative is because it's accelerating in the opposite direction.

;8

(define dt .5)
;NOTE: does not work when number 1 or 10 is in effect because you will run out of fuel; guarantees 0.0 with unlimited fuel supply
(define (constant-acc ship-state)
  (define (square x) (* x x))
  (if (= (velocity ship-state) 0)
      0
      (+ gravity (/ (/ (square (velocity ship-state)) 
                       (* 2 (height ship-state))) dt))))

;9

;(play (height-choice no-burn constant-acc 30))
;(play (height-choice no-burn constant-acc 20))


;10

(define (limit burn-rate)
  (if (> burn-rate 1)
      1
      burn-rate))

(define (update ship-state fuel-burn-rate)
  (if (> fuel-burn-rate (/ (fuel ship-state) dt))
      (make-ship-state
       (+ (height ship-state) (* (velocity ship-state) dt)) ; height 
       (+ (velocity ship-state)
          (* (- (* engine-strength (limit (/ (fuel ship-state) dt))) gravity)
             dt)); velocity
       (- (fuel ship-state) (* (/ (limit (fuel ship-state)) dt) dt))) ;fuel
      (make-ship-state
       (+ (height ship-state) (* (velocity ship-state) dt)) ; height 
       (+ (velocity ship-state)
          (* (- (* engine-strength (limit fuel-burn-rate)) gravity)
             dt))                                           ; velocity
        (- (fuel ship-state) (* (limit fuel-burn-rate) dt)))))   ; fuel

;11

(define (optimal-constant-acc ship-state)
  (define (square x) (* x x))
  (if (> (+ gravity(/ (/ (square (velocity ship-state)) 
     (* 2 (height ship-state))) dt)) 1)
      (constant-acc ship-state)
      (no-burn ship-state)))
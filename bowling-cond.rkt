#lang racket

(require rackunit)

(define (strike? roll) (eq? 10 roll))
(define (spare? rolls) (eq? 10 (apply + rolls)))

(define (score-cond rolls)
  (let* ([this (car rolls)]
         [next (cadr rolls)]
         [isLastFrame? (< (length rolls) 4)])
  (cond
    [(strike? this) (+ 10 next (caddr rolls) (if isLastFrame? 0 (score-cond (cdr rolls))))]
    [(spare? (list this next)) (+ 10 (caddr rolls) (if isLastFrame? 0 (score-cond (cddr rolls))))]
    [else (+ this next (if isLastFrame? 0 (score-cond (cddr rolls))))])))

(define (score rolls) (score-cond rolls))

; tests
(define (roll count digit) (build-list count (lambda (x) digit)))

(check-eq? (score `(1 1 1 1)) 4 "test")

(check-eq? (score (roll 20 0)) 0 "zero game")
(check-eq? (score (roll 20 1)) 20 "all ones")
(check-eq? (score (roll 20 4)) 80 "all fours")
(check-eq? (score (roll 21 5)) 150 "all spares")
(check-eq? (score (roll 12 10)) 300 "perfect game")
(check-eq? (score `(10 5 5 4 1 8 2 10 10 5 4 7 3 5 4 7 3 2 )) 148 "Realistic game")


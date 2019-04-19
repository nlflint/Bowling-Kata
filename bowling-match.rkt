#lang racket

(require rackunit)

(define (last-frame? rolls) (< (length rolls) 4))

(define (score rolls)
  (match rolls
    [(list rest ...) #:when (last-frame? rest)
     (apply + rest)]
    
    [(list 10 next next-after rest ...)
     (+ 10 next next-after (score (cons next-after rest)))]
    
    [(list this next next-after _ ...) #:when (= 10 (+ this next))
     (+ 10 next-after (score (cddr rolls)))]
    
    [(list this next _ ... )
     (+ this next (score (cddr rolls)))]))



; tests
(define (score-many count digit)
  (score (build-list count (lambda (x) digit))))

(check-eq? (score-many 20 0) 0 "zero game")
(check-eq? (score-many 20 1) 20 "all ones")
(check-eq? (score-many 20 4) 80 "all fours")
(check-eq? (score-many 21 5) 150 "all spares")
(check-eq? (score-many 12 10) 300 "perfect game")
(check-eq? (score `(10 5 5 4 1 8 2 10 10 5 4 7 3 5 4 7 3 2 )) 148 "Realistic game")



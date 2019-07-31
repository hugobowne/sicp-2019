#lang racket
(define (iterative-improve good-enough? improve-guess)
  ; User provides the good-enough? and improve-guess procedures
  ; Design question: What is the API of good-enough?   Does it take
  ; a single value (the guess) or does it take two successive values?
  (define (iterate guess)
    (if (good-enough? guess)
        guess
        (iterate (improve-guess guess)))
    )
  iterate
  )
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqroot x)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.000001)
     )
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0)
  )
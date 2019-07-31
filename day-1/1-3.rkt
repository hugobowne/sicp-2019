#lang racket
(define (f a b c)
  (cond ((and (>= a b) (>= c b)) (+ (* a a) (* c c)))
        ((and (>= b a) (>= c a)) (+ (* c c) (* b b)))
        ((and (>= b c) (>= a c)) (+ (* a a) (* b b)))
        )
  )
        
  
  


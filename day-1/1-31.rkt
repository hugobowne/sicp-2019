#lang racket
(define (prod term a next b)
  (if (> a b)
      1
      (*  (term a)
          (prod term (next a) next b))))



(define (pie n)
  (define (term a)
    (/ (* (+ a 1) (+ a 3)) (* (+ a 2) (+ a 2)))
    )
  (define (next a)
    (+ a 2))
  (prod term 1.0 next n))
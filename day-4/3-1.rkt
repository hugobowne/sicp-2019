#lang racket
; Write an accumulator-generating function
(define (make-accumulator x)
  (lambda (addend)
    (begin (set! x (+ x addend))
           x)))


(define A (make-accumulator 10))


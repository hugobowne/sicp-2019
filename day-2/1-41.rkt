#lang racket

(define (inc x)
  (+ x 1))

(define (double f)
  (define (g x)
    (f (f x)))
  g)


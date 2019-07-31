#lang racket
(define (repeat f n)
  (if (= n 1)
      f
      (repeat (lambda (x) (f (f x))) (- n 1))))
#lang racket
(define (cube x) (* x x x))

(define (improve y x)
  (/ (+ (/ x (* y y)) (* 2.0 y)) 3.0))

(define (good-enough? guess x)
(< (abs (- (cube guess) x)) 0.0001))


(define (cbrt-iter guess x) (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

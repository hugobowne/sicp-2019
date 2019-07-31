#lang racket
(define (make-segment start end)
  (cons start end)
  )

(define (start-segment segment)
  (car segment)
  )

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y)
  )

(define (x-point p)
  (car p)
  )

(define (y-point p)
  (cdr p)
  )

(define (midpoint-segment segment)
  ; could use make-point and use average 
  (define x (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2))
  (define y (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2))
  (cons x y)
  )

(define start (make-point 0 0))
(define end (make-point 1 1))

(define segment (make-segment start end))

(define midpoint (midpoint-segment segment))
  
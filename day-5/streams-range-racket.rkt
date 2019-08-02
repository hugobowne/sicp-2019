#lang racket
;(define (range start end)
;  (if (>= start end)
;      null
;      (cons start (range (+ start 1) end)))
;  )

; or w/ deferred eval
(define (range start end)
  (if (>= start end)
      null
      (cons start (lambda () (range (+ start 1) end))
  )))
a
((cdr(a)))

(define (s-map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (lambda () (s-map proc ((cdr items)))))))
(define a (range 1 10))
(define tens (s-map (lambda (x) (* x 10)) a))
; Read about Streams in section 3.5 for more.
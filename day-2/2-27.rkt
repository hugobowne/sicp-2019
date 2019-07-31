#lang racket
(define (reverse list)
  (define (iter list result)
    (if (null? list)
        result
        (iter (cdr list) (cons (car list) result))))
  (iter list null)
  )


(define x (list (list 1 2) (list 3 4)))
x
(reverse x)

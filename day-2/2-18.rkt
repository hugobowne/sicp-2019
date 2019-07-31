#lang racket
(define (reverse list)
  (define (iter list result)
    (if (null? list)
        result
        (iter (cdr list) (cons (car list) result))))
  (iter list null)
  )
           
      
  




(reverse (list 1 4 9 16 25))
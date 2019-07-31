#lang racket

(define (reverse list)
  (define (iter list result)
    (if (null? list)
        result
        (iter (cdr list) (cons (car list) result))))
  (iter list null)
  )

(define (same-parity . lst)
  (define s (even? (car lst)))
  (define (iter lst result)
    (if (null? lst)
        result
        (if (equal? s (even? (car lst)))
            (iter (cdr lst) (cons (car lst) result))
            (iter (cdr lst) result))))
  (reverse (iter lst null))
    )
            
        
    
    
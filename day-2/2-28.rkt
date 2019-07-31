#lang racket
(define (fringe x)
  (define (iter x result)
    ;(display x)
    ;(newline)
    ;(display result)
    (cond ((null? x) result)
          ((not (pair? x)) (cons x result))
          (else (iter (car x)
                      (iter (cdr x) result)))))
  (iter x null)
  )
                
  
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))


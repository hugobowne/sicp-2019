#lang racket
(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (m a b)
  (if (= b 0)
      0
      (+ a (m a (- b 1)))))

(define (m a b)
  (cond ((= b 0)  0)
        ((even? b) (m (double a) (halve b)))
        (else (+ a (m a (- b 1))))))
        



        

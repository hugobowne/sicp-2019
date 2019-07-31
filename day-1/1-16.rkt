#lang racket
(define (square  x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt (square b) (/ n 2)))
        (else (* b (fast-expt b (- n 1))))))


(define (expt-iter b n result)
  (cond ((= n 0) result)
        ((even? n) (expt-iter (square b) (/ n 2) result))
        (else (expt-iter(square b (- n 1) (* b result))))))


(define (expt-lin b n)
  (define (expt-iter b n result)
    (cond ((= n 0) result)
          ((even? n) (expt-iter (square b) (/ n 2) result))
          (else (expt-iter b (- n 1) (* b result)))))
  (expt-iter b n 1))
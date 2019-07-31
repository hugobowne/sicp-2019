#lang racket
(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))


(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (fact2 n)
  (fact-iter 1 1 n))
  









#lang racket
(define (sum term a next b)
  (if (> a b)
      0
      (+  (term a)
          (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0)
    )

(define (prod term a next b)
  (if (> a b)
      1
      (*  (term a)
          (prod term (next a) next b))))


(define (prod term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1)
    )

;(define (identity x) x)

(define (fact n)
  (prod identity 1 inc n))
  
#lang racket
; This answer gives outputs 1 & 2, not 0 & 1, but close enough ;)
(define (make-accumulator x)
  (lambda (addend)
    (begin (set! x (+ x addend))
           x)))

(define A (make-accumulator 10))
;(+ (A 0) (A 1))
;(+ (A 1) (A 0))


; from jason myers, an oscillating solution!
(define f (let ((prior 0) 
                (cur 0)) 
            (lambda (n) 
              (set! prior cur) 
              (set! cur n) 
              prior)))

; Dave's soln, thinking about state machines
(define (make-procedure)
  (let ((state 0))     ; Some kind of internal variable (that gets mutated)
    (define (f x)
      (cond ((and (= state 0) (= x 0)) (set! state 1) 0)
            ((and (= state 1) (= x 1)) (set! state 0) 0)
            ((and (= state 0) (= x 1)) (set! state 2) 1)
            ((and (= state 2) (= x 0)) (set! state 0) 0)
            )
      )
    f
    )
  )

; the scott-scillator, which provides consistent outputs
(define g
  (let ([discard #t])
    (λ (x) 
      (begin (set! discard (not discard))
             (if discard 0 x)))))

(+ (g 0) (g 1))
(+ (g 1) (g 0))
(+ (g 0) (g 1))
(+ (g 1) (g 0))
(+ (g 0) (g 1))
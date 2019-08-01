#lang racket
(define (make-monitored f)
  (define count 0)
  (λ (x)
  (cond [(equal? x 'how-many-calls) count]
        [(equal?  x 'reset-count) (begin (set! count 0) count)]
        [else (begin (set! count  (+ count 1)) (f x))]
        )
    )
  )
; Dave reckons this is pretty hacky:  in particularly, having a sqrt function
; that now takes other magic arguments to tell you things
; In Python, e.g., you could have an attribute that clocks the number of calls
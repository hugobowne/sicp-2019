#lang racket

; Box with message passing
(define (make-bob-box x y width height)
  (define (dispatch message)
    (cond ((equal? message 'width) width)
          ((equal? message 'height) height)
          ((equal? message 'type) 'bob-box)
          )
    )
  dispatch
  )

; Box with message passing
(define (make-alice-box x1 y1 x2 y2)
  (define (dispatch message)
    (cond ((equal? message 'width) (abs (- x1 x2)))
          ((equal? message 'height) (abs (- y1 y2)))
          ((equal? message 'type) 'bob-box)
          )
    )
  dispatch
  )


(define (width box)
  (box 'width))

(define (height box)
  (box 'height))
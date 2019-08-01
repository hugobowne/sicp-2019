#lang racket
(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

(cdr x)
; '(b)

; define mutable append
(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))


; define mutable lists
(define mx (mcons 'a (mcons 'b null)))
(define my (mcons 'c (mcons 'd null)))

(define w (append! mx my))

(mcdr mx)
; (mcons 'b (mcons 'c (mcons 'd '())))

; Dave's soln
; Exercise 3.12
(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))
;  Immutable lists
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
; Mutable lists
(define mx (mcons 'a (mcons 'b null)))
(define my (mcons 'c (mcons 'd null)))
(define w (append! mx my))
w
(mcdr mx)
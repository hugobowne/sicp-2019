#lang racket
(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))

; 3 pairs
(define ca (mcons 1 (mcons 2 (mcons 3 null))))
(count-pairs ca)

; 4 pairs
(define t (mcons 3 null))
(define cb (mcons 1 (mcons t t)))
(count-pairs cb)

; to get number of "these pairs", count number of arrows + 1 !
; visualize the 7 pairs: now can you build it?

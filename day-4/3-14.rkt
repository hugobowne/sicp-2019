#lang racket
; the following procedure is quite useful, although obscure
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define vv (mcons 'a (mcons 'b (mcons 'c  (mcons 'd null)))))

w
; (mcons 'd (mcons 'c (mcons 'b (mcons 'a '()))))

vv
; (mcons 'a '())

; the real mystery is why the hell you'd want to do this:
; reverse the list and remove everything except its 1st element

; also, box-and-pointers here: https://github.com/kana/sicp/blob/master/ex-3.14.md
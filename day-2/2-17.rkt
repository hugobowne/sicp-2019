#lang racket
(define (last-pair list)
  (if (null? (cdr list))
             list
             (last-pair (cdr list))))
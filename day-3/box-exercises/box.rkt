#lang racket

(define (attach-tag tag contents) (cons tag contents))
 (define (type-tag datum) (car datum))
 (define (contents datum) (cdr datum))

; constructor function
(define (make-bob-box  x y w h)
  (attach-tag  'bob-box
               (cons (cons x y)  (cons w h))))

; type-check function
(define (bob-box? b) (eq? (type-tag b) 'bob-box))

(define (bob-width b) (car (cdr (contents b))))

(define (bob-height b) (cdr (cdr (contents b))))

(define (bob-area box)
  (* (bob-width box)
     (bob-height box)))

(define (make-alice-box x1 y1 x2 y2)
  (attach-tag 'alice-box
  (cons (cons x1 y1) (cons x2 y2))))

; type-check function
(define (alice-box? b) (eq? (type-tag b) 'alice-box))

(define (alice-width box)
  (abs (- (car (cdr (contents box)))
          (car (car (contents box))))))

(define (alice-height box)
  (abs (- (cdr (cdr (contents box)))
          (cdr (car (contents box))))))

(define (alice-area box)
  (* (alice-width box)
     (alice-height box)))

(define (width b)
 (cond ((bob-box? b) (bob-width b))
 ((alice-box? b) (alice-width b))
 ))

(define (height b)
 (cond ((bob-box? b) (bob-height b))
 ((alice-box? b) (alice-height b))
 ))

(define (area b)
 (cond ((bob-box? b) (bob-area b))
 ((alice-box? b) (alice-area b))
 ))



 
 
 
#lang racket
(define (make-queue)
  (mcons null null))

; accessor functions to access rear/front
(define (front-ptr queue)
  (mcar queue))

(define (rear-ptr queue)
  (mcdr queue))

(define (set-front-ptr! queue item)
  (set-mcar! queue item))

(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

; Return 1st item in queue or error
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue, brosephina" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item null)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
        (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
; (mcons (mcons 'a '()) (mcons 'a '()))
(insert-queue! q1 'b)
; (mcons (mcons 'a (mcons 'b '())) (mcons 'b '()))
(delete-queue! q1)
; (mcons (mcons 'b '()) (mcons 'b '()))
(delete-queue! q1)
; (mcons '() (mcons 'b '()))
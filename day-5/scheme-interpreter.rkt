#lang racket

; Metacircular evaluator (Chapter 4)

; seval -- this procedure evaluates a scheme expression
(define (seval exp env)
  (cond ((primitive? exp) exp)
        ((symbol? exp) (environment-lookup env exp))
        ; Special forms
        ((define? exp) (eval-define exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (eval-lambda exp env))
        ((quoted? exp) (eval-quote exp env))

        ; Extensions - Implement these if you want to
        ; ((quoted? exp) (eval-quoted exp env))
        ; ((begin? exp) (eval-begin exp env))
        ; ((cond? exp) (eval-cond exp env))
        ; ((assignment? exp) (eval-assignment exp env))   ; (set! ... )
        ; how about cond?
        ;((cond? exp) (eval-cond exp env))
        ; Application means calling a procedure
        ((application? exp) (eval-application exp env))
        (else (error "Bad expression")))
  )

(define (primitive?  exp)
  (number? exp)
  )

; (define name value)
(define (define? exp)
  (and (pair? exp) (eq? (car exp) 'define))
  )

; some parsing code below
(define (define-name exp)
  (cadr exp))

(define (define-value exp)
  (caddr exp)
  )

(define (eval-define exp env)
  (environment-define! env (define-name exp) (seval (define-value exp) env))
  )

; quote
(define (quoted? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))

(define (eval-quote exp env)
  (cadr exp)
  )

; (if test consequence alternative)
(define (if? exp)
  (and (pair? exp) (eq? (car exp) 'if)))

(define (if-test exp) (cadr exp))
(define (if-consequence exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))

(define (eval-if exp env)
  (if (seval (if-test exp) env)
      (seval (if-consequence exp) env)
      (seval (if-alternative exp) env)))

; (cond clause1 ... clausen)
; clause -> (test expr1 ...  exprn)
; not implemented

; (lambda (args) expr1 ... exprn)
; User defined procedures

(define (lambda? exp)
  (and (pair? exp) (eq? (car exp) 'lambda)))

(define (lambda-args exp)
  (cadr exp))

(define (lambda-exprs exp) ; A lambda can have multiple expressions (a list)
  (cddr exp)
  )

(define (eval-lambda exp env)
  (let ((parameters (lambda-args exp))
        (exprs (lambda-exprs exp)))
  (lambda args
    (let ((localenv (environment-new env)))
      (bind-arguments parameters args localenv)
      (eval-expressions exprs localenv)
      )
    )
  )
  )

; Bind names and values in an environment
(define (bind-arguments parameters args env)
  (if (null? parameters)
       null
       (begin
         (environment-define! env (car parameters) (car args))
         (bind-arguments  (cdr parameters)  (cdr args) env)))
  )

; Evaluate multiple expressions. Return the result of the last one
(define (eval-expressions exprs env)
  (if (null? (cdr exprs))
      (seval (car exprs) env) ; Return result of last expression
      (begin
        (seval (car exprs) env)
        (eval-expressions (cdr exprs) env)))
  )




; Procedure application (proc e1 e2 ... en)
(define (application? exp)
  (pair?  exp) ; Test if exp is a list
  )

(define (eval-application exp env)
  ; make a call to a procedure
  (let ((proc (seval (car exp) env)) ; Getting the procedure
        (args (eval-arguments (cdr exp) env)))
    ; Call the procedure (somehow)
    (apply proc args) ; Python: proc(*args
    )
  )

(define (eval-arguments args env)
  (map (lambda (a) (seval a env)) args)
  )

; Environment is going to be a linked-list of hash tables

(define (make-environment)
  (cons (make-hash) null) ; Create a Racket hash table (not in book)
  )

(define (environment-lookup env exp)
  (cond ((null? env) (error "unknown name"))
  ((hash-has-key? (car env) exp) (hash-ref (car env) exp))
  (else (environment-lookup (cdr env) exp)))
 ; (hash-ref env exp) ; Racket hash table lookup (not in book)
  )

; Make a new definition. This only occurs in the most local scope
(define (environment-define! env name value)
  (hash-set! (car env) name value)) ; Racket hash table set (not in book) 

; Make a new child environment
(define (environment-new env)
  (cons (make-hash) env))


; The "default" global  environment
(define env  (make-environment))

; Define some builtin procedures (like +)
(environment-define! env '+ +)
(environment-define! env '- -)
(environment-define! env '* *)
(environment-define! env '/ /)
(environment-define! env '< <)
(environment-define! env '> >)
(environment-define! env '= =)


; (seval '42 null)

;env
;(environment-define! env 'x 42)
;env
;(seval 'x env)

;(seval '(define x 42) env)
;(seval 'x env)

; (seval '(if (< 2 3) 4 5) env)

; (define f (lambda (x) (* x x)))
; (f 5)

; (seval '(define f (lambda (x) (* x x))) env)
; (seval '(f 2) env)

; (seval '(define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) env)
; (seval '(fact 5) env)

; (seval '(define x 'a) env)
; (seval 'x env)

; Example of how you would use it (eventually)
; (seval '(+ 2 3) env) --->  5
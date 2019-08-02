#lang racket
; Seperate the analysis of the code (into functions) from the running of the code
; lazy evaluation
; Metacircular evaluator (Chapter 4)
; seval -- this procedure evaluates a scheme expression
(define (analyze exp)
  (cond ((primitive? exp) (lambda (env) exp))
        ((symbol? exp) (lambda (env) (environment-lookup env exp)))
        ; Special forms
        ((define? exp) (analyze-define exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ; Extensions - Implement these if you want to
        ; ((quoted? exp) (eval-quoted exp env))
        ; ((begin? exp) (eval-begin exp env))
        ; ((cond? exp) (eval-cond exp env))
        ; ((assignment? exp) (eval-assignment exp env))   ; (set! ... )
        
        ; Application means calling a procedure
        ((application? exp) (analyze-application exp))
        ;(else (error "Bad expression")))
  ))
(define (seval exp env) #f)
(define (primitive? exp)
  (number? exp)
  )
; (define name value)
(define (define? exp)
  (and (pair? exp) (eq? (car exp) 'define)))
(define (define-name exp)
  (cadr exp)
  )
(define (define-value exp)
  (caddr exp)
  )
(define (analyze-define exp)
  (let ((name (define-name exp))
        (value (analyze (define-value exp))))
    ; This is a function that will do the define *later*
    (lambda (env) 
      (environment-define! env name (value env)))
  )
  )
; (quote value)
(define (quoted? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))
(define (eval-quote exp env)
  (cadr exp)   ; Quoting leaves the thing being quoted unevaluated.
  )
; (if test consequence alternative)
(define (if? exp)
  (and (pair? exp) (eq? (car exp) 'if)))
(define (if-test exp) (cadr exp))
(define (if-consequence exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))
(define (analyze-if exp)
  (let ((test (analyze (if-test exp)))
        (consequence (analyze (if-consequence exp)))
        (alternative (analyze (if-alternative exp))))
    (lambda (env)
      (if (test env) (consequence env) (alternative env))
      )
    )
  )
; (cond clause1 clause2 clause3 ... clausen)
; clause -> (test expr1 expr2 expr3 .. exprn)
(define (cond? exp)
  (and (pair? exp) (eq? (car exp) 'cond)))
(define (eval-cond exp env)
  'not-implemented
  )
; (lambda (args) expr1 expr2 ... exprn)
; User defined procedures
(define (lambda? exp)
  (and (pair? exp) (eq? (car exp) 'lambda)))
(define (lambda-params exp)
  (cadr exp))
(define (lambda-exprs exp)   ; A lambda can have multiple expressions (a list)
  (cddr exp)
  )
(define (analyze-lambda exp)
  (let ((parameters (lambda-params exp))
        (exprs (map (lambda (e) (analyze e)) (lambda-exprs exp))))
    (lambda (env)
      (lambda args   ; Note: args are evaluated by the code in application
        (let ((localenv (environment-new env)))
          (bind-arguments parameters args localenv)
          (eval-expressions exprs localenv) ; exprs are already analyzed above
          )
        )
      )))
; Bind names and values in an environment
(define (bind-arguments parameters args env)
  (if (null? parameters)
      null
      (begin
        (environment-define! env (car parameters) (car args))
        (bind-arguments (cdr parameters) (cdr args) env)))
  )
; Evaluate multiple expressions. Return the result of the last one
; exprs is a list of already analyzed exprs.  So you call with env.
(define (eval-expressions exprs env)
  (if (null? (cdr exprs))
      ((car exprs) env)   ; Return result of last expression
      (begin
        ((car exprs) env)
        (eval-expressions (cdr exprs) env)))
  )
; Procedure application (proc e1 e2 ... en)
(define (application? exp)
  (list? exp)     ; Test if exp is a list 
  )
(define (analyze-application exp)
  ; Make a call to a procedure.
  (let ((proc (analyze (car exp)))    ; Getting the procedure
        (args (analyze-arguments (cdr exp))))
    ; Make a procedure to call the procedure
    (lambda (env)
       (apply (proc env) (map (lambda (a) (a env)) args))   ; Python: proc(*args)
    )
  )
)
(define (analyze-arguments args)
  (map (lambda (a) (analyze a)) args)
  )
; You define. Environment is going to be a linked-list of hash tables
(define (make-environment)
  (cons (make-hash) null)
  )
(define (environment-lookup env exp)
  (cond ((null? env) (error "Unknown name"))
        ((hash-has-key? (car env) exp) (hash-ref (car env) exp))
        (else (environment-lookup (cdr env) exp))))
; Make a new definition.  This only occurs in the most local scope
(define (environment-define! env name value)
  (hash-set! (car env) name value)  ; Racket hash table set (not book)
  )
; Make a new child environment
(define (environment-new env)
  (cons (make-hash) env))
; The "default" global environment
(define env (make-environment))
; Define some builtin procedures (like +)
(environment-define! env '+ +)
(environment-define! env '- -)
(environment-define! env '* *)
(environment-define! env '/ /)
(environment-define! env '= =)
(environment-define! env '< <)
(environment-define! env '> >)
(environment-define! env '>= >=)
(environment-define! env '<= <=)
; Example of how you would use it (eventually)
; (seval '(+ 2 3) env)  ---> 5

;(define a (analyze '(define x 42)))
;env
;(a env)
;env
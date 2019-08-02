#lang racket
#lang racket
; AMB Evaluator (Chapter 4)
; (amb e1 e2 e3 ... en)
; (require expr)       ; Assertion
; 
(define (analyze exp)
  (cond ((primitive? exp) (lambda (env succeed fail) (succeed exp fail)))
        ((symbol? exp) (lambda (env succeed fail)
                         (succeed (environment-lookup env exp) fail)))
        ; Special forms
        ((define? exp) (analyze-define exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ; ((amb? exp) (analyze-amb exp))
        
        ; Extensions - Implement these if you want to
        ((quoted? exp) (analyze-quoted exp))
        ; ((begin? exp) (eval-begin exp env))
        ; ((cond? exp) (eval-cond exp env))
        ; ((assignment? exp) (eval-assignment exp env))   ; (set! ... )
        
        ; Application means calling a procedure
        ((application? exp) (analyze-application exp))
        ;(else (error "Bad expression")))
  ))
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
    (lambda (env succeed fail)
      (display name)
      (newline)
      (display value)
      (newline)
      (value env (lambda (result fail2)
                   (displayln "HERE")
                   (environment-define! env name result)
                   (succeed 'ok fail2)) fail)
  )
  )
  )
; (quote value)
(define (quoted? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))
(define (analyze-quoted exp)
  (lambda (env succeed fail)
    (succeed (cadr exp) fail)))
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
    (lambda (env succeed fail)
      (test env (lambda (test-value fail)
                  (if test-value
                      (consequence env succeed fail)
                      (alternative env succeed fail)
                      )
                  ) fail)
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
    (lambda (env succeed fail)
      (succeed
       ; This is broken.  Need to have success/fail arguments specifically
       ; for the function call itself.  (Not the definition of the funtion originally)
       ;
       (lambda args   ; Note: args are evaluated by the code in application
                      ; Need success/fail arguments here.
         (let ((localenv (environment-new env)))
           (bind-arguments parameters args localenv)
           (eval-expressions exprs localenv succeed fail)
           )
         ) fail)
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
;  [e1] -> [e2] -> [e3] -> ... [en] -> succeed
;
(define (eval-expressions exprs env succeed fail)
  (if (null? (cdr exprs))
      ((car exprs) env succeed fail)   ; Return result of last expression
      ((car exprs) env (lambda (result fail)
                         (eval-expressions (cdr exprs) env succeed fail)) fail)
  )
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
    (lambda (env succeed fail)
      ; Must evaluate the procedure part
      (proc env (lambda (pproc fail2)
                  (get-args args env
                            (lambda (argvalues fail3)
                              (succeed (apply pproc argvalues) fail3)
                              ) fail2))
            fail)
      )
    )
  )
; Must build a list of evaluated arguments
; See pg. 433
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args) fail3))
                                fail2))
                      fail)))
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
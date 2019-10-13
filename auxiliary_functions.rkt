#lang racket

(provide (all-defined-out))

(define init-prog
  (lambda (program)
    foldr (lambda (i d) (dict-set d (car i) (cdr i))) #hash() program))

(define state-set
  (lambda (state x e)
      (dict-set state x (cons 'quote (list e)))))

(define empty-state  #hash())

(define (init-state vars d)
  (if (equal? (length vars) (length d))
      (for/fold ([st empty-state])
                ([i vars]
                 [j d])
        (state-set st i j))
      (error "cannot initialize state")))

(define substitute-in-expression
  (lambda (st e)
    (match e
      [(cons x y) (cons (substitute-in-expression st x) (substitute-in-expression st y))]
      [x (if (dict-has-key? st x) (dict-ref st x) x)])))

; because stackoverflow https://stackoverflow.com/questions/28947041/unbound-identifier-racket-operators said so
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define eval-exp
  (lambda (st e)
    (eval (substitute-in-expression st e) ns)))

(define newtail 
  (lambda (Q label) 
    (member label Q (lambda (s t) (equal? s (car t))))))


(define safe-car
  (lambda (p)
    (if (eq? p '()) '_ (car p))))
(define safe-cdr
  (lambda (p)
    (if (eq? p '()) '() (cdr p))))

(define static-by-division?
  (lambda (division e)
    (match e
    [(cons x y) (and (static-by-division? division x) (static-by-division? division y))]
    [x (not (set-member? division x))])))

(define get-labels
  (lambda (program)
    (for/list ([bb (cdr program)])
      (car bb))))
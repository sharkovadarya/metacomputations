#lang racket

(provide (all-defined-out))

(define init-prog
  (lambda (program)
    foldr (lambda (i d) (dict-set d (car i) (cdr i))) #hash() program))

(define state-set
  (lambda (x e state)
      (dict-set state x (cons 'quote (list e)))))

(define empty-state #hash())

(define init-state
  (lambda (program data)
    (foldr state-set empty-state program data)))

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
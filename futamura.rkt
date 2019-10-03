#lang racket

(require "int.rkt")
(require "mix.rkt")

(define first-futamura-projection
  (lambda (interpreter program)
    (int mix `(,interpreter (Right Left) ((Q) (,program))))))

; test
(int (first-futamura-projection tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1))) `((1 1 0 1 1 0 1)))
#lang racket

(require "int.rkt")
(require "mix.rkt")

(define first-futamura-projection
  (lambda (interpreter program)
    (int mix `(,interpreter (Right Left) ((Q) (,program))))))

; test
(int (first-futamura-projection tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1))) `((1 1 0 1 1 0 1)))

(define second-futamura-projection
  (lambda (interpreter program)
    (int (int mix `(,mix
                    (vs0 pending marked residual-code current pp vs code)
                    ((program division) (,interpreter (Right Left))))) `(((Q) (,program))))))

;test
(int (second-futamura-projection tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1))) `((1 1 0 1 1 0 1)))

;compare generated code
(first-futamura-projection tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))
(second-futamura-projection tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))
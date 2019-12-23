#lang racket

(require "int.rkt")
(require "mix.rkt")

(define first-futamura-projection-tm
  (lambda (interpreter program)
    (int mix `(,interpreter (Right Left) ((Q) (,program))))))

(int (first-futamura-projection-tm tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1))) `((1 1 0 1 1 0 1)))

(define second-futamura-projection-tm
  (lambda (interpreter program)
    (int (int mix `(,mix
                    (vs0 pending marked residual-code current pp vs code)
                    ((program division) (,interpreter (Right Left))))) `(((Q) (,program))))))

;test
(int (second-futamura-projection-tm tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1))) `((1 1 0 1 1 0 1)))

;compare generated code
(first-futamura-projection-tm tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))
(second-futamura-projection-tm tm-int '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))


(define find-name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))))

(define first-futamura-projection-fc
  (lambda (interpreter program)
    (int mix `(,interpreter (data state rv) ((program) (,program))))))

(int (first-futamura-projection-fc fc-int find-name) `((x (x y z) (1 2 3))))

(define second-futamura-projection-fc
  (lambda (interpreter program)
    (int (int mix `(,mix
                    (vs0 pending marked residual-code current pp vs code)
                    ((program division) (,interpreter (data state rv))))) `(((program) (,program))))))

;test
(int (second-futamura-projection-fc fc-int find-name) `((x (x y z) (1 2 3))))

;compare generated code
(first-futamura-projection-fc fc-int find-name)
(second-futamura-projection-fc fc-int find-name)

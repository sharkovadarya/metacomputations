#lang racket

(require "auxiliary_functions.rkt")
(provide int tm-int)

(define int
  (lambda (program data)
    (let ([prog (init-prog (cdr program))]
          [state (init-state (cdar program) data)])
      (int-basic-block prog state (cdadr program)))))

(define int-basic-block
  (lambda (program state basic-block)
    (match basic-block
      ['() (error "no basic block found")]
      [`(,head)
       (int-jump program state head)]
      [`(,head . ,tail)
       (int-basic-block program (int-assignment state head) tail)])))

(define int-jump
  (lambda (program state instruction)
    (match instruction
      [`(goto, label) (int-basic-block program state (dict-ref program label))]
      [`(if ,expression ,label1 ,label2)
       (if (eval-exp state expression)
           (int-basic-block program state (dict-ref program label1))
           (int-basic-block program state (dict-ref program label2)))]
      [`(return ,expression) (eval-exp state expression)])))

(define int-assignment
  (lambda (state assignment)
    (match assignment
      [`(:= ,variable ,expression) (let ([new-value (eval-exp state expression)]) (state-set state variable new-value))]
      [_ (error "no assignment found")])))

(define tm-int
  `((read Q Right)
    (init (:= Qtail Q) (:= Left '()) (goto loop))
    (loop (if (eq? Qtail '()) stop cont))
    (cont  (:= Instruction (cdar Qtail))           
           (:= Qtail (cdr Qtail))
           (:= operator (car Instruction))
           (if (equal? operator 'right) do-right cont1))
    (cont1 (if (equal? operator 'left)  do-left  cont2))
    (cont2 (if (equal? operator 'write) do-write cont3))
    (cont3 (if (equal? operator 'goto)  do-goto  cont4))
    (cont4 (if (equal? operator 'if)    do-if    error))
    (do-right (:= Left (cons (safe-car Right) Left))
              (:= Right (safe-cdr Right))
              (goto loop))
    (do-left  (:= Right (cons (safe-car Left) Right))
              (:= Left (safe-cdr Left))
              (goto loop))
    (do-write (:= Symbol (cadr Instruction))
              (:= Right (cons Symbol (cdr Right)))
              (goto loop))
    (do-goto  (:= Nextlabel (cadr Instruction))
              (goto jump))
    (do-if    (:= Symbol (cadr Instruction))
              (:= Nextlabel (cadddr Instruction))
              (if (equal? Symbol (safe-car Right)) jump loop))
    (jump (:= Qtail (newtail Q Nextlabel))
          (goto loop))
    (error  (return `(unknown instruction ,Instruction)))
    (stop (return Right))))
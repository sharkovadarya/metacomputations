#lang racket

(require "auxiliary_functions.rkt")
(provide int tm-int fc-int)

(define int
  (lambda (program data)
    (let ([prog (init-prog (cdr program))]
          [state (init-state (cdar program) data)])
      (int-basic-block prog state (caadr program)))))

(define (int-basic-block program state label)
  (let ([statements (dict-ref program label)])
    (match statements
      [(list assignments ... jump)
       (let()
         (for ([assignment assignments])
           (int-assignment state assignment))
         (int-jump program state jump))]
      [_ (error "no basic block found")])))

(define (int-assignment state assignment)
  (match assignment
    [`(:= ,variable ,expressioni) (let () (state-set state variable (eval-exp state expressioni)))]
    [_ (error "no assignment found")]))


(define (int-jump program state jump)
  (match jump
    [`(goto ,label) (int-basic-block program state label)]
    [`(if ,expression ,label1 ,label2)
     (if (eval-exp state expression)
         (int-basic-block program state label1)
         (int-basic-block program state label2))]
    [`(return ,expression) (eval-exp state expression)]))

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
    (jump  (:= Qtail (newtail Q Nextlabel))
           (goto cont))
    (error (return `(unknown instruction ,Instruction)))
    (stop   (return Right))))

(define fc-int
  `((read program data)
    (init (:= state (init-state (cdar program) data))
          (:= prog (init-prog (cdr program)))
          (:= label (caadr program))
          (goto loop))

    (loop (:= commands (dict-ref prog label))
          (goto cont))

    (cont (:= command (car commands))
          (:= commands (cdr commands))
          (if (equal? (car command) `:=)  do-assignment  cont1))
    (cont1 (if (equal? (car command) 'if) do-if cont2))
    (cont2 (if (equal? (car command) 'goto) do-jump cont3))
    (cont3 (if (equal? (car command) 'return) do-return error))
    (error (return 'undefined_command))

    (do-assignment (:= rv (state-set state (cadr command) (eval-exp state (caddr command))))
                   (goto cont))

    (do-if (if (eval-exp state (cadr command))  do-then  do-else))
    (do-then (:= label (caddr command))
             (goto loop))
    (do-else (:= label (cadddr command))
             (goto loop))

    (do-jump (:= label (cadr command))
             (goto loop))

    (do-return (return (eval-exp state (cadr command))))))
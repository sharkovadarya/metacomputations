#lang racket

(require "auxiliary_functions.rkt")
(provide mix)

(define mix 
  `((read program division vs0)
    (init (:= pp0 (caadr program))
          (:= pending `((,pp0 ,(init-state (car vs0) (cadr vs0)))))
          (:= marked '())
          (:= residual-code `(,(cons 'read (set-subtract (cdar program) (car vs0)))))
          (goto pending-loop))
    
    (pending-loop (if (equal? pending '()) pending-loop-stop pending-loop-continue))
    
    (pending-loop-continue (:= current (car pending))                           
                           (:= pending (cdr pending))
                           (:= pp (car current))
                           (:= vs (cadr current))
                           (:= code (cons current '()))                           
                           (:= labels (get-labels program division))
                           (goto label-loop))

    
    (label-loop (if (eq? labels '()) label-not-found label-loop-cont))
    
    (label-not-found (return `(label_not_found)))
    
    (label-loop-cont (:= pp1 (car labels))
                     (:= labels (cdr labels))
                     (if (equal? pp1 pp) get-bb label-loop))
    
    (get-bb (:= bb (dict-ref program pp1))
            (goto block-loop))
    

    (block-loop (if (empty? bb) block-loop-stop block-loop-cont))
    
    (block-loop-cont (:= command (car bb))
                     (:= bb (cdr bb))
                     (if (equal? (car command) ':=) do-assignment cont1))

    (cont1 (if (equal? (car command) 'if) do-if cont2))
    (cont2 (if (equal? (car command) 'goto) do-goto cont3))
    (cont3 (if (equal? (car command) 'return) do-return (error "unknown command")))
    

    (do-assignment (:= x (cadr command))
                   (:= exp (caddr command))
                   (if (static-by-division? division x) do-static-assignment do-dynamic-assignment))
    
    (do-static-assignment  (:= vs (state-set vs x (eval-exp vs exp)))
                           (goto block-loop))
    
    (do-dynamic-assignment (:= code (cons `(:= ,x ,(substitute-in-expression vs exp)) code))
                           (goto block-loop))

          
    (do-if (:= exp (cadr command))
           (:= then-label (caddr command))
           (:= else-label (cadddr command))
           (if (static-by-division? division exp) do-static-if do-dynamic-if))
    
    (do-static-if (if (eval-exp vs exp) static-if-then static-if-false))
    
    (static-if-then (:= bb (dict-ref program then-label))
                    (goto block-loop))
    
    (static-if-false (:= bb (dict-ref program else-label))
                     (goto block-loop))
    
    (do-dynamic-if (:= pending (if (set-member? marked `(,then-label ,vs)) pending (cons `(,then-label ,vs) pending)))                   
                   (:= pending (if (set-member? marked `(,else-label ,vs)) pending (cons `(,else-label ,vs) pending)))
                   (:= marked (cons `(,else-label ,vs) (cons `(,then-label ,vs) marked)))
                   (:= code (cons `(if ,(substitute-in-expression vs exp) (,then-label ,vs) (,else-label ,vs)) code))
                   (goto block-loop))
    
        
    (do-goto (:= bb (dict-ref program (cadr command)))
             (goto block-loop))

    (do-return (:= exp (cadr command)) 
               (:= code (cons `(return ,(substitute-in-expression vs exp)) code))
               (goto block-loop))

    (block-loop-stop (:= residual-code (cons (reverse code) residual-code))
                     (goto pending-loop))

    (pending-loop-stop (return (reverse residual-code)))))
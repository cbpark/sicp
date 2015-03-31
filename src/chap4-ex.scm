(load "chap4")

;;; Exercise 4.1

(define (list-of-values1 exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values1 (rest-operands exps) env)))
          (cons first rest)))))

(define (list-of-values2 exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values2 (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))

;;; Exercise 4.4

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        (else (error "Unknown expression type: EVAL" exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (and-operands exp) (cdr exp))
(define (and->if exp) (expand-and-operands (and-operands exp)))
(define (expand-and-operands operands)
  (if (null? operands)
      'true
      (make-if (car operands)
               (expand-and-operands (cdr operands))
               'false)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-operands exp) (cdr exp))
(define (or->if exp) (expand-or-operands (or-operands exp)))
(define (expand-or-operands operands)
  (if (null? operands)
      'false
      (make-if (car operands)
               'true
               (expand-or-operands (cdr operands)))))

;;; Exercise 4.5

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (let ((test (cond-predicate first))
                  (recepient (if (eq? (car (cond-actions first)) '=>)
                                 (cadr (cond-actions first))
                                 false)))
              (make-if test
                       (if recepient
                           (list recepient test)
                           (sequence->exp (cond-actions first)))
                       (expand-clauses rest)))))))

;;; Exercise 4.6

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        ((let? exp) (eval (let->combination exp)))
        ((let*? exp) (eval (let*->nested-lets exp)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp) (map car (cadr exp)))
(define (let-exps exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp))
        (let-exps exp)))

;;; Exercise 4.7

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-body exp) (caddr exp))
(define (let*-exps exp) (cadr exp))
(define (let*->nested-lets exp)
  (let ((exps (let*-exps exp))
        (body (let*-body exp)))
    (define (make-lets exps)
      (if (null? exps)
          body
          (list 'let (list (car exps)) (make-lets (cdr exps)))))
    (make-lets exps)))

;;; Exercise 4.8

(define (named-let? exp) (and (let? exp) (symbol? (cadr exp))))
(define (named-let-name exp) (cadr exp))
(define (named-let-body exp) (cadddr exp))
(define (named-let-parameters exp) (map car (caddr exp)))
(define (named-let-exps exp) (map cadr (caddr exp)))
(define (named-let->func exp)
  (list 'define (cons (named-let-name exp)
                      (named-let-parameters exp))
        (named-let-body exp)))
(define (let->combination exp)
  (if (named-let? exp)
      (sequence->exp
       (list (named-let->func exp)
             (cons (named-let-name exp)
                   (named-let-exps exp))))
      (cons (make-lambda (let-vars exp)
                         (list (let-body exp)))
            (let-exps exp))))

;;; Exercise 4.11

(define (make-frame vars vals)
  (map cons (vars vals)))

(define (add-binding-to-frame! var val frame)
  (define (add-binding! binding frame)
    (if (null? (cdr frame))
        (set-cdr! frame binding)
        (add-binding! binding (cdr frame))))
  (add-binding! (list (cons var val)) frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (let ((binding (assoc var frame)))
        (if binding
            (cdr binding)
            (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (let ((binding (assoc var frame)))
        (if binding
            (set-cdr! binding val)
            (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((binding (assoc var frame)))
      (if binding
          (set-cdr! binding val)
          (add-binding-to-frame! var val frame)))))

;;; Exercise 4.12

(define (frame-binding var frame)
  (assoc var frame))

(define (set-binding-in-frame! var val frame)
  (set-cdr! (frame-binding var frame) val))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (if (frame-binding var frame)
              (cdr (frame-binding var frame))
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (if (frame-binding var frame)
              (set-binding-in-frame! var val frame)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (frame-binding var frame)
        (set-binding-in-frame! var val frame)
        (add-binding-to-frame! val val frame))))

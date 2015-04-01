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

;;; Exercise 4.16

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (if (eq? (car vals) '*unassigned*)
                                      (error "Unassigned value" var)
                                      (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (scan-out-defines body)
  (define (name-unassigned defines)
    (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines))
  (define (set-values! defines)
    (map (lambda (x) (list 'set! (definition-variable x) (definition-value x)))
         defines))
  (define (defines->let exps defines not-defines)
    (cond ((null? exps)
           (if (null? defines)
               body
               (list (list 'let (name-unassigned defines)
                           (make-begin (append (set-values! defines)
                                               (reverse not-defines)))))))
          ((definition? (car exps))
           (defines->let (cdr exps) (cons (car exps) defines) not-defines))
          (else
           (defines->let (cdr exps) defines (cons (car exps) not-defines)))))
  (defines->let body '() '()))

;;; Exercise 4.20

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-inits exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (declare-variables exp)
  (map (lambda (x) (list (car x) '*unassigned*))
       (letrec-inits exp)))
(define (set-variables! exp)
  (map (lambda (x) (list 'set! (car x) (cadr x)))
       (letrec-inits exp)))
(define (letrec->let exp)
  (list 'let (declare-variables exp)
        (make-begin (append (set-variables! exp) (letrec-body exp)))))

;;; Exercise 4.22

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        ((let? exp) (analyze (let->combination exp)))
        (else (error "Unknown expression type: ANALYZE" exp))))

;;; Exercise 4.35

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;;; Exercise 4.36

(define (a-pythagorean-triple-greater-than low)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;;; Exercise 4.39

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (distinct? baker cooper fletcher miller smith))
    (list (list 'baker baker) (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))

;;; Exercise 4.40

(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((flether (amb 2 3 4)))
      (require (not (or (= flether cooper)
                        (= flether miller)
                        (= (abs (- cooper fletcher)) 1))))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (or (= smith cooper)
                          (= smith miller)
                          (= smith flether)
                          (= (abs (- smith fletcher)) 1))))
        (let ((baker (amb 1 2 3 4)))
          (require (distinct? (list baker cooper fletcher smith smiller)))
          (list (list 'baker baker) (list 'cooper cooper)
                (list 'fletcher fletcher) (list 'miller miller)
                (list 'smith smith)))))))

;;; Exercise 4.41

(define (flatmap proc li)
  (if (null? li)
      '()
      (let ((result (proc (car li)))
            (rest (flatmap proc (cdr li))))
        (if (pair? result)
            (append result rest)
            (cons result rest)))))

(define (permutations lists)
  (if (null? lists)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (y) (cons x y)) (permutations (cdr lists))))
               (car lists))))

(define (restrictions l)
  (apply (lambda (baker cooper fletcher miller smith)
           (and (> miller cooper)
                (not (= (abs (- smith fletcher)) 1))
                (not (= (abs (- fletcher cooper)) 1))
                (distinct? (list baker cooper fletcher miller smith))))
         l))

(define (mutiple-dwelling)
  (let ((baker '(1 2 3 4))
        (cooper '(2 3 4 5))
        (fletcher '(2 3 4))
        (miller '(3 4 5))
        (smith '(1 2 3 4 5)))
    (filter restrictions (permutations (list baker cooper fletcher miller smith)))))

;;; Exercise 4.42

(define (liars)
  (define (xor a b)
    (or (and a (not b)) (and (not a) b)))
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty) (list 'ethel ethel)
          (list 'joan joan) (list 'kitty kitty)
          (list 'mary mary))))

;;; Exercise 4.43

(define (sailors)
  (define father first)
  (define daughter second)
  (define yacht third)
  (define (different-names father)
    (not (eq? (daughter father) (yacht father))))
  (let ((moore (list 'moore 'mary-ann 'lorna))
        (hood (list 'hood  'melissa 'gabrielle))
        (hall (list 'hall (amb 'gabrielle 'lorna) 'rosalind))
        (downing (list 'downing (amb 'gabrielle 'lorna 'rosalind) 'melissa))
        (parker (list 'parker  (amb 'gabrielle 'lorna 'rosalind) 'mary-anne)))
    (let ((gabrielle-father (amb hall downing parker))
          (lorna-father (amb hall downing parker)))
      (require (eq? (daughter gabrielle-father) 'gabrielle))
      (require (eq? (daughter lorna-father) 'lorna))
      (require (different-names lorna-father))
      (require (different-names gabrielle-father))
      (require (eq? (daughter parker) (yacht gabrielle-father)))
      lorna-father)))

;;; Exercise 4.51

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((permanant-set? exp) (analyze-permanant-set exp))
        ((if-fail? exp) (analyze-if-fail exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (pernamenant-set? exp) (tagged-list? exp 'pernamenant-set!))
(define (analyze-pernamenant-set exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok  fail2))
             fail))))

;;; Exercise 4.52

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (analyze-if-fail exp)
  (let ((first (analyze (cadr exp)))
        (second (analyze (caddr exp))))
    (lambda (env succeed fail)
      (first env
             (lambda (value fail2) (succeed value fail2))
             (lambda () (second env succeed fail))))))

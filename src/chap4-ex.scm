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

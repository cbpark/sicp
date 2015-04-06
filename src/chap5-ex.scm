(load "chap5")

;;; Exercise 5.9

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs (map (lambda (e)
                       (if (label-exp? e)
                           (error "Can't operate on label" e)
                           (make-primitive-exp e machine labels)))
                     (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

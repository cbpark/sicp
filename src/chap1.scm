;;;
;;; 1. Building Abstraction with Procedures
;;;

;;; 1.1 The Elements of Programming

;;; 1.1.1 Expressions

;;; 1.1.2 Naming and the Environment

;;; 1.1.3 Evaluating Combinations

;;; 1.1.4 Compound Procedures

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;;; 1.1.5 The Substitution Model for Procedure Application

;;; Applicative order versus normal order

;;; 1.1.6 Conditional Expressions and Predicates

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else    x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))

;;; 1.1.7 Example: Square Roots by Newton's Method

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; 1.1.8 Procedures as Black-Box Abstractions

(define (square x)
  (* x x))

(define (square x)
  (exp (double (log x))))

(define (double x)
  (+ x x))

;;; Local names

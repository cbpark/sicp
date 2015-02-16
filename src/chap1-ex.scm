(load "chap1")

;;; Exercise 1.3
(define (sum-of-squares-of-two-larger-numbers x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((<= y z)                (sum-of-squares x z))
        (else                    (sum-of-squares x y))))

;;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -) a b))

;;; Exercise 1.5
(define (p)
  (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;;; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else      else-clause)))

;;; Exercise 1.7
(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (good-enough? guess prev-guess)
  (< (/ (abs (- guess prev-guess)) guess) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

;;; Exercise 1.8
(define (cbrt-iter guess pre-guess x)
  (if (good-enough? guess pre-guess)
      guess
      (cbrt-iter (improve-cube guess x) guess x)))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess)) 3))

(define (cbrt x)
  (cbrt-iter 1.0 0.0 x))

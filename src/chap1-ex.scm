(load "chap1")

;;; Exercise 1.3

(define (sum-of-squares-of-two-larger-numbers x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((<= y z) (sum-of-squares x z))
        (else (sum-of-squares x y))))

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
        (else else-clause)))

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

;;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (f n)
  (A 0 n))

(define (g n)
  (A 1 n))

(define (h n)
  (A 2 n))

(define (k n)
  (* 5 n n))

;;; Exercise 1.11

(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n))

;;; Exercise 1.12

(define (pascals-triangle row col)
  (cond ((or (> col row) (< col 0)) 0)
        ((= col 1) 1)
        (else (+ (pascals-triangle (- row 1) (- col 1))
                 (pascals-triangle (- row 1) col)))))

;;; Exercise 1.15

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

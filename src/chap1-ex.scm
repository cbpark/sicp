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

;;; Exercise 1.16

(define (expt b n)
  (expt-iter 1 b n))

(define (expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-iter a (square b) (/ n 2)))
        (else (expt-iter (* a b) b (- n 1)))))

;;; Exercise 1.17

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(define (halve n)
  (/ n 2))

;;; Exercise 1.18

(define (mult a b)
  (mult-iter 0 a b))

(define (mult-iter x a b)
  (cond ((= b 0) x)
        ((even? b) (mult-iter x (double a) (halve b)))
        (else (mult-iter (+ a x) a (- b 1)))))

;;; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (square p) (square q))
                                 (+ (square q) (double (* p q)))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;; Exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (runtime) start-time))))

(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (if (even? a)
      (search-for-primes (+ a 1) b)
      (cond ((< a b) (timed-prime-test a)
             (search-for-primes (+ a 2) b)))))

;;; Exercise 1.23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; Exercise 1.24

(define (timed-prime-test-fast n)
  (newline)
  (display n)
  (start-prime-test-fast n (runtime)))

(define (start-prime-test-fast n start-time)
  (if (fast-prime? n 100)
      (report-time (- (runtime) start-time))))

(define (search-for-primes-fast a b)
  (if (even? a)
      (search-for-primes-fast (+ a 1) b)
      (cond ((< a b) (timed-prime-test-fast a)
             (search-for-primes-fast (+ a 2) b)))))

;;; Exercise 1.27

(define (fermat-full n)
  (define (f-iter a)
    (cond ((= a 1) true)
          ((not (fermat-test n a)) false)
          (else (f-iter (- a 1)))))
  (define (fermat-test n a)
    (= (expmod a n n) a))
  (f-iter (- n 1)))

;;; Exercise 1.28

(define (expmod2 base exp m)
  (define (square-check x n)
    (if (and (not (or (= x 1) (= x (- n 1))))
             (= (remainder (square x) n) 1))
        0
        (remainder (square x) n)))
  (cond ((= exp 0) 1)
        ((even? exp) (square-check (expmod2 base (/ exp 2) m) m))
        (else (remainder (* base (expmod2 base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod2 a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

(define (fast-prime2 n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime2 n (- times 1)))
        (else false)))

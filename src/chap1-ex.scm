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

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

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

;;; Exercise 1.29

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4)) (y k)))
  (* (sum term 0 inc n) (/ h 3.0)))

;;; Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;; Exercise 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approx n)
  (define (term k)
    (/ (* 4 (square k)) (- (* 4 (square k)) 1)))
  (* 2.0 (product term 1 inc n)))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;;; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;; Exercise 1.33

(define (filtered-accumulate combiner null-value term a next b predicate)
  (cond ((> a b) null-value)
        ((predicate a) (combiner (term a)
                                 (filtered-accumulate combiner
                                                      null-value term
                                                      (next a) next b predicate)))
        (else (filtered-accumulate combiner
                                   null-value term
                                   (next a) next b predicate))))

(define (sum-primes-squared a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-relatively-prime n)
  (define (relatively-prime k)
    (= (gcd k n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) relatively-prime))

;;; Exercise 1.34

(define (f g)
  (g 2))

;;; Exercise 1.35

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;;; Exercise 1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-to-x)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (x-to-x-average)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))

;;; Exercise 1.37

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))

(define (cont-frac-iter n d k)
  (define (frac-iter i result)
    (if (= i 0)
        result
        (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (frac-iter (- k 1) (/ (n k) (d k))))

;;; Exercise 1.38

(define (e-approx k)
  (define (d i)
    (if (not (= 0 (remainder (+ i 1) 3)))
        1
        (* 2 (/ (+ i 1) 3))))
  (+ 2 (cont-frac (lambda (i) 1.0) d k)))

;;; Exercise 1.39

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

;;; Exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) 1)))

;;; Exercise 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

;;; Exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;; Exercise 1.43

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

;;; Exercise 1.44

(define (smooth f)
  (/ (f (- x dx)) (f x) (f (+ x dx))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

;;; Exercise 1.45

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (lambda (y)
                  (/ x (expt y (- n 1)))))
               1.0))

;;; Exercise 1.46

(define (iter-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iter-improve (lambda (guess)
                   (< (abs (- (square guess) x)) 0.001))
                 (lambda (guess)
                   (average guess (/ x guess))))
   1.0))

(define (iter-improve-fixed-point f guess)
  ((iter-improve (lambda (guess)
                   (< (abs (- (f guess) guess)) 0.00001))
                 (lambda (guess)
                   (f guess)))
   guess))

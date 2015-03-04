(load "chap2")

;;; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* -1 n) g) (/ (* -1 d) g))
        (cons (/ n g) (/ d g)))))

;;; Exercise 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
                (/ (+ (y-point start) (y-point end)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;; Exercise 2.3

(define (make-rect x y)
  (cons x y))

(define (rect-width r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (rect-height r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

(define (rect-perimeter r)
  (* 2 (+ (rect-width r) (rect-height r))))

(define (rect-area r)
  (* (rect-width r) (rect-height r)))

;;; Exercise 2.5

(define (cons2 a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car2 x)
  (num-divs x 2))

(define (cdr2 x)
  (num-divs x 3))

(define (num-divs n d)
  (define (iter x result)
    (if (= 0 (remainder x d))
        (iter (/ x d) (+ result 1))
        result))
  (iter n 0))

;;; Exercise 2.6

(define zero (lambda (f)
               (lambda (x)
                 x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one (lambda (f)
              (lambda (x)
                (f x))))

(define two (lambda (f)
              (lambda (x)
                (f (f x)))))

(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

;;; Exercise 2.7

(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

;;; Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;; Exercise 2.9

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;;; Exercise 2.10

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "The interval spans zero." y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;;; Exercise 2.11

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((> lx 0) (cond ((> ly 0) (make-interval (* lx ly) (* ux uy)))
                          ((< uy 0) (make-interval (* ux ly) (* lx uy)))
                          (else     (make-interval (* ux ly) (* uy ux)))))
          ((> ly 0) (cond ((< ux 0) (make-interval (* uy lx) (* ly ux)))
                          (else     (make-interval (* lx uy) (* ux uy)))))
          ((< ux 0) (cond ((< uy 0) (make-interval (* ux uy) (* lx ly)))
                          (else     (make-interval (* lx uy) (* lx ly)))))
          ((< uy 0)                 (make-interval (* ux ly) (* lx ly)))
          (else (let ((p1 (* (lower-bound x) (lower-bound y)))
                      (p2 (* (lower-bound x) (upper-bound y)))
                      (p3 (* (upper-bound x) (lower-bound y)))
                      (p4 (* (upper-bound x) (upper-bound y))))
                  (make-interval (min p1 p2 p3 p4)
                                 (max p1 p2 p3 p4)))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;;; Exercise 2.12

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

;;; Exercise 2.13

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;; Exercise 2.17

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

;;; Exercise 2.18

(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items)) (list (car items)))))

;;; Exercise 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

;;; Exercise 2.20

(define (same-parity x . items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (if (= (remainder (+ x (car items)) 2) 0)
                              (append result (list (car items)))
                              result))))
  (iter items (list x)))

;;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;;; Exercise 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (square (car things))
                                 answer))))
  (iter items '()))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons answer
                                 (square (car things))))))
  (iter items '()))

;;; Exercise 2.23

(define (for-each proc items)
  (cond ((null? items) '())
        (else (proc (car items))
              (for-each proc (cdr items)))))

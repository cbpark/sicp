;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Modularity, Objects, and State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 3.1 Assignment and Local State
;;;

;;; 3.1.1 Local State Variables

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;; (define W1 (make-withdraw 100))
;; (define W2 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

;; (define acc (make-account 100))

;;; 3.1.2 The Benefits of Introducing Assignment

;; (define rand (let ((x random-init))
;;                (lambda ()
;;                  (set! x (rand-update x))
;;                  x)))

;; (define (estimate-pi trials)
;;   (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;; (define (cesaro-test)
;;   (= (gcd (rand) (rand) 1)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((= (gcd x1 x2) 1) (iter (- trials-remaining 1)
                                       (+ trials-passed 1)
                                       x2))
              (else (iter (- trials-remaining 1)
                          trials-passed
                          x2))))))
  (iter trials 0 initial-x))

;;; 3.1.3 The Costs of Introducing Assignment

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balace))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

;;; Sameness and change

;;; Pitfalls of imperative programming

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter counter) (+ counter 1))))
  (iter 1 1))

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;;;
;;; 3.2 The Environment Model of Evaluation
;;;

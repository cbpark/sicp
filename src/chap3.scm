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

;;; 3.2.1 The Rules for Evaluation

;;; 3.2.2 Applying Simple Procedures

;;; 3.2.3 Frames as the Repository of Local State

;;; 3.2.4 Internal Definitions

;;;
;;; 3.3 Modeling with Mutable Data
;;;

;;; 3.3.1 Mutable List Structure

;; (define (cons x y)
;;   (let ((new (get-new-pair)))
;;     (set-car! new x)
;;     (set-cdr! new y)
;;     new))

;;; Sharing and identity

;;; Mutation is just assignment

;; (define (cons x y)
;;   (define (set-x! v) (set! x v))
;;   (define (set-y! v) (set! y v))
;;   (define (dispatch m)
;;     (cond ((eq? m 'car) x)
;;           ((eq? m 'cdr) y)
;;           ((eq? m 'set-car!) set-x!)
;;           ((eq? m 'set-cdr!) set-y!)
;;           (else (error "Undefined operation: CONS" m))))
;;   dispatch)
;; (define (car z) (z 'car))
;; (define (cdr z) (z 'cdr))
;; (define (set-car! z new-value)
;;   ((z 'set-car!) new-value) z)
;; (define (set-cdr! z new-value)
;;   ((z 'set-cdr!) new-value) z)

;;; 3.3.2 Representing Queues

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

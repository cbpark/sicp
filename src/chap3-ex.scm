(load "chap3")

;;; Exercise 3.1

(define (make-accumulator value)
  (lambda (x)
    (set! value (+ value x))
    value))

;;; Exercise 3.2

(define (make-monitored f)
  (define counter 0)
  (define (dispatch x)
    (cond ((eq? x 'how-many-calls?) counter)
          ((eq? x 'reset-count) (set! counter 0))
          (else (set! counter (+ counter 1))
                (f x))))
  dispatch)

;;; Exercise 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (not (eq? p password))
        (lambda (amount) "Incorrect password")
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))))
  dispatch)

;;; Exercise 3.4

(define (make-account balance password)
  (define incorrect-pass-counter 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               (set! incorrect-pass-counter 0)
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    (set! incorrect-pass-counter 0)
    balance)
  (define (call-the-cops) (lambda (amount) "Call the cops!"))
  (define (dispatch p m)
    (if (not (eq? p password))
        (if (>= incorrect-pass-counter 7)
            (call-the-cops)
            (lambda (amount)
              (set! incorrect-pass-counter (+ incorrect-pass-counter 1))
              "Incorrect password"))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))))
  dispatch)

;;; Exercise 3.5

(define (circ-pred cx cy r)
  (lambda (x y)
    (<= (+ (square (- x cx)) (square (- y cy))) (square r))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (rect-area x1 x2 y1 y2)
    (let ((width (- x2 x1))
          (height (- y2 y1)))
      (* width height)))
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials experiment) (rect-area x1 x2 y1 y2)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;; Exercise 3.6

(define random-init 0)
(define (rand-update x) (+ x 1))

(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate) (begin (set! x (rand-update x))
                                      x))
            ((eq? m 'reset) (lambda (new-value) (set! x new-value)))))
    dispatch))

;;; Exercise 3.7

(define (make-account balance password)
  (let ((password-list (list password)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (make-joint new-password)
      (set! password-list (cons new-password password-list))
      dispatch)
    (define (dispatch p m)
      (if (not (memq p password-list))
          (lambda (amount) "Incorrect password")
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'make-joint) make-joint)
                (else (error "Unknown request: MAKE-ACCOUNT" m)))))
    dispatch))

(define (make-joint account old-password new-password)
  ((account old-password 'make-joint) new-password))

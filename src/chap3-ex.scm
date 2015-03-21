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

;;; Exercise 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

;;; Exercise 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;; Exercise 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;;; Exercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;; Exercise 3.17

(define (count-pairs x)
  (let ((counted '()))
    (define (counter x)
      (if (or (not (pair? x)) (memq x counted))
          0
          (begin
            (set! counted (cons x counted))
            (+ (counter (car x)) (counter (cdr x)) 1))))
    (counter x)))

;;; Exercise 3.18

(define (cycle? x)
  (let ((visited '()))
    (define (iter x)
      (set! visited (cons x visited))
      (cond ((null? (cdr x)) false)
            ((memq (cdr x) visited) true)
            (else (iter (cdr x)))))
    (iter x)))

;;; Exercise 3.19

(define (cycle? x)
  (define (iter a b)
    (cond ((not (pair? a)) false)
          ((not (pair? b)) false)
          ((eq? a b) true)
          ((eq? a (cdr b)) true)
          (else (iter (cdr a) (cddr b)))))
  (iter (cdr x) (cddr x)))

;;; Exercise 3.21

(define (print-queue queue)
  (display (car queue)))

;;; Exercise 3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
               (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
              (else (set-cdr! (rear-ptr queue) new-pair)
                    (set-rear-ptr! queue new-pair)
                    queue))))
    (define (delete-queue!)
      (cond ((empty-queue? queue)
             (error "DELETE! called with an empty queue" queue))
            (else (set-front-ptr! queue (cdr (front-ptr queue)))
                  queue)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "QUEUE: undefined operation" m))))
    dispatch))

;;; Exercise 3.23

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque)
  (and (null? (front-ptr deque)) (null? (rear-ptr deque))))

(define (make-deque) (cons '() '()))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           '())
          (else
           (set-cdr! (cdr new-pair) (front-ptr deque))
           (set-car! (cdr (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)
           '()))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           '())
          (else
           (set-car! (cdr new-pair) (rear-ptr deque))
           (set-cdr! (cdr (rear-ptr deque)) new-pair)
           (set-rear-ptr! deque new-pair)
           '()))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         '())
        (else
         (set-front-ptr! deque (cddr (front-ptr deque)))
         (set-car! (cdr (front-ptr deque)) '())
         '())))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         '())
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (set-cdr! (cdr (rear-ptr deque)) '())
         '())))

(define (print-deque deque)
  (define (print q)
    (if (null? q)
        '()
        (cons (car q)
              (print (cddr q)))))
  (display (print (front-ptr deque))))

;;; Exercise 3.24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1 (cons key-2 value))
                                        (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;;; Exercise 3.25

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup keys)
      (define (iter table keys)
        (cond ((null? table) false)
              ((null? keys) false)
              (else (iter (assoc (car keys) (cdr table)))))
        (iter local-table keys)))
    (define (insert! keys value)
      (define (iter keys table)
        (if (null? keys)
            (set-cdr! table value)
            (let ((subtable (assoc (car keys) (cdr table))))
              (if subtable
                  (iter (cdr keys) subtable)
                  (append-item keys value table)))))
      (define (append-item keys value table)
        (if (null? keys)
            (set-cdr! table value)
            (let ((new-record (list (car keys))))
              (set-cdr! table (cons new-record (cdr table)))
              (append-item (cdr keys) value new-record))))
      (iter keys local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;;; Exercise 3.26

(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (car x) (car (entry set))) set)
        ((< (car x) (car (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        (else (make-tree (entry set)
                         (left-branch set)
                         (adjoin-set x (right-branch set))))))

(define (make-table)
  (let ((local-table '()))
    (define (lookup key records)
      (cond ((null? records) false)
            ((= key (car (entry records))) (entry records))
            ((< key (car (entry records))) (lookup key (left-branch records)))
            (else (lookup key (right-branch records)))))
    (define (insert! key value)
      (let ((record (lookup key local-table)))
        (if record
            (set-cdr! record value)
            (set! local-table (adjoin-set (cons key value) local-table)))))
    (define (get key)
      (lookup key local-table))
    (define (dispatch m)
      (cond ((eq? m 'get-proc) get)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;;; Exercise 3.28
;;; See chap3.scm

;;; Exercise 3.29

(define (or-gate a1 a2 output)
  (let ((x (make-wire))
        (y (make-wire))
        (z (make-wire)))
    (inverter a1 x)
    (inverter a2 y)
    (and-gate x y z)
    (inverter z output)))

;;; Exercise 3.39

(define (ripple-carry-adder a b s c)
  (let ((c-in (make-wire)))
    (if (null? (cdr a))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c)))

;;; Exercise 3.33

(define (averager a b average)
  (let ((sum (make-connector))
        (two (make-connector)))
    (adder a b sum)
    (constant 2 two)
    (multiplier two average sum)))

;;; Exercise 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;;; Exercise 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv n)
  (let ((c (make-connector)))
    (constant n c)
    c))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

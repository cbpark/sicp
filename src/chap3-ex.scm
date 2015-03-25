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

;;; Exercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc
                                           (map stream-cdr argstreams))))))

;;; Exercise 3.51

(define (show x)
  (display-line x)
  x)

;;; Exercise 3.52

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq (stream-map accum
                        (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

;;; Exercise 3.54

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))

;;; Exercise 3.55

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))

;;; Exercise 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< s1car s2car) (cons-stream s1car
                                                    (merge (stream-cdr s1) s2)))
                      ((> s1car s2car) (cons-stream s2car
                                                    (merge s1 (stream-cdr s2))))
                      (else (cons-stream s1car
                                         (merge (stream-cdr s1)
                                                (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;;; Exercise 3.58

(define (expand num den radix)
  (cons-stream (quotient (* num radix) den)
               (expand (remainder (* num radix) den) den radix)))

;;; Exercise 3.59

(define (integrate-series s)
  (stream-map / s integers))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1 (integrate-series sine-series)))
(define sine-series (cons-stream 0 (scale-stream (integrate-series cosine-series) -1)))

;;; Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

;;; Exercise 3.61

(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr s) (invert-unit-series s))
                             -1)))

;;; Exercise 3.62

(define (div-series num denom)
  (let ((denom-const (stream-car denom)))
    (if (zero? denom-const)
        (error "divided by zero: DIV-SERIES" denom-const)
        (mul-series num (scale-stream (invert-unit-series
                                       (scale-stream denom (/ 1 denom-const)))
                                      denom-const)))))

(define tan-series (div-series sine-series cosine-series))

;;; Exercise 3.64

(define (stream-limit s tolerance)
  (let ((scdr (stream-cdr s)))
    (if (< (abs (- (stream-car s) (stream-car scdr))) tolerance)
        (stream-car scdr)
        (stream-limit scdr tolerance))))

;; (define (sqrt x tolerance)
;;   (stream-limit (sqrt-stream x) tolerance))

;;; Exercise 3.65

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream (partial-sums (ln-summands 1)))

;;; Exercise 3.67

(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (interleave
                 (stream-map (lambda (x) (list x (stream-car t)))
                             (stream-cdr s))
                 (pairs (stream-cdr s) (stream-cdr t))))))

;;; Exercise 3.69

(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
               (interleave
                (stream-map (lambda (x) (append (list (stream-car s)) x))
                            (stream-cdr (pairs t u)))
                (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter (lambda (x)
                   (= (+ (square (car x))
                         (square (cadr x)))
                      (square (caddr x))))
                 (triples integers integers integers)))

;;; Exercise 3.70

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (if (<= (weight s1car) (weight s2car))
                    (cons-stream s1car
                                 (merge-weighted (stream-cdr s1)
                                                 s2
                                                 weight))
                    (cons-stream s2car
                                 (merge-weighted s1
                                                 (stream-cdr s2)
                                                 weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
                weight)))

;;; Exercise 3.71

(define (sum-of-cubes x)
  (+ (cube (car x)) (cube (cadr x))))

(define (ramanujan-numbers)
  (define (ramanujans sum-cubes)
    (let ((current (stream-car sum-cubes))
          (next (stream-car (stream-cdr sum-cubes))))
      (let ((ramanujan-num (sum-of-cubes current)))
        (cond ((= ramanujan-num (sum-of-cubes next))
               (cons-stream (list ramanujan-num current next)
                            (ramanujans (stream-cdr (stream-cdr sum-cubes)))))
              (else (ramanujans (stream-cdr sum-cubes)))))))
  (ramanujans (weighted-pairs integers integers sum-of-cubes)))

;;; Exercise 3.72

(define (sum-of-squares x)
  (+ (square (car x)) (square (cadr x))))

(define (square-stream s)
  (let ((scar (stream-car s))
        (scadr (stream-car (stream-cdr s)))
        (scaddr (stream-car (stream-cdr (stream-cdr s)))))
    (if (= (sum-of-squares scar) (sum-of-squares scadr) (sum-of-squares scaddr))
        (cons-stream (list (sum-of-squares scar) scar scadr scaddr)
                     (square-stream (stream-cdr (stream-cdr (stream-cdr s)))))
        (square-stream (stream-cdr s)))))

(define square-numbers
  (square-stream (weighted-pairs integers integers sum-of-squares)))

;;; Exercise 3.73

(define (RC r c dt)
  (lambda (i v0)
    (add-streams (scale-stream i r)
                 (integral (scale-stream i (/ 1.0 C)) v0 dt))))

;;; Exercise 3.74

;; (define (make-zero-crossings input-stream last-value)
;;   (cons-stream (sign-change-detector (stream-car input-stream) last-value)
;;                (make-zero-crossings (stream-cdr input-stream)
;;                                     (stream-car input-stream))))
;; (define zero-crossings (make-zero-crossings sense-data 0))

;; (define zero-crossings
;;   (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;;; Exercise 3.75

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream (sign-change-detetor avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

;;; Exercise 3.76

(define (smooth s)
  (stream-map average (cons-stream 0 s) s))

(define (make-zero-crossings input-stream smooth)
  (let ((smoothened (smooth input-stream)))
    (stream-map sign-change-detetor smoothened (cons-stream 0 smoothened))))

;;; Exercise 3.77

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;;; Exercise 3.78

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

;;; Exercise 3.79

(define (general-solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;;; Exercise 3.80

(define (RLC R L C dt)
  (lambda (v0 i0)
    (define vc (integral (delay dv) v0 dt))
    (define il (integral (delay di) i0 dt))
    (define dv (scale-stream il (/ -1.0 C)))
    (define di (add-streams (scale-stream il (- (/ R L)))
                            (scale-stream vc (/ 1.0 L))))
    (stream-map (lambda (a b) (cons a b)) vc il)))

;;; Exercise 3.81

(define (rand-stream s)
  (cons-stream (if (eq? (stream-car s) 'generate)
                   ((rand 'generate))
                   (begin (set! random-init (stream-car s))
                          ((rand 'reset))))
               (rand-stream (stream-cdr s))))

;;; Exercise 3.82

(define (random-number-pairs low1 high1 low2 high2)
  (cons-stream (cons (random-in-range low1 high1)
                     (random-in-range low2 high2))
               (random-number-pairs low1 high1 low2 high2)))

(define (estimate-integral p x1 x2 y1 y2)
  (let ((area (* (- x2 x1) (- y2 y1))))
    (scale-stream
     (monte-carlo (stream-map p (random-number-pairs x1 x2 y1 y2)) 0 0)
     area)))

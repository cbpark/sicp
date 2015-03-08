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

;;; Exercise 2.27

(define (deep-reverse x)
  (cond ((null? x) '())
        ((pair? (car x)) (append (deep-reverse (cdr x))
                                 (list (deep-reverse (car x)))))
        (else (append (deep-reverse (cdr x))
                      (list (car x))))))

;;; Exercise 2.28

(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

;;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
        (total-weight s)
        s)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-balanced? branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
        (balanced? s)
        true)))

(define (branch-torque branch)
  (* (branch-weight branch)
     (branch-length branch)))

(define (balanced? mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile)))
    (and (= (branch-torque l) (branch-torque r))
         (branch-balanced? l)
         (branch-balanced? r))))

;;; Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

;;; Exercise 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s)
                                  x)) rest)))))

;;; Exercise 2.33

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff)) 0 coefficient-sequence))

;;; Exercise 2.35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (n) (if (pair? n)
                                       (count-leaves n)
                                       1)) t)))

;;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;; Exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;;; Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

;;; Exercise 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;;; Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (unique-pairs n)))

;;; Exercise 2.41

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (ordered-triple-sum n s)
  (filter (lambda (triple) (= s (accumulate + 0 triple))) (ordered-triples n)))

;;; Exercise 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (make-position row col)
  (cons row col))

(define (position-row position)
  (car position))

(define (position-col position)
  (cdr position))

(define empty-board '())

(define (adjoin-position row col position)
  (append position (list (make-position row col))))

(define (safe? col position)
  (let ((kth-queen (list-ref position (- col 1)))
        (rest-of-queens (filter (lambda (q) (not (= col (position-col q))))
                                position)))
    (define (attacks q1 q2)
      (or (= (position-row q1) (position-row q2))
          (= (abs (- (position-row q1) (position-row q2)))
             (abs (- (position-col q1) (position-col q2))))))
    (define (iter-safe? q board)
      (or (null? board) (and (not (attacks q (car board)))
                             (iter-safe? q (cdr board)))))
    (iter-safe? kth-queen rest-of-queens)))

;;; Exercise 2.54

(define (equal? a b)
  (cond ((and (null? a) (null? b)) true)
        ((or (null? a) (null? b)) false)
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
                                        (equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) false)
        (else (eq? a b))))

;;; Exercise 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp) (make-exponent (base exp) (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base u)
  (cadr u))

(define (exponent u)
  (caddr u))

(define (make-exponent u n)
  (list '** u n))

;;; Exercise 2.57

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;;; Exercise 2.58

(define (sum?-infix x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend-infix s)
  (car s))

(define (augend-infix s)
  (caddr s))

(define (product?-infix x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier-infix p)
  (car p))

(define (multiplicand-infix p)
  (caddr p))

(define (make-sum-infix a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product-infix m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (deriv-infix exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        ((sum?-infix exp) (make-sum-infix (deriv-infix (addend-infix exp) var)
                                          (deriv-infix (augend-infix exp) var)))
        ((product?-infix exp)
         (make-sum-infix
          (make-product-infix (multiplier-infix exp)
                              (deriv-infix (multiplicand-infix exp) var))
          (make-product-infix (deriv-infix (multiplier-infix exp) var)
                              (multiplicand-infix exp))))
        (else (error "unknown expression type: DERIV-INFIX" exp))))

;;; Exercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;;; Exercise 2.60

(define (adjoin-set-dup x set)
  (cons x set))

(define (union-set-dup set1 set2)
  (append set1 set2))

;;; Exercise 2.61

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;;; Exercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      (else (cons x2 (union-set set1 (cdr set2)))))))))

;;; Exercise 2.63

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;;; Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;;; Exercise 2.65

(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list2)
          (else (let ((x1 (car list1))
                      (x2 (car list2)))
                  (cond ((= x1 x2) (cons x1
                                         (union-list (cdr list1) (cdr list2))))
                        ((< x1 x2) (cons x1
                                         (union-list (cdr list1) list2)))
                        (else (cons x2
                                    (union-list list1 (cdr list2)))))))))
  (list->tree (union-list (tree->list-1 set1)
                          (tree->list-1 set2))))

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
        '()
        (let ((x1 (car list1))
              (x2 (car list2)))
          (cond ((= x1 x2) (cons x1 (intersection-list (cdr list1) (cdr list2))))
                ((< x1 x2) (intersection-list (cdr list1) list2))
                (else (intersection-list list1 (cdr list2)))))))
  (list->tree (intersection-list (tree->list-1 set1)
                                 (tree->list-1 set2))))

;;; Exercise 2.66

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((= given-key (key (entry tree-of-records))) (entry tree-of-records))
        ((< given-key (ket (entry tree-of-records)))
         (lookup given-key (left-branch tree-of-records)))
        (else (lookup given-key (right-branch tree-of-records)))))

;;; Exercise 2.67

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (element-of-symbols? x symbols)
    (cond ((null? symbols) false)
          ((equal? x (car symbols)) true)
          (else (element-of-symbols? x (cdr symbols)))))
  (cond ((leaf? tree) '())
        ((element-of-symbols? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-symbols? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol: ENCODE-SYMBOL" symbol))))

;;; Exercise 2.69

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (cond ((null? set) '())
        ((null? (cdr set)) (car set))
        (else (successive-merge (adjoin-set (make-code-tree (car set) (cadr set))
                                            (cddr set))))))

;;; Exercise 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (car exp) var)
              (deriv (cadr exp) var)))
  (define (deriv-product exp var)
    (make-sum (make-product (car exp)
                            (deriv (cadr exp) var))
              (make-product (deriv (car exp) var)
                            (cadr exp))))
  (define (deriv-exponent exp var)
    (make-product (make-product (cadr exp)
                                (make-exponent (car exp)
                                               (make-sum (cadr exp) -1)))
                  (deriv (car exp) var)))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponent)
  'done)

(install-deriv-package)

;;; Exercise 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;; Exercise 2.77 - 2.80
;;; See chap2.scm

;;; Exercise 2.81

(define (apply-generic op . args)
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                         (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;; Exercise 2.82

(define (apply-generic op . args)
  (define (can-coerced-into types target-type)
    (and (map (lambda (type)
                (or (equal? type target-type)
                    (get-coercion type target-type)))
              types)))
  (define (find-coerced-type types)
    (or (map (lambda (target-type)
               (if (can-coerced-into types target-type)
                   target-type
                   false)))
        types))
  (define (coerce-to target-type)
    (map (lambda (arg)
           (let ((arg-type (type-tag arg)))
             (if (equal? arg-type target-type)
                 arg
                 ((get-coercion arg-type target-type) arg))))
         args))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((target-type (find-coerced-type type-tags)))
            (if target-type
                (apply-generic op (coerce-to target-type))
                (error "No method for these types"
                       (list op type-tags))))))))

;;; Exercise 2.83
;;; See chap2.scm

;;; Exercise 2.84

(define (apply-generic op . args)
  (define (raise-to source target)
    (let ((source-type (type-tag source))
          (target-type (type-tag target)))
      (let ((raise-proc (get 'raise (list source-type))))
        (cond ((equal? source-type target-type) source)
              (raise-proc (raise-to (raise-proc (contents source)) target))
              (else false)))))
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args))
                    (raise1 (raise-to a1 a2))
                    (raise2 (raise-to a2 a1)))
                (cond (raise1 (apply-generic op raise1 a2))
                      (raise2 (apply-generic op a1 raise2))
                      (else (error "No method for these types"
                                   (list op type-tags) ))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;; Exercise 2.85
;;; See also chap2.scm

(define (apply-generic op . args)
  (define (raise-to source target)
    (let ((source-type (type-tag source))
          (target-type (type-tag target)))
      (let ((raise-proc (get 'raise (list source-type))))
        (cond ((equal? source-type target-type) source)
              (raise-proc (raise-to (raise-proc (contents source)) target))
              (else false)))))
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args))
                    (raise1 (raise-to a1 a2))
                    (raise2 (raise-to a2 a1)))
                (cond (raise1 (apply-generic op raise1 a2))
                      (raise2 (apply-generic op a1 raise2))
                      (else (error "No method for these types"
                                   (list op type-tags) ))))
              (error "No method for these types"
                     (list op type-tags)))))))

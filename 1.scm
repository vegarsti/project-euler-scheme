;; Helpers
(define (filter predicate items)
  (if (null? items)
      '()
      (if (predicate (car items))
          (cons (car items)
            (filter predicate
                    (cdr items)))
          (filter predicate
                  (cdr items)))))

(define (mapp func items)
  (if (null? items)
      '()
      (cons (func (car items))
            (mapp func (cdr items)))))

(define (reduce func items base)
  (if (null? items)
      base
      (func (car items)
            (reduce func (cdr items) base))))

(define (predicate-and p1 p2)
  (lambda (x) (and (p1 x) (p2 x))))

(define (predicate-or p1 p2)
  (lambda (x) (or (p1 x) (p2 x))))


;; Examples
(filter even? '(1 2 3))
(mapp even? '(1 2 3))


;; 1: Find the sum of all the multiples of 3 or 5 below 1000.
(define (make-multiple-of n) (lambda (m) (zero? (remainder m n))))
(define multiple-of-3? (make-multiple-of 3))
(define multiple-of-5? (make-multiple-of 5))

(define (sum items) (reduce + items 0))

(define multiple-of-3-or-5? (predicate-or multiple-of-3? multiple-of-5?))
(define (ints-up-to n)
  (if (zero? (- n 1))
      '()
      (cons (- n 1) (ints-up-to (- n 1)))))
(sum (filter multiple-of-3-or-5? (ints-up-to 1000)))


;; 2: Find the sum of the even-valued Fibonacci terms below four million
(define (fibs-up-to limit)
  (define (fib-iter prev pprev items)
    (let ((next (+ prev pprev)))
      (if (> next limit)
          items
          (cons next (fib-iter next prev items)))))
  (fib-iter 0 1 '()))
(sum (filter even? (fibs-up-to 4000000)))


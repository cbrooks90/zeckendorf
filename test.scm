(load "addition.scm")
(load "fib-rep.scm")

(define (test-rep x)
  (if (equal? (reduce x) (normalize x))
      #t
      (begin
        (printf "~a -> ~a, should be ~a\n" x (reduce-min x) (normalize x))
        #f)))

(define (test-up-to n)
  (let loop ([xs (all-sums n)] [done '()] [fails 0] [passes 0])
    (cond [(null? xs) (values fails passes)]
          [(member (car xs) done)
           (loop (cdr xs) done fails passes)]
          [(test-rep (car xs))
           (loop (cdr xs) (cons (car xs) done) fails (+ passes 1))]
          [else
           (loop (cdr xs) (cons (car xs) done) (+ fails 1) passes)])))

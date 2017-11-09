(load "addition.scm")
(load "fib-rep.scm")

(define (test-equal li)
  (if (equal? (reduce li) (normalize li))
      #t
      (begin
        (printf "~a -> ~a, should be ~a\n" li (reduce li) (normalize li))
        #f)))

(define (test-valid l)
  (let loop ([li (reduce l)] [prev 0])
    (cond [(null? li) #t]
          [(< (+ prev (car li)) 2) (loop (cdr li) (car li))]
          [else #f])))

(define (test-up-to len)
  (let loop ([xs (all-min-fibonaccis len)] [done '()] [fails 0] [passes 0])
    (cond [(null? xs) (values fails passes)]
          [(member (car xs) done)
           (loop (cdr xs) done fails passes)]
          [(test-valid (car xs))
           (loop (cdr xs) (cons (car xs) done) fails (+ passes 1))]
          [else
           (loop (cdr xs) (cons (car xs) done) (+ fails 1) passes)])))

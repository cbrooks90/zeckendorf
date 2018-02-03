(load "subtraction.scm")
(load "fib-rep.scm")

(define (list-subtract a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else (cons (- (car a) (car b)) (list-subtract (cdr a) (cdr b)))]))

(define (test-valid li)
  (let loop ([li li] [prev 0])
    (cond [(null? li) #t]
          [(< (+ prev (car li)) 2) (loop (cdr li) (car li))]
          [else #f])))

(define (subtract a b)
  (let* ([li (list-subtract (rep a) (rep b))]
         [result (reduce li)])
    (cond [(not (= (un-fib-rep result) (- a b)))
           (printf "Subtraction is wrong ~a - ~a\n" a b)]
          [(not (test-valid result))
           (printf "Result list of ~a - ~a is invalid: ~a -> ~a\n" a b li result)])))

(define (all-diffs n)
  (let loop ([sum 0] [a 0] [b 0])
    (cond [(> sum n) void]
          [(> a b) (loop (+ sum 1) 0 (+ sum 1))]
          [else (begin (subtract b a)
                  (loop sum (+ a 1) (- b 1)))])))

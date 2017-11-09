(load "subtraction.scm")
(load "fib-rep.scm")

(define (test-valid li)
  (let loop ([li li] [prev 0])
    (cond [(null? li) #t]
          [(< (+ prev (car li)) 2) (loop (cdr li) (car li))]
          [else #f])))

(define (subtract a b)
  (let* ([li (list-addition (map (lambda (x) (* x -1)) (rep a)) (rep b))]
         [result (reduce li)])
    (cond [(not (= (un-fib-rep result) (- b a)))
           (printf "Subtraction is wrong ~a - ~a\n" b a)]
          [(not (test-valid result))
           (printf "Result list of ~a - ~a is invalid: ~a -> ~a\n" b a li result)])))

(define (all-diffs n)
  (let loop ([sum 0] [a 0] [b 0])
    (cond [(> sum n) void]
          [(> a b) (loop (+ sum 1) 0 (+ sum 1))]
          [else (begin (subtract a b)
                  (loop sum (+ a 1) (- b 1)))])))

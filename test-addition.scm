(load "addition.scm")
(load "fib-rep.scm")

(define (list-add a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else (cons (+ (car a) (car b)) (list-add (cdr a) (cdr b)))]))

(define (all-sums n)
  (let loop ([sum 0] [a 0] [b 0])
    (cond [(> sum n) '()]
          [(> a b) (loop (+ sum 1) 0 (+ sum 1))]
          [else (cons (list-add (rep a) (rep b))
                      (loop sum (+ a 1) (- b 1)))])))

; In case we want to skip adding a whole bunch of lists together (which
; introduces duplicates)
(define (all-min-fibonaccis max-len)
  (cons
    '()
    (let loop ([len 2] [accum '((1) (2))])
      (append
        accum
        (if (> len max-len) '()
            (loop (+ len 1)
                  (append
                    (map (lambda (x) (cons 0 x)) accum)
                    (map (lambda (x) (cons 1 x)) (filter (lambda (x) (< (car x) 2)) accum))
                    (map (lambda (x) (cons 2 x)) (filter (lambda (x) (= (car x) 0)) accum)))))))))

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
          [(test-equal (car xs))
           (loop (cdr xs) (cons (car xs) done) fails (+ passes 1))]
          [else
           (loop (cdr xs) (cons (car xs) done) (+ fails 1) passes)])))

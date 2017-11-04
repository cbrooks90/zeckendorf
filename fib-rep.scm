; Every positive integer can be written uniquely as a sum of distinct positive
; Fibonacci numbers such that no two successive Fibonacci numbers are excluded.
; In other words, a positive integer can be written as a list of 1's and 0's
; where the ith entry means that Fibonacci number is included; this list is
; unique if we require that the list does not contain two zeros in succession.
; We refer to this representation as the lazy Fibonacci representation.
;
; Examples:
;   * (lazy-rep 6) -> '(1 1 1) since 6 = 1 + 2 + 3. Does not return '(1 0 0 1)
;     since even though 6 = 1 + 5, this excludes two successive fibonaccis.
;   * (lazy-rep 18) -> '(0 1 1 1 1) since 18 = 2 + 3 + 5 + 8.
;     While 18 can be represented as '(0 0 0 1 0 1) or '(0 1 1 0 0 1), these
;     representations include successive zeros

; Finds two values: The highest Fibonacci number present in the lazy
; Fibonacci representation of 'n', and the next Fibonacci number.
(define (fib-bounds n)
  (let loop ([fib 1] [prev 1] [pprev 0])
    (if (< (+ n 1) fib)
        (values pprev prev)
        (loop (+ fib prev) fib prev))))

(define (fib-floor n)
  (let loop ([fib 1] [prev 0])
    (if (> fib n) (values prev (- fib prev))
        (loop (+ fib prev) fib))))

; Find the lazy Fibonacci representation of 'n'
(define (lazy-rep n)
  (let-values ([(fib prev) (fib-bounds n)])
    (let loop ([n n] [fib fib] [prev prev] [acc '()])
      (cond [(= fib 1) acc]
            [(< (+ n 1) fib) (loop n prev (- fib prev) (cons 0 acc))]
            [else (loop (- n prev) prev (- fib prev) (cons 1 acc))]))))

; Find the greedy (Zeckendorf) Fibonacci representation of 'n'
(define (greedy-rep n)
  (let-values ([(fib prev) (fib-floor n)])
    (let loop ([n n] [fib fib] [prev prev] [acc '()])
      (cond [(= prev 0) acc]
            [(> fib n) (loop n prev (- fib prev) (cons 0 acc))]
            [else (loop (- n fib) prev (- fib prev) (cons 1 acc))]))))

; Given a list of integers a_i, 0 <= i <= n, find \sum_{i=0}^n a_i*F_{i+2}
(define (un-fib-rep rep)
  (let loop ([rep rep] [fib 1] [prev 1] [acc 0])
    (if (null? rep) acc
        (loop (cdr rep) (+ fib prev) fib (+ acc (* (car rep) fib))))))

; All fibonacci representations where no digit is bigger than 'max'
(define (fib-rep* n max)
  (let loop ([n n] [fib 1] [prev 1] [count 0])
    (cond [(> count max) '()]
          [(= n 0) `((,count))]
          [(or (< n 0) (> fib n)) '()]
          [else (append (map (lambda (x) (cons count x))
                             (loop n (+ fib prev) fib 0))
                        (loop (- n fib) fib prev (+ count 1)))])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions for testing        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-fibonaccis max-len)
  (let loop ([len 2] [accum '((1) (2))])
    (if (> len max-len) accum
        (append
          accum
          (loop (+ len 1)
                (append
                  (map (lambda (x) (cons 0 x)) (filter (lambda (x) (not (= (car x) 0))) accum))
                  (map (lambda (x) (cons 1 x)) accum)
                  (map (lambda (x) (cons 2 x)) accum)))))))

(define rep greedy-rep)

(define (normalize li)
  (rep (un-fib-rep li)))

(define (list-addition a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else (cons (+ (car a) (car b)) (list-addition (cdr a) (cdr b)))]))

(define (all-sums n)
  (let loop ([sum 2] [a 1] [b 1])
    (cond [(> sum n) '()]
          [(> a b) (loop (+ sum 1) 1 sum)]
          [else (cons (list-addition (rep a) (rep b))
                      (loop sum (+ a 1) (- b 1)))])))

 (define (truncate n)
   (let loop ([li (rep n)])
     (if (null? li) '()
         (cons (un-fib-rep li) (loop (cdr li))))))

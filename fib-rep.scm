; Find the successive Fibonacci numbers f_i, f_i+1 for which f_i < n+1 < f_i+1.
; Useful for finding the maximal fibonacci representation, see 'fib-base'.
(define (fibonacci-bounds n)
  (let loop ([fib 1] [prev 1] [pprev 0])
    (if (< (+ n 1) fib)
        (values prev pprev)
        (loop (+ fib prev) fib prev))))

; Every positive integer can be written uniquely as a sum of distinct positive
; Fibonacci numbers such that no two successive Fibonacci numbers are excluded.
; This function finds such a representation for any integer, returned as a list
; of 1's and 0's where the ith entry means that Fibonacci number is included
; in the sum (starting with 1)
;
; Examples:
;   * (fib-base 6) -> '(1 1 1) since 6 = 1 + 2 + 3. Does not return '(1 0 0 1)
;     since even though 6 = 1 + 5, this excludes two successive fibonaccis.
;   * (fib-base 18) -> '(0 1 1 1 1) since 18 = 2 + 3 + 5 + 8.
;     While 18 can be represented as '(0 0 0 1 0 1) or '(0 1 1 0 0 1), these
;     representations include successive zeros
(define (fib-base n)
  (let-values ([(fib prev) (fibonacci-bounds n)])
    (let loop ([n n] [fib fib] [prev prev] [acc '()])
      (cond [(= fib 1) acc]
            [(< (+ n 1) fib) (loop n prev (- fib prev) (cons 0 acc))]
            [else (loop (- n prev) prev (- fib prev) (cons 1 acc))]))))

; Given a list of integers a_i, 0 <= i <= n, find \sum_{i=0}^n a_i*F_{i+2}
(define (un-fib-base rep)
  (let loop ([rep rep] [fib 1] [prev 1] [acc 0])
    (if (null? rep) acc
        (loop (cdr rep) (+ fib prev) fib (+ acc (* (car rep) fib))))))

; All fibonacci representations where no digit is bigger than 'max'
(define (fib-base* n max)
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

(define (normalize rep)
  (fib-base (un-fib-base rep)))

(define (list-addition a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else (cons (+ (car a) (car b)) (list-addition (cdr a) (cdr b)))]))

(define (addition-test n)
  (let loop ([sum 2] [a 1] [b 1])
    (cond [(> sum n) '()]
          [(> a b) (loop (+ sum 1) 1 sum)]
          [else (cons (cons
                        (list-addition (fib-base a)
                                       (fib-base b))
                        (fib-base sum))
                      (loop sum (+ a 1) (- b 1)))])))

(define (twos-test n)
  (let loop ([i 0] [li '(2)])
    (let ([val (un-fib-base li)])
      (if (> i n) '()
          (cons (cons li (fib-base val))
                (loop (+ i 1) (cons 2 li)))))))

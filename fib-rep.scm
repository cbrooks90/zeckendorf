; These functions concern the representation of integers as sums of Fibonacci
; numbers. If the ith entry is a_i, we add a_i * F_{i+2}. The i+2 is so that our
; 'basis' is distinct positive Fibonacci numbers, i.e. 1, 2, 3, 5, 8, ...
; Some examples:
;   '(1 0 0 2 0 3) -> 1*1 + 2*5 + 3*13 = 50
;   '(0 0 4 5 0 1 3) -> 4*3 + 5*5 + 1*13 + 3*21 = 113
;
; The function 'un-fib-rep' converts the list representation to an integer, and
; 'fib-reps' computes all list representations from a given integer. In this
; file, there is no support for negative integer arguments, even if some
; functions magically work correctly with them.

; Given a list of integers a_i, 0 <= i <= n, find \sum_{i=0}^n a_i*F_{i+2}
(define (un-fib-rep rep)
  (let loop ([rep rep] [fib 1] [prev 1] [acc 0])
    (if (null? rep) acc
        (loop (cdr rep) (+ fib prev) fib (+ acc (* (car rep) fib))))))

; All fibonacci representations where no digit is bigger than 'max'
(define (bdd-fib-reps n max)
  (let loop ([n n] [fib 1] [prev 1] [count 0])
    (cond [(> count max) '()]
          [(and (= n 0) (= count 0)) '(())]
          [(= n 0) `((,count))]
          [(or (< n 0) (> fib n)) '()]
          [else (append (map (lambda (x) (cons count x))
                             (loop n (+ fib prev) fib 0))
                        (loop (- n fib) fib prev (+ count 1)))])))

; Same as above but 'max' argument is optional
(define fib-reps
  (case-lambda
    [(n) (bdd-fib-reps n +inf.0)]
    [(n max) (bdd-fib-reps n max)]))

; A representation of an integer as a sum of Fibonacci numbers is not unique.
; If we require that the list contains only 0 and 1 and a_i * a_i+1 = 0 for all
; i, then the representation is unique. This 'greedy' Fibonacci representation
; is sometimes referred to as the Zeckendorf representation.
;
; Examples:
;   * (greedy-rep 7) -> '(0 1 0 1) since 7 = 2 + 5. Does not return '(0 1 1 0)
;     since even though 7 = 3 + 4, this includes two successive Fibonaccis.
;   * (greedy-rep 18) -> '(0 0 0 1 0 1) since 18 = 5 + 13.
;     While 18 can be represented as '(0 1 1 1 1) or '(0 1 1 0 0 1), these
;     representations include successive zeros

; Find the largest Fibonacci number less than or equal to 'n'
(define (fib-floor n)
  (let loop ([fib 1] [prev 0])
    (if (> fib n) (values prev (- fib prev))
        (loop (+ fib prev) fib))))


; Find the greedy (Zeckendorf) Fibonacci representation of 'n'
(define (greedy-rep n)
  (let-values ([(fib prev) (fib-floor n)])
    (let loop ([n n] [fib fib] [prev prev] [acc '()])
      (cond [(or (= fib 0) (= prev 0)) acc]
            [(> fib n) (loop n prev (- fib prev) (cons 0 acc))]
            [else (loop (- n fib) prev (- fib prev) (cons 1 acc))]))))

; There is a similar representation which instead requires that no two
; successive Fibonacci numbers are excluded; i.e. the list has no two 0's in
; succession. This will be referred to as the 'lazy' Fibonacci representation.

; Finds two values: The highest Fibonacci number present in the lazy
; Fibonacci representation of 'n', and the next Fibonacci number.
(define (fib-bounds n)
  (let loop ([fib 1] [prev 1] [pprev 0])
    (if (< (+ n 1) fib)
        (values pprev prev)
        (loop (+ fib prev) fib prev))))

; Find the lazy Fibonacci representation of 'n'
(define (lazy-rep n)
  (let-values ([(prev fib) (fib-bounds n)])
    (let loop ([n n] [fib fib] [prev prev] [acc '()])
      (cond [(= fib 1) acc]
            [(< (+ n 1) fib) (loop n prev (- fib prev) (cons 0 acc))]
            [else (loop (- n prev) prev (- fib prev) (cons 1 acc))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions for testing        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define rep greedy-rep)

(define (normalize li)
  (rep (un-fib-rep li)))

(define (list-addition a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else (cons (+ (car a) (car b)) (list-addition (cdr a) (cdr b)))]))

(define (all-sums n)
  (let loop ([sum 0] [a 0] [b 0])
    (cond [(> sum n) '()]
          [(> a b) (loop (+ sum 1) 0 (+ sum 1))]
          [else (cons (list-addition (rep a) (rep b))
                      (loop sum (+ a 1) (- b 1)))])))

(define (truncate n)
  (let loop ([li (rep n)])
    (if (null? li) '()
        (cons (un-fib-rep li) (loop (cdr li))))))

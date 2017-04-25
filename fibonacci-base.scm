(define (base-fibonacci n max)
  (let loop ([n n] [fib 1] [prev 1] [count 0])
    (cond [(> count max) '()]
          [(= n 0) `((,count))]
          [(or (< n 0) (> fib n)) '()]
          [else (append (map (lambda (x) (cons count x))
                             (loop n (+ fib prev) fib 0))
                        (loop (- n fib) fib prev (+ count 1)))])))

(define (fibonacci-floor n)
  (let loop ([fib 1] [prev 1] [pprev 0])
    (if (< (+ n 1) fib)
        (values prev pprev)
        (loop (+ fib prev) fib prev))))

(define (base-fibonacci-max n)
  (let-values ([(fib prev) (fibonacci-floor n)])
    (let loop ([n n] [fib fib] [prev prev] [acc '()])
      (cond [(= fib 1) acc]
            [(< (+ n 1) fib) (loop n prev (- fib prev) (cons 0 acc))]
            [else (loop (- n prev) prev (- fib prev) (cons 1 acc))]))))

(define (fib-base->integer rep)
  (let loop ([rep rep] [fib 1] [prev 1] [acc 0])
    (if (null? rep) acc
        (loop (cdr rep) (+ fib prev) fib (+ acc (* (car rep) fib))))))

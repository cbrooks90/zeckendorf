(load "fib-rep.scm")

(define (reducible? a b c)
  (let ([a (or a 1)])
    (or
      (and (> b 1) (> c 0))
      (and (> a 0) (> b 0) (> c 1)))))

(define (window p a b c d rest)
  (cond ; These are basic rules like (2 1) -> (1 0 1), depending on prev. value
        [(reducible? p a b) (window p (- a 1) (- b 1) (+ c 1) d rest)]
        [(reducible? a b c) (window p a (- b 1) (- c 1) (+ d 1) rest)]
        ; This catches rules like (0 0 2) -> (1 0 0 1)
        [(and (> c 1) (> c d) (or (= a 0) (not p)))
         (window p (+ a 1) b (- c 2) (+ d 1) rest)]
        ; This is a special rule for the beginning, where (2) -> (0 1)
        [(and (not p) (> a 1)) (window p (- a 2) (+ b 1) c d rest)]
        ; Control flow
        [(= 0 (+ a b c d)) '()]
        [(null? rest) (cons a (window a b c d 0 '()))]
        [else (cons a (window a b c d (car rest) (cdr rest)))]))

(define (reduce li)
  (cond [(null? li) '()]
        [(null? (cdr li)) (window #f (car li) 0 0 0 '())]
        [(null? (cddr li)) (window #f (car li) (cadr li) 0 0 '())]
        [(null? (cdddr li)) (window #f (car li) (cadr li) (caddr li) 0 '())]
        [else (window #f (car li) (cadr li) (caddr li) (cadddr li) (cddddr li))]))

;(reduce '(1 1 1 0 1 1 2 2 2 0 1 1 1 1 1 1))
;(reduce '(2 2 2 2 2 2 2 2 2 2 2 2))
;(reduce '(2 0 2 0 2 0 2))
;(reduce '(2 1 2 2 0 2 1 2)) <-There is a problem with a rule here
;(reduce '(2 1 2 1 2 1 2 1 2))
;(reduce '(2 2 1 2 1 1 2))
;(reduce '(2 1 2 1 2 2 2 1 2 2 2))

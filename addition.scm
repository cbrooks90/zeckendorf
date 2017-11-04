(define (end a b c d rest)
  (cond [(= b 2) (append (list 1 0 0 1) rest)]
        [(= c 2) (append (list 1 0 1 0) rest)]
        [(= d 2) (append (list 0 1 0 a) rest)]
        [else (append (list d c b a) rest)]))

(define (wodniw a b c d rest acc)
  (cond [(and (> b 0) (> c 0)) (wodniw (+ a 1) (- b 1) (- c 1) d rest acc)]
        [(and (> c 0) (> d 0)) (wodniw a (+ b 1) (- c 1) (- d 1) rest acc)]
        [(and (> b 1) (= a 0)) (wodniw (+ a 1) (- b 2) c (+ d 1) rest acc)]

        [(null? rest) (end a b c d acc)]
        [else (wodniw b c d (car rest) (cdr rest) (cons a acc))]))

(define (ecuder li)
  (cond [(or (null? li)
             (null? (cdr li))
             (null? (cddr li))
             (null? (cdddr li))) (reverse li)]
        [else (wodniw (car li) (cadr li) (caddr li) (cadddr li) (cddddr li) '())]))

(define (window a b c d rest acc)
  (cond [(and (> a 0) (> b 0)) (window (- a 1) (- b 1) (+ c 1) d rest acc)]
        [(and (> b 0) (> c 0)) (window a (- b 1) (- c 1) (+ d 1) rest acc)]
        [(and (> c 1) (= d 0)) (window (+ a 1) b (- c 2) (+ d 1) rest acc)]

        [(and (null? rest) (= 0 (+ a b c d))) (ecuder acc)]
        [(null? rest) (window b c d 0 '() (cons a acc))]
        [else (window b c d (car rest) (cdr rest) (cons a acc))]))

(define (start a b c d rest)
  (cond [(and (= a 2) (= b 0)) (window 0 1 c d rest '())]
        [(and (= a 0) (= b 2) (= c 0)) (window 1 0 1 d rest '())]
        [else (window a b c d rest '())]))

(define (reduce li)
  (cond [(null? li) '()]
        [(null? (cdr li)) (start (car li) 0 0 0 '())]
        [(null? (cddr li)) (start (car li) (cadr li) 0 0 '())]
        [(null? (cdddr li)) (start (car li) (cadr li) (caddr li) 0 '())]
        [else (start (car li) (cadr li) (caddr li) (cadddr li) (cddddr li))]))

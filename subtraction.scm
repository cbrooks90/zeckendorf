(define (nonzero-cons first rest)
  (if (zero? first) rest (cons first rest)))

(define (end a b c rest)
  (cond [(and (= a 1) (= b 1)) (list c 0 0 1)]
        [(= b 2) (append (list 1 0 1) rest)]
        [(= c 2) (append (list 0 1 0) rest)]
        [(and (= b 1) (= c 1)) (append (list 0 0 1) rest)]
        [(and (null? rest) (= a 0)) (list c b)]
        [else (append (list c b a) rest)]))

(define (wodniw a b c d rest acc)
  (cond [(and (= b 1) (= c 1)) (wodniw 1 0 0 d rest acc)]
        [(= b 2) (wodniw 1 0 0 (+ d 1) rest acc)]
        [(null? rest) (end b c d (nonzero-cons a acc))]; BAD
        [else (wodniw b c d (car rest) (cdr rest) (nonzero-cons a acc))])); BAD

(define (trim-zeros a b c rest)
  (cond [(zero? a) (trim-zeros b c (car rest) (cdr rest))]
        [(and (eqv? a 1) (eqv? b 1)) (wodniw 0 0 c (car rest) (cdr rest) '(1))]
        [else (wodniw a b c (car rest) (cdr rest) '())]))

(define (window a b c rest acc)
  (cond [(and (= a 0) (= b -1)) (window 1 0 (- c 1) rest acc)]
        [(and (= a -1) (= b 0)) (window 0 1 (- c 1) rest acc)]
        [(and (= b -1) (= c 1)) (window (+ a 1) 0 0 rest acc)]
        [(and (= a -1) (= b -1)) (window 0 0 (- c 1) rest acc)]
        [(null? rest) (trim-zeros c b a acc)]
        ;[(and (null? rest) (= b 0) (= c 0)) (wodniw 0 a (car acc) (cdr acc) '())]
        ;[(null? rest) (window b c 0 '() (cons a acc))]
        [else (window b c (car rest) (cdr rest) (cons a acc))]))

(define (reduce li)
  (cond [(or (null? li) (null? (cdr li))) li]
        [(= (car li) -1) (reduce `(1 ,(- (cadr li) 1) . ,(cddr li)))]
        [(null? (cddr li)) (window (car li) (cadr li) 0 '() '(#f #f #f))]
        [else (window (car li) (cadr li) (caddr li) (cdddr li) '(#f #f #f))]))

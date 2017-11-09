(define (end a b c rest)
  (cond [(and (= a 1) (= b 1)) (list c 0 0 1)]
        [(and (= b 1) (= c 1)) (append (list 0 0 1) rest)]
        [(= b 2) (append (list 1 0 1) rest)]
        [(= c 2) (append (list 0 1 a) rest)]
        [else (append (list c b a) rest)]))

(define (wodniw a b c d rest acc)
  (cond [(and (> b 0) (> c 0)) (wodniw 1 (- b 1) (- c 1) d rest acc)]
        [(not d) (end a b c acc)]
        [(= b 2) (wodniw 1 0 0 (+ d 1) rest acc)]
        [else (wodniw b c d (car rest) (cdr rest) (cons a acc))]))

(define (trim-zeros a b c rest)
  (cond [(not a) '()]
        [(= a 0) (trim-zeros b c (car rest) (cdr rest))]
        [(not b) (list a)]
        [(and (> a 0) (> b 0)) (wodniw 1 (- a 1) (- b 1) c rest '())]
        [(not c) (end 0 a b '())]
        [(= a 2) (wodniw 1 0 b (+ c 1) rest '())]
        [else (wodniw a b c (car rest) (cdr rest) '())]))

(define (window a b c rest acc)
  (cond [(and (= b -1) (> c -1)) (window (+ a 1) 0 (- c 1) rest acc)]
        [(= a -1) (window 0 (+ b 1) (- c 1) rest acc)]
        [(null? rest) (trim-zeros c b a acc)]
        [else (window b c (car rest) (cdr rest) (cons a acc))]))

(define (start a b c rest)
  (cond [(not a) '()]
        [(and (= a -1) (not c)) '(1)]; b must be 1
        [(not b) (trim-zeros a #f #f (list #f))]
        [(and (= a -1) (= b 1)) (window 1 0 c rest (list #f #f #f))]
        [(not c) (trim-zeros b a #f (list #f #f))]
        [(and (= a 1) (= b -1)) (window 0 1 (- c 1) rest (list #f #f #f))]
        [else (window a b c rest (list #f #f #f))]))

(define (reduce li)
  (let* ([a (and   (not (null?       li )) (car   li))]
         [b (and a (not (null? (cdr  li))) (cadr  li))]
         [c (and b (not (null? (cddr li))) (caddr li))]
         [rest (if c (cdddr li) '())])
    (start a b c rest)))

(define (b-mov a b c d rest acc start?)
  (cond [(and (null? rest) (null? acc))
         (append (if (= 0 (+ a b c)) '() (list (+ b c)))
                 (if (= 0 a) '() (list a)))]
        [(null? rest) (append (list (+ b c) a) acc)]
        [start? (b-win b c d (car rest) (cdr rest) acc #f)]
        [else (b-win b c d (car rest) (cdr rest) (cons a acc) #f)]))

(define (b-win a b c d rest acc start?)
  (cond [(and (> b 0) (> c 0)) (b-mov 1 (- b 1) (- c 1) d rest acc #f)]
        [(= b 2) (b-mov 1 0 c (+ d 1) rest acc #f)]
        [else (b-mov a b c d rest acc start?)]))

(define (f-mov a b c d rest acc)
  (cond [(and (null? rest) (= d 0)) (b-win d c b a acc '() #t)]
        [(null? rest) (f-win b c d 0 rest (cons a acc))]
        [else (f-win b c d (car rest) (cdr rest) (cons a acc))]))

(define (f-win a b c d rest acc)
  (cond [(and (> b 0) (> c 0)) (f-mov a (- b 1) (- c 1) (+ d 1) rest acc)]
        [(> c 1) (f-mov (+ a 1) b (- c 2) (+ d 1) rest acc)]
        [else (f-mov a b c d rest acc)]))

(define (reduce li)
  (cond [(null? li) '()]
        [(null? (cdr li)) (f-win 0 0 (car li) 0 '() '())]
        [else (f-win 0 0 (car li) (cadr li) (cddr li) '())]))

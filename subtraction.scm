(define (b-mov a b c d rest acc start?)
  (cond [(and (null? rest) (null? acc))
         (append (if (= 0 (+ a b c)) '() (list (+ b c)))
                 (if (= 0 a) '() (list a)))]
        [(null? rest) (append (list (+ b c) a) acc)]
        [(and start? (= a 0)) (b-win b c d (car rest) (cdr rest) acc #t)]
        [else (b-win b c d (car rest) (cdr rest) (cons a acc) #f)]))

(define (b-win a b c d rest acc start?)
  (cond [(= b 2) (b-mov (+ a 1) 0 c (+ d 1) rest acc #f)]
        [(and (> b 0) (> c 0)) (b-mov (+ a 1) (- b 1) (- c 1) d rest acc #f)]
        [else (b-mov a b c d rest acc start?)]))

(define (f-mov a b c d e rest acc)
  (if (null? rest) (b-win e d c b (cons a acc) '() #t)
      (f-win b c d e (car rest) (cdr rest) (cons a acc))))

(define (f-win a b c d e rest acc)
  (cond [(= c -1) (f-mov a b 0 0 (- e 1) rest acc)]
        [(and (= d -1) (> e -1))
         (cond [(= b 1) (f-mov    a    0 0 1 (- e 1) rest acc)]
               [(= c 1) (f-mov (+ a 1) 0 0 1 (- e 1) rest acc)]
               [else    (f-mov    a    0 1 0 (- e 1) rest acc)])]
        [else (f-mov a b c d e rest acc)]))

(define (start a b c d e rest)
  (if (= c -1)
      (f-win 0 0 1 (- d 1) e rest '())
      (f-win a b c d e rest '())))

(define (reduce li)
  (cond [(null? li) (f-mov 0 0 0 0 0 '() '())]
        [(null? (cdr li)) (f-mov 0 0 (car li) 0 0 '() '())]
        [(null? (cddr li)) (start 0 0 (car li) (cadr li) 0 '())]
        [else (start 0 0 (car li) (cadr li) (caddr li) (cdddr li))]))

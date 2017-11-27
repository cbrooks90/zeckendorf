; Fix the extra zeros at the very end
(define (b-win a b c d rest acc start?)
  (cond [(= b 2) (b-win (+ a 1) 0 c (+ d 1) rest acc start?)]
        [(and (> b 0) (> c 0)) (b-win (+ a 1) (- b 1) (- c 1) d rest acc #f)]
        [(null? rest) (append (list (+ b c) a) acc)]
        [(and start? (= a 0)) (b-win b c d (car rest) (cdr rest) acc #t)]
        [else (b-win b c d (car rest) (cdr rest) (cons a acc) #f)]))

(define (f-mov a b c d e rest acc)
  (cond [(and (null? rest) (= c 0) (= d 0) (= e 0)) (b-win 0 0 b a acc '() #t)]
        [(null? rest) (f-win b c d e 0 '() (cons a acc))]
        [else (f-win b c d e (car rest) (cdr rest) (cons a acc))]))

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

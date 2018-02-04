; Subtraction of Zeckendorf representations is completely analogous to addition;
; we make two passes of a sliding window, starting with LSB, performing at most
; one reduction at each step.

; Since we're subtracting, there are no 2s and instead we have to propagate -1s
; forward. To accomodate this, in the forward direction the window has size 5
; and the reduction rules are more specific, although they can be derived from
; 1 1 0 = 0 0 1
; 0 0 2 0 = 1 0 0 1.
; See the comments on the definition of "f-win" below.

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
  (cond ; Most of the time we can deal with a -1 before it gets to c. The only
        ; time we let a -1 get to c is when there are two -1s in a row:
        ; a b -1 -1 e -> a b 0 0 e-1
        [(= c -1) (f-mov a b 0 0 (- e 1) rest acc)]
        ; When a single -1 comes into the window (at d), we can propagate it
        ; forward depending on b and c (which cannot both be 1, otherwise we
        ; subtracted from an invalid list).
        [(and (= d -1) (> e -1))
         (cond [(= b 1) (f-mov    a    0 0 1 (- e 1) rest acc)]; a 1 0 -1 e
               [(= c 1) (f-mov (+ a 1) 0 0 1 (- e 1) rest acc)]; a 0 1 -1 e
               [else    (f-mov    a    0 1 0 (- e 1) rest acc)])]; a 0 0 -1 e
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

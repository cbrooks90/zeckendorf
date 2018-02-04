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
  (cond [(and (null? rest) (null? acc)) (list (+ a b))]
        [(null? rest) (cons (+ a b) acc)]
        [(and start? (= a 0)) (b-win b c d (car rest) (cdr rest) acc #t)]
        [else (b-win b c d (car rest) (cdr rest) (cons a acc) #f)]))

(define (b-win a b c d rest acc start?)
  (cond [(= b 2) (b-mov (+ a 1) 0 c (+ d 1) rest acc #f)]
        [(and (> b 0) (> c 0)) (b-mov (+ a 1) (- b 1) (- c 1) d rest acc #f)]
        [else (b-mov a b c d rest acc start?)]))

(define (f-mov a b c d e rest acc start?)
  (cond [(and start? (= c 1)) (f-mov a b d c e rest acc #f)]
        [(null? rest) (b-win e d c b (cons a acc) '() #t)]
        [else (f-win b c d e (car rest) (cdr rest) (cons a acc) #f)]))

(define (f-win a b c d e rest acc start?)
  (cond ; Most of the time we can deal with a -1 before it gets to c. The only
        ; time we let a -1 get to c is when there are two -1s in a row:
        ; a b -1 -1 e -> a b 0 0 e-1
        [(= c -1) (f-mov a b 0 0 (- e 1) rest acc #f)]
        ; When a single -1 comes into the window (at d), we can propagate it
        ; forward depending on b and c (which cannot both be 1, otherwise we
        ; subtracted from an invalid list).
        [(and (= d -1) (> e -1))
         (cond [(= b 1) (f-mov    a    0 0 1 (- e 1) rest acc #f)]; a 1 0 -1 e
               [(= c 1) (f-mov (+ a 1) 0 0 1 (- e 1) rest acc #f)]; a 0 1 -1 e
               [else    (f-mov    a    0 1 0 (- e 1) rest acc start?)])]; a 0 0 -1 e
        [else (f-mov a b c d e rest acc #f)]))

(define (reduce li)
  (cond [(null? li) '()]
        [(null? (cdr li)) (f-win 0 0 0 (car li) 0 '() '() #t)]
        [else (f-win 0 0 0 (car li) (cadr li) (cddr li) '() #t)]))

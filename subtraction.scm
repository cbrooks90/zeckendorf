(define (wodniw a b c d rest acc start?)
  (cond [(= b 2) (wodniw (+ a 1) 0 c (+ d 1) rest acc #f)]
        [(and (> b 0) (> c 0)) (wodniw (+ a 1) (- b 1) (- c 1) d rest acc #f)]
        [(and start? (= a 0)) (wodniw b c d (car rest) (cdr rest) acc #t)]
        [(null? rest) acc]
        [else (wodniw b c d (car rest) (cdr rest) (cons a acc) #f)]))

(define (window a b c d rest acc)
  (cond [(and (= b -1) (< c 1) (> d -1)) (window a 0 (+ c 1) (- d 1) rest acc)]
        [(= b -1) (window (+ a 1) 0 (- c 1) d rest acc)]
        [(and (null? rest) (= a 0) (= b 0) (= c 0) (= d 0)) (wodniw d c b a acc '() #t)]
        [(null? rest) (window b c d 0 '() (cons a acc))]
        [else (window b c d (car rest) (cdr rest) (cons a acc))]))

(define (reduce li)
  (window 0 0 0 0 li '()))

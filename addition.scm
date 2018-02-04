; If we have two numbers in the Zeckendorf representation, we can add them digit
; by digit. The procedure "reduce" takes a list representing such a sum and
; produces the Zeckendorf representation for the sum.

; The method is to scan the list twice with a sliding window of size 4---once
; moving forward and then going backward. By forward we mean LSB->MSB and vice
; versa. This is for convenience with processing lists in Scheme.

; Every time the window slides by one unit, at most one reduction can be made,
; essentially propagating carries from the original sum. The backward pass is to
; correct for a possible violation in the Zeckendorf invariants which cannot be
; known until we make a full forward pass.

; Both passes essentially rely on the reductions (remember LSB is on the left)
;   1 1 0 -> 0 0 1
; 0 0 2 0 -> 1 0 0 1

(define (b-mov a b c d rest acc start?)
  (cond [(and (null? rest) (null? acc)) (if (zero? (+ a b)) '() (list (+ a b)))]
        [(null? rest) (cons (+ a b) acc)]
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
  (if (null? li) '()
      (f-mov 0 0 0 (car li) (cdr li) '())))

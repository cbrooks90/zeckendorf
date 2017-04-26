(load "pmatch.scm")

(define (reduce li)
  (pmatch li
    [`() '()]
    [`(2 0 . ,rest) (reduce `(0 1 . ,rest))]; only at start?
    [`(2 2 ,x . ,rest) (reduce `(1 1 ,(+ x 1) . ,rest))]
    [`(2 1 1 2 . ,rest) (reduce `(1 0 2 2 . ,rest))]
    [`(2 1 1 1 . ,rest) (reduce `(0 1 0 2 . ,rest))]
    [`(1 2 ,x . ,rest) (reduce `(0 1 ,(+ x 1) . ,rest))]
    [`(,x ,y 3 ,z . ,rest) (reduce `(,(+ x 1) ,y 1 ,(+ z 1) . ,rest))]
    [`(1 3 ,x . ,rest) (reduce `(0 2 ,(+ x 1) . ,rest))]
    [`(1 2) `(0 1 1)]
    [`(,x . ,rest) `(,x . ,(reduce rest))]))

(define (my-odd? x)
  (if (= 0 (- x (* 2 (quotient x 2)))) #t
      #f))

(define (my-even? x)
  (if (= 1 (- x (* 2 (quotient x 2)))) #t
      #f))

(define (power b e)
  (define (helper b e res)
    (if  (= e 1) res
      (helper b (- e 1) (* res b))))
  (helper b e b))

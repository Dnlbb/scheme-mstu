(define (my-gcd A B)
  (cond
    ((= (abs A) (abs B)) (abs A))
    ((> (abs A) (abs B)) (my-gcd  (- (abs A) (abs B)) B))
    ((> (abs B) (abs A)) (my-gcd A (- (abs B) (abs A))))))


(define (my-lcm A B)
  (/ (* A B) (my-gcd A B)))


(define (prime? n)
  (if (< n 2)
      #f
      (let ((prime? #t)
            (limit (ceiling (sqrt n))))
        (do ((i 2 (+ i 1)))
            ((or (>= i limit) (not prime?)) prime?)
          (if (= (remainder n i) 0)
              (set! prime? #f))))))

(define (modified-factorial)
  (let ((result-cache (list (cons 0 1))))
    (define (retrieve-from-cache num)
      (assoc num result-cache))

    (define (cache-update num factorial-result)
      (set! result-cache (cons (cons num factorial-result) result-cache))
      factorial-result)

    (define (calculate-factorial num)
      (let ((cached-result (retrieve-from-cache num)))
        (if cached-result
            (cdr cached-result)
            (cache-update num (if (= num 0)
                                  1
                                  (* num (calculate-factorial (- num 1))))))))

    (lambda (num) (calculate-factorial num))))

(define memoized-factorial (modified-factorial))



(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))


(define (lazy-car tw)
  (car tw))


(define (lazy-cdr tw)
  (force (cdr tw)))


(define (lazy-head xs k)
  (if (= k 0)
      '()
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))


(define (lazy-ref xs k)
  (if (= k 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))


(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))


(define (factorial-aux n acc)
  (lazy-cons acc (factorial-aux (+ n 1) (* acc (+ n 1)))))


(define (factorials)
  (factorial-aux 1 1))


(define (lazy-factorial n)
  (lazy-ref (factorials) (- n 1)))


(display (lazy-head (naturals 10) 12)) 
(newline)
(begin
  (display (lazy-factorial 10)) (newline)
  (display (lazy-factorial 50)) (newline))

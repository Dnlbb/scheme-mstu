(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex obj) (let((x obj)) (write 'obj) (display " => ")
                      (write x)(newline) x))))

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))
(zip '(1 2 3) '(one two three))

(define-syntax test
  (syntax-rules ()
    ((test obj res) (list 'obj res))))

(define run-test (lambda (test)
                   (write (car test))
                   (define res (eval (car test)
                               (interaction-environment)))
                   (if (equal? res (cadr test))
                       (begin
                         (display " ok")
                         #t)
                       (begin
                         (display " FAIL")
                         (newline)
                         (display "  Expected: ")
                         (write (cadr test))
                         (newline)
                         (display "  Returned: ")
                         (write res)
                         #f))
                   (newline)))

(define (run-tests xs)   
  (cond ((null? xs) #t)
        (else (and (run-test (car xs )) (run-tests (cdr xs))))))

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1)
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

(run-tests the-tests)


(define (refl1 xs num)
  (define (ref_helper xs  num accum)
    (cond  ((null? xs) #f)
           ((= (+ 1 accum) num) (car xs))
           (else (ref_helper (cdr xs) num (+ 1 accum)))))
  (if(< (length xs) num )
     #f
     (ref_helper xs num -1)))

(define (refs1 str num)
  (let* ((a (string->list str))
         (p (refl1 a num)))p))
        
(define (refv1 vec num)
  (let* ((a (vector->list vec))
         (p (refl1 a num)))p))


(define (refs2 str num a)
  (cond ((< num 0) #f)
        ((< (string-length str) num) #f)
        ((not (char? a)) #f)
        (else (begin (write (string-append
                          (substring str 0 num)
                                     (string a)
                                 (substring str num)))
                                          (newline)))))


(define (refl2 lst num a)
  (define (ref_helper0 lst1 lst2 a num accum)
    (cond ((< num 0) #f)
          ((= (+ accum 1) num)
           (let ((xs (append lst1 (list a) lst2)))
             xs))
          (else (ref_helper0 (append  lst1 (list(car lst2)))
                           (cdr lst2) a num (+ accum 1)))))
  (and (not (< (length lst) num))
       (ref_helper0 '() lst a num -1)))

(define (refv2 vec num a)
  (let ((b (vector->list vec))) (list->vector (refl2 b num a))))

(define (ref lst . args)
  (if (null? (cdr args))
      (cond ((list? lst) (refl1 lst (car args)))
            ((string? lst) (refs1 lst (car args)))
            ((vector? lst) (refv1 lst (car args))))
      (cond((list? lst) (refl2 lst (car args) (cadr args)))
           ((vector? lst) (refv2 lst (car args) (cadr args)))
           ((string? lst) (refs2 lst (car args) (cadr args))))))





(define (factorize expr)
  (define a (cadr (cadr expr)))
  (define b (cadr (caddr expr)))
  (cond
    ((and (equal? (caddr (cadr expr)) 2)
          (equal? (car expr) (quote -)))
     (list (quote *) (list (quote -) a b)
           (list (quote +) a b)))
    ((and (equal? (caddr (cadr expr)) 3)
          (equal? (car expr) (quote -)))
     (list (quote *) (list (quote -) a b)
           (list (quote +) `(expt ,a 2)
                (list (quote *) a b) `(expt ,b 2))))
    ((and (equal? (caddr (cadr expr)) 3)
          (equal? (car expr) (quote +)))
     (list (quote *) (list (quote +) a b)
           (list (quote +) `(expt ,a 2)
  (list (quote -) (list (quote *) a b)) `(expt ,b 2))))
    (else expr)))
     
(factorize '(- (expt x 2) (expt y 2)))
(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
(eval (list (list 'lambda 
                  '(x y) 
                  (factorize '(- (expt x 2) (expt y 2))))
            1 2)
      (interaction-environment))

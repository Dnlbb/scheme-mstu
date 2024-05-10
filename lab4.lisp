;;1
(define travel #f)

(define-syntax use-assertations
  (syntax-rules ()
    ((use-assertations) (call-with-current-continuation
                         (lambda (cc) (set! travel cc))))))

(define-syntax assert
  (syntax-rules ()
    ((assert expr) (if (not expr)
                       (begin (display "FAILED: ")
                              (display '(expr)) (newline)
                              (travel))))))

(use-assertations)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))

;;2
(define (save-data data file)
  (let ((fileq (open-output-file file)))
    (write data fileq)
    (close-output-port fileq)))

(define (load-data file)
  (let ((fileq (open-input-file file)))
    (let load-data-helper ((x (read-char fileq)) (accum '()))
      (if (eof-object? x ) (reverse accum)
          (load-data-helper (read-char fileq) (cons x accum))))))

(define (lengthq file)
  (define (length-helper accum fileq previous)
    (let ((x (read-char fileq)))
      (cond
        ((eof-object? x) (begin (close-input-port fileq) accum))
        ((and(eq? x #\newline) (not(eq? previous #\newline))) (length-helper (+ 1 accum) fileq x))
        (else (length-helper accum fileq x)))))
  (let ((fileq (open-input-file file)))
    (length-helper 1 fileq #f)))

(lengthq "lab4.rkt")

;;3

(define (trib_nomem n)
  (cond ((<= n 1) 0)
        ((= n 2) 1)
        ((> n 2 ) (+ (trib_nomem (- n 1)) (trib_nomem (- n 2)) (trib_nomem (- n 3))))))

(define trib_mem 
  (let ((known-results '()))
    (lambda (n)
      (let* ((args (list n))
             (res (assoc args known-results)))
        (if res
            (cadr res)
            (let ((res (cond ((<= n 1) 0)
                             ((= n 2) 1)
                             (else (+ (trib_mem (- n 1)) (trib_mem (- n 2)) (trib_mem (- n 3)))))))
              (set! known-results (cons (list args res) known-results))
              res))))))
(trib_mem 0)
(trib_mem 1)
(trib_nomem 2)
(trib_mem 2)
(trib_mem 3)
(trib_mem 5)

;;4
(define-syntax my-if
  (syntax-rules ()
    ((my-if condit T F)
     (force (or (and condit
                     (delay T))
                (delay F))))))

(my-if #t 1 (/ 1 0))
(my-if #f (/ 1 0) 1)

;;5
(define-syntax my-let
    (syntax-rules ()
        ((my-let ((var1 val1) (var2 val2) ...)
                 func)
             ((lambda (var1 var2 ...)
                  func) val1 val2 ...))))


(define-syntax my-let*
    (syntax-rules ()
        ((my-let* ((var val)) action)
                  ((lambda (var) action) val))
              ((my-let* ((var val) . more) action)
                        ((lambda (var) (my-let* more action)) val))))


;;6
;;a
(define-syntax when
  (syntax-rules ()
    ((when condition expr) (if condition expr))
    ((when condition expr . exprs) (if (and condition (not (null? . exprs)))
                                       (begin expr 
                                              (when condition . exprs))))))
(define-syntax unless
  (syntax-rules ()
    ((unless condition expr) (if (not condition) expr))
    ((unless condition expr . exprs) (if (and (not condition) (not(null? . exprs)))
                                         (begin expr
                                                (when condition . exprs))))))
(define x 3)
(when (> x 0) (display "x > 0")  (newline))
(unless (= x 0) (display "x = 0") (newline))
;;б

(define-syntax for
  (syntax-rules (in as)
    ((for i in xs expr . exprs)
     (let loop ((lst xs))
       (if (not(null? lst))
           (begin
             (let ((i (car lst)))
               expr
               (begin . exprs))
             (loop (cdr lst))))))
    ((for xs as i expr . exprs) (for i in xs expr . exprs))))
     
           

(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))

;;в
(define-syntax while
  (syntax-rules ()
    ((while condition . actions)
     (letrec ((loop (lambda ()
                      (if  condition
                           (begin
                             (begin . actions)
                             (loop))))))
       (loop)))))




(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))

;;г
(define-syntax repeat
  (syntax-rules (until)
    ((repeat actions until cond?) (letrec ((loop (lambda ()
                                                   (begin
                                                     (begin . actions)
                                                     (if (not cond?)
                                                         (loop))))))
                                    (loop)))))

(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))

;;д
(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << arg)
     (display arg))
    ((cout << endl . args)
     (begin
       (newline)
       (cout . args)))
    ((cout << arg . args)
     (begin
       (display arg)
       (cout . args)))))


(cout << "a = " << 1 << endl << "b = " << 2 << endl)


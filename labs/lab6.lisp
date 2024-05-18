;;1

;* - число в строковом виде 
;<all>::= <sign><number1*><numbers1*> "/" <number2*><numbers2*> | "#f"
;<sign>::= "-" | E
;<numbers1*>::=<number1*><numbers1*> | E
;<numbers2*>::=<number2*><numbers2*> | E
;<number1*>::=<number1>
;<number2*>::=<number2>
;<number1>::= "1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"0"
;<number2>::= "1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"0"


(define (split-string str)
  (let loop ((chars (string->list str))
             (before '())
             (after '())
             (found? #f))
    (cond
     ((null? chars) (list (list->string
                           (reverse before))
                          (list->string (reverse after))))
     ((char=? (car chars) #\/)
      (if found?
          (loop (cdr chars) before (cons (car chars)
                                         after) found?)
          (loop (cdr chars) before after #t)))
     (found?
      (loop (cdr chars) before (cons (car chars)
                                     after) found?))
     (else
      (loop (cdr chars) (cons (car chars) before)
            after found?)))))

(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define (is-digit? char)
  (and (char>=? char #\0) (char<=? char #\9)))

(define (string-to-int str)
  (let loop ((chars (string->list str))
             (result 0)
             (sign 1))
    (cond
      ((null? chars) (* result sign))
      ((char=? (car chars) #\-)
       (loop (cdr chars) result -1))
      ((char=? (car chars) #\+)
       (loop (cdr chars) result sign))
      ((is-digit? (car chars))
       (loop (cdr chars) (+ (* 10 result)
                            (char->digit (car chars))) sign)))))


(define (check-frac str)
  (and (not(equal? "+" (car (split-string str))))
       (not(equal? "+" (cadr (split-string str))))
           (not(equal? "-" (car (split-string str))))
           (not(equal? "-" (cadr (split-string str))))
           (not(equal? "" (car (split-string str))))
           (not(equal? "" (cadr (split-string str))))
           (number? (string-to-int (car(split-string str))))
           (number? (string-to-int (cadr(split-string str))))
           (> (string-to-int (cadr(split-string str))) 0)))




(define (scan-frac str)
  (cond ((and (not(equal? "+" (car (split-string str))))
              (not(equal? "+" (cadr (split-string str))))
              (not(equal? "-" (car (split-string str))))
              (not(equal? "-" (cadr (split-string str))))
              (number? (string-to-int (car(split-string str))))
              (number? (string-to-int (cadr(split-string str))))
              (not(equal? "" (cadr (split-string str)))))
         (/ (string-to-int (car(split-string str)))
            (string-to-int (cadr(split-string str)))))
        (else #f)))



(define (is-space? ch)
  (or (char=? ch #\space)
      (char=? ch #\newline)
      (char=? ch #\tab)))

(define (creator str)
  (let loop ((chars (string->list str))
             (word '())
             (words '()))
    (cond ((null? chars)
           (if (null? word)
               (reverse words)
               (reverse
                (cons (list->string (reverse word)) words))))
          ((is-space? (car chars))
           (if (null? word)
               (loop (cdr chars) word words)
               (loop (cdr chars) '()
                     (cons
                      (list->string (reverse word)) words))))
          (else
           (loop (cdr chars) (cons (car chars) word) words)))))

;<FracsList> ::= <Fraction> | <Fraction> <Whitespace> <FracsList>
;<Fraction> ::= <Integer> "/" <Integer2>
;<Integer> ::= <Sign> <Digits>
;<Integer2>::=<Digits>
;<Sign> ::= "-" | E
;<Digits> ::= <Digit> | <Digit> <Digits>
;<Digit> ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
;<Whitespace> ::= " " | "\n" | "\t" | <Whitespace> " " |
;                           <Whitespace> "\n" | <Whitespace> "\t"




(define (scan-many-fracs str)
  (define (scan-many-fracs-helper xs incorrect_frac res) 
    (cond ((equal? xs #f) #f)
          ((= incorrect_frac 1) #f)
          ((null? xs) (reverse res))
          ((check-frac (car xs))
           (scan-many-fracs-helper (cdr xs)
                                   incorrect_frac
                                   (cons (scan-frac (car xs)) res)))
          (else (scan-many-fracs-helper xs
                                        (+ 1 incorrect_frac) res))))
  (scan-many-fracs-helper (creator str) 0 '()))



;;2


;<Program>::= <Articles> <Body> .
;<Articles>::= <Article> <Articles> | .
;<Article>::= define word <Body> end .
;<Body>::= if <Body> endif <Body> | 
;                                    integer <Body> |
;                                             word <Body> | .



(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

 (define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))


(define (next stream)
  (let ((x (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    x))



(define (parse vec)
  (define stream (make-stream (vector->list vec) 
"EOF"))
  (call-with-current-continuation
   (lambda (error)
     (let ((res (program stream error)))
       (if (equal? (peek stream) "EOF")  
           res
           #f)))))



(define (program stream error)
  (list (articles stream error)
    (body stream error)))
    
(define (articles stream error)
  (if (equal? (peek stream) 'define)
        (cons (article stream error)
              (articles stream error))
      '()))

(define (article stream error)
  (if (equal? (peek stream) 'define)
      (begin
        (next stream) 
        (let ((word (next stream))
         (body-result (body stream error)))
      (cond ((equal? word 'define) (error #f))
            ((equal? word 'if) (error #f))
            ((equal? word 'endif) (error #f))
            ((equal? word 'end) (error #f))
           ((equal? (peek stream) 'end)
                 (begin
                   (next stream) 
                   (list word body-result)))
                
(else(error #f "Ожидался 'end после body")))))
      '()))



(define (body stream error)
  (cond
    ((equal? (peek stream) 'if)
     (begin (next stream)
        (let ((res-body (body stream error))
              (word (next stream))
              (body-after (body stream error)))
          (if (equal? word 'define) (error #f)
          (if (equal? word 'endif)
             (cons (list 'if res-body) body-after)
               (error #f))))))
    ((integer? (peek stream))
     (cons (next stream)
           (body  stream error)))
    ((and (symbol? (peek stream))
          (not(equal? 'define (peek stream)))
          (not(equal? 'end (peek stream)))
          (not(equal? 'endif (peek stream))))
     (cons (next stream) (body stream error)))
    (else '())))

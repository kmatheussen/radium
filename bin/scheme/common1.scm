(provide 'common1.scm)

#||
(define (c-display . rest)
  (for-each (lambda (d)
              (display d)
              (display " "))
            rest)
  (newline))
||#

(define (to-displayable-string a)
  (cond ((keyword? a)
         (<-> "#:" (to-displayable-string (keyword->symbol a))))
        ((symbol? a)
         (<-> "'" (symbol->string a)))
        ((string? a)
         a)
        ((number? a)
         (number->string a))
        ((equal? #t a)
         "#t")
        ((equal? #f a)
         "#f")
        ((list? a)
         (<-> "(" (apply <-> (map (lambda (b) (<-> b " ")) a)) ")"))
        ((pair? a)
         (<-> "(" (to-displayable-string (car a)) " . " (to-displayable-string (cdr a)) ")"))
        (else
         "#unknown type")))

(define (to-string a)
  (cond ((symbol? a)
         (symbol->string a))
        ((number? a)
         (number->string a))
        ((equal? #t a)
         "#t")
        ((equal? #f a)
         "#f")
        ((keyword? a)
         (<-> "#:" (to-string (keyword->symbol a))))
        (else
         a)))

(define (<-> . args) (apply string-append (map to-string args)))
(define (<_> . args) (string->symbol (apply <-> args)))

(define (<-displayable-> . args) (apply string-append (map to-displayable-string args)))


#||
(<-displayable-> (list 2 3 4))
(<-displayable-> (cons 3 4))
(<-displayable-> :ga)
(<-displayable-> ':ga)
(<-displayable-> 'ga)
(<-displayable-> :'ga)
||#

(define (c-display . args)
  (for-each (lambda (arg)
              (display arg)
              (display " "))
            args)
  (newline))

(define *my-gensym-N* 0)

(define (nth n list)
  (list-ref list (- n 1)))

(define (1+ n)
  (+ n 1))

(define (1- n)
  (- n 1))


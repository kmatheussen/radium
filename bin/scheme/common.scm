(provide 'common.scm)

(define (c-display . rest)
  (for-each (lambda (d)
              (display d)
              (display " "))
            rest)
  (newline))

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

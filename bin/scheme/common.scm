
(define (c-display . rest)
  (for-each (lambda (d)
              (display d)
              (display " "))
            rest)
  (newline))


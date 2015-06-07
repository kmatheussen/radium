(provide 'init.scm)

(set! (*s7* 'stacktrace-defaults)
      '(1000 ;; max-frames
        2450 ;; code-cols
        4550 ;; total-cols
        2450  ;; "where to place comments"
        #t ;; whether the entire output should be displayed as a comment
        ))


;;(set! (*stacktrace* 'max-frames) 1000)
;(set! (*stacktrace* 'code-cols) 2000)
;(set! (*stacktrace* 'total-cols) 2450)

(require stuff.scm)
(require write.scm)

(load "common1.scm")

(define (my-require what)
  (if (provided? what)
      (c-display what "already provided")
      (let loop ((load-path *load-path*))
        (if (null? load-path)
            (c-display what "not found")
            (let ((filename (<-> (car load-path) "/" what)))
              (if (file-exists? filename)
                  (load filename)
                  (loop (cdr load-path))))))))
                      

(my-require 'define-match.scm)

(my-require 'common2.scm)

(my-require 'mouse.scm)

(my-require 'quantitize.scm)


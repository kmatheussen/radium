(provide 'init.scm)

(set! (*s7* 'stacktrace-defaults)
      '(1000 ;; max-frames
        245 ;; code-cols
        455 ;; total-cols
        245  ;; "where to place comments"
        #t ;; whether the entire output should be displayed as a comment
        ))

(require stuff.scm)
(require write.scm)

(set! (hook-functions *error-hook*) 
      (list (lambda (hook)
              (display (ow!)))))

;;(set! (*stacktrace* 'max-frames) 1000)
;(set! (*stacktrace* 'code-cols) 2000)
;(set! (*stacktrace* 'total-cols) 2450)

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

;;(gc #f)

(provide 'init.scm)

(set! (*stacktrace* 'max-frames) 100)
(set! (*stacktrace* 'code-cols) 100)
(set! (*stacktrace* 'total-cols) 145)

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

(my-require 'mouse/mouse.scm)

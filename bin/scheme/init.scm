(provide 'init.scm)

(require write.scm)

(load "common.scm")

(define (my-require what)
  (if (provided? what)
      (c-display what "already provided")
      (let loop ((load-path *load-path*))
        (if (null? load-path)
            (c-display what "not found")
            (let ((filename (<-> (car load-path) "/" what)))
              (if (file-exists? filename)
                  (load filename)
                  (loop (cdr filename))))))))
                      

(my-require 'define-match.scm)

(my-require 'mouse/mouse.scm)



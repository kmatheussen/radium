(provide 'init.scm)

(set! (*s7* 'stacktrace-defaults)
      '(1000 ;; max-frames
        245 ;; code-cols
        455 ;; total-cols
        245  ;; "where to place comments"
        #t ;; whether the entire output should be displayed as a comment
        ))

(set! (*s7* 'history-size) 40)

(define *is-initializing* #t)


;;(set! (*stacktrace* 'max-frames) 1000)
;(set! (*stacktrace* 'code-cols) 2000)
;(set! (*stacktrace* 'total-cols) 2450)

(define (get-full-path-of-scm-file filename)
  (let loop ((load-path *load-path*))
    (if (null? load-path)
        filename
        (let ((full-path (string-append (ra:get-program-path) "/" (car load-path) "/" filename)))
          (if (file-exists? full-path)
              full-path
              (loop (cdr load-path)))))))
  

(define *currently-loading-file* #f)
(define *currently-reloading-file* #f)

(set! (hook-functions *rootlet-redefinition-hook*)
      (list (lambda (hook)
              (let ((message (string-append "Warning: Redefining "
                                            (symbol->string (hook 'symbol))
                                            (if *currently-loading-file*
                                                (string-append " while loading " *currently-loading-file*)
                                                "."))))
                (cond (*is-initializing*
                       (display "Error during initializationg: ")
                       (display message)
                       (newline)
                       (catch #t
                              (lambda ()
                                (ra:show-message message))
                              (lambda args
                                #t))
                       (exit))
                      ((and *currently-loading-file*
                            (not *currently-reloading-file*))
                       (ra:show-message message))
                      ((defined? 'c-display (rootlet))
                       (c-display message))
                      (else
                       (display message)
                       (newline)))))))


;; Redefine load so that we can show a warning window if redefining a symbol when loading a file for the first time
(let ((org-load load))
  (set! load
        (let ((loaded-names (make-hash-table 39 string=?)))
          (lambda (filename . args)
            (set! filename (get-full-path-of-scm-file filename))
            (define env (if (null? args) #f (car args)))
            (define old-loading-filename *currently-loading-file*)
            (define old-reloading *currently-reloading-file*)
            (set! *currently-loading-file* filename)
            (let ((ret (catch #t
                              (lambda ()
                                (set! *currently-reloading-file* (loaded-names filename))
                                (hash-table-set! loaded-names filename #t)
                                (if env
                                    (org-load filename env)
                                    (org-load filename)))
                              (lambda args
                                (display (ow!))))))
              (set! *currently-reloading-file* old-reloading)
              (set! *currently-loading-file* old-loading-filename)
              ret)))))



(require stuff.scm)
(require write.scm)


(set! (hook-functions *error-hook*) 
      (list (lambda (hook)
              (display (ow!)))))



(load "common1.scm")

(define (my-require what)
  (if (provided? what)
      (c-display what "already provided")
      (load (get-full-path-of-scm-file (symbol->string what)))))

(my-require 'define-match.scm)

(my-require 'common2.scm)

(my-require 'nodes.scm)

(my-require 'mouse.scm)

(my-require 'instruments.scm)

(my-require 'fxrange.scm)

(my-require 'various.scm)

(my-require 'quantitize.scm)

(my-require 'gui.scm)


(set! *is-initializing* #f)

;;(gc #f)

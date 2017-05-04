(provide 'init.scm)

(set! (*s7* 'stacktrace-defaults)
      '(1000 ;; max-frames
        245 ;; code-cols
        455 ;; total-cols
        245  ;; "where to place comments"
        #t ;; whether the entire output should be displayed as a comment
        ))

(set! (*s7* 'history-size) 80)

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
                                            (format #t "~A ~A~%" (hook 'name) (hook 'value))
                                            ;;(symbol->string (hook 'symbol))
                                            (if *currently-loading-file*
                                                (string-append " while loading " *currently-loading-file*)
                                                "."))))
                (cond (*is-initializing*
                       (display "Error during initializationg: ")
                       (display message)
                       (newline)
                       (catch #t
                              (lambda ()
                                (ra:add-message message))
                              (lambda args
                                #t))
                       (exit))
                      ((and *currently-loading-file*
                            (not *currently-reloading-file*))
                       (ra:add-message message))
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

(define-constant go-wrong-finished-called-because-something-went-wrong 0)
(define-constant go-wrong-finished-called-because-it-was-excplicitly-called 1)

(define go-wrong-hooks '())

(set! (hook-functions *error-hook*) 
      (list (lambda (hook)
              (display (ow!))
              (let ((gwhs go-wrong-hooks))
                (set! go-wrong-hooks '())
                (for-each (lambda (go-wrong-hook)
                            (go-wrong-hook))
                          gwhs))
              (if *is-initializing*
                  (exit)))))


(define-expansion (inc! var how-much)
  `(set! ,var (+ ,var ,how-much)))

(define-expansion (push! list el)
  `(set! ,list (cons ,el ,list)))

(define-expansion (push-back! list el)
  `(set! ,list (append ,list (list ,el))))


(define (delete-from das-list element)
  (if (eqv? (car das-list) element)
      (cdr das-list)
      (cons (car das-list)
            (delete-from (cdr das-list) element))))

(define (delete-from2 das-list element)
  (if (equal? (car das-list) element)
      (cdr das-list)
      (cons (car das-list)
            (delete-from (cdr das-list) element))))

(define (delete-list-from das-list elements)
  (if (null? elements)
      das-list
      (delete-list-from (delete-from das-list (car elements))
                        (cdr elements))))

;; This cleanup function will either be called when 'finished' is explicitly called,
;; or an exception happens. 'reason' will be the value of go-wrong-finished-called-because-something-went-wrong
;; or the value of go-wrong-finished-called-because-it-was-excplicitly-called.
;; Note that the argument for 'cleanup' can only be go-wrong-finished-called-because-something-went-wrong once.
;; See examples below.
(define (call-me-if-something-goes-wrong cleanup block)
  (define (remove-hook)
    (if (member? go-wrong-hook go-wrong-hooks)
        (set! go-wrong-hooks (delete-from go-wrong-hooks go-wrong-hook))))
  (define (go-wrong-hook)
    (remove-hook)
    (cleanup go-wrong-finished-called-because-something-went-wrong))
  (define (finished)
    (remove-hook)
    (cleanup go-wrong-finished-called-because-it-was-excplicitly-called))
  (push! go-wrong-hooks go-wrong-hook)
  (block finished))

#!!
(call-me-if-something-goes-wrong
 (lambda (reason)
   (assert (eqv? reason go-wrong-finished-called-because-it-was-excplicitly-called))
   (c-display "finished normally " reason))
 (lambda (finished)
   (finished)))

(call-me-if-something-goes-wrong
 (lambda (reason)
   (assert (eqv? reason go-wrong-finished-called-because-something-went-wrong))
   (c-display "finished because something went wrong " reason))
 (lambda (finished)
   (aasdeqrtpoiujqerotiuqpert)
   (finished)))
!!#


(define *logtype-hold* (ra:get-logtype-hold))
(define *logtype-linear* 0)


(load "common1.scm")

(define (my-require what)
  (if (provided? what)
      (c-display what "already provided")
      (load (get-full-path-of-scm-file (symbol->string what)))))

(my-require 'define-match.scm)

(my-require 'common2.scm)

(my-require 'gui.scm) ;; Must be loaded early since it creates the <gui> expansion macro.

(my-require 'nodes.scm)

;;(my-require 'mouse.scm)

(my-require 'instruments.scm)

(my-require 'fxrange.scm)

(my-require 'various.scm)

(my-require 'quantitize.scm)

(my-require 'timing.scm)



(define (init-step-2)
  (set! *is-initializing* #t)
  (my-require 'mouse.scm)
  (my-require 'mixer-strips.scm)
  (my-require 'pluginmanager.scm)
  (set! *is-initializing* #f))


(set! *is-initializing* #f)




;;(gc #f)

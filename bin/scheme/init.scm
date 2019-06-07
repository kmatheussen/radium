(provide 'init.scm)

(set! (*s7* 'stacktrace-defaults)
      '(1000 ;; max-frames
        245 ;; code-cols
        455 ;; total-cols
        245  ;; "where to place comments"
        #t ;; whether the entire output should be displayed as a comment
        ))


(set! (*s7* 'history-size) 80)

(define-constant morally-equal? equivalent?)

;;
;; Important: Radium has not been initialized when this file is loaded.
;; Many of the ra: function does work though, but many will also crash the program
;; if accessed here.
;;
;; Code that uses ra: functions should be loaded in the function 'init-step-2'
;; in the end of this function.
;;


(define *is-initializing* #t)

(define (eq2? a b)
  (when (not (<ra> :release-mode))
    (assert (symbol? a))
    (assert (symbol? b)))
  (eq? a b))


;; Note! This function is called from the error handler.
(define (get-as-displayable-string-as-possible info)
  (define (fallback)
    (object->string info))
  (catch #t
         (lambda ()
           (cond ((defined? 'to-displayable-string)
                  (to-displayable-string info))
                 ((defined? 'pp)
                  (pp info))
                 (else
                  (object->string info))))
         (lambda args
           (display "(get-as-displayable-string-as-possible info) failed. info:")
           (display info)
           (newline)
           (fallback))))

(define (safe-display-txt-as-displayable-as-possible txt)
  (display txt)
  (if (defined? 'safe-add-message-window-txt)
      (safe-add-message-window-txt txt)))

;; ow! from stuff.scm with html formatting
(define (my-ow!)
  (call-with-output-string
   (lambda (p)
     (let ((ow (owlet))
	   (elist (list (rootlet))))
       
       ;; show current error data
       (format p "error: ~A" (ow 'error-type))
       (let ((info (ow 'error-data)))
	 (if (and (pair? info)
		  (string? (car info)))
	     (format p ": ~A" (catch #t 
				(lambda () 
				  (apply format #f info))
				(lambda args 
				  "<error in format>")))
	     (if (not (null? info))
		 (format p ": ~A" info))))

       (format p "~%<br>error-code: ~S" (ow 'error-code))
       (when (ow 'error-line)
	 (format p "~%<br>error-file/line: ~A[~A]" (ow 'error-file) (ow 'error-line)))
	   
       ;; show history, if available
       (when (pair? (ow 'error-history)) ; a circular list, starts at error-code, entries stored backwards
	 (let ((history ())
	       (lines ())
	       (files ())
	       (start (ow 'error-history)))
	   (do ((x (cdr start) (cdr x))
		(i 0 (+ i 1)))
	       ((or (eq? x start)
		    (null? (car x))
		    (= i (*s7* 'history-size)))
		(format p "~%<br>error-history:~%<br>    ~S" (car start))
                (define i2 0)
                (define is-finished #f)
		(do ((x history (cdr x))
		     (line lines (cdr line))
		     (f files (cdr f))
                     )
                    ((null? x))
                  (set! i2 (+ i2 1))
                  (define is-call (and (pair? (car x))
                                       (or (procedure? (car (car x)))
                                           (symbol? (car (car x))))))
                  (define call-func-name (and is-call
                                              (string->symbol (format #f "~A" (car (car x))))))
                  (cond ((or is-finished
                             (equal? (car x)
                                     '(if (and rethrow maybe-rethrow) (throw *try-finally-rethrown*) ret))
                             (and is-call
                                  (eq? call-func-name 'FROM-C-catch-all-errors-and-display-backtrace-automatically)))
                         (set! is-finished #t)
                         "")
                        ((and is-call
                              (memq call-func-name
                                    '(let define define* delafina lambda catch begin else cond if do or and format display c-display)))
                         "")
                        (else
                         (format p (if (and (integer? (car line))
                                            (string? (car f))
                                            (not (string=? (car f) "*stdout*")))
                                       (values "~%<br><font color='black'>~A:</font>&nbsp;&nbsp;&nbsp;    <pre>~S</pre>~40T;<font color='black'>~A</font>[~A]" i2 (car x) (car f) (car line))
                                       (values "~%<br><font color='black'>~A:</font>&nbsp;&nbsp;&nbsp;    <pre>~S</pre>" i2 (car x)))))))
                (format p "~%<br>"))
	     (set! history (cons (car x) history))
	     (set! lines (cons (and (pair? (car x)) (pair-line-number (car x))) lines))
	     (set! files (cons (and (pair? (car x)) (pair-filename (car x))) files)))))
       
       ;; show the enclosing contexts
       (let ((old-print-length (*s7* 'print-length)))
	 (set! (*s7* 'print-length) 8)
	 (do ((e (outlet ow) (outlet e))) 
	     ((memq e elist)
	      (set! (*s7* 'print-length) old-print-length))
	   (if (> (length e) 0)
	       (format p "~%<br>~{~A~| ~}~%<br>" e))
	   (set! elist (cons e elist))))))))


;; Note! This function is called from the error handler.
(define (history-ow!)
  (define history (copy (s7:get-history)))
  (call-with-output-string
   (lambda (p)
     ;; show history, if available
     (when (pair? history)
       (let ((history ())
             (lines ())
             (files ())
             (start history))
         (do ((x (cdr start) (cdr x))
              (i 0 (+ i 1)))
             ((or (eq? x start)
                  (null? (car x))
                  (= i (*s7* 'history-size))
                  (and (> i (- (*s7* 'history-size)
                               8))
                       (or (equal? (car x) '(history-ow!))
                           (equal? (car x) '(safe-history-ow!)))))
              (format p "~%<br>history:~%<br>    ~S" (if (pair? x) (car x) (car start)))
              (do ((x history (cdr x))
                   (line lines (cdr line))
                   (f files (cdr f))
                   (i2 0 (+ i 1))
                   )
                  ((null? x))
                (format p (if (and (integer? (car line))
                                   (string? (car f))
                                   (not (string=? (car f) "*stdout*")))
                              (values "~%<br>~A:&nbsp;&nbsp;&nbsp;    ~S~40T;<font color='red'>~A</font>[~A]" i2 (car x) (car f) (car line))
                              (values "~%<br>~A:&nbsp;&nbsp;&nbsp;    ~S<br>" i2 (car x)))))
              (format p "~%<br>"))
           (set! history (cons (car x) history))
           (set! lines (cons (and (pair? (car x)) (pair-line-number (car x))) lines))
           ;;(for-each c-display lines)
           (set! files (cons (and (pair? (car x)) (pair-filename (car x))) files))))))))




;; Note! This function is called from the error handler.
(define (safe-history-ow!)
  (string-append (catch #t
                        (lambda ()
                          (let ((ow (owlet)))
                            (display "ERROR_CODE:") (display (ow 'error-code)) (newline)
                            (string-append (format #f "~%<br>;error-code: ~S~%<br>" (ow 'error-code))
                                           (if (ow 'error-line)
                                               (format #f "~%;error-file/line: <font color='red'>~S</font>[~A]~%<br>" (ow 'error-file) (ow 'error-line))
                                               (format #f "~%;error-file/line: ; <font color='red'>no file/linenum</font><br>")))))
                        (lambda args
                          (display "ARGS1:")(display args)(newline)
                          (get-as-displayable-string-as-possible (list "s7 error-code/error-file/line failed: " args))))
                 (catch #t
                        (lambda ()
                          (history-ow!))
                        (lambda args
                          (display "ARGS2:")(display args)(newline)
                          (get-as-displayable-string-as-possible (list "history-ow! failed: " args))))))

#!!
(safe-history-ow!)
(+ a b)
!!#

(define (safe-display-history-ow!)
  (safe-display-txt-as-displayable-as-possible (safe-history-ow!)))

(define (handle-assertion-failure-during-startup info)
  (when *is-initializing*
    (display info)(newline)
    (catch #t
           (lambda ()
             (define infostring (get-as-displayable-string-as-possible info))
             (display "infostring: ")
             (display infostring)             
             ;;(ow!) ;; to strip down some unnecessary history produced by history-ow!
             (newline)
             (ra:show-error infostring)
             )
           (lambda args
             (display "(Something failed 1. This is not good.)")(display args)(display (defined? 'ra:show-error))(newline)
             (safe-display-ow!)))
    (exit)))


;;(handle-assertion-failure-during-startup "testingexit")
;;(ra:show-error "hello")
;;;(exit)


;;(set! (*stacktrace* 'max-frames) 1000)
;(set! (*stacktrace* 'code-cols) 2000)
;(set! (*stacktrace* 'total-cols) 2450)

(define (get-full-path-of-scm-file filename)
  (let loop ((load-path *load-path*))
    (if (null? load-path)
        filename
        ;; (let ((full-path (string-append (ra:get-program-path) "/" (car load-path) "/" filename))) ;; can't use absolute paths in s7.
        (let ((full-path (string-append (car load-path) "/" filename)))
          (if (file-exists? full-path)
              full-path
              (loop (cdr load-path)))))))
  

(define *currently-loading-file* #f)
(define *currently-reloading-file* #f)

(set! (hook-functions *rootlet-redefinition-hook*)
      (list (lambda (hook)
              (let ((message (string-append "Warning: Redefining "
                                            (format #f "~A ~A~%" (hook 'name) (hook 'value))
                                            ;;(symbol->string (hook 'symbol))
                                            (if *currently-loading-file*
                                                (string-append " while loading " *currently-loading-file*)
                                                "."))))
                (cond (*is-initializing*
                       (when (not (eq? (hook 'name) 'define-class))
                         (display "Error during initializationg: ")
                         (display message)
                         (newline)
                         (catch #t
                                (lambda ()
                                  (ra:add-message message));(ra:get-html-from-text message)))
                                (lambda args
                                  #t))
                         ;;(handle-assertion-failure-during-startup message)
                         ))
                      ;;((and *currently-loading-file*
                      ;;      (not *currently-reloading-file*))
                      ;; (ra:add-message message));(ra:get-html-from-text message)))
                      ((defined? 'c-display (rootlet))
                       (c-display message))
                      (else
                       (display message)
                       (newline)))))))


(define (my-equal? a b)
  (morally-equal? a b))
#||
  ;;(c-display "my-equal?" a b)
  (cond ((and (pair? a)
              (pair? b))
         (and (my-equal? (car a)
                         (car b))
              (my-equal? (cdr a)
                         (cdr b))))
        ((and (vector? a)
              (vector? b))
         (my-equal? (vector->list a)
                    (vector->list b)))
        ((and (procedure? a)
              (procedure? b))
         (structs-equal? a b))
        (else
         (morally-equal? a b))))
||#


;; assert and ***assert***
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (assert something)
  (when (not something)
    (handle-assertion-failure-during-startup 'assert-failed) ;; we have a better call to handle-assertion-failure in ***assert***
    (error 'assert-failed)))

;; It is assumed various places that eqv? can be used to compare functions.
(assert (eqv? assert ((lambda () assert))))


(define (***assert*** a b)
  (define (test -__Arg1 -__Arg2)
    (define (-__Func1)
      (let ((A -__Arg1))
        (if (my-equal? A -__Arg2)
            (begin
              (newline)
              (pretty-print "Correct: ")
              (pretty-print (to-displayable-string A))
              (pretty-print "")
              (newline)
              #t)
            (-__Func2))))
    (define (-__Func2)
      (let ((A -__Arg1))
        (let ((B -__Arg2))
          (begin
            (newline)
            (c-display ". Correct: ")
            (c-display (pp B))
            (c-display "Wrong. Result: ")
            (c-display (pp A))
            (handle-assertion-failure-during-startup (list "***assert*** failed. Result:" A ". Expected:" B))
            (newline)
            (if *is-initializing*
                (error 'assert-failed))
            #f))))
    (-__Func1))

  (test a b))




;; Redefine load so that we can show a warning window if redefining a symbol when loading a file for the first time
;;
(let ((org-load load))
  (set! load
        (let ((loaded-names (make-hash-table 39 string=?)))
          (lambda (filename . args)
            (set! filename (get-full-path-of-scm-file filename))
            (define is-first-time (loaded-names filename))
            (define env (if (null? args) #f (car args)))
            (define old-loading-filename *currently-loading-file*)
            (define old-reloading *currently-reloading-file*)
            (set! *currently-loading-file* filename)
            (define do-rethrow #f)
            (let ((ret (catch #t
                              (lambda ()
                                (set! *currently-reloading-file* (loaded-names filename))
                                (hash-table-set! loaded-names filename #t)
                                (if env
                                    (org-load filename env)
                                    (org-load filename)))
                              (lambda args
                                (set! do-rethrow (not is-first-time))
                                (cond ((defined? 'safe-display-ow!)
                                       (safe-display-ow!))
                                      ((defined? 'safe-ow!)
                                       (safe-display-txt-as-displayable-as-possible (safe-ow!)))
                                      (else
                                       (display (my-ow!))))))))
              (set! *currently-reloading-file* old-reloading)
              (set! *currently-loading-file* old-loading-filename)
              (if (and #f do-rethrow)
                  (error 'loading-failed)
                  ret))))))



(require stuff.scm)

(define *safe-ow-recursive-level* 0)

(define (safe-ow!)
  (if (> *safe-ow-recursive-level* 0)
      (begin
        (display "====== detected possible infinite recursion in safe-ow! ========")
        (newline)
        (<ra> :schedule 1000
              (lambda ()
                (set! *safe-ow-recursive-level* 0)))
        "safe-ow!-recursion-detected")
      (let ((ret (catch #t
                        (lambda ()
                          (inc! *safe-ow-recursive-level* 1)
                          (my-ow!))
                        (lambda args
                          (safe-display-history-ow!)
                          (get-as-displayable-string-as-possible (list "ow! failed: " args))))))
        (set! *safe-ow-recursive-level* 0)
        ret)))
        
        

  
(define (safe-display-ow!)
  (safe-display-txt-as-displayable-as-possible (safe-ow!)))

(require write.scm)

(define-constant go-wrong-finished-called-because-something-went-wrong 0)
(define-constant go-wrong-finished-called-because-it-was-excplicitly-called 1)

(define go-wrong-hooks '())

(assert (defined? 'safe-display-ow!))
(assert (defined? 'safe-ow!))
(set! (hook-functions *error-hook*) 
      (list (lambda (hook)
              (define backtrace-txt (safe-ow!))
              (safe-display-txt-as-displayable-as-possible backtrace-txt)
              (catch #t
                     (lambda ()
                       (let ((gwhs go-wrong-hooks))
                         (set! go-wrong-hooks '())
                         (for-each (lambda (go-wrong-hook)
                                     (go-wrong-hook))
                                   gwhs)))
                     (lambda args
                       (safe-display-ow!)
                       (get-as-displayable-string-as-possible (list "Custom error hook catch failed:\n"))))
              (handle-assertion-failure-during-startup (list "error-hook failed\n" backtrace-txt)))))

-;;(handle-assertion-failure-during-startup "hello")

;; Test startup crash: (crash reporter should show up, and program exit). Startup crashes before this point (i.e. before the error hook is added, are not handled.)
;;(+ a 90)


(define-expansion (inc! var how-much)
  `(set! ,var (+ ,var ,how-much)))

(define-expansion (push! list el)
  `(set! ,list (cons ,el ,list)))

(define-expansion (push-back! list . elements)
  `(set! ,list (append ,list (list ,@elements))))


(define (delete-from das-list element)
  (assert (not (null? das-list)))
  (if (eqv? (car das-list) element)
      (cdr das-list)
      (cons (car das-list)
            (delete-from (cdr das-list) element))))

(define (delete-from2 das-list element)
  (assert (not (null? das-list)))
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


(define-constant *logtype-hold* (ra:get-logtype-hold))
(define-constant *logtype-linear* 0)


(load "common1.scm")

(define (my-require what)
  (if (provided? what)
      (c-display what "already provided")
      (load (get-full-path-of-scm-file (symbol->string what)))))

(my-require 'define-match.scm)

(my-require 'common2.scm)

(my-require 'nodes.scm)

;;(my-require 'mouse.scm)

(my-require 'instruments.scm)
(my-require 'solo.scm)

(my-require 'fxrange.scm)

(my-require 'various.scm)

(my-require 'quantitize.scm)

(my-require 'timing.scm) ;; Note that the program assumes that g_scheme_has_inited1 is true after timing.scm has been loaded.

(my-require 'main_menus.scm)


(define-constant *functions-called-from-evalScheme* 
  '(show-global-swing-track-popup-menu
    minimize-lowertab
    remake-mixer-strips
    FROM_C-redraw-mixer-strips
    toggle-current-mixer-strips-fullscreen
    set-random-sample-for-all-selected-sampler-instruments
    show-async-message
    FROM-C-show-help-window
    FROM_C-cut-all-selected-seqblocks
    FROM_C-paste-sequencer-blocks
    FROM_C-copy-all-selected-seqblocks
    FROM_C-delete-all-selected-seqblocks
    pmg-start
    make-instrument-conf
    FROM_C-keybindings-have-been-reloaded
    add-protracker-trackline         ;; import_mod.scm isn't loaded until we need it.
    start-adding-protracker-events!  ;;
    set-protracker-instrumentlist!   ;;
    set-protracker-playlist!         ;;
    set-protracker-pattern-format    ;;
    ra:schedule
    ra:add-message
    show/hide-instrument-gui
    system
    create-change-editor-font-requester
    create-change-system-font-requester
    for-each
    line-zoom-in-exponentially
    line-zoom-out-exponentially
    begin
    c-display
    moduloskew-track
    replace-with-random-notes-in-range
    replace-with-random-notes-in-track
    replace-with-random-notes-in-block
    replace-with-random-velocities-in-range
    replace-with-random-velocities-in-track
    replace-with-random-velocities-in-block
    moduloskew-range
    moduloskew-track
    moduloskew-block
    shuffle-range
    shuffle-track
    shuffle-block
    distribute-range-evenly
    distribute-track-evenly
    distribute-block-evenly
    set-random-octaves-for-notes-in-range
    set-random-octaves-for-notes-in-track
    set-random-octaves-for-notes-in-block
    randomly-delete-notes-range
    randomly-delete-notes-track
    randomly-delete-notes-block
    fullshuffle-range
    fullshuffle-track
    fullshuffle-block
    randomize-note-durations-range
    randomize-note-durations-track
    randomize-note-durations-block
    show-bars-and-beats-or-line-numbers-popup-menu
    popup-menu
    delete-all-unused-MIDI-instruments
    FROM_C-create-modulator-gui
    FROM_C-create-blocks-table-gui
    FROM_C-create-instruments-table-gui
    ra:request-load-instrument-preset
    ra:request-load-preset-instrument-description
    FROM_C-update-seqblock-track-on-off-configuration
    randomly-delete-notes-range
    randomly-delete-notes-track
    randomly-delete-notes-block
    FROM_C-split-sample-seqblock-under-mouse
    if
    FROM_C-call-me-when-num-seqtracks-might-have-changed
    ra:set-instrument-color
    FROM_C-reconfigure-sequencer-left-part
    FROM_C-reconfigure-sequencer-right-part
    FROM_C-show-blocklist-popup-menu
    FROM_C-show-playlist-popup-menu
    load
    FROM_C-set-current-seqblock!
    FROM_C-call-me-when-curr-seqtrack-has-changed
    FROM-C-sequencer-gui-in-window
    FROM_C-create-menu-entry-widget
    FROM_C-reconfigure-sequencer-timing-part
    FROM_C-paint-sequencer-grid
    FROM_C-show-effect-popup-menu
    show-instrument-color-dialog
    ;;FROM_C-prepare-seqblock-stretch-automation-for-interface2
    FROM_C-request-rename-instrument
    FROM_C-copy-editor-track-on/off-to-seqblock
    FROM_C-copy-seqblock-track-on/off-to-editor
    FROM_C-create-editor-lower-part-gui
    FROM_C-reconfigure-editor-lower-part-gui!
    FROM_C-create-granular-vizualization-gui-for-sample-player
    generate-main-menus
    ra:set-curr-seqtrack
    FROM_C-switch-solo-for-selected-instruments
    FROM_C-switch-mute-for-selected-instruments
    FROM_C-switch-bypass-for-selected-instruments
    ra:enable-metronome
    ))

(define-constant *functions-called-from-evalScheme-that-are-not-available-at-program-startup*
  '(add-protracker-trackline         ;; import_mod.scm isn't loaded until we need it.
    start-adding-protracker-events!  ;; -------- '' --------
    set-protracker-instrumentlist!   ;; -------- '' --------
    set-protracker-playlist!         ;; -------- '' --------
    set-protracker-pattern-format    ;; -------- '' --------
    ))

(define (FROM-C-assert-that-function-can-be-called-from-evalScheme funcname)
  (if (not (memq (string->symbol funcname) *functions-called-from-evalScheme*))
      (ra:show-error (<-> "The function -" funcname "- has not been added to the list of functions that can be called from evalScheme"))))

(define (assert-functions-called-from-evalScheme)
  (for-each (lambda (funcname)
              (c-display "CHECKING that" funcname "is defined")
              (if (not (memq funcname *functions-called-from-evalScheme-that-are-not-available-at-program-startup*))
                  (assert (defined? funcname))))
            *functions-called-from-evalScheme*))
              

;; Called during startup. Will be overridden later.
(define (FROM_C-call-me-when-num-seqtracks-might-have-changed new-num-seqtracks)
  (assert (not (provided? 'mouse.scm))))

;; The files loaded in init-step-2 can use ra: functions.
(define (init-step-2)

  (set! *is-initializing* #t)
  
  ;; gui.scm can be loaded this late since expansion macros now can be defined after they are used.
  ;; Prev comment: Must be loaded early since it creates the <gui> expansion macro.
  (my-require 'gui.scm)

  ;; Evaluate before loading main_layout.scm since main_layout creates the edit tab, which uses keybindings.
  (<ra> :reload-keybindings)

  (my-require 'main_layout.scm)
  
  (my-require 'mouse.scm)
  (my-require 'mixer-strips.scm)
  (my-require 'pluginmanager.scm)
  (my-require 'seqblock_audio.scm)
  (my-require 'editor_lower_part.scm)

  (assert-functions-called-from-evalScheme)

  (common1-finished-loading)
  
  (set! *is-initializing* #f))


(set! *is-initializing* #f)




;;(gc #f)

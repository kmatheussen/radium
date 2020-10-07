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
;;(define *all-files-loaded #f)

(define-constant *empty-symbol* '___empty_symbol) ;; s7 doesn't allow converting empty string to symbol

(define-constant *functions-and-symbols-used-by-C*
  '(show-global-swing-track-popup-menu
    FROM_C-minimize-lowertab
    remake-mixer-strips
    FROM_C-redraw-mixer-strips
    toggle-current-mixer-strips-fullscreen
    show-async-message
    FROM_C-cut-all-selected-seqblocks
    FROM_C-paste-sequencer-blocks
    FROM_C-copy-all-selected-seqblocks
    FROM_C-delete-all-selected-seqblocks
    pmg-start
    make-instrument-conf
    add-protracker-trackline         ;; import_mod.scm isn't loaded until we need it.
    start-adding-protracker-events!  ;;
    set-protracker-instrumentlist!   ;;
    set-protracker-playlist!         ;;
    set-protracker-pattern-format    ;;
    ra:schedule
    ra:add-message
    add-message-window-message
    show/hide-instrument-gui
    system
    create-change-editor-font-requester
    create-change-system-font-requester
    for-each
    line-zoom-in-exponentially
    line-zoom-out-exponentially
    begin
    set!
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
    replace-with-fixed-velocities-in-range
    replace-with-fixed-velocities-in-track
    replace-with-fixed-velocities-in-block
    show-bars-and-beats-or-line-numbers-popup-menu
    popup-menu
    delete-all-unused-MIDI-instruments
    ra:request-load-instrument-preset
    ra:request-load-preset-instrument-description
    FROM_C-update-seqblock-track-on-off-configuration
    randomly-delete-notes-range
    randomly-delete-notes-track
    randomly-delete-notes-block
    if
    ra:set-instrument-color
    load
    FROM_C-show-instrument-color-dialog
    generate-main-menus
    ra:set-curr-seqtrack
    
    safe-history-ow!
    ra:enable-metronome
    
    
    ;; git grep s7extra_get_func_from_funcname|cut -f2 -d"\""
create-block-timings
create-filledout-swings2

    ;;git grep find_and_protect_scheme_value|cut -f2 -d"\""
FROM-C-catch-all-errors-and-display-backtrace-automatically
safe-scale
+
-
*
/
+
-
*
/
quantitize
error
*eat-errors-failed-return-value*

    ;; git grep S7EXTRA_GET_FUNC|cut -f2 -d"\""
FROM_C-create-menu-entry-widget
FROM_C-paint-seqblock-stuff
FROM_C-paint-sequencer-grid
FROM_C-call-me-when-curr-seqtrack-has-changed

;; git grep S7CALL2|cut -f2 -d"\""
FROM_C-playlist-insert!
FROM_C-playlist-remove!
FROM_C-recreate-block/audio-list-guis
FROM_C-update-playlist-area
FROM_C-recreate-playlist-area
FROM_C-create-bock-and-playlist-gui
FROM_C-call-me-when-num-seqtracks-might-have-changed
FROM_C-reconfigure-editor-lower-part-gui!
FROM_C-reconfigure-editor-lower-part-gui!
FROM_C-redraw-mixer-strips
FROM_C-create-editor-lower-part-gui
FROM_C-reconfigure-sequencer-left-part
FROM_C-reconfigure-sequencer-right-part
FROM_C-reconfigure-sequencer-timing-part
mid-horizontal-layout
FROM_C-create-mixer-strips-gui
FROM_C-mixer-strips-get-num-rows
FROM_C-mixer-strips-change-num-rows
mixer-strips-get-vert-ratio
mixer-strips-change-vert-ratio
mixer-strips-reset-configuration!
mixer-strips-get-configuration
mixer-strips-set-configuration!
FROM-C-get-main-y-splitter
FROM-C-get-lowertab-gui
FROM-C-set-lowertab-includes-instrument
FROM-C-set-lowertab-includes-sequencer
FROM-C-show-instrument-gui
FROM-C-hide-instrument-gui
FROM-C-instrument-gui-is-visible
FROM-C-show-edit-gui
FROM-C-hide-edit-gui
FROM-C-edit-gui-is-visible
FROM-C-get-edit-gui
FROM-C-show-edit/quantitize-tab
FROM-C-show-edit/transpose-tab
FROM-C-show-edit/randomize-tab
FROM-C-show-edit/various-tab
FROM-C-show-sequencer-gui
FROM-C-hide-sequencer-gui
FROM-C-sequencer-gui-is-visible
FROM_C-create-standalone-mixer-strip
select-track-instrument
async-replace-instrument
async-load-instrument-preset
create-instrument-popup-menu
FROM-C-generate-new-color-for-all-selected-seqblocks
FROM-C-generate-new-color-for-all-selected-instruments
FROM_C-set-solo-for-instruments
FROM_C-set-mute-for-instruments
FROM_C-set-bypass-for-instruments
FROM_C-switch-solo-for-selected-instruments
FROM_C-switch-mute-for-selected-instruments
FROM_C-switch-bypass-for-selected-instruments
FROM_C-redraw-mixer-strips
FROM_C-copy-editor-track-on/off-to-seqblock
FROM_C-copy-seqblock-track-on/off-to-editor
;;FROM_C-jump-to-mark
;;FROM_C-jump-prev-mark
;;FROM_C-jump-next-mark
FROM-C-configure-sequencer-widget!
FROM-C-sequencer-gui-in-window
FROM-C-assert-that-function-can-be-called-from-C
FROM_C-create-modulator-gui
FROM_C-create-granular-vizualization-gui-for-sample-player
FROM_C-update-implicit-solo-connections!
FROM_C-update-implicit-solo-connections!
radium-mouse-press
radium-mouse-move
radium-mouse-release
mylint-and-eval-string
eval-string
mylint-and-eval-string
eval-string
FROM_C-remove-instrument-from-connection-path
FROM_C-get-displayable-keybinding
FROM_C-request-rename-instrument
FROM_C-update-implicit-solo-connections!
FROM_C-display-solo-status-in-statusbar
FROM_C-display-mute-status-in-statusbar
FROM_C-display-bypass-status-in-statusbar
FROM_C-set-current-seqblock!
FROM_C-show-blocklist-popup-menu
FROM_C-show-playlist-popup-menu
FROM_C-minimize-lowertab
remake-mixer-strips
toggle-current-mixer-strips-fullscreen
generate-main-menus
FROM_C-show-effect-popup-menu
show-bars-and-beats-or-line-numbers-popup-menu
show-global-swing-track-popup-menu
set-random-sample-for-all-selected-sampler-instruments
FROM-C-show-help-window
FROM_C-create-blocks-table-gui
FROM_C-create-instruments-table-gui
FROM_C-keybindings-have-been-reloaded
FROM_C-jump-next-mark
FROM_C-jump-prev-mark
FROM_C-jump-to-mark
FROM_C-move-current-instrument-left
FROM_C-move-current-instrument-right
FROM_C-move-current-instrument-up
FROM_C-move-current-instrument-down
recreate-seqtracks-config-area
draw-button
FROM_C-create-editor-track-headers-gui
FROM_C-reconfigure-editor-track-headers-gui!
FROM_C-show-mixer-config-popup-menu
FROM_C-show-mixer-config-reset-popup-menu
FROM_C-window-mode-popup-menu
FROM_C-show-modular-popup-menu
FROM_C-show-instrument-in-mixer-popup-menu
FROM_C-show-sequencer-in-mixer-popup-menu
FROM_C-show-cpu-usage-in-mixer-popup-menu
FROM_C-show-mixer-connections-popup-menu
FROM_C-show-mixer-bus-connections-popup-menu
FROM_C-show-mixer-zoom-reset-popup-menu
FROM_C-show-mixer-rotate-popup-menu
FROM_C-set-pianoroll-autorange
FROM_C-show-mixer-help-popup-menu
FROM_C-playlist-up!
FROM_C-playlist-down!
FROM_C-show-sequencer-in-full-mode!
FROM_C-show-bottom-bar-octave-down-popup-menu
FROM_C-show-bottom-bar-octave-up-popup-menu
FROM_C-show-bottom-bar-undo-popup-menu
FROM_C-show-bottom-bar-redo-popup-menu
FROM_C-show-bottom-bar-switch-drunk_velocity-popup-menu
FROM_C-show-bottom-bar-switch-edit-popup-menu
FROM_C-show-bottom-bar-switch-click-popup-menu
FROM_C-show-bottom-bar-switch-play-cursor-popup-menu
FROM_C-show-bottom-bar-switch-editor-follows-play-cursor-popup-menu
FROM_C-show-mixer-ratio13-popup-menu
FROM_C-show-mixer-ratio11-popup-menu
FROM_C-show-mixer-ratio31-popup-menu
FROM_C-show-mixer-R1-popup-menu
FROM_C-show-mixer-R2-popup-menu
FROM_C-show-mixer-R3-popup-menu
FROM_C-show-mixer-R4-popup-menu
FROM_C-insert-seqtrack
FROM_C-delete-seqtrack
FROM_C-call-me-after-seqtrack-has-been-deleted
undo-block
FROM_C-create-quantitize-gui
ra:load-song
quantitize-note
FROM_C-nsm-open
FROM_C-nsm-save
FROM_C-nsm-new-song
FROM_C-nsm-save-as
FROM_C-nsm-quit
FROM_C-cut-selected-pianonotes!
FROM_C-copy-selected-pianonotes
FROM_C-paste-pianonotes!
FROM_C-delete-selected-pianonotes!
show-keybinding-help-window
FROM_C-ensure-curr-block-is-visible-in-blocklist
FROM_C-call-me-after-curr-seqblock-under-mouse-has-been-called
FROM_C-show-lock-instrument-popup-menu
FROM_C-show-mixer-popup-menu
FROM_C-show-mixer-connection-popup-menu
FROM_C-delete-editor-beat
FROM_C-delete-editor-bar
FROM_C-show-keybindings-editor
FROM_C-select-prev-instrument-program
FROM_C-select-next-instrument-program
FROM_C-select-prev-instrument-program-popup-menu 
FROM_C-select-next-instrument-program-popup-menu
FROM_C-load-next-instrument-preset
FROM_C-load-prev-instrument-preset
FROM_C-load-prev-instrument-preset-popup-menu 
FROM_C-load-next-instrument-preset-popup-menu
FROM_C-load-instrument-preset-popup-menu
FROM_C-replace-instrument-preset-popup-menu
FROM_C-show-hide-instrument-gui-popup-menu
FROM_C-select-instrument-a/b-popup-menu
))

(define-constant *functions-and-symbols-used-by-C-that-are-not-available-at-program-startup*
  '(add-protracker-trackline         ;; import_mod.scm isn't loaded until we need it.
    start-adding-protracker-events!  ;; -------- '' --------
    set-protracker-instrumentlist!   ;; -------- '' --------
    set-protracker-playlist!         ;; -------- '' --------
    set-protracker-pattern-format    ;; -------- '' --------
    recreate-seqtracks-config-area
    ))

(define-constant *eat-errors-failed-return-value* (gensym "catch-all-errors-and-display-backtrace-automatically-failed-value"))

(define (FROM-C-catch-all-errors-and-display-backtrace-automatically func . args)
  (catch #t
         (lambda ()
           (apply func args))
         (lambda args
           ;;(display "    FROM_C_catch2. args:") (display args) (newline)
           (when (not (catch-args-are-from-try-finally args)) ;; if it is from try-finally, we don't need to display backtrace.
             (display "FROM-C-catch-all-errors-and-display-backtrace-automatically: func failed. Args:")(newline)(display "    ")(display args)(newline)
             (if (defined? 'safe-display-ow!)
                 (catch #t
                        safe-display-ow!
                        (lambda args
                          (display "    FROM_C_catch3")(newline)
                          (display "safe-display-ow! failed:")(newline)
                          (display args)
                          (newline))))
             *eat-errors-failed-return-value*))))




(define-constant morally-equal? equivalent?)



;;
;; Important: Radium has not been initialized when this file is loaded.
;; Many of the ra: function does work though, but many will also crash the program
;; if accessed here.
;;
;; Code that uses ra: functions should be loaded in the function 'init-step-2'
;; in the end of this function.
;;


(define (eq2? a b)
  (when (not (ra:release-mode))
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

;; ow! from stuff.scm with html formatting
(define (my-ow!)
  (newline)(newline)(newline)
  (call-with-output-string
   (lambda (p)
     (let ((ow (owlet))
	   (elist (list (rootlet))))
       
       ;; show current error data
       (format p "error: ~A " (ow 'error-type))
       (let ((info (ow 'error-data)))
	 (if (and (pair? info)
		  (string? (car info)))
	     (format p " <font color='black' font size=\"+2\">~A</font><br>" (catch #t 
				(lambda () 
				  (apply format #f info))
                                  ;;(ra:get-html-from-text (apply format #f info)))
				(lambda args 
				  "<error in format>")))
	     (if (not (null? info))
		 (format p "~A: " info))))

       (format p "~%<br>error-code: <font color='black' font size=\"+2\">~S</font><br>" (ow 'error-code))
       (when (ow 'error-line)
	 (format p "~%<br>error-file/line: <font color='black'>~A[~A]</font><br>" (ow 'error-file) (ow 'error-line)))
	   
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
		(format p "~%<br>error-history: ~%<br><br>~S<br>" (ra:get-html-from-text (format #f "~A" (car start))))
                (define i2 0)
                (define is-finished #f)
		(do ((x history (cdr x)) ;; x))) ;;(cdr x))
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
                             ;(and is-call
                             ;     (eq? call-func-name 'FROM-C-catch-all-errors-and-display-backtrace-automatically))
                             (and is-call
                                  (eq? call-func-name 'eval-string))
                             )
                         (set! is-finished #t)
                         "")
                        ((and is-call
                              ;;(not (integer? (car line)))
                              (memq call-func-name
                                    '(let define define* delafina lambda catch begin else cond if do or and format display c-display when call-with-exit)))
                         "")
                        (else
                         (if (and is-call
                                  (eq? call-func-name 'FROM-C-catch-all-errors-and-display-backtrace-automatically))
                             (set! is-finished #t))
                         (format p (if (and (integer? (car line))
                                            (string? (car f))
                                            (not (string=? (car f) "*stdout*")))
                                       (values "~%<br><font color='black'>~A:</font>&nbsp;&nbsp;&nbsp;    <pre>~S</pre>~40T;<font color='black'>~A</font>[~A]"
                                               i2
                                               (ra:get-html-from-text (format #f "~A" (car x)))
                                               (car f)
                                               (car line))
                                       (values "~%<br><font color='black'>~A:</font>&nbsp;&nbsp;&nbsp;    <pre>~S</pre>"
                                               i2
                                               (ra:get-html-from-text (format #f "~A" (car x)))))))))
                (format p "~%<br>"))
	     (set! history (cons (car x) history))
	     (set! lines (cons (and (pair? (car x))
                                    (pair-line-number (car x)))
                               lines))
	     (set! files (cons (and (pair? (car x))
                                    (pair-filename (car x)))
                               files)))))
       
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
(arity handle-assertion-failure-during-startup)
(aritable? handle-assertion-failure-during-startup 2)
(aritable? handle-assertion-failure-during-startup 1)
(aritable? handle-assertion-failure-during-startup 0)
(signature handle-assertion-failure-during-startup)
(signature ra:change-audio-connections)
(signature ra:has-audio-connection)
(signature round)
(signature +)
(signature string->symbol)
(signature list)
!!#

(define (safe-display-history-ow!)
  (define bt (safe-history-ow!))
  (when *is-initializing*
    (display (ra:get-text-from-html bt)))
  (safe-display-txt-as-displayable-as-possible bt))

(define (handle-assertion-failure-during-startup info)
  (when *is-initializing*
    ;;(display (ra:get-text-from-html (format #f "~A" info)))(newline)
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
             (if (defined? 'safe-display-ow!)
                 (safe-display-ow!))))
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

(define *overridable-funcs* '())
(define (declare-overridable funcname)
  (push! *overridable-funcs* funcname))

(define (delete el l comp)
  (if (comp el (car l))
      (cdr l)
      (cons (car l)
            (delete el (cdr l) comp))))

(set! (hook-functions *rootlet-redefinition-hook*)
      (list (lambda (hook)
              (define name (hook 'name)) ;;(string->symbol (format #f "~A" (hook 'name))))
              (if (memq name *overridable-funcs*)
                  (delete name *overridable-funcs* eq?)
                  (let ((message (string-append "Warning: Redefining "
                                                (format #f "~A ~A~%" name (hook 'value))
                                                ;;(symbol->string (hook 'symbol))
                                                (if *currently-loading-file*
                                                    (string-append " while loading " *currently-loading-file*)
                                                    "."))))
                    (cond (*is-initializing*                       
                           (when (not (eq? name 'define-class))
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
                           (newline))))))))


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


(define (handle-assertion something throwit)
  (display "assert-non-release: ASSERTION ERROR for \"")(display something) (display "\"")(newline)
  (when throwit
    (handle-assertion-failure-during-startup 'assert-failed) ;; we have a better call to handle-assertion-failure in ***assert***
    (eval `(error ',(list 'assert-failed ": " something)))))
  
(define-expansion (assert something)
  `(if (not ,something)
       (handle-assertion ',something #t)))

;; It is assumed various places that eqv? can be used to compare functions.
(assert (eqv? assert ((lambda () assert))))

(define-expansion (assert-non-release something)
  `(if (not ,something)
       (handle-assertion ',something (not (<ra> :release-mode)))))


#!!
(let ((hepp #f))
  (assert-non-release (and hepp))
  (assert-non-release hepp))
!!#

(define (***assert-custom-comp*** comp? a b)
  (define (test -__Arg1 -__Arg2)
    (define (-__Func1)
      (let ((A -__Arg1))
        (catch #t
               (lambda ()
                 (if (comp? A -__Arg2)
                     (begin
                       (newline)
                       (pretty-print "Correct: ")
                       (if (defined? 'to-displayable-string)
                           (pretty-print (to-displayable-string A))
                           (pretty-print A))
                       (pretty-print "")
                       (newline)
                       #t)
                     (-__Func2 a b)))
               (lambda (error* . args)
                 ;;(c-display "ARGS:" args)
                 (-__Func2 (list (symbol->keyword error*) (apply format (cons #f (car args))) (cdr args))
                           (list a b))))))
    (define (-__Func2 A B)
      (newline)
      (c-display "Wrong. Result:\n  " (pp A))
      (c-display "  Expected:\n  " (pp B))
      (handle-assertion-failure-during-startup (list "***assert*** failed. Result:" A ". Expected:" B))
      (newline)
      (if *is-initializing*
          (error 'assert-failed))
      #f)
    (-__Func1))

  (test a b))
  
(define (***assert*** a b)
  (***assert-custom-comp*** my-equal? a b))
  
(define (***assert-map*** func a b)
  (catch #t
         (lambda ()
           (***assert*** (func a) b))
         (lambda args
           (newline)
           (c-display "Wrong. Result: " (pp a))
           (c-display "  Expected: " (pp b))
           (handle-assertion-failure-during-startup (list "***assert-map*** failed. Result:" a ". Expected:" b))
           'failed)))
    
(define-macro (***assert-error*** a what)
  `(***assert*** (let ((ret #f))
                   (catch #t
                          (lambda ()
                            ,a)
                          (lambda args
                            (display "ret:")(display ret)(newline)
                            (if (eq? ,what (car args))
                                (set! ret #t)
                                (c-display "...expected " ,what ", not " (car args)))
                            ))
                   ret)
                 #t))

        


(define-constant *mylint-load-file-during-startup* (and #t
                                                        (not (ra:release-mode))
                                                        (ra:optimized-build)
                                                        (= (random 5) 0) ;; Takes a little bit time. Don't want to run every time.
                                                        (not (string=? (ra:get-os-name) "windows")) ;; Printing to the console takes a lot of time on windows.
                                                        ))

  
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
                                       (safe-display-txt-as-displayable-as-possible (ra:get-text-from-html (safe-ow!))))
                                      (else
                                       (display (ra:get-text-from-html (my-ow!)))))
                                (handle-assertion-failure-during-startup (string-append "Loading failed for \"" filename "\"."))))))
              (set! *currently-reloading-file* old-reloading)
              (set! *currently-loading-file* old-loading-filename)

              (if (or #f
                      (not *is-initializing*)
                      *mylint-load-file-during-startup*)
                  (mylint-file filename))
        
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
        (ra:schedule 1000
              (lambda ()
                (set! *safe-ow-recursive-level* 0)))
        "safe-ow!-recursion-detected")
      (let ((ret (catch #t
                        (lambda ()
                          (set! *safe-ow-recursive-level* (+ *safe-ow-recursive-level* 1))
                          (my-ow!))
                        (lambda args
                          (safe-display-history-ow!)
                          (get-as-displayable-string-as-possible (list "ow! failed: " args))))))
        (set! *safe-ow-recursive-level* 0)
        ret)))
        

  
(define (safe-display-ow!)
  (define bt (safe-ow!))
  (when *is-initializing*
    (display (ra:get-text-from-html bt))
    (newline))
  (safe-display-txt-as-displayable-as-possible bt))


(require write.scm)


(define-constant go-wrong-finished-called-because-something-went-wrong 0)
(define-constant go-wrong-finished-called-because-it-was-excplicitly-called 1)

(define go-wrong-hooks '())

(assert (defined? 'safe-display-ow!))
(assert (defined? 'safe-ow!))
(set! (hook-functions *error-hook*) 
      (list (lambda (hook)
              (define backtrace-txt (safe-ow!))
              ;;(display backtrace-txt)
              ;;(when *is-initializing*
              ;;  (display (ra:get-text-from-html backtrace-txt)))
              (safe-display-txt-as-displayable-as-possible backtrace-txt)
              (catch #t
                     (lambda ()
                       ;;(display go-wrong-hooks)(newline)
                       (let ((gwhs go-wrong-hooks))
                         (set! go-wrong-hooks '())
                         ;;(display gwhs)(newline)
                         ;;(display go-wrong-hooks)(newline)
                         (for-each (lambda (go-wrong-hook)
                                     (go-wrong-hook))
                                   gwhs)))
                     (lambda args
                       (safe-display-ow!)
                       (get-as-displayable-string-as-possible (list "Custom error hook catch failed:\n"))))
              (handle-assertion-failure-during-startup (list "error-hook failed\n" backtrace-txt)))))

;;(handle-assertion-failure-during-startup "hello")

;; Test startup crash: (crash reporter should show up, and program exit). Startup crashes before this point (i.e. before the error hook is added, are not handled.)
;;(+ a 90)


(load "mylint.scm")

'(set! (hook-functions *load-hook*)
       (list (lambda (hook)
               (mylint-file (hook 'name)))))


(load "semi-primitives.scm")

(c-define-expansion (*<ra>* command . args)
  `( ,(<_> 'ra: (keyword->symbol command)) ,@args))

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



(<declare-variable> show-async-message)
(<declare-variable> popup-menu)
(<declare-variable> get-popup-menu-args)
(<declare-variable> popup-menu-from-args)
(<declare-variable> FROM_C-show-instrument-color-dialog)
(<declare-variable> pmg-start)
(<declare-variable> FROM-C-show-help-window)

(my-require 'gui.scm)

(my-require 'nodes.scm)

;;(my-require 'mouse.scm)

(my-require 'instruments.scm)
(my-require 'solo.scm)

(my-require 'fxrange.scm)

(my-require 'various.scm)

(my-require 'quantitize.scm)

(my-require 'timing.scm) ;; Note that the program assumes that g_scheme_has_inited1 is true after timing.scm has been loaded.

(my-require 'main_menus.scm)

(my-require 'nsm.scm)


(define (FROM-C-assert-that-function-can-be-called-from-C funcname)
  (define sym (string->symbol funcname))
  (when (and (not (memq sym *functions-and-symbols-used-by-C*))
             (not (memq sym *functions-and-symbols-used-by-C-that-are-not-available-at-program-startup*)))
    (define message (string-append "The function \"" funcname "\" has not been added to the list of functions that can be called from C"))
    (display message)(newline)
    (ra:show-error message)))

(define (assert-functions-and-symbols-called-from-C)
  (for-each (lambda (funcname)
              (c-display "CHECKING that" funcname "is defined")
              (if (not (memq funcname *functions-and-symbols-used-by-C-that-are-not-available-at-program-startup*))
                  (assert (defined? funcname))))
            *functions-and-symbols-used-by-C*)
  (display "(assert-functions-and-symbols-called-from-C) finished")
  (newline))
              

;; Called during startup. Will be overridden later.
(define (FROM_C-call-me-when-num-seqtracks-might-have-changed new-num-seqtracks)
  (assert (not (provided? 'mouse.scm))))

;; The files loaded in init-step-2 can use ra: functions.
(define (init-step-2)

  (set! *is-initializing* #t)

  ;; gui.scm can be loaded this late since expansion macros now can be defined after they are used.
  ;; Prev comment: Must be loaded early since it creates the <gui> expansion macro.


  ;; Evaluate before loading main_layout.scm since main_layout creates the edit tab, which uses keybindings.
  (ra:reload-keybindings)

  (my-require 'popupmenu.scm)
  
  (my-require 'main_layout.scm)

  (my-require 'mixer-strips.scm)

  (my-require 'sequencer_upper_part.scm)
  (my-require 'seqtrack-headers.scm)
  (my-require 'seqtracks_config.scm)
  (my-require 'sequencer_right_part.scm)
  (my-require 'seqblock-paint.scm)
  (my-require 'seqblock_audio.scm)
  
  (my-require 'mouse.scm)
  (my-require 'pluginmanager.scm)
  (my-require 'editor_lower_part.scm)
  (my-require 'editor_track_headers.scm)

  (my-require 'mixer.scm)

  (assert-functions-and-symbols-called-from-C)
  (assert-declared-variables-declared)

  (common1-finished-loading)
  
  (set! *is-initializing* #f)
  ;;(set! *all-files-loaded #t)
  )



(set! *is-initializing* #f)




;;(gc #f)

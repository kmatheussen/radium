(provide 'api_autotesting.scm)


(my-require 'api_protos.scm)

#!!
(load "api_protos.scm")
!!#

;; TODO: Keep track of how many times the functions have been tested so that new functions can be tested extensively.


(define blacklisted-api-protos '(ra:init_radium
                                 ra:quit

                                 ra:start-autotesting-mode
                                 ra:stop-autotesting-mode
                                 
                                 ra:eval-scheme
                                 ra:eval-python
                                 
                                 ra:start-ignoring-undo
                                 ra:stop-ignoring-undo
                                 ra:open-undo
                                 ra:close-undo
                                 ra:cancel-last-undo
                                 
                                 ra:test-crashreporter
                                 ra:test-crashreporter-in-audio-thread
                                 ra:show-warning
                                 ra:show-error

                                 ra:open-progress-window
                                 ra:show-progress-window-message
                                 ra:close-progress-window

                                 ra:obtain-keyboard-focus
                                 ra:release-keyboard-focus

                                 ra:open-requester
                                 ra:close-requester
                                 ra:safe-to-call-close-requester
                                 ra:request-integer
                                 ra:request-float
                                 ra:request-string

                                 ra:gui_disable-updates
                                 ra:gui_enable-updates

                                 ra:save-block
                                 ra:load-block
                                 ra:save-track
                                 ra:load-track
                                 ra:save-soundfile
                                 ra:save
                                 ra:save-as
                                 ra:save-with-embedded-samples
                                 ra:load
                                 ra:load-song
                                 ra:import-midi
                                 ra:request-import-mod
                                 ra:import-xm
                                 ra:save-instrument-preset
                                 ra:open-file-for-reading
                                 ra:open-file-for-writing
                                 ra:close-file
                                 ra:write-to-file
                                 ra:file-at-end
                                 ra:read-line-from-file
                                 ra:open-file-for-binary-reading
                                 ra:read-le32-from-file
                                 ra:read-le-u32-from-file
                                 ra:read-be-u32-from-file
                                 ra:read-le16-from-file
                                 ra:read-be-u16-from-file
                                 ra:read8-from-file
                                 ra:read-u8-from-file                                 

                                 ;; These are actually internal functions
                                 ra:add-menu-menu
                                 ra:go-previous-menu-level
                                 ra:add-menu-item
                                 ra:add-checkable-menu-item
                                 ra:add-menu-separator

                                 ;; various
                                 ra:get-conf-path
                                 ra:msleep
                                 ra:set-faust-gui-style
                                 ra:set-modal-windows

                                 ;; gui
                                 ra:gui_set-parent ;; Sometimes makes the program stall for a very long time.
                                 ra:gui_set-modal
                                 ra:gui_set-as-window
                                 ra:gui_set-full-screen
                                 ra:gui_set-enabled
                                 ))

(define (chances . rest)
  (let loop ((rest rest))
    (if (null? (cdr rest))
        (car rest)
        (let ((chance (car rest))
              (value (cadr rest)))
          (if (<= (myrand 0 1) chance)
              value
              (loop (cddr rest)))))))

(define-constant *radium-test-log-filename* "/tmp/radiumtest.log")

(define (add-to-radium-test-log tekst)
  (c-display (<-> "adding -" tekst "- to log."))
  (system (<-> "echo \"" tekst "\" >> " *radium-test-log-filename*)))  

(define (get-random-api-args args)
  (map (lambda (arg)
         (define type (car arg))
         (define varname (cadr arg))
         (define default (cl-caddr arg))
         (define use-default (and default (< (integer-myrand 0 9)
                                             9)))
         (if use-default
             (cond ((eq? default 'false)
                    #f)
                   ((eq? default 'true)
                    #t)
                   ((eq? default 'g_uninitialized_dyn)
                    #<unspecified>)
                   ((eq? default 'g_dyn_minus_one)
                    -1)
                   (else
                    default))
             (cond ((eq? type 'const_char*)
                    "teststring")
                   
                   ((or (eq? type 'float)
                        (eq? type 'double))
                    (myrand -1 20))
                   
                   ((eq? varname 'instrument_id)
                    (assert (eq? type 'int64_t))
                    (chances 0.9 (<ra> :get-audio-instrument-id (integer-myrand 0 (1- (<ra> :get-num-audio-instruments))))
                             (integer-myrand -10 100)))
                   
                   ((eq? varname 'windownum)
                    (assert (eq? type 'int))
                    (chances 0.98 -1
                             0.5 -2
                             0.5 1134513))
                   
                   ((eq? varname 'blocknum)
                    (assert (eq? type 'int))
                    (chances 0.9 -1
                             0.4 (integer-myrand 0 (1- (<ra> :get-num-blocks)))
                             (integer-myrand -10 100)))
                   
                   ((eq? varname 'tracknum)
                    (assert (eq? type 'int))
                    -1)

                   ((and (eq? varname 'note)
                         (eq? type 'dyn_t))
                    (chances 0.9 (integer-myrand 0 (1- (<ra> :get-num-notes)))
                             0.3 #<unspecified>
                             0.3 -1
                             0.3 1345631456
                             "asdfas"))
                     
                   ((eq? varname 'seqtracknum)
                    (assert (eq? type 'int))
                    (chances 0.8 0
                             0.5 (integer-myrand 0 (1- (<ra> :get-num-seqtracks)))
                             (integer-myrand -100 1000)))

                   ((eq? varname 'automationnum)
                    (assert (eq? type 'int))
                    (chances 0.9 (integer-myrand 0 (1- (<ra> :get-num-seq-automations 0)))
                             (integer-myrand -1000 1000)))

                   ((eq? varname 'nodenum)
                    (assert (eq? type 'int))
                    (chances 0.9 (integer-myrand 0 3)
                             (integer-myrand -1000 1000)))

                   ((eq? varname 'time)
                    (assert (eq? type 'int64_t))
                    (chances 0.9 (integer-myrand 0 100000)
                             (integer-myrand -100000 1000000)))

                   ((eq? varname 'abstime)
                    (assert (or (eq? type 'double)  ;; Seems like the format is frames, and not seconds, even when it takes double as argument.
                                (eq? type 'int64_t)))
                    (chances 0.9 (myrand 0 (* 48000 200))
                             (myrand (* 48000 -100) (* 48000 400))))

                   ((eq? varname 'logtype)
                    (assert (eq? type 'int))
                    (chances 0.45 (<ra> :get-logtype-hold)
                             0.90 (<ra> :get-logtype-linear)
                             (integer-myrand -1000 1000)))
                                                 
                   ((or (eq? varname 'guinum)
                        (eq? varname 'parentgui))
                    (assert (eq? type 'int64_t))
                    (chances 0.9 (<gui> :random)
                             0.5 -1
                             134513451345))
                   
                   ((or (eq? type 'int)
                        (eq? type 'int64_t))
                    (integer-myrand -1 20))
                   
                   ((eq? type 'Place)
                    (chances 0.9 (+ (integer-myrand 0 (<ra> :get-num-lines))
                                    (/ (integer-myrand 0 1)
                                       2))
                             0.5 -1
                             0.5 1235134
                             0.5 (/ 1235133 2)
                             -2))
                   
                   ((eq? type 'bool)
                    (= 0 (integer-myrand 0 1)))
                   
                   ((eq? type 'func_t*)
                    (lambda x
                      (c-display "api test callback called with arguments " x)
                      #f))
                   
                   ((eq? type 'dyn_t)
                    (integer-myrand -1 100))
                   
                   (else
                    (c-display "TYPE" type)
                    (assert #f)))))
       args))
                        
                

(define test-funcs (map (lambda (api-proto)
                          (c-display api-proto)
                          (define return-type (car api-proto))
                          (define funcname (cadr api-proto))
                          (define func (eval funcname))
                          (define args (cddr api-proto))
                          (lambda (gui num)
                            (c-display "  -Test- (about to call " funcname ")")
                            (define funcargs (get-random-api-args args))
                            (c-display "  -Test- API_TEST. Calling \"" funcname "\" with arguments" funcargs)
                            (<gui> :set-value gui (<-> (<gui> :get-value gui) "<br>\n" num ": Calling \"" funcname "\" with arguments" funcargs))
                            (c-display "  -Test- (finished setting gui value")
                            (add-to-radium-test-log (<-> "("
                                                         funcname
                                                         (apply string-append (map (lambda (funcarg)
                                                                                     (<-> " "
                                                                                          (cond ((string? funcarg)
                                                                                                 (apply <-> (map (lambda (char)
                                                                                                                   (if (char=? #\" char)
                                                                                                                       "\\\""
                                                                                                                       char))
                                                                                                                 funcarg)))
                                                                                                ((procedure? funcarg)
                                                                                                 (with-output-to-string
                                                                                                   (lambda ()
                                                                                                     (display funcarg))))
                                                                                                (else
                                                                                                 (to-displayable-string funcarg)))))
                                                                                   funcargs))
                                                         ")"))
                            (try-finally (lambda ()
                                           (apply func funcargs)))))
                        (remove (lambda (api-proto)
                                  (memq (cadr api-proto) blacklisted-api-protos))
                                ra:api-protos)))


(let ((num-funcs (length test-funcs)))
  (define gui (<gui> :text-edit "" #t))
  (<gui> :show gui)
  (<gui> :set-size gui 1000 400)
  ;;(<gui> :move-to-centre-of gui -1)
  (<gui> :raise gui)
  (add-to-radium-test-log "")
  (add-to-radium-test-log "")
  (add-to-radium-test-log "")
  (<ra> :start-autotesting-mode)
  (let loop ((i 100))
    (if (<= i 0)
        (<ra> :stop-autotesting-mode)
        (begin
          (define func (test-funcs (integer-myrand 0 (1- num-funcs))))
          (func gui (1- i))
          (try-finally (lambda ()
                         (c-display "   -Test- (calling undo 1)")
                         (<ra> :undo)
                         (c-display "   -Test- (calling undo 2)")
                         (<ra> :undo)
                         (c-display "   -Test- (calling redo 1)")
                         (<ra> :redo)
                         (c-display "   -Test- (calling redo 2)")
                         (<ra> :redo)))
          (<ra> :schedule 100
                (lambda ()
                  (loop (1- i))
                  #f))))))


;;(ra:add-pianonote-pitch 927529/50000 17 2 -1 -1 -1)


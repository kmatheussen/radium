(provide 'api_autotesting.scm)


(my-require 'api_protos.scm)

#!!
(load "api_protos.scm")
!!#


(define blacklisted-api-protos '(ra:init_radium
                                 ra:quit

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

                                 ra:get-conf-path

                                 ra:set-modal-windows
                                 
                                 ra:gui_set-parent ;; Sometimes makes the program stall for a very long time.
                                 ra:gui_set-modal
                                 ra:gui_set-as-window
                                 ra:gui_set-full-screen
                                 ra:gui_set-enabled
                                 ))
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
                   ((or (eq? type 'int)
                        (eq? type 'int64_t))
                    (integer-myrand -1 20))
                   ((eq? type 'Place)
                    (integer-myrand 0 63))
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
                          (lambda (gui)
                            (define funcargs (get-random-api-args args))
                            (<gui> :set-value gui (<-> (<gui> :get-value gui) "<br>\n" "Calling \"" funcname "\" with arguments" funcargs))
                            (c-display "  API_TEST. Calling \"" funcname "\" with arguments" funcargs)
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
  (let loop ((i 100))
    (when (> i 0)
      (define func (test-funcs (integer-myrand 0 (1- num-funcs))))
      (func gui)
      (<ra> :schedule 100
            (lambda ()
              (loop (1- i))
              #f)))))



(provide 'sequencer.scm)

(define (for-each-seqtracknum func)
  (let loop ((seqtracknum 0))
    (when (< seqtracknum (<ra> :get-num-seqtracks))
      (func seqtracknum)
      (loop (1+ seqtracknum)))))

  
(define (for-each-seqblocknum func)
  (define (for-each-seqblocknum2 seqtracknum func)
    (let loop ((seqblocknum 0))
      (when (< seqblocknum (<ra> :get-num-seqblocks seqtracknum))
        (func seqblocknum)
        (loop (1+ seqblocknum)))))
  
  (call-with-exit
   (lambda (return)
     (for-each-seqtracknum
      (lambda (seqtracknum)
        (for-each-seqblocknum2
         seqtracknum
         (lambda (seqblocknum)
           (define ret (func seqtracknum seqblocknum))
           (if (and (pair? ret) (pair? (cdr ret)) (eq? 'stop (car ret)) (null? (cddr ret)))
               (return (cadr ret)))))))
     (return #t))))

(define (map-all-seqblocks func)
  (let loop ((seqblocknum 0)
             (seqtracknum 0))
    (cond ((= seqtracknum (<ra> :get-num-seqtracks))
           '())
          ((= seqblocknum (<ra> :get-num-seqblocks seqtracknum)) ;; use-gfx))
           (loop 0 (1+ seqtracknum)))
          (else
           (cons (func seqtracknum seqblocknum)
                 (loop (1+ seqblocknum) seqtracknum))))))               

(define (for-each-selected-seqblock func)
  (for-each-seqblocknum (lambda (seqtracknum seqblocknum)
                          (when (<ra> :is-seqblock-selected seqblocknum seqtracknum)
                            ;;(c-display "funcing" seqtracknum seqblocknum)
                            (func seqtracknum seqblocknum)))))
  


;; see enum SeqtrackHeightType in nsmtracker.h
(define (get-seqtrack-height-type boxname)
  (cond ((eq? boxname 'custom) 0)
        ((eq? boxname '1-row) 1)
        ((eq? boxname '2-rows) 2)
        ((eq? boxname '3-rows) 3)
        ((eq? boxname 'unlimited) 4)
        (else
         (c-display "************** boxname:" boxname)
         (assert #f))))

(define (get-select-seqtrack-size-type-gui seqtracknum is-min gotit-callback)
  (define getter (if is-min ra:get-seqtrack-min-height-type ra:get-seqtrack-max-height-type))
  (define setter (if is-min ra:set-seqtrack-min-height-type ra:set-seqtrack-max-height-type))
  (define has-started #f)
  
  (define gui (<gui> :vertical-layout))
  
  (define (gotit type)    
    (when has-started
      (if #t
          (begin
            (setter seqtracknum type)
            (if gotit-callback
                (gotit-callback)
                (show-select-both-seqtrack-size-types-gui seqtracknum)))
          (<ra> :schedule 30
                (lambda ()
                  (eat-errors :try (lambda ()
                                      (setter seqtracknum type)
                                      (if gotit-callback
                                          (gotit-callback)))
                               :finally (lambda ()
                                          (<ra> :schedule 100
                                                (lambda ()
                                                  ;;(<gui> :close gui)
                                                  #f))))
                  #f)))))

  (define curr-min-type (<ra> :get-seqtrack-min-height-type seqtracknum))
  (define curr-max-type (<ra> :get-seqtrack-max-height-type seqtracknum))
  (define custom-type (get-seqtrack-height-type 'custom))
  (define unlimited-type (get-seqtrack-height-type 'unlimited))
    
  (for-each (lambda (name text)
              (define type (get-seqtrack-height-type
                            (if (or (eq? 'unlimited1 name)
                                    (eq? 'unlimited2 name))
                                'unlimited
                                name)))
              (set! text (cond ((eq? 'unlimited1 name)
                                "1/3 row")
                               ((eq? 'unlimited2 name)
                                "Unlimited")
                               (else
                                text)))
              (define is-disabled (or (and (not is-min)
                                           (eq? 'unlimited1 name))
                                      (and is-min
                                           (eq? 'unlimited2 name))))
              (if (and (not is-disabled)
                       (not (= type custom-type)))
                  (if is-min
                      (if (and (not (= unlimited-type type))
                               (not (= curr-max-type custom-type))
                               (> type curr-max-type))
                          (set! is-disabled #t))
                      (if (and (not (= curr-min-type unlimited-type))
                               (not (= curr-min-type custom-type))
                               (< type curr-min-type))
                          (set! is-disabled #t))))
              
              (define button (<gui> :radiobutton text (and (not is-disabled)
                                                           (= type (getter seqtracknum)))
                                    (lambda (val)
                                      (if val
                                          (gotit type)))))
              (<gui> :add gui button)
                
              (if is-disabled
                  (<gui> :set-enabled button #f)))
            '(unlimited1
              1-row
              2-rows
              3-rows
              ;;custom
              ;;unlimited2
              )
            '("Unlimited"
              "1 row"
              "2 rows"
              "3 rows"
              ;;"Current size"
              ;;"Unlimited"
              ))
  
  (set! has-started #t)

  gui)
  

(define *seqtrack-size-gui-uses-popup #f)

(define *seqtrack-size-gui-seqtracknum* -1)
(define (seqtrack-size-gui-open? seqtracknum)
  (= seqtracknum *seqtrack-size-gui-seqtracknum*))

(define (show-seqtrack-height-gui seqtracknum use-popup)

  (set! *seqtrack-size-gui-seqtracknum* seqtracknum)
  
  (define gui #f)
  
  (if (and (not use-popup)
           (not *seqtrack-size-gui-uses-popup))

      (begin
        (show-select-both-seqtrack-size-types-gui seqtracknum)
        ;;(<gui> :set-pos *curr-seqtrack-size-type-gui* (floor (<ra> :get-global-mouse-pointer-x)) (floor (<ra> :get-global-mouse-pointer-y)))
        (set! gui *curr-seqtrack-size-type-gui*)
        )
        
      (begin

        (set! gui (<gui> :popup))
        
        (define (gotit-callback)
          (<gui> :close gui)
          )
        
        (<gui> :add gui (get-select-seqtrack-size-type-gui seqtracknum #t gotit-callback))
        
        ;;(<gui> :set-modal gui #t)
        ;;(<gui> :set-pos gui (floor (<ra> :get-global-mouse-pointer-x)) (floor (<ra> :get-global-mouse-pointer-y)))
        
        ;;(<ra> :schedule 0 ;;100 ;; Add some delay to avoid mouse click not working the first time after closing the popup menu. (don't know what's happening)
          ;;    (lambda ()
                (<gui> :show gui)
                ;;    #f)))))
                ))

  (<gui> :add-deleted-callback gui
         (lambda (radium-runs-custom-exec)
           (c-display "DELETED")
           (set! *seqtrack-size-gui-seqtracknum* -1)
           (*sequencer-left-part-area* :update-me!)
           )))
        



(define *curr-seqtrack-size-type-gui* #f) ;; only show one at a time.
(define *curr-seqtrack-size-type-content* #f)

(define (show-select-both-seqtrack-size-types-gui seqtracknum)
  (set! *seqtrack-size-gui-seqtracknum* seqtracknum)
  (define min-gui (get-select-seqtrack-size-type-gui seqtracknum #t #f))
  (define max-gui (get-select-seqtrack-size-type-gui seqtracknum #f #f))
  (define header-text (<-> "               Seqtrack height for \"" (<ra> :get-seqtrack-name seqtracknum) "\" (#" seqtracknum ")               "))
  (define content (<gui> :vertical-layout
                         ;;(mid-horizontal-layout (<gui> :text header-text))
                         ;;(<gui> :horizontal-layout)
                         (if #t 
                             (<gui> :group header-text
                                    min-gui)
                             (<gui> :horizontal-layout
                                    (<gui> :group "Minimium size"
                                           min-gui)
                                    (<gui> :group "Maximum size"
                                           max-gui)))
                         ;;(<gui> :horizontal-layout)
                         ;;(mid-horizontal-layout (<gui> :text (<-> "(Note that this GUI operates on current seqtrack)")))
                         ))

  
  (<gui> :set-layout-spacing content 5 2 2 2 2)
  
  (<gui> :add content (<gui> :button "Close"
                             (lambda ()
                               (when *curr-seqtrack-size-type-gui*
                                 (<gui> :hide *curr-seqtrack-size-type-gui*)
                                 (set! *seqtrack-size-gui-seqtracknum* -1)))))

  (if (or (not *curr-seqtrack-size-type-gui*)
          (not (<gui> :is-open *curr-seqtrack-size-type-gui*)))
      (begin
        (set! *curr-seqtrack-size-type-gui* (<gui> :vertical-layout))
        (<gui> :set-window-title *curr-seqtrack-size-type-gui* "Seqtrack height limits")
        (<gui> :add *curr-seqtrack-size-type-gui* content)
        (<gui> :set-parent *curr-seqtrack-size-type-gui* (<gui> :get-sequencer-gui)))
      (begin
        (<gui> :replace *curr-seqtrack-size-type-gui* *curr-seqtrack-size-type-content* content)
        (<gui> :close *curr-seqtrack-size-type-content*)))

  (set! *curr-seqtrack-size-type-content* content)
  (<gui> :show *curr-seqtrack-size-type-gui*))


#!!
(show-select-both-seqtrack-size-types-gui 1)
!!#
                     
(define (select-seqtrack-size-type seqtracknum is-min)
  (define gui (get-select-seqtrack-size-type-gui seqtracknum is-min))
  (<gui> :set-parent gui (<gui> :get-sequencer-gui))
  (<gui> :show gui))
  

#!!
(select-seqtrack-size-type 0 #t)
!!#

(define (set-min-seqtrack-size seqtracknum)
  (select-seqtrack-size-type seqtracknum #t))

(define (set-max-seqtrack-size seqtracknum)
  (select-seqtrack-size-type seqtracknum #f))

(define (FROM_C-call-me-when-curr-seqtrack-has-changed seqtracknum)
  (if (and *curr-seqtrack-size-type-gui*
           (<gui> :is-open *curr-seqtrack-size-type-gui*)
           (<gui> :is-visible *curr-seqtrack-size-type-gui*))
      (show-select-both-seqtrack-size-types-gui seqtracknum)))

(define (get-nonstretched-seqblock-duration seqblocknum seqtracknum)
  (- (<ra> :get-seqblock-interior-end seqblocknum seqtracknum)
     (<ra> :get-seqblock-interior-start seqblocknum seqtracknum)))
      


;; see enum SeqblockBoxSelected in nsmtracker.h
(define (get-selected-box-num boxname)
  (cond ((eq? boxname 'non) 0)
        ((eq? boxname 'fade-left) 1)
        ((eq? boxname 'fade-right) 2)
        ((eq? boxname 'interior-left) 3)
        ((eq? boxname 'interior-right) 4)
        ((eq? boxname 'speed-left) 5)
        ((eq? boxname 'speed-right) 6)
        ((eq? boxname 'stretch-left) 7)
        ((eq? boxname 'stretch-right) 8)
        (else
         (c-display "************** boxname:" boxname)
         (assert #f))))
        


(define (get-interior-displayable-string value)
  (if (= value 0)
      "0.00s"
      (let ((seconds (/ value
                        (<ra> :get-sample-rate))))
        (if (< seconds 0.01)
            (let* ((ms (* 1000 seconds))                         
                   (sms (two-decimal-string ms)))
              (if (string=? sms "0.00")
                  "0.01ms"
                  (<-> sms "ms")))
            (<-> (two-decimal-string seconds) "s")))))

(define (get-left-interior-string2 value)
  (<-> "----|: " (get-interior-displayable-string value)))

(define (get-left-interior-string seqblocknum seqtracknum)
  (get-left-interior-string2 (<ra> :get-seqblock-interior-start seqblocknum seqtracknum #t)))

(define (left-interior-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-interior-start seqblocknum seqtracknum #t)))
    (not (= value 0.0))))

(define (set-left-interior-status-bar2 seqblocknum seqtracknum value)
  (if (and seqblocknum seqtracknum)
      (set-seqblock-selected-box 'interior-left seqblocknum seqtracknum))
  (set-editor-statusbar (get-left-interior-string2 value)))

(define (set-left-interior-status-bar seqblocknum seqtracknum)
  (set-left-interior-status-bar2 seqblocknum seqtracknum (<ra> :get-seqblock-interior-start seqblocknum seqtracknum #t)))

(define (get-right-interior-string2 seqblocknum seqtracknum right-interior-value)
  (<-> "|----: " (get-interior-displayable-string (- (<ra> :get-seqblock-default-duration seqblocknum seqtracknum)
                                                     right-interior-value))))

(define (get-right-interior-string seqblocknum seqtracknum)
  (get-right-interior-string2 seqblocknum seqtracknum (<ra> :get-seqblock-interior-end seqblocknum seqtracknum #t)))

#!!
(list :original-duration (<ra> :get-seqblock-default-duration 0 1)
      :resample-ratio (<ra> :get-seqblock-resample-ratio (<ra> :get-seqblock-id 0 1))
      :test (* (<ra> :get-sample-length (<ra> :get-seqblock-sample 0 1))
               (<ra> :get-seqblock-resample-ratio (<ra> :get-seqblock-id 0 1)))
      :test2 (* (<ra> :get-sample-length (<ra> :get-seqblock-sample 0 1))
                (/ 44100
                   96000.0))
      :interior-end (<ra> :get-seqblock-interior-end 0 1)
      :stretch-speed (<ra> :get-seqblock-stretch-speed (<ra> :get-seqblock-id 0 1))
      :sample-length (<ra> :get-sample-length (<ra> :get-seqblock-sample 0 1)))
!!#

(define (right-interior-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-interior-end seqblocknum seqtracknum #t)))
    (not (= value (<ra> :get-seqblock-default-duration seqblocknum seqtracknum)))))
  
(define (set-right-interior-status-bar2 seqblocknum seqtracknum right-interior-value)
  (set-seqblock-selected-box 'interior-right seqblocknum seqtracknum)
  (set-editor-statusbar (get-right-interior-string2 seqblocknum seqtracknum right-interior-value)))

(define (set-right-interior-status-bar seqblocknum seqtracknum)
  (set-right-interior-status-bar2 seqblocknum seqtracknum (<ra> :get-seqblock-interior-end seqblocknum seqtracknum #t)))

(define (get-speed-string2 value)
  (<-> "Speed: " (two-decimal-string (/ 1.0 value))))

(define (get-speed-string seqblockid)
  (get-speed-string2 (<ra> :get-seqblock-speed seqblockid #t)))

(define (speed-touched? seqblockid)
  (let ((speed (<ra> :get-seqblock-speed seqblockid #t)))
    (not (= speed 1.0))))

(define (get-stretch-string2 value)
  (<-> "Stretch: " (two-decimal-string value)))

(define (get-stretch-string seqblockid)
  (get-stretch-string2 (<ra> :get-seqblock-stretch seqblockid #t)))

(define (stretch-touched? seqblockid)
  (let ((stretch (<ra> :get-seqblock-stretch seqblockid #t)))
    (not (= stretch 1.0))))

(define (get-fade-string value seqblocknum seqtracknum)
  (<-> (if (= value 0)
           "0.00"
           (let* ((ms (* 1000
                         (/ (* value (- (<ra> :get-seqblock-end-time seqblocknum seqtracknum #t)
                                        (<ra> :get-seqblock-start-time seqblocknum seqtracknum #t)))
                            (<ra> :get-sample-rate))))
                  (sms (two-decimal-string ms)))
             (if (string=? sms "0.00")
                 "0.01"
                 sms)))
       "ms"))

(define (get-fade-string-left2 value seqblocknum seqtracknum)
  (<-> "Fade in: " (get-fade-string value seqblocknum seqtracknum)))

(define (get-fade-string-left seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-in seqblocknum seqtracknum)))
    (get-fade-string-left2 value seqblocknum seqtracknum)))

(define (fade-left-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-in seqblocknum seqtracknum)))
    (not (= value 0))))

(define (get-fade-string-right2 value seqblocknum seqtracknum)
  (<-> "Fade out: " (get-fade-string value seqblocknum seqtracknum)))

(define (get-fade-string-right seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-out seqblocknum seqtracknum)))
    (get-fade-string-right2 value seqblocknum seqtracknum)))

(define (fade-right-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-out seqblocknum seqtracknum)))
    (not (= value 0))))

(define (set-fade-status-bar is-left seqblocknum seqtracknum)
  (if is-left
      (begin
        (set-seqblock-selected-box 'fade-left seqblocknum seqtracknum)
        (set-editor-statusbar (get-fade-string-left seqblocknum seqtracknum)))
      (begin
        (set-seqblock-selected-box 'fade-right seqblocknum seqtracknum)
        (set-editor-statusbar (get-fade-string-right seqblocknum seqtracknum)))))

(define (get-seqtrack-popup-menu-entries seqtracknum)
  (list
   (list "Swap with next seqtrack"
         :enabled (< seqtracknum (- (<ra> :get-num-seqtracks) 1))
         (lambda ()
           (define (swapit)
             (<ra> :undo-sequencer)
             (<ra> :swap-seqtracks seqtracknum (1+ seqtracknum)))
           (if (and (= 0 seqtracknum)
                    (<ra> :seqtrack-for-audiofiles 1))
               (ask-user-about-first-audio-seqtrack
                (lambda (doit)
                  (if doit
                      (swapit))))
               (swapit))))
   (list "Set height"
         (lambda ()
           (show-select-both-seqtrack-size-types-gui seqtracknum)))))


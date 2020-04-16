
(provide 'sequencer.scm)

(define *sequencer-left-part-area* #f)
(define *sequencer-right-part-area* #f)

(define *sequencer-window-gui* (if (defined? '*sequencer-window-gui*)
                                   (begin
                                     (<declare-variable> *sequencer-window-gui*)
                                     *sequencer-window-gui*)
                                   #f))

(define *sequencer-window-gui-active* (if (defined? '*sequencer-window-gui-active*)
                                          (begin
                                            (<declare-variable> *sequencer-window-gui-active*)
                                            *sequencer-window-gui-active*)
                                          #f))

(define2 *current-seqtrack-num* (curry-or not integer?) #f)


(define (find-first-visible-seqtrack)
  (let loop ((seqtracknum 0))
    (cond ((= seqtracknum (<ra> :get-num-seqtracks))
           0)
          ((<ra> :get-seqtrack-visible seqtracknum)
           seqtracknum)
          (else
           (loop (+ 1 seqtracknum))))))
        
(define (find-last-visible-seqtrack)
  (let loop ((seqtracknum (- (<ra> :get-num-seqtracks) 1)))
    (cond ((= seqtracknum 0)
           0)
          ((<ra> :get-seqtrack-visible seqtracknum)
           seqtracknum)
          (else
           (loop (- seqtracknum 1))))))

#!!
(find-first-visible-seqtrack)
(find-last-visible-seqtrack)
!!#

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
  

(define (move-seqblock! seqblockid new-start-time)
  (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblockid))
  (define seqblocks (to-list (<ra> :get-seqblocks-state seqtracknum)))
  (define new-seqblocks
    (let loop ((seqblocks seqblocks))
      (if (null? seqblocks)
          '()
          (let ((seqblock (car seqblocks)))
            (if (= (seqblock :id) seqblockid)
                (let* ((start-time (seqblock :start-time))
                       (diff (- new-start-time start-time)))
                  (cons (copy-hash seqblock
                                   :start-time new-start-time
                                   :end-time (+ (seqblock :end-time) diff))
                        (cdr seqblocks)))
                (cons seqblock
                      (loop (cdr seqblocks))))))))
  (try-finally
   :try (lambda ()
          (<ra> :create-gfx-seqblocks-from-state new-seqblocks seqtracknum)
          (<ra> :undo-sequencer)
          (<ra> :apply-gfx-seqblocks seqtracknum))
   :failure (lambda ()
              (<ra> :cancel-gfx-seqblocks seqtracknum))))

(define (swap-seqblock-with-next! id1)
  (define seqtracknum (<ra> :get-seqblock-seqtrack-num id1))
  (define seqblocks (to-list (<ra> :get-seqblocks-state seqtracknum)))
  (define new-seqblocks
    (let loop ((seqblocks seqblocks))
      (if (or (null? seqblocks)
              (null? (cdr seqblocks)))
          seqblocks
          (let ((seqblock1 (car seqblocks)))
            (if (= (seqblock1 :id) id1)
                (let* ((seqblock2 (cadr seqblocks))
                       (start1 (seqblock1 :start-time))
                       (end1 (seqblock1 :end-time))
                       (start2 (seqblock2 :start-time))
                       (end2 (seqblock2 :end-time)))
                  (cons (copy-hash seqblock2
                                   :start-time start1
                                   :end-time (+ start1 (- end2 start2)))
                        (cons (copy-hash seqblock1
                                         :start-time start2
                                         :end-time (+ start2 (- end1 start1)))
                              (cddr seqblocks))))
                (cons seqblock1
                      (loop (cdr seqblocks))))))))
  (try-finally
   :try (lambda ()
          (<ra> :create-gfx-seqblocks-from-state new-seqblocks seqtracknum)
          (<ra> :undo-sequencer)
          (<ra> :apply-gfx-seqblocks seqtracknum))
   :failure (lambda ()
              (<ra> :cancel-gfx-seqblocks seqtracknum))))

(define (FROM-C-generate-new-color-for-all-selected-seqblocks mix-background)
  (define color (<ra> :generate-new-block-color mix-background))
  
  (define (generate seqblockid seqtracknum seqblocknum)
    (if (<ra> :seqblock-holds-sample seqblocknum seqtracknum)
        (<ra> :set-audiofile-color color (<ra> :get-seqblock-sample seqblocknum seqtracknum))
        (<ra> :set-block-color color (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))))
  
  (undo-block (lambda ()
                (if (> (<ra> :get-num-selected-seqblocks) 0)
                    (for-each-selected-seqblock (lambda (seqtracknum seqblocknum)
                                                  (generate (<ra> :get-seqblock-id seqblocknum seqtracknum) seqtracknum seqblocknum)))
                    (let ((seqblockid (<ra> :get-curr-seqblock-id)))
                      (if (>= seqblockid 0)
                          (generate seqblockid
                                    (<ra> :get-seqblock-seqtrack-num seqblockid)
                                    (<ra> :get-seqblock-seqblock-num seqblockid))))))))

(define (insert-pause-in-seqtrack! seqtracknum pos duration)
  (define seqblocks (to-list (<ra> :get-seqblocks-state seqtracknum)))
  (define new-seqblocks
    (let loop ((seqblocks seqblocks))
      (if (null? seqblocks)
          seqblocks
          (let ((seqblock (car seqblocks)))            
            (cons (if (>= (seqblock :start-time) pos)
                      (copy-hash seqblock
                                 :start-time (+ (seqblock :start-time) duration)
                                 :end-time (+ (seqblock :end-time) duration))
                      seqblock)
                  (loop (cdr seqblocks)))))))
  (try-finally
   :try (lambda ()
          (<ra> :create-gfx-seqblocks-from-state new-seqblocks seqtracknum)
          (<ra> :undo-sequencer)
          (<ra> :apply-gfx-seqblocks seqtracknum))
   :failure (lambda ()
              (<ra> :cancel-gfx-seqblocks seqtracknum))))
  
         

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
  (define gui (get-select-seqtrack-size-type-gui seqtracknum is-min #f))
  (<gui> :set-parent gui (<gui> :get-sequencer-gui))
  (<gui> :show gui))
  

#!!
(select-seqtrack-size-type 0 #t)
!!#

(define (set-min-seqtrack-size seqtracknum)
  (select-seqtrack-size-type seqtracknum #t))

(define (set-max-seqtrack-size seqtracknum)
  (select-seqtrack-size-type seqtracknum #f))

(define *block-and-playlist-area* #f)

(define (FROM_C-call-me-when-curr-seqtrack-has-changed seqtracknum)
  (if (and *curr-seqtrack-size-type-gui*
           (<gui> :is-open *curr-seqtrack-size-type-gui*)
           (<gui> :is-visible *curr-seqtrack-size-type-gui*))
      (show-select-both-seqtrack-size-types-gui seqtracknum))
  (if *block-and-playlist-area*
      (*block-and-playlist-area* :recreate)))

(define (get-nonstretched-seqblock-duration seqblocknum seqtracknum)
  (- (<ra> :get-seqblock-interior-end seqblocknum seqtracknum)
     (<ra> :get-seqblock-interior-start seqblocknum seqtracknum)))
      

(define *old-selected-box-seqblocknum* -1)
(define *old-selected-box-seqtracknum* -1)
(define (set-seqblock-selected-box which-one seqblocknum seqtracknum)
  ;;(c-display "   setting " which-one seqblocknum seqtracknum " old: " *old-selected-box-seqblocknum* *old-selected-box-seqtracknum*)
  (when (and #t ;;#f
             (or (not (= seqblocknum *old-selected-box-seqblocknum*))
                 (not (= seqtracknum *old-selected-box-seqtracknum*)))
             (>= *old-selected-box-seqtracknum* 0)
             (< *old-selected-box-seqtracknum* (<ra> :get-num-seqtracks))
             (>= *old-selected-box-seqblocknum* 0)
             (< *old-selected-box-seqblocknum* (<ra> :get-num-seqblocks *old-selected-box-seqtracknum*)))
    ;;(c-display (history-ow!))
    ;;(c-display "UNSETTING ")
    (<ra> :set-seqblock-selected-box (get-selected-box-num 'non) *old-selected-box-seqblocknum* *old-selected-box-seqtracknum*))
  
  (set! *old-selected-box-seqblocknum* seqblocknum)
  (set! *old-selected-box-seqtracknum* seqtracknum)

  (when (>= seqblocknum 0)
    ;;(if (eq? which-one 'non)
    ;;    (c-display "UNSETTING2 " which-one)
    ;;    (c-display "SETTING2 " which-one))
    (<ra> :set-seqblock-selected-box (get-selected-box-num which-one) seqblocknum seqtracknum)))


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
  (<declare-variable> set-custom-seq-indicator)
  (set-custom-seq-indicator (<ra> :get-seqblock-start-time seqblocknum seqtracknum #t)
                            -1
                            "sequencer_block_interior_box_color")
  (set-editor-statusbar (get-left-interior-string2 value)))

(define (set-left-interior-status-bar seqblocknum seqtracknum)
  (define value (<ra> :get-seqblock-interior-start seqblocknum seqtracknum #t))
  ;;(c-display "gakk:" (<ra> :get-seqblock-start-time seqblocknum seqtracknum #t) value)
  (set-seqblock-selected-box 'interior-left seqblocknum seqtracknum)
  (set-left-interior-status-bar2 seqblocknum seqtracknum value))

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
  (<declare-variable> set-custom-seq-indicator)
  (set-custom-seq-indicator (<ra> :get-seqblock-end-time seqblocknum seqtracknum #t)
                            -1
                            "sequencer_block_interior_box_color")
  (set-editor-statusbar (get-right-interior-string2 seqblocknum seqtracknum right-interior-value)))

(define (set-right-interior-status-bar seqblocknum seqtracknum)
  (set-seqblock-selected-box 'interior-right seqblocknum seqtracknum)
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
  (<declare-variable> set-custom-seq-indicator)
  (if is-left
      (begin
        (define time (round (scale (<ra> :get-seqblock-fade-in seqblocknum seqtracknum)
                                   0 1
                                   (<ra> :get-seqblock-start-time seqblocknum seqtracknum)
                                   (<ra> :get-seqblock-end-time seqblocknum seqtracknum))))
        (set-custom-seq-indicator time
                                  -1
                                   "sequencer_block_fade_box_color")
        (set-seqblock-selected-box 'fade-left seqblocknum seqtracknum)
        (set-editor-statusbar (get-fade-string-left seqblocknum seqtracknum)))
      (begin
        (define time (round (scale (<ra> :get-seqblock-fade-out seqblocknum seqtracknum)
                                   1 0
                                   (<ra> :get-seqblock-start-time seqblocknum seqtracknum)
                                   (<ra> :get-seqblock-end-time seqblocknum seqtracknum))))
        (set-custom-seq-indicator time
                                  -1
                                  "sequencer_block_fade_box_color")
        (set-seqblock-selected-box 'fade-right seqblocknum seqtracknum)
        (set-editor-statusbar (get-fade-string-right seqblocknum seqtracknum)))))

(define (ask-user-about-first-audio-seqtrack2 callback)
  (show-async-message (<gui> :get-sequencer-gui)
                      (<-> "Are you sure?\n"
                           "\n"
                           "We use the first seqtrack for timing and grid, but audio seqtracks don't provide this information.\n"
                           "In order to support timing and grid, we will switch to sequencer timing mode."
                           )
                      (list "No" "Yes") ;; yes-dont-show-again)
                      :is-modal #t
                      :callback callback))

(define (ask-user-about-first-audio-seqtrack callback)
  (if (<ra> :is-using-sequencer-timing)
      (callback #t)
      (ask-user-about-first-audio-seqtrack2
       (lambda (res)
         (define arg (string=? "Yes" res))
         (undo-block
          (lambda ()                               
            (when arg
              (<ra> :undo-sequencer)
              (<ra> :set-using-sequencer-timing #t))
            (callback arg)))))))

(define (FROM_C-call-me-after-seqtrack-has-been-deleted)
  (<declare-variable> *current-seqblock-info*)
  (set! *current-seqblock-info* #f)
  (if *current-seqtrack-num*
      (set! *current-seqtrack-num* (min (- (<ra> :get-num-seqtracks) 1)
                                        *current-seqtrack-num*))))

(define (FROM_C-delete-seqtrack seqtracknum)
  (if (and (= 0 seqtracknum)
           (not (<ra> :seqtrack-for-audiofiles 0))
           (<ra> :seqtrack-for-audiofiles 1))
      (ask-user-about-first-audio-seqtrack
       (lambda (doit)
         (if doit
             (<ra> :force-delete-seqtrack seqtracknum))))
      (<ra> :force-delete-seqtrack seqtracknum)))

(define (FROM_C-insert-seqtrack for-audiofiles seqtracknum is-bus)
  (if (= -1 seqtracknum)
      (set! seqtracknum (<ra> :get-curr-seqtrack)))
  (if (and (= 0 seqtracknum)
           for-audiofiles
           (not (<ra> :seqtrack-for-audiofiles 0)))
      (ask-user-about-first-audio-seqtrack
       (lambda (doit)
         (if doit
             (<ra> :insert-seqtrack #t seqtracknum is-bus #t))))
      (<ra> :insert-seqtrack for-audiofiles seqtracknum is-bus #t)))


(define (get-seqtrack-config-popup-menu-entries)
  (list
   "----Insert seqtrack"
   (list "E+ Insert editor seqtrack"
         ra:insert-editor-seqtrack)
   (list "A+ Insert audio seqtrack"
         ra:insert-audio-seqtrack)
   (list "A+ Insert audio bus"         
         ra:insert-bus-seqtrack)
   "----Delete seqtrack"
   (list (<-> "Delete \"" (<ra> :get-seqtrack-name (<ra> :get-curr-seqtrack)) "\"")
         :enabled (> (<ra> :get-num-seqtracks) 1)
         ra:delete-seqtrack)
   "----Append seqtrack"
   (list "+A Append editor seqtrack"
         ra:append-editor-seqtrack)
   (list "+A Append audio seqtrack"
         ra:append-audio-seqtrack)
   (list "+A Append audio bus"
         ra:append-bus-seqtrack)
   ))

   
   
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

(define (get-sequencer-conf-menues)
  (list 
        "--------Sequencer timeline"
        (list
         :radio-buttons
         (list "Free"
               :check (and (not (<ra> :is-seqlooping))
                           (not (<ra> :is-seqpunching)))
               (lambda (val)
                 (c-display "new no-looping-or-punch:" val)))
         (list "Looping"
               :check (<ra> :is-seqlooping)
               (lambda (val)
                 (<ra> :set-seqlooping val)))
         (list "Punch in/out (recording)"
               :check (<ra> :is-seqpunching)
               (lambda (val)
                 (<ra> :set-seqpunching val)
                 (c-display "new punch in/out:" val))))
        ;;"------- Sequencer configuration" ;;Various"
        "-------Sequencer lanes"
        (list "Song tempo automation"
              :check (<ra> :seqtempo-visible)
              (lambda (doit)
                (<ra> :set-seqtempo-visible doit)))
        (list "Time"
              :check (<ra> :show-time-sequencer-lane)
              :enabled (or #t
                           (not (<ra> :show-time-sequencer-lane))
                           (<ra> :show-bars-and-beats-sequencer-lane))
              (lambda (doit)
                (if (and (not doit)
                         (not (<ra> :show-bars-and-beats-sequencer-lane)))
                    (<ra> :set-show-bars-and-beats-sequencer-lane #t))
                    ;;(show-async-message (<gui> :get-sequencer-gui)
                    ;;                    "Either the time lane or the bars+beats lane must be visible")
                (<ra> :set-show-time-sequencer-lane doit)))
        (list "Bars and beats"
              :check (<ra> :show-bars-and-beats-sequencer-lane)
              :enabled (or #t
                           (not (<ra> :show-bars-and-beats-sequencer-lane))
                           (<ra> :show-time-sequencer-lane))
              (lambda (doit)
                (if (and (not doit)
                         (not (<ra> :show-time-sequencer-lane)))
                    (<ra> :set-show-time-sequencer-lane #t))
                    ;;(show-async-message (<gui> :get-sequencer-gui)
                    ;;                    "Either the time lane or the bars+beats lane must be visible")
                (<ra> :set-show-bars-and-beats-sequencer-lane doit)))
        (list "Tempos"
              :check (<ra> :show-tempos-sequencer-lane)
              (lambda (doit)
                (<ra> :set-show-tempos-sequencer-lane doit)))
        (list "Signatures"
              :check (<ra> :show-signatures-sequencer-lane)
              (lambda (doit)
                (<ra> :set-show-signatures-sequencer-lane doit)))
        (list "Markers"
              :check (<ra> :show-markers-sequencer-lane)
              (lambda (doit)
                (<ra> :set-show-markers-sequencer-lane doit)))
        "-------"
        (list :radio-buttons
              (list "Use sequencer timing"
                    :check (<ra> :is-using-sequencer-timing)
                    :enabled (not (<ra> :seqtrack-for-audiofiles 0))
                    (lambda (doit)
                      (if doit
                          (<ra> :set-using-sequencer-timing #t))))
              (list "Use editor timing"
                    :check (not (<ra> :is-using-sequencer-timing))
                    :enabled (not (<ra> :seqtrack-for-audiofiles 0))
                    (lambda (doit)
                      (when doit
                        (<ra> :set-using-sequencer-timing #f)))))
        "-------Sequencer"
        (list "Visible"
              :check (<ra> :sequencer-is-visible)
              :shortcut ra:show-hide-sequencer
              (lambda (doit)
                (if doit
                    (<ra> :show-sequencer)
                    (<ra> :hide-sequencer))))
        (list "Preferences"
              (lambda ()
                (<ra> :open-sequencer-preferences-dialog)))))


(define (FROM_C-jump-to-mark marknum)
  (c-display "marknum:" marknum)
  (define markers (<ra> :get-all-sequencer-markers))
  (cond ((= marknum 0)
         (ra:set-song-pos 0))
        ((> marknum (length markers))
         (ra:set-song-pos (ra:get-song-length-in-frames)))
        (else
         (ra:set-song-pos (floor (markers (- marknum 1) :time))))))

(define (FROM_C-jump-next-mark)
  (let loop ((markers (to-list (ra:get-all-sequencer-markers))))
    (if (null? markers)
        (ra:set-song-pos (ra:get-song-length-in-frames))
        (let ((marktime (floor (markers 0 :time))))
          (if (> marktime (ra:get-song-pos))
              (ra:set-song-pos marktime)
              (loop (cdr markers)))))))

(define (FROM_C-jump-prev-mark)
  (define is-playing-song (<ra> :is-playing-song))
  (define (get-fuzzy-song-pos)
    (define songpos (<ra> :get-song-pos))
    (if is-playing-song
        (- songpos (/ (<ra> :get-sample-rate) 1)) ;; 1000ms fuzz
        songpos))
  (define fuzzy-song-pos (get-fuzzy-song-pos))
  ;;(c-display "fuzzy:" is-playing-song (/ fuzzy-song-pos 44100.0) ". songpos:" (/ (<ra> :get-song-pos) 44100.0))
  (let loop ((markers (reverse (to-list (ra:get-all-sequencer-markers)))))
    (if (null? markers)
        (ra:set-song-pos 0)
        (let ((marktime (floor (markers 0 :time))))
          (if (< marktime fuzzy-song-pos)
              (ra:set-song-pos marktime)
              (loop (cdr markers)))))))

#!!
(pretty-print (<ra> :get-all-sequencer-markers))
(<ra> :get-sample-rate)
!!#

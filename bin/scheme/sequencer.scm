
(provide 'sequencer.scm)

(<declare-variable> create-audio-seqblock-gui) ;; in seqblock_audio.scm
(<declare-variable> show-seqblock-track-on-off-configuration) ;; seqblock_editor.scm


(define *seqnode-min-distance* (* 1 (<ra> :get-half-of-node-width)))

(define *clipboard-seqtrack-automation* #f)
(define2 *seqblock-clipboard* list? '())


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


(define (get-normalized-seqblock-gain seqblockid)
  (let ((max-gain (<ra> :get-max-seqblock-sample-gain seqblockid)))
    (if (> max-gain 0)
        (/ 1.0 max-gain)
        100)))


(define2 *current-seqtrack-num* (curry-or not integer?) #f)

;; Current seqblock, and sequencer block order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-current-seqblock! seqtracknum id)
  ;;(assert (<ra> :seqblock-is-alive id))
  (define old-order (to-list (<ra> :get-seqblock-z-order seqtracknum)))  
  (define new-order (cons id (delete-maybe id old-order =)))
  ;;(c-display "id:" id "old:" old-order ". new-order: " new-order)
  (<ra> :set-curr-seqblock id)
  (<ra> :set-seqblock-z-order
        new-order
        seqtracknum))

(define (FROM_C-set-current-seqblock! seqtracknum id)
  (set-current-seqblock! seqtracknum id))



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
            (setter type seqtracknum)
            (if gotit-callback
                (gotit-callback)
                (show-select-both-seqtrack-size-types-gui seqtracknum)))
          (<ra> :schedule 30
                (lambda ()
                  (eat-errors :try (lambda ()
                                      (setter type seqtracknum)
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
              
              (define button (<gui> :radiobutton
                                    text
                                    (and (not is-disabled)
                                         (= type (getter seqtracknum)))
                                    (lambda (val)
                                      (if val
                                          (gotit type)))))

              (define (get-keybinding type)
                (let ((keybinding (get-displayable-keybinding "ra:set-seqtrack-min-height-type" (list type))))
                  (if (string=? "" keybinding)
                      "unassigned"
                      keybinding)))

              (define width1 (ceiling (* 1.5 (<gui> :text-width "1/3 row   "))))
              (define width2 (ceiling (* 1.5 (apply max (map (lambda (type)
                                                               (<gui> :text-width (get-keybinding type)))
                                                             (iota 4))))))
              (define text-width (+ width1 width2))
              
              (<gui> :set-min-width button text-width)

              (define (paint-keybinding width height)
                (draw-keybinding button (- width width2) 0 width height (get-keybinding type)))
            
              (<gui> :add-paint-callback button paint-keybinding #t)
            
              (<gui> :add gui button)
              
              (add-keybinding-configuration-to-gui button "ra:set-seqtrack-min-height-type" (list type) "FOCUS_SEQUENCER")
              
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

#!!
(null? (get-keybindings-from-command-without-focus-and-mouse "ra.setSeqtrackMinHeightType 13"))

!!#

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

(delafina (show-select-both-seqtrack-size-types-gui :seqtracknum (<ra> :get-curr-seqtrack))
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


(define *open-record-config-windows* (make-hash-table))
(define *curr-record-config-window* #f) ;; only show one at a time.

(define (show-record-popup-menu seqtracknum)
  (if *curr-record-config-window*
      (<gui> :close *curr-record-config-window*))
  
  (define popup #f)
  (define radiobuttons
    (map (lambda (ch)
           (<gui> :radiobutton (<-> ch "") #f (lambda (val)
                                                ;;(if popup
                                                ;;    (<gui> :close popup))
                                                #t)))
         (map 1+ (iota 8))))
  
  (define (create-options)
    (let ((options
           (<gui> :vertical-layout
                  
                  (<gui> :group "Source"
                         (<gui> :horizontal-layout
                                (<gui> :radiobutton "System input"
                                       (<ra> :get-seqtrack-record-from-system-input seqtracknum)
                                       (lambda (ison)
                                         (<ra> :set-seqtrack-record-from-system-input seqtracknum ison)))
                                (<gui> :radiobutton "Input connections to the instrument"
                                       (not (<ra> :get-seqtrack-record-from-system-input seqtracknum))
                                       (lambda (ison)
                                         (<ra> :set-seqtrack-record-from-system-input seqtracknum (not ison))))
                                
                                ))
                                         ;;;(<gui> :radiobutton "Output of instrument main pipe #f")))
                  
                  (<gui> :group "Source channel -> Soundfile channel"
                         (let ((matrix (<gui> :horizontal-layout
                                              (map (lambda (input-channel)
                                                     (<gui> :vertical-layout
                                                            (map (lambda (soundfile-channel)
                                                                   (<gui> :checkbox (<-> input-channel " -> " soundfile-channel)
                                                                          (<ra> :get-seqtrack-recording-matrix seqtracknum input-channel soundfile-channel)
                                                                          #t
                                                                          (lambda (ison)
                                                                            (<ra> :set-seqtrack-recording-matrix seqtracknum input-channel soundfile-channel ison)
                                                                            ;;(c-display (<-> input-channel " -> " soundfile-channel ": " ison))
                                                                            )))
                                                                 (iota 8))))
                                                   (iota 8)))))
                           matrix))
                  
                  (<gui> :group "Use custom settings for this seqtrack?"
                         (<gui> :vertical-layout
                                (<gui> :radiobutton "Yes. (These settings apply to this seqtrack only)"
                                       (<ra> :get-seqtrack-use-custom-recording-config seqtracknum)
                                       (lambda (ison)
                                         (<ra> :set-seqtrack-use-custom-recording-config seqtracknum ison)))                                         
                                (<gui> :radiobutton "No. (These settings apply to all seqtracks with non-custom settings)"
                                       (not (<ra> :get-seqtrack-use-custom-recording-config seqtracknum))
                                       (lambda (ison)
                                         (<ra> :set-seqtrack-use-custom-recording-config seqtracknum (not ison))))))
                  
                  )))
      (<gui> :set-layout-spacing options 5 2 2 2 2)
      options))
  
  (define options #f)
  
  (define (recreate-options)
    (define new-options (create-options))
    (when options
      (<gui> :replace content options new-options)
      (<gui> :close options))
    (set! options new-options))

  (recreate-options)
  
  (define content #f)

  (define reset-button (<gui> :button "Reset values"
                              (lambda ()
                                (<ra> :reset-seqtrack-recording-options seqtracknum)
                                (recreate-options))))

  ;;(when (<ra> :seqtrack-is-recording seqtracknum)
  ;;  (<gui> :set-enabled options #f)
  ;;  (<gui> :set-enabled reset-button #f))
    
  (set! content (<gui> :vertical-layout
                       (mid-horizontal-layout (<gui> :text (<-> "Recording options for \"" (<ra> :get-seqtrack-name seqtracknum) "\" (#" seqtracknum ")")))
                       options
                       (<gui> :horizontal-layout
                              reset-button
                              (<gui> :button "Close"
                                     (lambda ()
                                       (if popup
                                           (<gui> :close popup)))))))
    
  (<gui> :set-layout-spacing content 5 2 2 2 2)

  (if #f
      (set! popup (<gui> :popup))
      (begin
        (set! popup (<gui> :widget))
        ;;(<gui> :set-modal popup #t)
        (<gui> :set-parent popup -2)))
  
  (<gui> :add popup content)
                                        ;(<gui> :set-parent widget -2)
  (<gui> :show popup)
  (<gui> :minimize-as-much-as-possible popup)
                                        ;(<gui> :set-pos widget (floor (<ra> :get-global-mouse-pointer-x)) (floor (<ra> :get-global-mouse-pointer-y)))

  (set! *curr-record-config-window* popup)

  (<gui> :add-deleted-callback popup
         (lambda (radium-runs-custom-exec)
           (set! (*open-record-config-windows* seqtracknum) #f)
           (set! *curr-record-config-window* #f)))
  )


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


(<declare-variable> *current-seqblock-info*)

(define (FROM_C-call-me-after-seqtrack-has-been-deleted)
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


;; Note: Used for shortcut
(delafina (swap-with-prev-seqtrack :seqtracknum (<ra> :get-curr-seqtrack))
  (define (swapit)
    (<ra> :undo-sequencer)
    (<ra> :swap-seqtracks (- seqtracknum 1) seqtracknum)
    (<ra> :set-curr-seqtrack (- seqtracknum 1)))
  (if (and (= 1 seqtracknum)
           (<ra> :seqtrack-for-audiofiles 1))
      (ask-user-about-first-audio-seqtrack
       (lambda (doit)
         (if doit
             (swapit))))
      (swapit)))

;; Note: Used for shortcut
(delafina (swap-with-next-seqtrack :seqtracknum (<ra> :get-curr-seqtrack))
  (define (swapit)
    (<ra> :undo-sequencer)
    (<ra> :swap-seqtracks seqtracknum (1+ seqtracknum))
    (<ra> :set-curr-seqtrack (1+ seqtracknum)))          
  (if (and (= 0 seqtracknum)
           (<ra> :seqtrack-for-audiofiles 1))
      (ask-user-about-first-audio-seqtrack
       (lambda (doit)
         (if doit
             (swapit))))
      (swapit)))

;; Note: Used for shortcut
(delafina (show-set-seqtrack/seqblock-name-requester :seqtracknum (<ra> :get-curr-seqtrack-under-mouse #f #t) ;; (<ra> :get-curr-seqtrack)
                                                     :seqblock-id (and *current-seqblock-info*
                                                                       (*current-seqblock-info* :id)))
  (if seqblock-id
      (set! seqtracknum (<ra> :get-seqblock-seqtrack-num seqblock-id)))

  (when seqtracknum
    
    (if seqblock-id
        (set-current-seqblock! seqtracknum seqblock-id)
        (<ra> :set-curr-seqtrack seqtracknum))
    
    (define current-name (if seqblock-id
                             (<ra> :get-seqblock-name seqblock-id)
                             (<ra> :get-seqtrack-name seqtracknum)))
    (<ra> :schedule 0
          (lambda ()
            (let ((new-name (if seqblock-id
                                (<ra> :request-w-string (<-> "New seqblock name: ") #t current-name)
                                (<ra> :request-string (<-> "New seqtrack name: ") #t current-name))))
              (if (and (not (string=? "" new-name))
                       (not (string=? current-name new-name)))
                  (if seqblock-id
                      (<ra> :set-seqblock-name new-name seqblock-id #t)
                      (<ra> :set-seqtrack-name new-name seqtracknum))))
            #f))))

(define (get-seqtrack-popup-menu-entries seqtracknum)
  (list
   (list "Swap with prev seqtrack"
         :enabled (> seqtracknum 0)
         :shortcut swap-with-prev-seqtrack
         (lambda ()
           (swap-with-prev-seqtrack seqtracknum)))
   (list "Swap with next seqtrack"
         :enabled (< seqtracknum (- (<ra> :get-num-seqtracks) 1))
         :shortcut swap-with-next-seqtrack
         (lambda ()
           (swap-with-next-seqtrack seqtracknum)))
   (list "Set height"
         :shortcut show-select-both-seqtrack-size-types-gui
         (lambda ()
           (show-select-both-seqtrack-size-types-gui seqtracknum)))
   (list "Set name"
         :shortcut show-set-seqtrack/seqblock-name-requester
         (lambda ()
           (show-set-seqtrack/seqblock-name-requester seqtracknum)))))

;; Note: Used for shortcut
(define (set-no-looping-or-punching-in-sequencer)
  (<ra> :set-seqlooping #f)
  (<ra> :set-seqpunching #f))
  
;; Note: Used for shortcut
(define (switch-looping-in-sequencer)
  (<ra> :set-seqlooping (not (<ra> :is-seqlooping))))
  
;; Note: Used for shortcut
(define (switch-punching-in-sequencer)
  (<ra> :set-seqpunching (not (<ra> :is-seqpunching))))
  
;; Note: Used for shortcut
(define (switch-seqtempo-visible)
  (<ra> :set-seqtempo-visible (not (<ra> :seqtempo-visible))))

;; Note: Used for shortcut
(define (switch-show-time-sequencer-lane)
  (<ra> :set-show-time-sequencer-lane (not (<ra> :show-time-sequencer-lane))))

;; Note: Used for shortcut
(define (switch-show-bars-and-beats-sequencer-lane)
  (<ra> :set-show-bars-and-beats-sequencer-lane (not (<ra> :show-bars-and-beats-sequencer-lane))))

;; Note: Used for shortcut
(define (switch-show-tempos-sequencer-lane)
  (<ra> :set-show-tempos-sequencer-lane (not (<ra> :show-tempos-sequencer-lane))))

;; Note: Used for shortcut
(define (switch-show-signatures-sequencer-lane)
  (<ra> :set-show-signatures-sequencer-lane (not (<ra> :show-signatures-sequencer-lane))))

;; Note: Used for shortcut
(define (switch-show-markers-sequencer-lane)
  (<ra> :set-show-markers-sequencer-lane (not (<ra> :show-markers-sequencer-lane))))

;; Note: Used for shortcut
(define (switch-set-using-sequencer-timing)
  (<ra> :set-using-sequencer-timing (not (<ra> :is-using-sequencer-timing))))


(define (get-sequencer-conf-menues)
  (list 
        "--------Sequencer timeline"
        (list
         :radio-buttons
         (list "Free"
               :check (and (not (<ra> :is-seqlooping))
                           (not (<ra> :is-seqpunching)))
               :shortcut set-no-looping-or-punching-in-sequencer
               (lambda (val)
                 (c-display "new no-looping-or-punch:" val)))
         (list "Looping"
               :check (<ra> :is-seqlooping)
               :shortcut switch-looping-in-sequencer
               (lambda (val)
                 (<ra> :set-seqlooping val)))
         (list "Punch in/out (recording)"
               :check (<ra> :is-seqpunching)
               :shortcut switch-punching-in-sequencer
               (lambda (val)
                 (<ra> :set-seqpunching val)
                 (c-display "new punch in/out:" val))))
        ;;"------- Sequencer configuration" ;;Various"
        "-------Sequencer lanes"
        (list "Song tempo automation"
              :check (<ra> :seqtempo-visible)
              :shortcut switch-seqtempo-visible
              (lambda (doit)
                (<ra> :set-seqtempo-visible doit)))
        (list "Time"
              :check (<ra> :show-time-sequencer-lane)
              :shortcut switch-show-time-sequencer-lane
              (lambda (doit)
                (<ra> :set-show-time-sequencer-lane doit)))
        (list "Bars and beats"
              :check (<ra> :show-bars-and-beats-sequencer-lane)
              :shortcut switch-show-bars-and-beats-sequencer-lane
              (lambda (doit)
                (<ra> :set-show-bars-and-beats-sequencer-lane doit)))
        (list "Tempos"
              :check (<ra> :show-tempos-sequencer-lane)
              :shortcut switch-show-tempos-sequencer-lane
              (lambda (doit)
                (<ra> :set-show-tempos-sequencer-lane doit)))
        (list "Signatures"
              :check (<ra> :show-signatures-sequencer-lane)
              :shortcut switch-show-signatures-sequencer-lane
              (lambda (doit)
                (<ra> :set-show-signatures-sequencer-lane doit)))
        (list "Markers"
              :check (<ra> :show-markers-sequencer-lane)
              :shortcut switch-show-markers-sequencer-lane
              (lambda (doit)
                (<ra> :set-show-markers-sequencer-lane doit)))
        "-------Timing mode"
        (list :radio-buttons
              (list "Use sequencer timing"
                    :check (<ra> :is-using-sequencer-timing)
                    :enabled (not (<ra> :seqtrack-for-audiofiles 0))
                    :shortcut switch-set-using-sequencer-timing
                    (lambda (doit)
                      (if doit
                          (<ra> :set-using-sequencer-timing #t))))
              (list "Use editor timing"
                    :check (not (<ra> :is-using-sequencer-timing))
                    :enabled (not (<ra> :seqtrack-for-audiofiles 0))
                    :shortcut switch-set-using-sequencer-timing
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
        (list "Preferences" ra:open-sequencer-preferences-dialog)))


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


(define (get-sequencer-x time)
  (scale time
         (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)
         (<ra> :get-seqtimeline-area-x1) (<ra> :get-seqtimeline-area-x2)))

(define (get-sequencer-time x)
  (scale x
         (<ra> :get-seqtimeline-area-x1) (<ra> :get-seqtimeline-area-x2)
         (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
  

;; Note: Used for shortcut
(delafina (split-seqblock :pos (<ra> :get-seq-gridded-time (round (get-sequencer-time (<ra> :get-mouse-pointer-x -2))))
                          :seqblock-id (and *current-seqblock-info*
                                            (*current-seqblock-info* :id)))
  (when seqblock-id
    (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblock-id))
    (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblock-id))
    (call-with-exit
     (lambda (return)
       (if (not (<ra> :seqtrack-for-audiofiles seqtracknum))
           (return #f)) ;; not supported yet. (difficult)
       (define seqblocks-state (to-list (<ra> :get-seqblocks-state seqtracknum)))
       (define seqblock (seqblocks-state seqblocknum))
       
       (define stretch (<ra> :get-seqblock-stretch-speed (seqblock :id)))
       (define t1 (seqblock :start-time))
       (define i1 (seqblock :interior-start))
       (define i2 (seqblock :interior-end))
       (define s1 (- t1 (* i1 stretch)))
       
       (define interior-split (to-integer (/ (- pos s1) stretch)))
       (if (<= interior-split i1)
           (return #f))
       (if (>= interior-split i2)
           (return #f))
       (define seqblock1 (copy-hash seqblock
                                    :end-time pos
                                    :interior-end interior-split))
       
       (define seqblock2 (copy-hash seqblock
                                    :id -1
                                    :start-time pos
                                    :interior-start interior-split))
       
       (define new-seqblocks-state (append (if (= 0 seqblocknum)
                                               '()
                                               (take seqblocks-state seqblocknum))
                                           (list seqblock1 seqblock2)
                                           (if (= (1- (length seqblocks-state)) seqblocknum)
                                               '()
                                               (sublist seqblocks-state (1+ seqblocknum) (length seqblocks-state)))))
       
       (try-finally :try (lambda ()                   
                           (<ra> :create-gfx-seqblocks-from-state new-seqblocks-state seqtracknum)
                           (<ra> :undo-sequencer)
                           (<ra> :apply-gfx-seqblocks seqtracknum))
                    :failure (lambda ()
                               (<ra> :cancel-gfx-seqblocks seqtracknum)))
       #t))))

(define (get-audiofile-menu-entry-text audiofile)
  (define info (<ra> :get-file-info audiofile))
  (<ra> :append-base64-strings
        (<ra> :get-base64-from-filepath (info :filename))
        (<ra> :to-base64 (<-> ", " (info :num-ch) "ch, " (get-displayable-seconds (/ (info :num-frames)
                                                                                     (info :samplerate)))))))

;; Note: Used for shortcut
(delafina (insert-existing-block-or-audiofile-in-sequencer :seqtracknum (<ra> :get-curr-seqtrack-under-mouse #f #t)
                                                           :X (<ra> :get-mouse-pointer-x -2))
  ;;(c-display "X:" X "seqgracknum:" seqtracknum)
  (if (>= seqtracknum 0)
      (let ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X)))))
        (c-display "POS:" pos)
        (define (create-new-audiofile)
          (create-file-requester "Choose audio file" (<ra> :create-illegal-filepath) "audio files" (<ra> :get-audiofile-postfixes) #t #f -1
                                 (lambda (filename)
                                   (<ra> :create-sample-seqblock seqtracknum filename pos))))
    
        (if (<ra> :seqtrack-for-audiofiles seqtracknum)
            
            (let ((audiofiles (to-list (<ra> :get-audio-files))))
              (cond ((null? audiofiles)
                     (create-new-audiofile))
                    ;;((= 1 (length audiofiles))
                    ;; (<ra> :create-sample-seqblock seqtracknum (car audiofiles) pos))
                    (else
                     (apply popup-menu
                            `(,(list "New audio file" create-new-audiofile)
                              "------------"
                              ,@(map (lambda (audiofile)  
                                       (list (get-audiofile-menu-entry-text audiofile)
                                             :base64 #t                       
                                             (lambda ()
                                               (<ra> :create-sample-seqblock seqtracknum audiofile pos))))
                                     audiofiles))))))
                                     
            
            (if (and #f (= 1 (<ra> :get-num-blocks)))
                (<ra> :create-seqblock seqtracknum 0 pos)                                          
                (apply popup-menu
                       `(,(list "Create new block"
                                (lambda ()
                                  (<ra> :create-seqblock seqtracknum (<ra> :append-block) pos)
                                  ))
                         "------------Existing blocks:"
                         ,@(map (lambda (blocknum)
                                  (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
                                        (lambda ()
                                          (<ra> :create-seqblock seqtracknum blocknum pos))))
                                (iota (<ra> :get-num-blocks))))))))))

;; Note: Used for shortcut
(delafina (insert-current-block-or-audiofile-in-sequencer :seqtracknum (<ra> :get-curr-seqtrack-under-mouse #f #t)
                                                          :X (<ra> :get-mouse-pointer-x -2))
  (<declare-variable> *curr-audiofile-num*)
  (if (>= seqtracknum 0)
      (let ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X)))))
        (if (<ra> :seqtrack-for-audiofiles seqtracknum)
            (let ((audiofiles (to-list (<ra> :get-audio-files))))
              (if (not (null? audiofiles))
                  (let ((currnum (between 0 *curr-audiofile-num* (- (length audiofiles) 1))))
                    (<ra> :create-sample-seqblock seqtracknum (audiofiles currnum) pos))))
            (<ra> :create-seqblock seqtracknum (<ra> :current-block) pos)))))


(define (request-instrument-id-and-effect-num seqtracknum callback)
  (define (instrument-popup-menu instrument-id)
    (popup-menu (map (lambda (effectnum)
                       (list (<-> effectnum ". " (<ra> :get-instrument-effect-name effectnum instrument-id))
                             (lambda ()
                               (callback instrument-id effectnum))))
                     (iota (<ra> :get-num-instrument-effects instrument-id)))))
 
  (define seqtrack-instrument-id (and (<ra> :seqtrack-for-audiofiles seqtracknum)
                                      (<ra> :get-seqtrack-instrument seqtracknum)))
  
  (define all-instruments (get-all-audio-instruments))

  (popup-menu
   (if (and seqtrack-instrument-id
            (<ra> :is-legal-instrument seqtrack-instrument-id))
       (list (<ra> :get-instrument-name seqtrack-instrument-id)
             (lambda ()
               (instrument-popup-menu seqtrack-instrument-id))
             "---------------------")
       '())
   (map (lambda (num instrument-id)
          (list (<-> num ". " (<ra> :get-instrument-name instrument-id))
                (lambda ()
                  (instrument-popup-menu instrument-id))))
        (iota (length all-instruments))
        all-instruments)))


(define-struct seqtrack-automation
  :instrument-id
  :effect-num
  :nodes)

(define-struct seqtrack-automation-node
  :time
  :value
  :logtype)
  
(define (get-seqtrack-automation seqtracknum automationnum)
  (make-seqtrack-automation :instrument-id (<ra> :get-seq-automation-instrument-id automationnum seqtracknum)
                            :effect-num (<ra> :get-seq-automation-effect-num automationnum seqtracknum)
                            :nodes (map (lambda (nodenum)
                                          (make-seqtrack-automation-node :time (<ra> :get-seq-automation-time nodenum automationnum seqtracknum)
                                                                         :value (<ra> :get-seq-automation-value nodenum automationnum seqtracknum)
                                                                         :logtype (<ra> :get-seq-automation-logtype  nodenum automationnum seqtracknum)))
                                        (iota (<ra> :get-num-seqtrack-automation-nodes automationnum seqtracknum)))))
#!!
(pretty-print (get-seqtrack-automation 1 2))
(pretty-print (get-seqtrack-automation 0 0))
!!#


;; Note: Used for shortcut
(delafina (paste-seqtrack-automation :seqtracknum (<ra> :get-curr-seqtrack-under-mouse #f #t)
                                     :time (<ra> :get-seq-gridded-time (round (get-sequencer-time (<ra> :get-mouse-pointer-x -2))))
                                     :seqtrack-automation *clipboard-seqtrack-automation*)

  (when (and seqtracknum
             seqtrack-automation)
    (define instrument-id (seqtrack-automation :instrument-id))
    (define effect-num (seqtrack-automation :effect-num))
    
    (define (doit2 instrument-id effect-num)
      (define nodes (seqtrack-automation :nodes))
      
      (define node1 (car nodes))
      (define node2 (cadr nodes))
      
      (define time1 (node1 :time))
      
      (define (get-time node)
        (+ (- (node :time)
              time1)
           time))
      
      (define (apply-logtype node nodenum automationnum)
        (<ra> :set-seq-automation-node
              (get-time node)
              (node :value)
              (node :logtype)
              nodenum
              automationnum
              seqtracknum))

      (c-display "HEPP:" (get-time node1) (node1 :value) (get-time node2) (node2 :value)
                         effect-num
                         instrument-id
                         seqtracknum)
      (define hash (<ra> :add-seq-automation2
                         (get-time node1) (node1 :value) (get-time node2) (node2 :value)
                         effect-num
                         instrument-id
                         seqtracknum))
      
      (define automationnum (hash :automationnum))
      (define nodenum1 (hash :nodenum1))
      (define nodenum2 (hash :nodenum2))
      
      ;;(c-display "apply automation. seqtracknum:" seqtracknum ". automationnum:" automationnum ". nodenums:" nodenum1 nodenum2)
      
      (apply-logtype node1 nodenum1 automationnum)
      (apply-logtype node2 nodenum2 automationnum)
      
      (for-each (lambda (node)
                  ;;(c-display "time node3:" (get-time node))
                  (<ra> :add-seq-automation-node (get-time node) (node :value) (node :logtype) automationnum seqtracknum))
                (cddr nodes))
      )
    
    (define (doit1 instrument-id effect-num)
      (undo-block
       (lambda ()
         (doit2 instrument-id effect-num))))
    
    (if (or (not (<ra> :instrument-is-open-and-audio instrument-id))
            (< effect-num 0)
            (>= effect-num (<ra> :get-num-instrument-effects instrument-id)))
        (show-async-message (<gui> :get-sequencer-gui)
                            "Instrument for automation in clipboard doesn't exist anymore. Do you want to select new effect?"
                            (list "Yes" "No") #t
                            (lambda (res)
                              (if (string=? "Yes" res)                                 
                                  (request-instrument-id-and-effect-num
                                   seqtracknum
                                   doit1))))
        (doit1 instrument-id effect-num))))
       

;; Note: Used for shortcut
(delafina (copy-seqtrack-automation :seqtracknum (and *current-seqautomation/distance*
                                                      (*current-seqautomation/distance* :seqtrack))
                                    :automationnum (and *current-seqautomation/distance*
                                                        (*current-seqautomation/distance* :automation-num)))
  (if seqtracknum
      (set! *clipboard-seqtrack-automation* (get-seqtrack-automation seqtracknum automationnum))))

;; Note: Used for shortcut
(delafina (cut-seqtrack-automation :seqtracknum (and *current-seqautomation/distance*
                                                     (*current-seqautomation/distance* :seqtrack))
                                   :automationnum (and *current-seqautomation/distance*
                                                       (*current-seqautomation/distance* :automation-num)))
  (when seqtracknum
    (copy-seqtrack-automation seqtracknum automationnum)
    (undo-block
     (lambda ()
       (remove-seqtrack-automation seqtracknum automationnum)))))

(define (remove-seqtrack-automation seqtracknum automationnum)
  (define num-automations (<ra> :get-num-seqtrack-automations seqtracknum))
  (while (= num-automations (<ra> :get-num-seqtrack-automations seqtracknum))
    (<ra> :delete-seq-automation-node 0 automationnum seqtracknum)))


(define (move-seqtrack-automation-to-different-seqtrack from-seqtracknum automationnum to-seqtracknum)
  (define automation (get-seqtrack-automation from-seqtracknum automationnum))
  (define time (automation :nodes 0 :time))
  (paste-seqtrack-automation to-seqtracknum time automation)
  (remove-seqtrack-automation from-seqtracknum automationnum))


(define (get-seq-automation-display-name automationnum seqtracknum)
  (define instrument-id (<ra> :get-seq-automation-instrument-id automationnum seqtracknum))
  (define instrument-name (<ra> :get-instrument-name instrument-id))
  (define effect-num (<ra> :get-seq-automation-effect-num automationnum seqtracknum))
  (define effect-name (<ra> :get-instrument-effect-name effect-num instrument-id))  
  (<-> instrument-name "/" effect-name))


;; Note: Used for shortcut
(delafina (create-sequencer-automation :seqtracknum (<ra> :get-curr-seqtrack-under-mouse #f #t) ;;get-curr-seqtrack)
                                       :X (<ra> :get-mouse-pointer-x -2)
                                       :Y (<ra> :get-mouse-pointer-y -2))
  (if (>= seqtracknum 0)
      (request-instrument-id-and-effect-num
       seqtracknum
       (lambda (instrument-id effectnum)
         (define Time1 (get-sequencer-time X))
         (define Time2 (get-sequencer-time (+ X (* 5 *seqnode-min-distance*))))
         (define Value (scale Y (<ra> :get-seqtrack-y1 seqtracknum) (<ra> :get-seqtrack-y2 seqtracknum) 1 0))
         ;;(c-display effectnum)
         (<ra> :add-seq-automation
               (floor Time1) Value
               (floor Time2) Value
               effectnum
               instrument-id
               seqtracknum)))))

(define (get-seqtrack-menu-entries seqtracknum X Y)
  (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define for-blocks (not for-audiofiles))

  (list
   (<-> "--------------------Seqtrack #" seqtracknum)
   
   (get-delete-all-pauses-menu-entry seqtracknum)
   (get-seqtrack-popup-menu-entries seqtracknum)
   
   "-------------------Automation"
   
   "New automation" :shortcut create-sequencer-automation (lambda ()
                                                            (create-sequencer-automation seqtracknum X Y))
   
   (list (<-> "Paste automation")
         :enabled *clipboard-seqtrack-automation*
         :shortcut paste-seqtrack-automation
         (lambda ()
           (let ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X)))))
             (paste-seqtrack-automation seqtracknum pos *clipboard-seqtrack-automation*))))
   
   (map (lambda (automationnum)
          (list (get-seq-automation-display-name automationnum seqtracknum)
                :check (<ra> :get-seq-automation-enabled automationnum seqtracknum)
                (lambda (checked)
                  (<ra> :set-seq-automation-enabled automationnum seqtracknum checked)
                  (c-display "checked" checked)))
          )
        (iota (<ra> :get-num-seqtrack-automations seqtracknum)))
   
   (if for-blocks
       
       (list
        "--------------------Editor Seqtrack" ;;Editor blocks"

        (list                                          
         "Insert current block"
         :shortcut insert-current-block-or-audiofile-in-sequencer
         (lambda ()
           (insert-current-block-or-audiofile-in-sequencer seqtracknum X)))
        
        (list
         "Insert block"
         :shortcut insert-existing-block-or-audiofile-in-sequencer
         (lambda ()
           (insert-existing-block-or-audiofile-in-sequencer seqtracknum X)))
        ;;   Sub menues version. It looks better, but it is less convenient.
        ;;"Insert existing block" (map (lambda (blocknum)
        ;;                               (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
        ;;                                     (lambda ()
        ;;                                       (let ((pos (get-sequencer-pos-from-x X)))
        ;;                                         (<ra> :add-block-to-seqtrack seqtracknum blocknum pos))
        ;;                                       (<ra> :select-block blocknum))))
        ;;                             (iota (<ra> :get-num-blocks)))
        
                                        ;(list                                           
                                        ; "Insert new block"
                                        ; (lambda ()
                                        ;   (let* ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X))))
                                        ;          (blocknum (<ra> :append-block)x))
                                        ;     (<ra> :create-seqblock seqtracknum blocknum pos))))
        
                                        ;(list
                                        ; "Insert new block from disk (BETA)"
                                        ; (lambda ()
                                        ;   (let* ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X))))
                                        ;          (num-blocks (<ra> :get-num-blocks)))
                                        ;     (<ra> :load-block)
                                        ;     (if (not (= num-blocks (<ra> :get-num-blocks)))
                                        ;         (<ra> :create-seqblock seqtracknum num-blocks pos))
                                        ;     )
                                        ;   )
        )
       
       (list
        "--------------------Audio Seqtrack"
        (if (<ra> :release-mode)
            '()
            (list                                               
             "Insert my soundfile"
             (lambda ()
               (let* ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X)))))
                 ;;(<ra> :create-sample-seqblock seqtracknum (<ra> :to-base64 "/home/kjetil/demosong_24000.wav") pos))))
                 ;;(<ra> :create-sample-seqblock seqtracknum (<ra> :to-base64 "/home/kjetil/karin_24000.wav") pos))))
                 ;;(<ra> :create-sample-seqblock seqtracknum (<ra> :to-base64 "/home/kjetil/karin.wav") pos))))
                 (<ra> :create-sample-seqblock seqtracknum (<ra> :get-path "/home/kjetil/390514__tylean__counting-1-to-10.wav") pos))))
            ;;(<ra> :create-sample-seqblock seqtracknum (<ra> :to-base64 "/home/kjetil/tannenbaum.ogg") pos)))
            )
        ;;
        (list
         "Insert audio file"
         :shortcut insert-existing-block-or-audiofile-in-sequencer
         (lambda ()
           (insert-existing-block-or-audiofile-in-sequencer seqtracknum X)))
        (list                                          
         "Insert current audiofile"
         :shortcut insert-current-block-or-audiofile-in-sequencer
         (lambda ()
           (insert-current-block-or-audiofile-in-sequencer seqtracknum X)))
        (list
         "Recording options"
         (lambda ()
           (show-record-popup-menu seqtracknum)))
        ))
   
   ;;"--------------------"
  
  
   ;;"-----------------"
   ;;
   ;;"Insert sequencer track" (lambda ()
   ;;                           (<ra> :insert-seqtrack seqtracknum))
   ;;(list "Delete sequencer track"
   ;;      :enabled (> (<ra> :get-num-seqtracks) 1)
   ;;      (lambda ()
   ;;        (set! *current-seqblock-info* #f)
   ;;        (<ra> :delete-seqtrack seqtracknum)))
   ;;"Append sequencer track" (lambda ()
   ;;                           (<ra> :append-seqtrack))
   
   (get-sequencer-conf-menues)
   ))

;; Note: used for shortcut
(delafina (show-seqtrack-popup-menu :seqtracknum (<ra> :get-curr-seqtrack-under-mouse #f #t)
                                    :X (<ra> :get-mouse-pointer-x -2)
                                    :Y (<ra> :get-mouse-pointer-y -2))
  (popup-menu (get-seqtrack-menu-entries seqtracknum X Y)))

(define (get-main-sequencer-popup-menu-entries seqtracknum X Y)
  (list "---------------------Sequencer"
        (<-> "Popup menu for seqtrack #" seqtracknum)
        :shortcut show-seqtrack-popup-menu
        (lambda ()
          (show-seqtrack-popup-menu seqtracknum X Y))))


(delafina (get-curr-seqblock-infos-under-mouse :mix-audio-and-editor-seqblocks #f
                                               :selected-seqblock-infos (get-selected-seqblock-infos)
                                               :current-seqblock-info *current-seqblock-info*)
  
  (define seqblock-infos (if current-seqblock-info
                             (if (<ra> :is-seqblock-selected (current-seqblock-info :seqblocknum) (current-seqblock-info :seqtracknum))
                                 selected-seqblock-infos
                                 (list current-seqblock-info))
                             selected-seqblock-infos))
  
  (if (or (null? seqblock-infos)
          mix-audio-and-editor-seqblocks)
      seqblock-infos
      (let ((seqtracknum (if current-seqblock-info
                             (current-seqblock-info :seqtracknum)
                             (<ra> :get-curr-seqtrack-under-mouse #t #t))))
        
        (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
        
        (keep (lambda (seqblock-info)
                (eq? (<ra> :seqtrack-for-audiofiles (seqblock-info :seqtracknum))
                     for-audiofiles))
              seqblock-infos))))

#!!
(get-curr-seqblock-infos-under-mouse)
!!#

;; Note: Used for shortcut
(define (replace-seqblocks seqblock-infos
                           get-block-or-audiofile)
  (c-display seqblock-infos)
  
  (when (not (null? seqblock-infos))
    (define for-audiofiles (<ra> :seqtrack-for-audiofiles (seqblock-infos 0 :seqtracknum)))
    
    (if (not (null? seqblock-infos))
        (if for-audiofiles
            (get-block-or-audiofile for-audiofiles
                                    (lambda (audiofile)
                                      (undo-block
                                       (lambda ()
                                         (for-each (lambda (seqblock-info)
                                                     (let* ((seqblocknum (seqblock-info :seqblocknum))
                                                            (seqtracknum (seqblock-info :seqtracknum))
                                                            (pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                                                       (set! *current-seqblock-info* #f)
                                                       (<ra> :delete-seqblock (seqblock-info :id))
                                                       (<ra> :create-sample-seqblock seqtracknum audiofile pos)))
                                                   seqblock-infos)))))
            (get-block-or-audiofile for-audiofiles
                                    (lambda (blocknum)
                                      (undo-block
                                       (lambda ()
                                         (for-each (lambda (seqblock-info)
                                                     (let* ((seqblocknum (seqblock-info :seqblocknum))
                                                            (seqtracknum (seqblock-info :seqtracknum))
                                                            (pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                                                       (set! *current-seqblock-info* #f)
                                                       (<ra> :delete-seqblock (seqblock-info :id))
                                                       (<ra> :create-seqblock seqtracknum blocknum pos)))
                                                   seqblock-infos)))
                                      (<ra> :select-block blocknum)))))))

(delafina (replace-seqblocks-with-existing-or-new-block-or-audiofile :seqblock-infos (get-curr-seqblock-infos-under-mouse))
  (replace-seqblocks seqblock-infos
                     (lambda (for-audiofiles gotit)
                       (if for-audiofiles
                           (let ((audiofiles (to-list (<ra> :get-audio-files))))
                             (if (not (null? audiofiles))          
                                 (apply popup-menu
                                        `(,(list "New audio file"
                                                 (lambda ()
                                                   (create-file-requester "Choose audio file" (<ra> :create-illegal-filepath) "audio files" (<ra> :get-audiofile-postfixes) #t #f -1
                                                                          gotit)))
                                          "------------"
                                          ,@(map (lambda (audiofile)
                                                   (list (get-audiofile-menu-entry-text audiofile)
                                                         :base64 #t
                                                         (lambda ()
                                                           (gotit audiofile))))
                                                 audiofiles)))))
                           (apply popup-menu
                                  (list "Create new block"
                                        (lambda ()
                                          (undo-block
                                           (lambda ()
                                             (gotit (<ra> :append-block))))))
                                  "-----------Existing blocks:"
                                  (map (lambda (blocknum)
                                         (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
                                               (lambda ()
                                                 (gotit blocknum))))
                                       (iota (<ra> :get-num-blocks))))))))

(delafina (replace-seqblocks-with-current-block-or-audiofile :seqblock-infos (get-curr-seqblock-infos-under-mouse))
  (replace-seqblocks seqblock-infos
                     (lambda (for-audiofiles gotit)
                       (if for-audiofiles
                           (c-display "not implemented")
                           (gotit (<ra> :current-block))))))

                       
#!!
(get-curr-seqblock-infos-under-mouse)
!!#

;; Note: used for shortcut
(delafina (switch-seqblock-automation-enabled :automation-num
                                              :seqblock-id (and *current-seqblock-info*
                                                                (*current-seqblock-info* :id)))
  (when (and seqblock-id
             (< automation-num (<ra> :get-num-seqblock-automations
                                     (<ra> :get-seqblock-seqblock-num seqblock-id)
                                     (<ra> :get-seqblock-seqtrack-num seqblock-id))))
    (<ra> :set-seqblock-automation-enabled
          (not (<ra> :get-seqblock-automation-enabled automation-num seqblock-id))
          automation-num
          seqblock-id)))

(define (create-seqblock-automation-popup-menu-entry automationnum seqblockid)
  (list (<-> (<ra> :get-seqblock-automation-name automationnum) " automation")
        :check (<ra> :get-seqblock-automation-enabled automationnum seqblockid)
        :shortcut (list switch-seqblock-automation-enabled automationnum)
        (lambda (enable)
          (<ra> :set-seqblock-automation-enabled enable automationnum seqblockid))))



(define (get-seqblock-gain-text seqblock-id)
  (db-to-text (<ra> :gain-to-db (<ra> :get-seqblock-gain seqblock-id)) #t))

;; Note: Used for shortcut
(delafina (set-seqblock-gain :seqblock-id (and *current-seqblock-info*
                                               (*current-seqblock-info* :id)))
  (when seqblock-id
    (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblock-id))
    (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblock-id))
    (define new (<ra> :request-float (<-> "New gain (now: " (get-seqblock-gain-text seqblock-id)  ")")
                      -1000
                      1000))
    (if (>= new -1000)
        (<ra> :set-seqblock-gain (<ra> :db-to-gain new) seqblock-id))))

(define (get-set-seqblock-gain-popup-menu-entries seqblock-id)
  (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblock-id))
  (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblock-id))
  (list
   (<-> "Set gain (now: " (get-seqblock-gain-text seqblock-id) ")")
   :shortcut set-seqblock-gain
   (lambda ()
     (set-seqblock-gain seqblock-id))))
  

(define (get-seqblock-separator-text text seqblockid)
  (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblockid))
  (define seqtracknum  (<ra> :get-seqblock-seqtrack-num seqblockid))
  
  (define for-audio (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define for-editor (not for-audio))
  
  (define name (<ra> :get-seqblock-name seqblockid))
  (define name-is-compatible (and #t (<ra> :base64-string-is-8bit-compatible name)))
  (define name8 (and name-is-compatible
                     (<ra> :from-base64 name)))

  (define blocknum (and for-editor
                        (<ra> :get-seqblock-blocknum seqblocknum seqtracknum)))
  
  (define display-name (cond ((and for-editor
                                   name-is-compatible)
                              (<-> " (#" blocknum ": " name8 ")"))
                             (for-editor
                              (<-> " (#" blocknum ")"))
                             (name-is-compatible
                              (<-> " (" name8 ")"))
                             (else
                              "")))
  (<-> "------------" text (cut-string-if-longer-than display-name
                                                      50)))

(define (get-audio-seqblock-popup-menu-entries seqblocknum seqtracknum seqblockid X)
  (define seqblock-info *current-seqblock-info*)
  (define seqblock-infos-under-mouse (get-curr-seqblock-infos-under-mouse #f))
  (list
   (get-seqblock-separator-text "Audio Seqblock" seqblockid)
   (map (lambda (automationnum)
          (create-seqblock-automation-popup-menu-entry automationnum seqblockid))
        (iota (<ra> :get-num-seqblock-automations seqblocknum seqtracknum)))
   
   "---------------------"

   (list (if (and seqblock-info
                  (= 1 (length seqblock-infos-under-mouse)))
             "Replace audio file"
             "Replace selected audio files")
         :enabled (not (null? seqblock-infos-under-mouse))
         :shortcut replace-seqblocks-with-existing-or-new-block-or-audiofile
         (lambda ()
           (replace-seqblocks-with-existing-or-new-block-or-audiofile seqblock-infos-under-mouse)))

   (list
    "Split audio file"
    :shortcut split-seqblock
    (lambda ()
      (let* ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X)))))
        (split-seqblock pos seqblockid))))
   
   "---------------------"

   (get-set-seqblock-gain-popup-menu-entries seqblockid)
   
   (let ((get-normalized-gain (lambda ()
                                (get-normalized-seqblock-gain seqblockid))))
     (list
      (<-> "Set normalized gain (" (db-to-text (<ra> :gain-to-db (get-normalized-gain)) #t) ")")
      (lambda ()
        (<ra> :set-seqblock-gain (get-normalized-gain) seqblockid))))

   ;;(list "Reset stretch"
   ;;      :enabled (and seqblocknum
   ;;                    (not (= 1.0 (<ra> :get-seqblock-stretch seqblocknum seqtracknum))))
   ;;      (lambda ()
   ;;        (c-display "stretch:" (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
   ;;        (define start-time (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
   ;;        (define blocklength (<ra> :get-block-length blocknum))
   ;;        (<ra> :position-seqblock start-time (+ start-time blocklength) seqblocknum seqtracknum)
   ;;        (c-display "hepp")))
    

   "---------------------"
   
   (list "Copy filename to system clipboard"
         (lambda ()
           (<ra> :copy-filepath-to-clipboard (<ra> :get-seqblock-sample seqblocknum seqtracknum))
           #t))
   
   (list "Settings"
         :shortcut "Double-click"
         (lambda ()
           (create-audio-seqblock-gui seqblocknum seqtracknum)))))

;; Note: used for shortcut
(delafina (config-seqblock-block :seqblock-id (and *current-seqblock-info*
                                                   (*current-seqblock-info* :id)))
  (when seqblock-id
    (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblock-id))
    (when (not (<ra> :seqtrack-for-audiofiles seqtracknum))
      (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblock-id))
      (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
      (<ra> :config-block blocknum))))


;; Note: used for shortcut
(delafina (clone-seqblock-block :seqblock-id (and *current-seqblock-info*
                                                  (*current-seqblock-info* :id))
                                :seqblock-infos (get-curr-seqblock-infos-under-mouse))
  (when seqblock-id
    (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblock-id))
    (when (not (<ra> :seqtrack-for-audiofiles seqtracknum))
      (when (and (not (null? seqblock-infos))
                 (not (<ra> :seqtrack-for-audiofiles (seqblock-infos 0 :seqtracknum))))
        (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblock-id))
        (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
        (undo-block
         (lambda ()
           (<ra> :select-block blocknum)
           (<ra> :copy-block)
           (for-each (lambda (seqblock-info)
                       (define new-blocknum (<ra> :append-block))
                       (<ra> :select-block new-blocknum)
                       (<ra> :paste-block))
                     seqblock-infos)))))))

;; Note: used for shortcut
(delafina (configure-seqblock-color :seqblock-id (and *current-seqblock-info*
                                                      (*current-seqblock-info* :id)))
  (when seqblock-id
    (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblock-id))
    (define seqblocknum (<ra> :get-seqblock-seqblock-num seqblock-id))
    (if (<ra> :seqtrack-for-audiofiles seqtracknum)
        (let ((filename (<ra> :get-seqblock-sample seqblocknum seqtracknum)))
          (<ra> :color-dialog (<ra> :get-audiofile-color filename #f) -1
                (lambda (color)
                  (<ra> :set-audiofile-color color filename))))
        (let ((blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum)))
          (<ra> :color-dialog (<ra> :get-block-color blocknum -1 #f) -1
                (lambda (color)
                  (<ra> :set-block-color color blocknum)))))))


(define (get-editor-seqblock-popup-menu-entries seqblock-infos seqblocknum seqtracknum seqblockid X)
  (define seqblock-info *current-seqblock-info*)
  (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
  (define seqblock-infos-under-mouse (get-curr-seqblock-infos-under-mouse #f seqblock-infos seqblock-info))
  (list
   (get-seqblock-separator-text "Editor Seqblock" seqblockid)
   (create-seqblock-automation-popup-menu-entry 0 seqblockid)
   
   "------------------------"
   
   ;;(list "Replace with current block"
   ;;      :enabled seqblock-info
   ;;      (lambda ()
   ;;        (undo-block
   ;;         (lambda ()
   ;;           (define pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
   ;;           (<ra> :delete-seqblock seqblocknum seqtracknum)                 
   ;;           (<ra> :add-block-to-seqtrack seqtracknum (<ra> :current-block) pos)))))

   ;; Doesn't work since current block is changed when right-clicking a seqblock.
   ;;(list (if (and seqblock-info
   ;;               (= 1 (length seqblock-infos-under-mouse)))
   ;;          "Replace with current block"
   ;;          "Replace selected blocks with current block")
   ;;      :enabled (not (null? seqblock-infos-under-mouse))
   ;;      :shortcut replace-seqblocks-with-current-block-or-audiofile
   ;;      (lambda ()
   ;;        (replace-seqblocks-with-current-block-or-audiofile seqblock-infos-under-mouse)))
   
   (list (if (and seqblock-info
                  (= 1 (length seqblock-infos-under-mouse)))
             "Replace block"
             "Replace selected blocks")
         :enabled (not (null? seqblock-infos-under-mouse))
         :shortcut replace-seqblocks-with-existing-or-new-block-or-audiofile
         (lambda ()
           (replace-seqblocks-with-existing-or-new-block-or-audiofile seqblock-infos-under-mouse)))
   
   ;;   Sub menues version. It looks better, but it is less convenient.
   ;;(list "Replace with existing block"
   ;;      :enabled seqblock-info
   ;;      (if seqblock-info
   ;;          (let ((pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
   ;;            (map (lambda (blocknum)
   ;;                   (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
   ;;                         (lambda ()
   ;;                           (undo-block
   ;;                            (lambda ()
   ;;                              (<ra> :delete-seqblock seqblocknum seqtracknum)
   ;;                              (<ra> :add-block-to-seqtrack seqtracknum blocknum pos)))
   ;;                           (<ra> :select-block blocknum))))                                                         
   ;;                 (iota (<ra> :get-num-blocks))))
   ;;          (lambda ()
   ;;            #f)))
   
   ;; Doesn't make sense since we select block under mouse when right-clicking on it.
   ;;(list "Replace with current block"
   ;;      :enabled seqblock-info
   ;;      (lambda ()
   ;;        (let ((pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
   ;;              (blocknum (<ra> :current-block)))
   ;;          (undo-block
   ;;           (lambda ()
   ;;             (<ra> :delete-seqblock seqblocknum seqtracknum)
   ;;             (<ra> :add-block-to-seqtrack seqtracknum blocknum pos)))
   ;;          (<ra> :select-block blocknum))))

   ;; Put this functionality into replace-seqblocks
   ;;(list (if (pair? seqblock-infos) "Replace blocks with new block" "Replace with new block")
   ;;      :enabled (or (pair? seqblock-infos)
   ;;                   seqblock-info)
   ;;      (lambda ()                                                                 
   ;;        (undo-block
   ;;         (lambda ()
   ;;           (let ((blocknum (<ra> :append-block)))
   ;;             (for-each (lambda (seqblock-info)
   ;;                         (let* ((seqblocknum (seqblock-info :seqblocknum))
   ;;                                (seqtracknum (seqblock-info :seqtracknum))
   ;;                                (pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
   ;;                           (set! *current-seqblock-info* #f)
   ;;                           (<ra> :delete-seqblock (seqblock-info :id))
   ;;                           (<ra> :create-seqblock seqtracknum blocknum pos)))
   ;;                       (if (null? seqblock-infos)
   ;;                           (list seqblock-info)
   ;;                           seqblock-infos))
   ;;             (<ra> :select-block blocknum))))))
   
   
   (list "Seqblock track on/off editor" ;;Enable/disable editor tracks (double click)"
         :enabled (and blocknum seqblocknum)
         :shortcut "Double-click"
         (lambda ()
           (show-seqblock-track-on-off-configuration seqblockid)))

   "------------------------"
   
   (get-set-seqblock-gain-popup-menu-entries seqblockid)
   
   "------------------------"
   
   (list "Advanced"
         (list
          (list "Copy track on/off editor => seqblock"
                :enabled (and blocknum seqblocknum)
                ra:copy-editor-track-on-off-to-seqblock)
          
          (list "Copy track on/off seqblock => editor"
                :enabled (and blocknum seqblocknum)
                ra:copy-seqblock-track-on-off-to-editor)
                   
          (list "Configure block"
                :enabled (and blocknum seqblock-info (not (<ra> :is-playing-song)))
                :shortcut config-seqblock-block
                (lambda ()
                  (config-seqblock-block seqblockid)))
          
          "-----------------------------"
          (list (<-> "Clone (create new block" (if (pair? seqblock-infos) "s" "") " from selected block " (if (pair? seqblock-infos) "s" "") ")")
                :enabled (and blocknum
                              (not (null? seqblock-infos-under-mouse))
                              (not (<ra> :is-playing-song)))
                :shortcut clone-seqblock-block
                (lambda ()
                  (clone-seqblock-block seqblockid seqblock-infos-under-mouse)))
          ))
   )
  )
   



(define (get-seqblock-popup-menu-entries seqblock-infos seqblocknum seqtracknum seqblockid X Y)
  (define seqblock-info *current-seqblock-info*)
  (define blocknum (and seqblock-info
                        (<ra> :seqblock-holds-block seqblocknum seqtracknum)
                        (<ra> :get-seqblock-blocknum seqblocknum seqtracknum)))
  
  (define is-selected (and seqblocknum
                           (<ra> :is-seqblock-selected seqblocknum seqtracknum)))
  (define num-selected (<ra> :get-num-selected-seqblocks))
  (define num-selected-with-current (+ num-selected
                                       (if (or (not seqblocknum)
                                               is-selected)
                                           0
                                           1)))
  (list

   (if blocknum
       (get-editor-seqblock-popup-menu-entries seqblock-infos seqblocknum seqtracknum seqblockid X)
       (get-audio-seqblock-popup-menu-entries seqblocknum seqtracknum seqblockid X))

   (if (> num-selected-with-current 1)       
       "-----------Seqblocks (selection)"
       (get-seqblock-separator-text "Seqblock" seqblockid))
   
   (list "Copy"
         :enabled (> num-selected-with-current 0)
         :shortcut ra:copy-selected-seqblocks
         (lambda ()
           (if (and seqblocknum
                    (not (<ra> :is-seqblock-selected seqblocknum seqtracknum)))
               (<ra> :select-seqblock #t seqblocknum seqtracknum))
           (<ra> :copy-selected-seqblocks)))
   ;;(copy-blocks-to-clipboard (list (make-seqblock-info2 seqtracknum seqblocknum))))))
   
   (list "Cut"
         :enabled (> num-selected-with-current 0)
         :shortcut ra:cut-selected-seqblocks
         (lambda ()
           (if (and seqblocknum
                    (not (<ra> :is-seqblock-selected seqblocknum seqtracknum)))
               (<ra> :select-seqblock #t seqblocknum seqtracknum))
           (<ra> :cut-selected-seqblocks)))
   
   (list "Delete all selected"
         :enabled (> num-selected-with-current 1)
         :shortcut ra:delete-selected-seqblocks
         (lambda ()
           (if (and seqblocknum
                    (not (<ra> :is-seqblock-selected seqblocknum seqtracknum)))
               (<ra> :select-seqblock #t seqblocknum seqtracknum))
           (<ra> :delete-selected-seqblocks)))
   
   (list "Paste"
         :enabled (not (empty? *seqblock-clipboard*))
         :shortcut ra:paste-seqblocks
         (lambda ()
           (let ((pos (<ra> :get-seq-gridded-time (round (get-sequencer-time X)))))
             (<ra> :paste-seqblocks seqtracknum pos))))
   
   "------------------"
   
   (list
    "Set name"
    :enabled seqblock-info
    :shortcut show-set-seqtrack/seqblock-name-requester
    (lambda ()
      (show-set-seqtrack/seqblock-name-requester seqtracknum seqblockid)))
   
   (list "Configure color"
         :enabled seqblock-info
         :shortcut configure-seqblock-color
         (lambda ()
           (configure-seqblock-color seqblockid)))
   
   (list "Generate new color"
         :enabled seqblock-info
         :shortcut ra:generate-new-color-for-all-selected-seqblocks
         (lambda ()
           (let ((color (<ra> :generate-new-block-color 1.0)))
             (if blocknum
                 (<ra> :set-block-color color blocknum)
                 (let ((filename (<ra> :get-seqblock-sample seqblocknum seqtracknum)))
                   (<ra> :set-audiofile-color color filename))))))
   
   (list "Delete current"
         :shortcut ra:delete-seqblock ;;*shift-right-mouse*
         :enabled seqblock-info
         (lambda ()
           (set! *current-seqblock-info* #f)
           (<ra> :delete-seqblock seqblockid)
           (set! *current-seqblock-info* #f)))

   "---------------------"
   
   (list "Select previous seqblock"
         ra:select-prev-seqblock)
   (list "Select next seqblock"
         ra:select-next-seqblock)

   (get-main-sequencer-popup-menu-entries seqtracknum X Y)
   ))



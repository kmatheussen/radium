
(provide 'seqtrack-headers.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)
(my-require 'sequencer.scm)


;;(define *curr-seqtrack-color* "#7c3a3a")
;;(define *curr-seqtrack-color* "#776757")
(define *curr-seqtrack-color* (ra:gui_mix-colors *current-mixer-strip-border-color* "black" 0.6))
;;(define *curr-seqtrack-color* (<gui> :mix-colors *current-mixer-strip-border-color* "white" 0.92))

(define (get-normalized-seqblock-gain seqblockid)
  (let ((max-gain (<ra> :get-max-seqblock-sample-gain seqblockid)))
    (if (> max-gain 0)
        (/ 1.0 max-gain)
        100)))

(define (get-seqtrack-background-color gui seqtracknum)
  (if (not (<ra> :seqtrack-for-audiofiles seqtracknum))
      (if (= seqtracknum (<ra> :get-curr-seqtrack))
          *curr-seqtrack-color*
          (get-instrument-background-color gui -1))
      (begin
        (define instrument-id (<ra> :get-seqtrack-instrument seqtracknum))
        (let ((background-color (get-mixer-strip-background-color gui instrument-id)))
          (if (= seqtracknum (<ra> :get-curr-seqtrack))
              (<gui> :mix-colors
                     (<gui> :mix-colors *curr-seqtrack-color* background-color 0.2)
                     "white"
                     0.95)
              background-color)))))

(define (get-sequencer-header-popup-menu-entries seqtracknum instrument-id effect-name parentgui)
  (list
   (and effect-name
        (get-effect-popup-entries instrument-id effect-name))
   
   (get-instrument-popup-entries instrument-id parentgui :include-delete-and-replace #f)
   
   "------------Seqtrack"
   (get-seqtrack-popup-menu-entries seqtracknum)))
   
(define (show-sequencer-header-popup-menu seqtracknum instrument-id effect-name parentgui)
  (if (and effect-name
           (<ra> :shift-pressed))
      (begin
        (c-display "Resetting " effect-name)
        (<ra> :reset-instrument-effect instrument-id effect-name))
      (<ra> :schedule 0
            (lambda ()
              (popup-menu (get-sequencer-header-popup-menu-entries seqtracknum instrument-id effect-name parentgui))
              #f))))

(define *sequencer-window-gui* (if (defined? '*sequencer-window-gui*)
                                   *sequencer-window-gui*
                                   #f))
(define *sequencer-window-gui-active* (if (defined? '*sequencer-window-gui-active*)
                                          *sequencer-window-gui-active*
                                          #f))
(define *lower-tabs-height-before-full-or-active* 10)

(define (remember-lower-tabs-height)
  ;;(c-display "|||||||||||||| ========REMEMBER lower tabs height:" (<gui> :height *lowertab-gui*))
  (set! *lower-tabs-height-before-full-or-active* (<gui> :height *lowertab-gui*)))

(define (recall-lower-tabs-height)
  ;;(c-display "|||||||||||||| ========recall lower tabs height:" *lower-tabs-height-before-full-or-active*)
  (<gui> :set-size *lowertab-gui* (<gui> :width *lowertab-gui*) *lower-tabs-height-before-full-or-active*))
  

;; Remove from main tabs, and open in new window
(define (move-sequencer-to-window)
  (assert (not *sequencer-window-gui-active*))
  (set! *sequencer-window-gui-active* #t)
  
  (let* ((sequencer-gui (<gui> :get-sequencer-gui))
         (has-window *sequencer-window-gui*)
         (window (if has-window
                     *sequencer-window-gui*
                     (let ((window (<gui> :vertical-layout)))
                       (<gui> :set-size
                              window
                              (<gui> :width (<gui> :get-parent-window sequencer-gui))
                              (+ (floor (* (get-fontheight) 1.5))
                                 (<gui> :height sequencer-gui)))
                       (<gui> :set-takes-keyboard-focus window #f)
                       window))))
    
    (<gui> :remove-parent sequencer-gui)
    
    (define bottom-bar (if has-window
                           (let ((bottom-bar (<gui> :child window "bottom-bar")))
                             (<gui> :remove-parent bottom-bar)
                             bottom-bar)
                           (let ((bottom-bar (<gui> :bottom-bar #f #f)))
                             (<gui> :set-name bottom-bar "bottom-bar")
                             bottom-bar)))
    
    (<gui> :add window sequencer-gui)
    (<gui> :add window bottom-bar)
    
    (<gui> :set-as-window window (if (<ra> :sequencer-window-is-child-of-main-window)
                                     -1
                                     -3
                                     ))
    (<gui> :show sequencer-gui)
    (<gui> :show window)
    
    (when (not has-window)

      (<gui> :add-close-callback window
             (lambda (radium-runs-custom-exec)
               ;;(move-sequencer-to-main-tabs)
               (<gui> :hide window)
               #f))
      
      (set! *sequencer-window-gui* window))
    
    ))


;; Remove from sequencer window, add to main tabs, hide sequencer window gui.
(define (move-sequencer-to-main-tabs)
  (assert *sequencer-window-gui-active*)    
  (set! *sequencer-window-gui-active* #f)
    
  (let ((sequencer-gui (<gui> :get-sequencer-gui))
        (window *sequencer-window-gui*))
    (<gui> :remove-parent sequencer-gui)
    (<gui> :add-tab *lowertab-gui* *sequencer-gui-tab-name* sequencer-gui 0)
    (<gui> :set-current-tab *lowertab-gui* 0)
    (<gui> :hide window)))


#!!
(<ra> :show-upper-part-of-main-window)
(<ra> :hide-upper-part-of-main-window)
!!#


(define *has-shown-record-message* #t)
(define (maybe-show-record-message)
  (when (not *has-shown-record-message*)
    (show-async-message :text "Recording audio is a technology preview. It seems to work fine, but it could have some bugs.<p>Current limitations:<UL><LI>You can only record from the input connections of the seqtrack instrument,<br>not from the main inputs of the program. (For now you have to manually<br>connect a \"System In\" object to the seqtrack instrument.)<LI>You can only record stereo files.<LI>There is no punch in and punch out yet.</UL>")
    (set! *has-shown-record-message* #t)))


(define (get-num-recording-channels seqtracknum)
  (define ret 0)
  (for-each (lambda (input-channel)
              (for-each (lambda (soundfile-channel)
                          (if (<ra> :get-seqtrack-recording-matrix seqtracknum input-channel soundfile-channel)
                              (set! ret (max ret (1+ soundfile-channel)))))
                        (iota 8)))
            (iota 8))
  ret)

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



;; There's a lot of copy-paste code from mixer-strip.scm:create-mixer-strip-mutesolo here, but I hope this code will eventually replace mixer-strip.scm:create-mixer-strip-mutesolo some day
(def-area-subclass (<mute-solo-buttons> :gui :x1 :y1 :x2 :y2
                                        :instrument-id
                                        :use-single-letters 
                                        :stack-horizontally
                                        :seqtracknum)
  
  (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define for-blocks (not for-audiofiles))
  
  (define volume-on-off-name (and for-audiofiles
                                  (get-instrument-volume-on/off-effect-name instrument-id)))

  (define (get-muted)
    (if for-audiofiles
        (< (<ra> :get-instrument-effect instrument-id volume-on-off-name) 0.5)
        (<ra> :get-editor-seqtrack-muted seqtracknum)))
  (define (get-soloed)
    (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
  (define (get-recording)
    (<ra> :seqtrack-is-recording seqtracknum))
  
  (define (turn-off-all-mute except)
    (for-each (lambda (instrument-id)
                (when (and (not (= instrument-id except))
                           (< (<ra> :get-instrument-effect instrument-id volume-on-off-name) 0.5))
                  (<ra> :undo-instrument-effect instrument-id volume-on-off-name)
                  (<ra> :set-instrument-effect instrument-id volume-on-off-name 1)
                  ))
              (get-all-audio-instruments)))
  
  (define (turn-off-all-solo except)
    (for-each (lambda (instrument-id)
                (when (and (not (= instrument-id except))
                           (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
                  ;;(<ra> :undo-instrument-effect instrument-id "System Solo On/Off")
                  (set-instrument-solo! instrument-id #f)
                  ))
              (get-all-audio-instruments)))
  
  (define last-drawn-implicitly-muted (and for-audiofiles
                                           (<ra> :instrument-is-implicitly-muted instrument-id)))
  
  (when for-audiofiles
    (<ra> :schedule (random 1000) (lambda ()
                                    (and is-alive (<gui> :is-open gui) (<ra> :instrument-is-open-and-audio instrument-id)
                                         (begin
                                           (if (not (eq? last-drawn-implicitly-muted
                                                         (<ra> :instrument-is-implicitly-muted instrument-id)))
                                               (update-me!))
                                           100))))
    
    (add-area-effect-monitor! instrument-id volume-on-off-name #t #t
                              (lambda (on/off automation)
                                (update-me!)))
    
    (add-area-effect-monitor! instrument-id "System Solo On/Off" #t #t
                              (lambda (on/off automation)
                                ;;(c-display "Solo changed for" instrument-id)
                                (update-me!))))
  
  (define (get-selected type)
    (cond ((eq? type 'height)
           (seqtrack-size-gui-open? seqtracknum))
          ((eq? type 'record)
           (get-recording))
          ((eq? type 'solo)
           (get-soloed))
          ((eq? type 'mute)
           (get-muted))
          (else
           (assert #f))))
  
  (define layout-func (if stack-horizontally
                          horizontally-layout-areas
                          vertically-layout-areas))
  
  (layout-func x1 y1 x2 y2
               (if for-audiofiles
                   '(record mute solo height)
                   '(mute height))
               :spacing -1
               :callback
               (lambda (type x1 y1 x2 y2)
                 ;;(c-display "   BOX:" type x1 y1 x2 y2)
                 (define box (<new> :checkbox gui x1 y1 x2 y2
                                    (lambda ()
                                      (get-selected type))
                                    (lambda (is-selected)
                                      (undo-block
                                       (lambda ()
                                         (cond ((eq? type 'height)
                                                (show-seqtrack-height-gui seqtracknum #t))
                                               ((eq? type 'record)
                                                (if (and (not (get-recording))
                                                         (not (*open-record-config-windows* seqtracknum)))
                                                    (maybe-show-record-message))
                                                (cond ((= 0 (get-num-recording-channels seqtracknum))
                                                       (<ra> :show-async-message "Must select at least one recording channel"))
                                                      ((and (<ra> :release-mode)
                                                            (not (<ra> :has-session)))
                                                       (<ra> :show-async-message "Session is required to record audio files. Please save song first."))
                                                      (else
                                                       (<ra> :set-seqtrack-is-recording seqtracknum is-selected)))
                                                )
                                               ((eq? type 'solo)
                                                (<ra> :set-instrument-solo instrument-id is-selected)
                                                (if (<ra> :control-pressed)
                                                    (turn-off-all-solo instrument-id)))
                                               ((eq? type 'mute)
                                                (if for-audiofiles
                                                    (<ra> :set-instrument-mute instrument-id is-selected)
                                                    (<ra> :set-editor-seqtrack-muted is-selected seqtracknum))
                                                ;;(c-display "mute: " is-muted)
                                                (if (and for-audiofiles
                                                         (<ra> :control-pressed))
                                                    (turn-off-all-mute instrument-id)))
                                               (else
                                                (assert #f))))))
                                    :paint-func
                                    (lambda (is-selected)
                                      (if (eq? type 'solo)
                                          (set! last-drawn-implicitly-muted (<ra> :instrument-is-implicitly-muted instrument-id)))
                                      (draw-mutesolo gui
                                                     type
                                                     instrument-id
                                                     x1 y1 x2 y2
                                                     is-selected
                                                     use-single-letters
                                                     :background-color (get-seqtrack-background-color gui seqtracknum)
                                                     :border 0
                                                     :implicit-border 3
                                                     ))
                                    :right-mouse-clicked-callback (lambda ()
                                                                    (cond ((eq? type 'height)
                                                                           (show-seqtrack-height-gui seqtracknum #f))
                                                                          ((eq? type 'record)
                                                                           (show-record-popup-menu seqtracknum))
                                                                          ((eq? type 'solo)
                                                                           (show-sequencer-header-popup-menu seqtracknum instrument-id "System Solo On/Off" gui))
                                                                          ((eq? type 'mute)
                                                                           (if for-audiofiles
                                                                               (show-sequencer-header-popup-menu seqtracknum instrument-id "System Volume On/Off" gui)
                                                                               (popup-menu "Reset"
                                                                                           (lambda ()
                                                                                             (<ra> :set-editor-seqtrack-muted #f seqtracknum)
                                                                                             (update-me!)))))
                                                                          (else
                                                                           (assert #f))))))

                 (if (eq? type 'record)
                     (box :add-statusbar-text-handler "Right-click the \"R\" button to configure recording options."))
                 (add-sub-area-plain! box)))

  )


(delafina (create-seqtrack-name-area :gui :x1 :y1 :x2 :y2
                                     :instrument-id
                                     :seqtracknum)

  (define last-painted-name (<ra> :get-seqtrack-name seqtracknum))

  (define line-input (<new> :line-input gui x1 y1 x2 y2 
                            :prompt "New name:"
                            :text last-painted-name
                            :background-color (lambda ()
                                                (if (= seqtracknum (<ra> :get-curr-seqtrack))
                                                    (get-seqtrack-background-color gui seqtracknum)
                                                    (get-instrument-background-color gui instrument-id)))
                            :callback (lambda (new-name)
                                        (if (string=? "" new-name)
                                            #f
                                            (begin
                                              (set! last-painted-name new-name)
                                              (<ra> :set-seqtrack-name new-name seqtracknum)
                                              new-name)))))

  (if (>= instrument-id 0)
      (<ra> :schedule (random 1000)
            (lambda ()
              (and (line-input :is-alive)
                   (<gui> :is-open gui)
                   (<ra> :instrument-is-open-and-audio instrument-id)
                   (begin
                     (if (not (string=? last-painted-name (<ra> :get-instrument-name instrument-id)))
                         (line-input :update-me!))
                     300)))))
  
  line-input)

#||                                        
(def-area-subclass (<seqtrack-name> :gui :x1 :y1 :x2 :y2
                                    :instrument-id
                                    :seqtracknum)
  
  (define last-painted-name "")

  (add-sub-area-plain! (<new> :text-area gui x1 y1 x2 y2
                              :text (lambda ()
                                      (set! last-painted-name (<ra> :get-seqtrack-name seqtracknum))
                                      last-painted-name)
                              :background-color
                              (lambda ()
                                (if (= seqtracknum (<ra> :get-curr-seqtrack))
                                    (get-seqtrack-background-color gui seqtracknum)
                                    (get-instrument-background-color gui instrument-id)))
                              :align-left #t))
  
  '(define-override (post-paint)
    (define background-color (<gui> :get-background-color gui))
    (<gui> :draw-box gui background-color (+ 0 x1) (+ 0 y1) (- x2 0) (- y2 0) 2 0 0)
    (<gui> :draw-box gui *mixer-strip-border-color* x1 y1 x2 y2 1.5 5 5)
    )

  (if (>= instrument-id 0)
      (<ra> :schedule (random 1000)
            (lambda ()
              (and is-alive (<gui> :is-open gui) (<ra> :instrument-is-open-and-audio instrument-id)
                   (begin
                     (if (not (string=? last-painted-name (<ra> :get-instrument-name instrument-id)))
                         (update-me!))
                     300)))))
  
  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (and (= button *left-button*)
                                       (<ra> :schedule 0
                                             (lambda ()
                                               (let* ((old-name (<ra> :get-seqtrack-name seqtracknum))
                                                      (new-name (<ra> :request-string "New name:" #t old-name)))
                                                 (c-display "GAKKKGAKK_________ NEWNAME" (<-> "-" new-name "-"))
                                                 (when (and (not (string=? new-name ""))
                                                            (not (string=? new-name old-name)))
                                                   (<ra> :set-seqtrack-name new-name seqtracknum)
                                                   (update-me!))
                                                 #f)))
                                       #t)))
  )
||#

(def-area-subclass (<instrument-volume-slider> :gui :x1 :y1 :x2 :y2
                                               :instrument-id
                                               :effect-name "System Volume"
                                               :display-instrument-name #t
                                               :get-color
                                               :seqtracknum)
  (define has-made-undo #f)
  
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (set! has-made-undo #t)
      (<ra> :undo-instrument-effect instrument-id effect-name)))

  (define (get-radium-normalized)
    (<ra> :get-stored-instrument-effect instrument-id effect-name))

  (define (get-db-value radium-normalized)
    (radium-normalized-to-db radium-normalized))

  (define (set-db-value db)    
    ;;(c-display "      -------set-db-val:" (db-to-radium-normalized db))
    (<ra> :set-instrument-effect instrument-id effect-name (db-to-radium-normalized db)))
  
  (define (get-scaled-value radium-normalized)
    (db-to-slider (get-db-value radium-normalized)))

  (define (get-volume-slider-value-text value)
    (db-to-text (slider-to-db value) #t))
  
  (define (get-volume-slider-text radium-normalized)
    (define midi-learn-text (if (<ra> :instrument-effect-has-midi-learn instrument-id effect-name)
                                "[M] "
                                ""))
    (let ((volume-text (get-volume-slider-value-text (get-scaled-value radium-normalized))))
      (if display-instrument-name
          (let ((instrument-name (<ra> :get-instrument-name instrument-id)))
            (<-> midi-learn-text instrument-name ": " volume-text))
          (<-> " " midi-learn-text volume-text))))
  
  (define last-painted-radium-normalized -10000)

  (define automation-value #f)
  (define automation-color (<ra> :get-instrument-effect-color instrument-id effect-name))
  (define (get-automation-data kont)
    (if automation-value
        (kont automation-value automation-color)))

  (add-area-effect-monitor! instrument-id effect-name #t #t
                            (lambda (radium-normalized automation)
                              (when radium-normalized
                                (when (> (abs (- radium-normalized last-painted-radium-normalized))
                                         0.0001)
                                  (update-me!)))
                              (when automation
                                (set! automation-value (if (< automation 0)
                                                           #f
                                                           (radium-normalized-to-slider automation)))
                                (update-me!))))
  
  (define-override (paint)
    (define b 1)
    (define radium-normalized (get-radium-normalized))
    (set! last-painted-radium-normalized radium-normalized)

    (paint-horizontal-instrument-slider gui
                                        instrument-id
                                        (get-scaled-value radium-normalized)
                                        (get-volume-slider-text radium-normalized)
                                        #t
                                        #f
                                        get-automation-data
                                        (+ b x1)
                                        (+ b x1) (+ b y1) (- x2 b) (- y2 b)
                                        (get-color)
                                        ))
  
  (define start-mouse-value #f)
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set! has-made-undo #f)
     (cond ((= button *right-button*)
            (show-sequencer-header-popup-menu seqtracknum instrument-id "System Volume" gui)
            'eat-mouse-cycle)
           ((= button *left-button*)
            (define radium-normalized (get-radium-normalized))
            (set! start-mouse-value (get-scaled-value radium-normalized));;(scale x* x1 x2 0 1));;(get-db-value));;(<ra> :get-stored-instrument-effect instrument-id effect-name))
            ;;(c-display "press button/x/y" x* y*)
            (set-statusbar-text! (get-statusbar-text))
            #t)
           (else
            #f)))
   (lambda (button x* y* dx dy)
     (assert start-mouse-value)
     (maybe-make-undo)
     (define slider-value (between 0 (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                   1))
     (set-db-value (slider-to-db slider-value))
     (set-statusbar-text! (get-statusbar-text))
     (update-me!)
     )
   (lambda (button x* y* dx dy)
     ;;(c-display "release button/x/y" x* y*)
     #f
     ))
  
  (define (get-statusbar-text)
    (get-volume-slider-text (get-radium-normalized)))
  
  (add-statusbar-text-handler get-statusbar-text)
                                

  '(define (mouse-callback button state x y)
    (if (and (>= x x1)
             (< x x2)
             (>= y y1)
             (< y y2))
        (let ((status-text (get-volume-slider-text (get-radium-normalized))))
          ;;(c-display "hepp " status-text)
          (<ra> :set-statusbar-text status-text)
          )))

  )


(def-area-subclass (<editor-seqtrack-volume-slider> :gui :x1 :y1 :x2 :y2
                                                    :display-instrument-name #t
                                                    :get-color
                                                    :seqtracknum)
  (define has-made-undo #f)
  
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (set! has-made-undo #t)
      (<ra> :undo-seqtrack-note-gain seqtracknum)))

  (define (get-gain)
    (<ra> :get-seqtrack-note-gain seqtracknum))
  
  (define (get-db-value gain)
    (<ra> :gain-to-db gain))

  (define (set-db-value db)    
    ;;(c-display "      -------set-db-val:" (<ra> :db-to-gain db))
    (<ra> :set-seqtrack-note-gain (<ra> :db-to-gain db) seqtracknum))
  
  (define (get-scaled-value gain)
    (db-to-slider (get-db-value gain)))

  (define (get-volume-slider-value-text gain)
    (db-to-text (get-db-value gain) #t))
  
  (define (get-volume-slider-text gain)
    (let ((volume-text (get-volume-slider-value-text gain)))
      (if display-instrument-name
          (let ((instrument-name (<ra> :get-seqtrack-name seqtracknum)))
            (<-> instrument-name ": " volume-text))
          (<-> " " volume-text))))
  
  (define last-painted-gain -10000)

  (define-override (paint)
    (define b 1)
    (define gain (get-gain))
    (set! last-painted-gain gain)

    (paint-horizontal-slider :widget gui
                             :value (get-scaled-value gain)
                             :text (get-volume-slider-text gain)
                             :x1 (+ b x1)
                             :y1 (+ b y1)
                             :x2 (- x2 b)
                             :y2 (- y2 b)
                             :color (if (procedure? get-color)
                                        (get-color)
                                        get-color)
                             :is-enabled #t
                             :is-current #f
                             :get-automation-data #f
                             :text-x1 (+ b x1)
                             ))
  
  (define start-mouse-value #f)
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set! has-made-undo #f)
     (cond ((= button *right-button*)
            (popup-menu "Reset"
                        (lambda ()
                          (<ra> :undo-seqtrack-note-gain seqtracknum)
                          (<ra> :set-seqtrack-note-gain 1.0 seqtracknum)
                          (update-me!)))
            'eat-mouse-cycle)
           ((= button *left-button*)
            (define gain (get-gain))
            (set! start-mouse-value (get-scaled-value gain));;(scale x* x1 x2 0 1));;(get-db-value));;(<ra> :get-stored-instrument-effect instrument-id effect-name))
            ;;(c-display "press button/x/y" x* y*)
            (set-statusbar-text! (get-statusbar-text))
            #t)
           (else
            #f)))
   (lambda (button x* y* dx dy)
     (maybe-make-undo)
     (define slider-value (between 0 (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                   1))
     (set-db-value (slider-to-db slider-value))
     (set-statusbar-text! (get-statusbar-text))
     (update-me!)
     )
   (lambda (button x* y* dx dy)
     ;;(c-display "release button/x/y" x* y*)
     #f
     ))
  
  (define (get-statusbar-text)
    (get-volume-slider-text (get-gain)))
  
  (add-statusbar-text-handler get-statusbar-text)
                                
  )

#!!
(let ()
  (define (recreate gui width height state)
    (<new> :editor-seqtrack-volume-slider
           gui 0 0 width height
           :display-instrument-name #t
           :get-color "green"
           :seqtracknum 0))

  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback recreate))
  (<gui> :show (testarea :get-gui))
  )

(<ra> :get-seqtrack-note-gain 0)
!!#


(def-area-subclass (<instrument-pan-slider> :gui :x1 :y1 :x2 :y2
                                            :instrument-id
                                            :get-color
                                            :seqtracknum)

  (define (pan-enabled?)
    (>= (<ra> :get-instrument-effect instrument-id "System Pan On/Off") 0.5))

  (define (get-pan-slider-value normalized-value)
    (floor (scale normalized-value
                  0 1
                  -90 90)))
    
  (define (get-pan)
    (get-pan-slider-value (<ra> :get-stored-instrument-effect instrument-id "System Pan")))

  (define (get-pan-slider-text pan)
    (<-> "Pan: " (round pan)))

  (define last-painted-normalized-pan -1000)
  (define automation-slider-value -1000)
  (define pan-automation-color (<ra> :get-instrument-effect-color instrument-id "System Pan"))

  (define-override (paint)
    (define value (get-pan))
    (set! last-painted-normalized-pan (scale value -90 90 0 1))
    (define is-on (pan-enabled?))
    ;;(<gui> :filled-box gui (get-color) x1 y1 x2 y2)
    (define background-color (get-color))
    (define background (if is-on
                           (<gui> :mix-colors background-color "black" 0.39)
                           (<gui> :mix-colors background-color "white" 0.95)))
    (<gui> :filled-box gui background x1 y1 x2 y2 5 5 #f)
    (define col1 (<gui> :mix-colors "white" background 0.4))
    (define col2 (<gui> :mix-colors "#010101" background 0.5))
    
    (define inner-width/2 (scale 1 0 18 0 (get-fontheight)))
    (define outer-width/2 (* inner-width/2 2))
    
    (define middle (scale value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))
    
    (<gui> :filled-box gui col1 (+ x1 (- middle inner-width/2))               (+ y1 2) (+ x1 middle inner-width/2)               (- y2 3) -1 -1 #f)
    (<gui> :filled-box gui col2 (+ x1 (- middle inner-width/2 outer-width/2)) (+ y1 2) (+ x1 (- middle inner-width/2))           (- y2 3) -1 -1 #f)
    (<gui> :filled-box gui col2 (+ x1 (+ middle inner-width/2))               (+ y1 2) (+ x1 middle inner-width/2 outer-width/2) (- y2 3) -1 -1 #f)
    ;;(<gui> :draw-text gui "white" (<-> value "o") 0 0 width height #t)
    
    (when (> automation-slider-value -100)
      (define middle (scale automation-slider-value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))
      (<gui> :draw-line gui pan-automation-color (+ x1 middle) (+ y1 2) (+ x1 middle) (- y2 3) 2.0))
    
    (<gui> :draw-box gui "#404040" x1 y1 x2 y2 2)
    
    (when (<ra> :instrument-effect-has-midi-learn instrument-id "System Pan")
      (define midi-learn-color (<gui> :mix-colors *text-color* background 0.2))
      (<gui> :draw-text gui midi-learn-color "[M]" (+ x1 2) (+ y1 2) (- x2 2) (- y2 2)
             #f ;; wrap text
             #f ;; align left
             #f ;; align top
             0 ;; rotate
             #f ;; cut text to fit
             #t ;; scale font size
             )))
    
  (add-area-effect-monitor! instrument-id "System Pan" #t #t
                            (lambda (normalized-value automation)
                              (when normalized-value
                                (if (> (abs (- last-painted-normalized-pan normalized-value))
                                       0.001)
                                    (update-me!)))
                              (when automation
                                (if (< automation 0)
                                    (set! automation-slider-value -100)
                                    (set! automation-slider-value (get-pan-slider-value automation)))
                                (update-me!))))
  
  (add-area-effect-monitor! instrument-id "System Pan On/Off" #t #t
                            (lambda (on/off automation)
                              (update-me!)))
  
  (define has-made-undo #t)

  (define (maybe-make-undo)
    (when (not has-made-undo)
      (undo-block
       (lambda ()
         (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
         (<ra> :undo-instrument-effect instrument-id "System Pan")))
      (set! has-made-undo #t)))

  (define (set-new-value! pan)
    (<ra> :set-instrument-effect instrument-id "System Pan On/Off" 1.0)
    (<ra> :set-instrument-effect instrument-id "System Pan" (scale pan -90 90 0 1)))

  (define (enable! onoff)
    (when (not (eq? onoff (pan-enabled?)))
      (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
      (<ra> :set-instrument-effect instrument-id "System Pan On/Off" (if onoff 1.0 0.0))))

  (define start-mouse-value #f)

  (define (show-popup)
    (<ra> :schedule 0 ;; Workaround. Opening a popup menu causes Qt to skip the drag and release mouse events.
          (lambda ()
            (define pan-enabled (pan-enabled?))
            (popup-menu ;;(list "Reset Pan" (lambda ()
             ;;                (<ra> :undo-instrument-effect instrument-id "System Pan")
             ;;                (<ra> :set-instrument-effect instrument-id "System Pan" 0.5)))
             (list "Pan Enabled"
                   :check pan-enabled
                   enable!)
             (get-sequencer-header-popup-menu-entries seqtracknum instrument-id "System Pan" gui))
            #f)))
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set! has-made-undo #f)
     (cond ((and (= button *right-button*)
                 #t );;(not (<ra> :shift-pressed)))
            (if (<ra> :shift-pressed)
                (undo-block
                 (lambda ()                  
                   (<ra> :reset-instrument-effect instrument-id "System Pan On/Off")
                   (<ra> :reset-instrument-effect instrument-id "System Pan")))
                (show-popup))
            'eat-mouse-cycle)
           ((= button *left-button*)
            (define pan (get-pan))
            (set! start-mouse-value pan)
            ;;(c-display "Start value:" pan)
            ;;(c-display "press button/x/y" x* y*)
            (set-statusbar-text! (get-pan-slider-text pan))
            #t)
           (else
            #f)))
   (lambda (button x* y* dx dy)
     (maybe-make-undo)
     (define slider-value (between -90 (+ start-mouse-value
                                          (scale dx 0 width 0 180))
                                   90))
     ;;(c-display "start:" start-mouse-value ". dp:" (scale dx 0 width 0 180) ". New:" slider-value)
     (set-new-value! slider-value)
     (set-statusbar-text! (get-pan-slider-text slider-value))
     (update-me!)
     )
   (lambda (button x* y* dx dy)
     (c-display "Gakk. xrelease button/x/y" x* y*)))


  (define (get-statusbar-text)
    (get-pan-slider-text (get-pan)))
  
  (add-statusbar-text-handler get-statusbar-text)
                                
  )


(def-area-subclass (<seqtrack-header> :gui :x1 :y1 :x2 :y2
                                      :use-two-rows
                                      :show-panner
                                      :seqtracknum)

  (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define for-blocks (not for-audiofiles))
  (define instrument-id (if for-blocks
                            -1
                            (<ra> :get-seqtrack-instrument seqtracknum)))

  (if for-blocks
      (set! show-panner #f))
  
  ;;(<gui> :set-background-color gui "blue")

  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define mutesolo-width (myfloor (* 1.8 (<gui> :text-width (if for-audiofiles
                                                                "H R M S "
                                                                "M H ")))))
  (define meter-width (if for-audiofiles
                          (max 4 (myfloor (/ fontheight 2)))
                          0))

  (define name-height (if use-two-rows
                          (* fontheight 1.3)
                          height))

  (define b (max 1 (myfloor (/ fontheight 10)))) ;; border between areas.
  
  (define x-meter-split (- x2 (+ b meter-width)))
  (define x1-split (+ x1 (myfloor (* 1.8 (<gui> :text-width "9:")))))
  (define x2-split (- x-meter-split (+ b mutesolo-width)))
  (define y-split (myfloor (+ y1 name-height)))

  (delafina (get-background-color :gakk #f)
    (cond ((= seqtracknum (<ra> :get-curr-seqtrack))
           (get-seqtrack-background-color gui seqtracknum))
          (for-blocks
           "sequencer_background_color")
          (gakk
           (get-instrument-background-color gui instrument-id))
          (else
           (<ra> :get-instrument-color instrument-id)))
    )

  (if (or use-two-rows
          for-blocks)
      (add-sub-area-plain! (create-seqtrack-name-area
                            gui
                            (- x1-split 0) y1
                            x2-split
                            y-split
                            instrument-id
                            seqtracknum)))

  (add-sub-area-plain! (<new> :text-area gui
                              x1 y1
                              x1-split y-split
                              (<-> seqtracknum)
                              :text-color "black"
                              :background-color (lambda () (<gui> :mix-colors "white" (get-background-color #t) 0.02))
                              :border-rounding 100 ;;(if use-two-rows 1 8)
                              ))
  
  (define panner-x2 (if use-two-rows x-meter-split x2-split))
  (define panner-y1 (if use-two-rows
                        (+ b y-split)
                        y1))
  (define panner-y2 (min (+ panner-y1 fontheight)
                         (- y2 fontheight)))
  
  
  (define vol-x2 panner-x2)
  (define vol-y1 (if show-panner
                     (+ 2 panner-y2)
                     panner-y1))
  (define vol-y2 y2)
    
  (add-sub-area-plain! (<new> :mute-solo-buttons gui
                              (+ b x2-split) y1
                              x-meter-split y-split
                              instrument-id #t #t seqtracknum))
  
  (if show-panner
      (add-sub-area-plain! (<new> :instrument-pan-slider gui
                                  x1 panner-y1
                                  panner-x2 panner-y2
                                  instrument-id
                                  :get-color get-background-color
                                  seqtracknum
                                  )))

  (if for-audiofiles
      (add-sub-area-plain! (<new> :instrument-volume-slider gui
                                  (if (not use-two-rows) x1-split x1) vol-y1
                                  vol-x2 vol-y2
                                  instrument-id
                                  :effect-name "System Volume"
                                  :display-instrument-name (not use-two-rows)
                                  :get-color get-background-color
                                  seqtracknum
                                  ))
      (add-sub-area-plain! (<new> :editor-seqtrack-volume-slider gui
                                  (if (not use-two-rows) x1-split x1) vol-y1
                                  vol-x2 vol-y2
                                  :display-instrument-name (not use-two-rows)
                                  :get-color get-background-color
                                  :seqtracknum seqtracknum
                                  )))

  (if for-audiofiles
      (<gui> :add-vertical-audio-meter gui (find-meter-instrument-id instrument-id) (+ b x-meter-split) y1 x2 y2))

  ;;(define vam2 (<gui> :add-vertical-audio-meter gui instrument-id (- x2 8) y1 x2 y2))
  
  ;;(<gui> :remove-vertical-audio-meter vam)


  (when (and #f for-blocks) ;; Disable this. Too confusing.
    (add-sub-area-plain! (<new> :block-seqtrack-volume-slider gui
                                x1 vol-y1
                                vol-x2 vol-y2
                                seqtracknum
                                :display-seqtrack-name (not use-two-rows)
                                :get-color (lambda ()
                                             (get-seqtrack-background-color gui seqtracknum))
                                )))
    

  
  (define-override (paint)
    (if (= seqtracknum (<ra> :get-curr-seqtrack))
        (<gui> :filled-box gui (get-seqtrack-background-color gui seqtracknum) x1 y1 x2 y2)))
  
  (define-override (paint?)
    (or for-blocks
        (<ra> :instrument-is-open-and-audio instrument-id)))
  
  '(set! paint (lambda ()
                 ;;(define background-color (get-mixer-strip-background-color gui instrument-id))
                 (define background-color (<gui> :get-background-color gui))
                 ;;(define background-color (<ra> :generate-new-color))
                 (<gui> :filled-box gui background-color x1 y1 x2 y2)
                 )
         )
  
  '(set! post-paint (lambda ()
                     (define background-color (get-mixer-strip-background-color gui instrument-id))
                     (define background-color (<ra> :generate-new-color))
                     (<gui> :do-alpha gui 0.65
                            (lambda ()
                              (<gui> :filled-box gui background-color x1 y1 x2 y2)))
                     )
        )

  (add-nonpress-mouse-cycle!
   :enter-func (lambda (x* y)
                 (set-mouse-pointer ra:set-normal-mouse-pointer gui)
                 #f))

  (define-override (get-mouse-cycle button x* y*)
    (when (inside? x* y*)
      ;;(c-display "____HEADER seqtracknum:" seqtracknum)
      (<ra> :set-curr-seqtrack seqtracknum))
    (super:get-mouse-cycle button x* y*))

  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (if (and (= button *right-button*)
                                           (<ra> :shift-pressed)
                                           (> (<ra> :get-num-seqtracks) 1))
                                      (begin
                                        (delete-seqtrack-and-maybe-ask seqtracknum)
                                        #t)
                                      #f)))
  (if for-audiofiles
      (add-mouse-cycle! :press-func (lambda (button x* y*)
                                      (if (and (= button *right-button*)
                                               (not (<ra> :shift-pressed)))
                                          (begin
                                            (show-sequencer-header-popup-menu seqtracknum instrument-id #f gui)
                                            #t)
                                          #f))))
  )


(def-area-subclass (<sequencer-height-dragger> :gui :x1 :y1 :x2 :y2)
  (define background-color (<gui> :get-background-color gui))
  (define border-color (get-default-button-color gui))

  (define (is-active)
    (and (<ra> :upper-part-of-main-window-is-visible)
         (not *sequencer-window-gui-active*)))
  
  (define-override (paint)
    (<gui> :filled-box gui background-color x1 y1 x2 y2)
    (when (is-active)
      (<gui> :draw-text gui *text-color* "=" x1 y1 x2 y2
             #f ;; wrap-lines
             #f ;; align top
             #f)
      (<gui> :draw-box gui "black" x1 y1 x2 y2 1.1 3 3)))
;      (<gui> :draw-box gui border-color
;             (1+ x1) (1+ y1)
;             (1- x2) (1- y2)
;             1.3 0 0))) ;; align left
      
  (add-nonpress-mouse-cycle!
   :enter-func (lambda (x* y)
                 ;;(c-display "ENTER DRAGGER" class-name)
                 (if (is-active)
                     (begin
                       (set-mouse-pointer ra:set-vertical-split-mouse-pointer gui)
                       #t)
                     #f))
   :leave-func (lambda (button-was-pressed)
                 ;;(c-display "LEAVE DRAGGER")
                 (set-mouse-pointer ra:set-normal-mouse-pointer gui)
                 #f))

  (add-statusbar-text-handler (lambda ()
                                (if (is-active)
                                    "Change sequencer height"
                                    "")))
  
  (add-delta-mouse-cycle!
   :press-func (lambda (button x* y*)
                 (if (is-active)
                     (begin
                       (set-mouse-pointer ra:set-vertical-split-mouse-pointer gui)
                       #t)
                     #f))
   :drag-func
   (lambda (button x* y* dx dy)
     
     (define size0 ((<gui> :get-splitter-sizes *ysplitter*) 0))
     (define size1 ((<gui> :get-splitter-sizes *ysplitter*) 1))
     
     (define new-size0 (+ size0 dy))
     (when (< new-size0 0)
       (set! new-size0 0)
       (set! dy (- size0)))
     
     (define new-size1 (- size1 dy))
     (when (< new-size1 0)
       (set! new-size1 0))
     
     ;;(c-display "Y:" y ", start-y:" start-y ". DY:" (- y start-y))
     (<gui> :set-splitter-sizes *ysplitter* (list new-size0 new-size1)))

   :release-func
   (lambda (button x* y* dx dy)
     (set-mouse-pointer ra:set-normal-mouse-pointer gui))
   )
  )

#!!
(<ra> :schedule 3000
      (lambda ()
        (let loop ((n (floor (myrand 2 17))))
          (when (> n 0)
            (define dy (floor (myrand -100 100)))
            (define size0 ((<gui> :get-splitter-sizes *ysplitter*) 0))
            (define size1 ((<gui> :get-splitter-sizes *ysplitter*) 1))
            
            (define new-size0 (+ size0 dy))
            (when (< new-size0 0)
              (set! new-size0 0)
              (set! dy (- size0)))
            
            (define new-size1 (- size1 dy))
            (when (< new-size1 0)
              (set! new-size1 0))
        
            ;;(c-display "Y:" y ", start-y:" start-y ". DY:" (- y start-y))
            (<gui> :set-splitter-sizes *ysplitter* (list new-size0 new-size1))
            (loop (- n 1))))
        (if (= 0 (myrand 0 50))
            5000
            (floor (myrand 5 20)))))

(define added 0)
(<ra> :schedule 500
      (lambda ()
        (if (< added (myrand 30 50))
            (begin
              (<ra> :append-seqtrack)
              (set! added (1+ added)))
            (begin
              (let loop ((n (floor (myrand 2 (- added 2)))))
                (<ra> :delete-seqtrack 0)
                (set! added (- added 1)))))
        2000))

(<ra> :toggle-full-screen)
!!#

(define (ask-user-about-first-audio-seqtrack callback)
  (if (<ra> :is-using-sequencer-timing)
      (callback #t)
      (show-async-message (<gui> :get-sequencer-gui)
                          (<-> "Are you sure?\n"
                               "\n"
                               "We use the first seqtrack for timing and grid, but audio seqtracks don't provide this information.\n"
                               "In order to support timing and grid, we will switch to sequencer timing mode."
                               )
                          (list "No" "Yes") ;; yes-dont-show-again)
                          :is-modal #t
                          :callback
                          (lambda (res)
                            (define arg (string=? "Yes" res))
                            (undo-block
                             (lambda ()                               
                               (when arg
                                 (<ra> :undo-sequencer)
                                 (<ra> :set-using-sequencer-timing #t))
                               (callback arg)))))))

(define (delete-seqtrack-and-maybe-ask seqtracknum)
  (define (deleteit)
    (<ra> :delete-seqtrack seqtracknum)
    (if *current-seqtrack-num*
        (set! *current-seqtrack-num* (min (- (<ra> :get-num-seqtracks) 1)
                                          *current-seqtrack-num*)))
    )

  (if (and (= 0 seqtracknum)
           (not (<ra> :seqtrack-for-audiofiles 0))
           (<ra> :seqtrack-for-audiofiles 1))
      (ask-user-about-first-audio-seqtrack
       (lambda (doit)
         (if doit
             (deleteit))))
      (deleteit)))

(def-area-subclass (<sequencer-left-part-buttons> :gui :x1 :y1 :x2 :y2)

  (define (callback type)
    (cond ((eq? type '+E)
           (<ra> :insert-seqtrack #f))
          ((eq? type '+A)
           (define seqtracknum (<ra> :get-curr-seqtrack))
           (if (and (= 0 seqtracknum)
                    (not (<ra> :seqtrack-for-audiofiles 0)))
               (ask-user-about-first-audio-seqtrack
                (lambda (doit)
                  (if doit
                      (<ra> :insert-seqtrack #t seqtracknum))))
               (<ra> :insert-seqtrack #t seqtracknum)))
          ((eq? type '-)
           (when (> (<ra> :get-num-seqtracks) 1)
             (define seqtracknum (<ra> :get-curr-seqtrack))
             (set! *current-seqblock-info* #f)
             (delete-seqtrack-and-maybe-ask seqtracknum)))
          ((eq? type 'AppendE)
           (<ra> :append-seqtrack #f))
          ((eq? type 'AppendA)
           (<ra> :append-seqtrack #t))
          (else
           (assert #f))))

  (define-override (get-nonpress-mouse-cycle x* y*)
    (when (inside? x* y*)
      (set-mouse-pointer ra:set-normal-mouse-pointer gui))
    (super:get-nonpress-mouse-cycle x* y*))

  (add-nonpress-mouse-cycle!
   :enter-func (lambda (x* y)
                 (set-mouse-pointer ra:set-normal-mouse-pointer gui)
                 #f))

  (define b (max 0 (/ (get-fontheight) 6)))
  (horizontally-layout-areas x1 y1 (- x2 2) y2
                             (list '+E '+A '- 'AppendE 'AppendA)
                             :y1-border 0 ;;(1+ b)
                             :y2-border 0 ;;(1+ b)
                             :spacing b
                             :callback
                             (lambda (type x1 y1 x2 y2)
                               (add-sub-area-plain! (<new> :button gui x1 y1 x2 y2
                                                           :text (cond ((eq? type '+E) "+ e")
                                                                       ((eq? type '+A ) "+ a")
                                                                       ((eq? type '-) "-")
                                                                       ((eq? type 'AppendE) "+A e")
                                                                       ((eq? type 'AppendA) "+A a")
                                                                       (else
                                                                        (assert #f)))
                                                           :statusbar-text (list #t
                                                                                 (cond ((eq? type '+E) "Insert editor seqtrack")
                                                                                       ((eq? type '+A ) "Insert audio seqtrack")
                                                                                       ((eq? type '-) "Delete current seqtrack")
                                                                                       ((eq? type 'AppendE) "Append editor seqtrack")
                                                                                       ((eq? type 'AppendA) "Append audio seqtrack")
                                                                                       (else
                                                                                        (assert #f))))
                                                           :callback-release (lambda ()
                                                                               (callback type))))))

  ;;(define background-color (<gui> :get-background-color gui))
  ;;
  ;;(define-override (paint)
  ;;  ;;(c-display "   Scheme: Painting left part")
  ;;  (<gui> :filled-box gui background-color x1 y1 x2 y2)  ;; To avoid the last visible seqtrack header gui to be visible between the buttons.
  ;;  )

  )


#!!
(<ra> :get-topmost-visible-seqtrack)
(<ra> :get-num-seqtracks)
!!#

(def-area-subclass (<sequencer-left-part-top-bar> :gui :x1 :y1 :x2 :y2)
  (define in-window *sequencer-window-gui-active*)
  (define buttons-width (myfloor (* 1.8 (<gui> :text-width "| W | F |"))))
  (define buttons-x1 (if in-window
                         x1
                         (- x2 buttons-width)))

  (if (not in-window)
      (add-sub-area-plain! (<new> :sequencer-height-dragger gui x1 y1 (- buttons-x1 2) y2)))

  (horizontally-layout-areas
   buttons-x1 y1 x2 y2
   (if in-window
       '(window)
       '(window full))
   :spacing 2
   :callback
   (lambda (type x1 y1 x2 y2)
     (add-sub-area-plain! (<new> :checkbox gui x1 y1 x2 y2
                                 (lambda ()
                                   (if (eq? type 'window)
                                       *sequencer-window-gui-active*
                                       (not (<ra> :upper-part-of-main-window-is-visible))))
                                 (lambda (new-value)
                                   ;;(c-display type "new-value:" new-value)
                                   (if (eq? type 'window)
                                       (if new-value
                                           (begin                                               
                                             ;; show sequencer in new window
                                             (if (not (<ra> :upper-part-of-main-window-is-visible))
                                                 (<ra> :show-upper-part-of-main-window)
                                                 (remember-lower-tabs-height))
                                             (FROM-C-sequencer-set-gui-in-window! #t))
                                           (begin
                                             ;; put back sequencer into main tabs
                                             (FROM-C-sequencer-set-gui-in-window! #f)
                                             (recall-lower-tabs-height)))
                                       (if new-value
                                           (begin
                                             ;; show sequencer full
                                             (remember-lower-tabs-height)
                                             (<ra> :hide-upper-part-of-main-window))
                                           (begin
                                             ;; don't show sequencer full
                                             (recall-lower-tabs-height)
                                             (<ra> :show-upper-part-of-main-window))))
                                   (<gui> :update gui)) ;; Necessary if the dragger is at topmost position. Then the dragger gfx won't be updated.
                                 :text (if (eq? type 'window) "W" "F"))))
   )
  )
   

(def-area-subclass (<sequencer-left-part> :gui :x1 :y1 :x2 :y2)
  (define num-seqtracks (<ra> :get-num-seqtracks))

  (define topmost-seqtrack (<ra> :get-topmost-visible-seqtrack))
  
  (define seqtrack0-y1 (<ra> :get-seqtrack-y1 topmost-seqtrack))
  
  (define b 0)
  (define b/2 (/ b 2))

  (define topbar-height (myfloor (+ ((<ra> :get-box seqtimeline-area) :height)
                                     (if (not (<ra> :seqtempo-visible))
                                         0
                                         ((<ra> :get-box seqtempo-area) :height)))))
  
  (define ty1-height (myfloor (- seqtrack0-y1
                                 (<ra> :get-sequencer-y1))))
;;                                 (<ra> :get-seqtimeline-area-y1))))
  
  (define ty1 (+ y1 ty1-height))    
  (define ty2 (- y2 (* 1.5 (get-fontheight)))) ;; If changing this, also change the y2 value for setClipRect in paintVamps in api_gui.cpp too.

  ;;(c-display "       ___:" x1 y1 x2 y2 ty1 ty2)

  (define topbar-y2 (+ y1 topbar-height))

  (add-sub-area-plain! (<new> :sequencer-left-part-top-bar gui x1 y1 x2 topbar-y2))

  (define show-tempos (<ra> :show-tempos-sequencer-lane))
  (define show-signatures (<ra> :show-signatures-sequencer-lane))
  (define show-markers (<ra> :show-markers-sequencer-lane))

  (if (or show-tempos show-signatures show-markers)
      (let* ((timeline-height (- (<ra> :get-seqtimeline-area-y2) (<ra> :get-sequencer-y1)))
             (y1 (+ y1 timeline-height))
             (header-height (- seqtrack0-y1 (<ra> :get-seqtimeline-area-y2)))
             (y2 (- (+ y1 header-height)
                    6))) ;; Hack. Don't know why. The value is probably scaled by font size too, so this might need to be fixed.
        ;;(c-display "timeline-height:" timeline-height ". header-height:" header-height)
        (add-sub-area-plain! (<new> :sequencer-timeline-headers gui x1 y1 x2 y2))))
  
  (define header-area (<new> :area gui x1 topbar-y2 x2 (- ty2 1)))
  (add-sub-area-plain! header-area)
  
  (let loop ((seqtracknum topmost-seqtrack))
    (when (< seqtracknum num-seqtracks)
      (define seqtrack-box (<ra> :get-box seqtrack seqtracknum))
      (define sy1 (+ ty1 (- (seqtrack-box :y1) seqtrack0-y1)))
      (define sy2 (+ ty1 (- (seqtrack-box :y2) seqtrack0-y1)))

      (if (< sy2 topbar-y2)
          (loop (1+ seqtracknum))
          (when (< sy1 ty2)
            ;;(set! sy1 (scale seqtracknum 0 num-seqtracks ty1 ty2))
            ;;(set! sy2 (scale (1+ seqtracknum) 0 num-seqtracks ty1 ty2))
            
            (set! sy1 (+ sy1
                         (if (= topmost-seqtrack seqtracknum)
                             0
                             b/2)))
            (set! sy2 (1+ (ceiling (- sy2
                                      (if (= (1- num-seqtracks) seqtracknum)
                                          0
                                          b/2)))))
            
            (define use-two-rows (> (seqtrack-box :height)
                                    (* 2.5 (get-fontheight))))
            
            (define show-panner (> (seqtrack-box :height)
                                   (* 4.0 (get-fontheight))))
            
            (if (or (not (<ra> :seqtrack-for-audiofiles seqtracknum))
                    (>= (<ra> :get-seqtrack-instrument seqtracknum) 0))
                (header-area :add-sub-area-plain! (<new> :seqtrack-header gui x1 sy1 x2 sy2 use-two-rows show-panner seqtracknum)))
            
            (loop (1+ seqtracknum))))))


  (add-sub-area-plain! (<new> :sequencer-left-part-buttons gui x1 ty2 x2 y2))

  (define background-color (<gui> :get-background-color gui))

  (define-override (paint)
    ;;(c-display "   Scheme: Painting left part")
    (<gui> :filled-box gui background-color x1 y1 x2 y2)
    )
  
  )

'(def-area-subclass (<sequencer-left-part> :gui :x1 :y1 :x2 :y2)
  #t)


(define *testarea* (and *use-testgui* (<new> :area *testgui* 20 30 2000 2000)))

(if *use-testgui*
    (<gui> :add-paint-callback *testgui*
           (lambda (width height)
             (eat-errors :try (lambda ()
                                 (*testarea* :paint-internal 0 0 width height))))))

(if *use-testgui*
    (<gui> :add-mouse-callback *testgui*
           (lambda (button state x y)
             ;;(c-display "asd" x y)
             (*testarea* :mouse-callback-internal button state x y)
             (if (*testarea* :has-mouse)
                 #t
                 #f))))

(if *use-testgui*
    (<gui> :add-deleted-callback *testgui*
           (lambda (radium-runs-custom-exec)
             (*testarea* :i-am-removed!))))


(if *use-testgui*
    (<gui> :show *testgui*))
  

(define (get-sequencer-left-part-position kont)
  (define header-box (<ra> :get-box sequencer-left-part))
  (kont (header-box :x1) (header-box :y1)
        (header-box :x2) (header-box :y2)))

(define *had-sequencer-paint-callback* (defined? '*sequencer-left-part-area*))

(define *sequencer-left-part-area* #f)
(define *sequencer-right-part-area* #f)

(define (get-sequencer-left-part-area)
  (when (not *sequencer-left-part-area*)
    (set! *sequencer-left-part-area* (if *use-testgui*
                                         *testarea*
                                         (get-sequencer-left-part-position
                                          (lambda (x1 y1 x2 y2)
                                            (<new> :area (<gui> :get-sequencer-gui)
                                                   x1 y1 x2 y2)))))
    (when (and (not *use-testgui*)
               (not *had-sequencer-paint-callback*))

      (define gui (<gui> :get-sequencer-gui))

      (<gui> :add-paint-callback gui
             (lambda (width height)
               (get-sequencer-left-part-area)
               (eat-errors :try (lambda ()
                                   (if (not *sequencer-left-part-area*)
                                       (c-display "*sequencer-left-part-area* is false")
                                       (*sequencer-left-part-area* :paint-internal 0 0 width height))
                                   (if *sequencer-right-part-area*
                                       (*sequencer-right-part-area* :paint-internal 0 0 width height))
                                   (if *sequencer-timing-area*
                                       (*sequencer-timing-area* :paint-internal 0 0 width height))
                                   ))))

      (<gui> :add-mouse-wheel-callback gui
             (lambda (is-up x* y*)
               (eat-errors :try (lambda ()
                                  (or (and *sequencer-left-part-area*
                                           (*sequencer-left-part-area* :mouse-wheel-moved-internal! is-up x* y*))
                                      (and *sequencer-right-part-area*
                                           (*sequencer-right-part-area* :mouse-wheel-moved-internal! is-up x* y*))
                                      (and *sequencer-timing-area*
                                           (*sequencer-timing-area* :mouse-wheel-moved-internal! is-up x* y*))
                                      ))
                           :failure (lambda ()
                                      #f))))
      
      (<gui> :add-mouse-callback gui
             (lambda (button state x y)
               (get-sequencer-left-part-area)
               ;;(c-display "asd" x y)
               (*sequencer-left-part-area* :mouse-callback-internal button state x y)
               (if *sequencer-right-part-area*
                   (*sequencer-right-part-area* :mouse-callback-internal button state x y))
               (if *sequencer-timing-area*
                   (*sequencer-timing-area* :mouse-callback-internal button state x y))
               (if (or (*sequencer-left-part-area* :has-mouse)
                       (and *sequencer-right-part-area*
                            (*sequencer-right-part-area* :has-mouse))
                       (and *sequencer-timing-area*
                            (*sequencer-timing-area* :has-mouse)))
                   #t
                   #f)))
      ;(<gui> :add-resize-callback gui ;; TODO: I think this resize callback can be removed.
      ;       (lambda (width height)
      ;         (eat-errors :try FROM_C-reconfigure-sequencer-left-part)))

      ))
  
  *sequencer-left-part-area*)

(define (FROM_C-reconfigure-sequencer-left-part)
  ;;(c-display "   Scheme: Reconfiguring left part")

  (get-sequencer-left-part-area)

  (define gui (if *use-testgui*
                  *testgui*
                  (<gui> :get-sequencer-gui)))

  (<gui> :remove-all-vertical-audio-meters gui)

  (if *use-testgui*
      (begin

        (define height ((<ra> :get-box sequencer) :height))
        ;;(define height (- (<gui> :height *testgui*) 60))

        ;;(define *testarea* (<new> :horizontal-instrument-slider *testgui* 20 30 110 120 (car (get-all-audio-instruments))))
        ;;(define *testarea* (<new> :mute-solo-buttons *testgui* 20 30 110 120 (car (get-all-audio-instruments)) #t #t))
        (*sequencer-left-part-area* :reset! 20 30 210 (+ 30 height))
        
        (*sequencer-left-part-area* :add-sub-area-plain! (<new> :sequencer-left-part gui
                                                                20 30 
                                                                210 (+ 30 height))))
      (get-sequencer-left-part-position
       (lambda (x1 y1 x2 y2)         
         (*sequencer-left-part-area* :reset! x1 y1 x2 y2)        
         (*sequencer-left-part-area* :add-sub-area-plain! (<new> :sequencer-left-part gui
                                                                 x1 y1 x2 y2)))))
  )


(when *use-testgui*
  (<gui> :add-resize-callback *testgui*
         (lambda (width height)
           (eat-errors :try FROM_C-reconfigure-sequencer-left-part)))

  (FROM_C-reconfigure-sequencer-left-part))

#!!
(FROM_C-reconfigure-sequencer-left-part)
!!#


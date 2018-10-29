
(provide 'seqtrack-headers.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)


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
      *curr-seqtrack-color*
      (begin
        (define instrument-id (<ra> :get-seqtrack-instrument seqtracknum))
        (let ((background-color (get-mixer-strip-background-color gui instrument-id)))
          (if (= seqtracknum (<ra> :get-curr-seqtrack))
              (<gui> :mix-colors
                     (<gui> :mix-colors *curr-seqtrack-color* background-color 0.2)
                     "white"
                     0.95)
              background-color)))))

(define (show-sequencer-header-popup-menu instrument-id effect-name parentgui)
  (popup-menu
   (list "Reset volume"
         (lambda ()
           (<ra> :undo-instrument-effect instrument-id "System Volume")
           (<ra> :set-instrument-effect instrument-id "System Volume" (db-to-radium-normalized 0.0))))
   "-------------"
   (get-effect-popup-entries instrument-id effect-name)
   "------------"
   (get-instrument-popup-entries instrument-id parentgui)))


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

  (define volume-on-off-name (get-instrument-volume-on/off-effect-name instrument-id))

  (define (get-muted)
    (< (<ra> :get-instrument-effect instrument-id volume-on-off-name) 0.5))
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

  (define last-drawn-implicitly-muted (<ra> :instrument-is-implicitly-muted instrument-id))
  
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
                              (update-me!)))
  
  (define (get-selected type)
    (cond ((eq? type 'record)
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
               (list 'record 'mute 'solo)
               :callback
               (lambda (type x1 y1 x2 y2)
                 (define box (<new> :checkbox gui x1 y1 x2 y2
                                    #t
                                    (lambda (_)
                                      (define is-selected (get-selected type))
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
                                                     :implicit-border 1
                                                     ))
                                    (lambda (_)
                                      (define is-selected (not (get-selected type)))
                                      (undo-block
                                       (lambda ()
                                         (cond ((eq? type 'record)
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
                                                (<ra> :set-instrument-mute instrument-id is-selected)
                                                ;;(c-display "mute: " is-muted)
                                                (if (<ra> :control-pressed)
                                                    (turn-off-all-mute instrument-id)))
                                               (else
                                                (assert #f))))))
                                    :right-mouse-clicked-callback (lambda ()
                                                                    (cond ((eq? type 'record)
                                                                           (show-record-popup-menu seqtracknum))
                                                                          ((eq? type 'solo)
                                                                           (show-sequencer-header-popup-menu instrument-id "System Solo On/Off" gui))
                                                                          ((eq? type 'mute)
                                                                           (show-sequencer-header-popup-menu instrument-id "System Volume On/Off" gui))
                                                                          (else
                                                                           (assert #f))))))

                 (if (eq? type 'record)
                     (box :add-statusbar-text-handler "Right-click the \"R\" button to configure recording options."))
                 (add-sub-area-plain! box)))

  )

(def-area-subclass (<seqtrack-name> :gui :x1 :y1 :x2 :y2
                                    :instrument-id
                                    :seqtracknum)
  
  (define last-painted-name "")

  (define-override (paint)
    ;;(<gui> :set-clip-rect gui x1 y1 x2 y2)
    (let* ((b 0)
           (x1 (+ x1 b))
           (y1 (+ y1 b))
           (x2 (- x2 b))
           (y2 (- y2 b)))
      
      (if (= seqtracknum (<ra> :get-curr-seqtrack))
          (<gui> :filled-box gui (get-seqtrack-background-color gui seqtracknum) x1 y1 x2 y2)
          (paint-instrument-background-color gui x1 y1 x2 y2 instrument-id))
      
      (define seqtrack-name (<ra> :get-seqtrack-name seqtracknum))
      (set! last-painted-name seqtrack-name)
      (<gui> :draw-text gui *text-color* (<-> seqtracknum ": " seqtrack-name)
             (+ 4 x1) y1 x2 y2
             #f ;; wrap-lines
             #f ;; align top
             #t) ;; align left
      (define background-color (<gui> :get-background-color gui))
      (<gui> :draw-box gui background-color (+ 0 x1) (+ 0 y1) (- x2 0) (- y2 0) 2 0 0)
      (<gui> :draw-box gui *mixer-strip-border-color* x1 y1 x2 y2 1.5 5 5))
    ;;(<gui> :cancel-clip-rect gui)
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
                                  (= button *left-button*))
                    :release-func
                    (lambda (button x* y*)
                      (and (= button *left-button*)
                           (let* ((old-name (<ra> :get-seqtrack-name seqtracknum))
                                  (new-name (<ra> :request-string "New name:" #t old-name)))
                             (c-display "GAKKKGAKK_________ NEWNAME" (<-> "-" new-name "-"))
                             (when (and (not (string=? new-name ""))
                                        (not (string=? new-name old-name)))
                               (<ra> :set-seqtrack-name new-name seqtracknum)
                               (update-me!))
                             #f)))) ;; Mouse cycle is screwed up when focus is switeched to a different widget. #f fixes this.
  )


(def-area-subclass (<instrument-volume-slider> :gui :x1 :y1 :x2 :y2
                                               :instrument-id
                                               :effect-name "System Volume"
                                               :display-instrument-name #t
                                               :get-color)
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
     (and (= button *left-button*)
          (begin
            (define radium-normalized (get-radium-normalized))
            (set! start-mouse-value (get-scaled-value radium-normalized));;(scale x* x1 x2 0 1));;(get-db-value));;(<ra> :get-stored-instrument-effect instrument-id effect-name))
            ;;(c-display "press button/x/y" x* y*)
            (set-statusbar-text! (get-statusbar-text))
            #t)))
   (lambda (button x* y* dx dy)
     (maybe-make-undo)
     (define slider-value (between 0 (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                   1))
     (set-db-value (slider-to-db slider-value))
     (set-statusbar-text! (get-statusbar-text))
     (update-me!)
     )
   (lambda (button x* y*)
     (c-display "release button/x/y" x* y*)))
  
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

#||
;; Not used, too confusing. A little bit buggy too, but that should be easily fixed.
(def-area-subclass (<block-seqtrack-volume-slider> :gui :x1 :y1 :x2 :y2
                                                   :seqtracknum
                                                   :display-seqtrack-name #t
                                                   :get-color)
  (define has-made-undo #f)
  
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (set! has-made-undo #t)
      ;;(<ra> :undo-instrument-effect instrument-id effect-name)))
      ))

  (define (get-radium-normalized)
    (c-display "get-r-n: " seqtracknum (<ra> :get-seqtrack-note-gain seqtracknum) (db-to-text (slider-to-db (<ra> :get-seqtrack-note-gain seqtracknum)) #t))
    (<ra> :get-seqtrack-note-gain seqtracknum))

  (define (get-db-value radium-normalized)
    (radium-normalized-to-db radium-normalized))

  (define (set-db-value db)
    (c-display "      -------set-db-val:" (db-to-radium-normalized db))
    (<ra> :set-seqtrack-note-gain (db-to-radium-normalized db) seqtracknum ))
  
  (define (get-scaled-value radium-normalized)
    (db-to-slider (get-db-value radium-normalized)))

  (define (get-volume-slider-value-text value)
    (db-to-text (slider-to-db value) #t))
  
  (define (get-volume-slider-text radium-normalized)
    (define midi-learn-text "")
    (let ((volume-text (get-volume-slider-value-text (get-scaled-value radium-normalized))))
      (if display-seqtrack-name
          (let ((seqtrack-name (<ra> :get-seqtrack-name seqtracknum)))
            (<-> midi-learn-text seqtrack-name ": " volume-text))
          (<-> " " midi-learn-text volume-text))))

  (define last-painted-radium-normalized -10000)
  
  (define-override (paint)
    (define b 1)
    (define radium-normalized (get-radium-normalized))
    (set! last-painted-radium-normalized radium-normalized)

    (paint-horizontal-instrument-slider gui
                                        -1
                                        (get-scaled-value radium-normalized)
                                        (get-volume-slider-text radium-normalized)
                                        #t
                                        #f
                                        #f ;;get-automation-data
                                        (+ b x1)
                                        (+ b x1) (+ b y1) (- x2 b) (- y2 b)
                                        (get-color)
                                        ))
  
  (define start-mouse-value #f)
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set! has-made-undo #f)
     (and (= button *left-button*)
          (begin
            (define radium-normalized (get-radium-normalized))
            (set! start-mouse-value (get-scaled-value radium-normalized));;(scale x* x1 x2 0 1));;(get-db-value));;(<ra> :get-stored-instrument-effect instrument-id effect-name))
            ;;(c-display "press button/x/y" x* y*)
            (set-statusbar-text! (get-statusbar-text))
            #t)))
   (lambda (button x* y* dx dy)
     (maybe-make-undo)
     (define slider-value (between 0 (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                   1))
     (set-db-value (slider-to-db slider-value))
     (set-statusbar-text! (get-statusbar-text))
     (update-me!)
     )
   (lambda (button x* y*)
     (c-display "release button/x/y" x* y*)))
  
  (define (get-statusbar-text)
    (c-display "GETAST for" seqtracknum (get-radium-normalized) (get-volume-slider-text (get-radium-normalized)))
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
||#


(def-area-subclass (<instrument-pan-slider> :gui :x1 :y1 :x2 :y2
                                            :instrument-id
                                            :get-color)

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
    (<gui> :filled-box gui background x1 y1 x2 y2 5 5)
    (define col1 (<gui> :mix-colors "white" background 0.4))
    (define col2 (<gui> :mix-colors "#010101" background 0.5))
    
    (define inner-width/2 (scale 1 0 18 0 (get-fontheight)))
    (define outer-width/2 (* inner-width/2 2))
    
    (define middle (scale value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))
    
    (<gui> :filled-box gui col1 (+ x1 (- middle inner-width/2))               (+ y1 2) (+ x1 middle inner-width/2)               (- y2 3))
    (<gui> :filled-box gui col2 (+ x1 (- middle inner-width/2 outer-width/2)) (+ y1 2) (+ x1 (- middle inner-width/2))           (- y2 3))
    (<gui> :filled-box gui col2 (+ x1 (+ middle inner-width/2))               (+ y1 2) (+ x1 middle inner-width/2 outer-width/2) (- y2 3))
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
                  (popup-menu (list "Reset Pan" (lambda ()
                                              (<ra> :undo-instrument-effect instrument-id "System Pan")
                                              (<ra> :set-instrument-effect instrument-id "System Pan" 0.5)))
                              (list "Pan Enabled"
                                    :check pan-enabled
                                    enable!)
                              "------------"
                              (get-effect-popup-entries instrument-id "System Pan"
                                                        :pre-undo-block-callback (lambda ()
                                                                                   (enable! #t)))
                              "------------"
                              (get-instrument-popup-entries instrument-id gui))
                  #f)))

  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set! has-made-undo #f)
     (cond ((and (= button *right-button*)
                 (not (<ra> :shift-pressed)))
            (show-popup)
            #t)
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
   (lambda (button x* y*)
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

  ;;(<gui> :set-background-color gui "blue")

  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define mutesolo-width (myfloor (* 1.8 (<gui> :text-width "R M S "))))
  (define meter-width (max 4 (myfloor (/ fontheight 2))))

  (define name-height (if use-two-rows
                          (* fontheight 1.3)
                          height))

  (define b (max 1 (myfloor (/ fontheight 6)))) ;; border between areas.
  
  (define x-meter-split (- x2 (+ b meter-width)))
  (define x1-split (- x-meter-split (+ b mutesolo-width)))
  (define y-split (myfloor (+ y1 name-height)))
  
  (if (or use-two-rows
          for-blocks)
      (add-sub-area-plain! (<new> :seqtrack-name gui
                                  x1 y1
                                  (if for-audiofiles
                                      x1-split
                                      x2)
                                  y-split
                                  instrument-id
                                  seqtracknum)))

  (define panner-x2 (if use-two-rows x-meter-split x1-split))
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
    
  (when for-audiofiles
    (add-sub-area-plain! (<new> :mute-solo-buttons gui
                                (+ b x1-split) y1
                                x-meter-split y-split
                                instrument-id #t #t seqtracknum))

    (if show-panner
        (add-sub-area-plain! (<new> :instrument-pan-slider gui
                                    x1 panner-y1
                                    panner-x2 panner-y2
                                    instrument-id
                                    :get-color (lambda ()
                                                 (if (= seqtracknum (<ra> :get-curr-seqtrack))
                                                     (get-seqtrack-background-color gui seqtracknum)
                                                     (<ra> :get-instrument-color instrument-id)))
                                    )))

    (add-sub-area-plain! (<new> :instrument-volume-slider gui
                                x1 vol-y1
                                vol-x2 vol-y2
                                instrument-id
                                :effect-name "System Volume"
                                :display-instrument-name (not use-two-rows)
                                :get-color (lambda ()
                                             (if (= seqtracknum (<ra> :get-curr-seqtrack))
                                                 (get-seqtrack-background-color gui seqtracknum)
                                                 (<ra> :get-instrument-color instrument-id)))
                                ))
    
    (<gui> :add-vertical-audio-meter gui instrument-id (+ b x-meter-split) y1 x2 y2))

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
                                            (<ra> :schedule 0 ;; Workaround. Opening a popup menu causes Qt to skip the drag and release mouse events.
                                                  (lambda ()
                                                    (show-sequencer-header-popup-menu instrument-id "System Volume" gui)
                                                    #f))
                                            #t)
                                          #f))))
  )


(def-area-subclass (<sequencer-height-dragger> :gui :x1 :y1 :x2 :y2)
  (define background-color (<gui> :get-background-color gui))
  (define border-color (<gui> :mix-colors background-color "black" 0.5))

  (define-override (paint)
    ;;(<gui> :filled-box gui background-color 0 0 width height)
    (<gui> :draw-text gui *text-color* "=" x1 y1 x2 y2
           #f ;; wrap-lines
           #f ;; align top
           #f)
    (<gui> :draw-box gui border-color
           (1+ x1) (1+ y1)
           (1- x2) (1- y2)
           1.3 0 0)) ;; align left

  (add-nonpress-mouse-cycle!
   :enter-func (lambda (x* y)
                 (set-mouse-pointer ra:set-vertical-resize-mouse-pointer gui)
                 #f))

  (add-statusbar-text-handler "Change sequencer height")
  
  (add-delta-mouse-cycle!
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
   (lambda (button x* y*)
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

(define *show-first-seqtrack-warning* #t)

(define (ask-user-about-first-audio-seqtrack callback)
  (define yes-dont-show-again "Yes, don't show again")
  (if (not *show-first-seqtrack-warning*)
      (callback #t)
      (show-async-message (<gui> :get-sequencer-gui)
                          (<-> "Are you sure?\n"
                               "\n"
                               "We use the first seqtrack for timing and grid, but audio seqtracks can't provide this information.\n"
                               )
                          (list "No" "Yes" yes-dont-show-again) #t
                          (lambda (res)
                            (if (string=? yes-dont-show-again res)
                                (set! *show-first-seqtrack-warning* #f))                        
                            (callback (or (string=? "Yes" res)
                                          (string=? yes-dont-show-again res)))))))

(define (delete-seqtrack-and-maybe-ask seqtracknum)
  (if (and (= 0 seqtracknum)
           (<ra> :seqtrack-for-audiofiles 1))
      (ask-user-about-first-audio-seqtrack
       (lambda (doit)
         (if doit
             (<ra> :delete-seqtrack seqtracknum))))
      (<ra> :delete-seqtrack seqtracknum)))

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
                                                                               (callback type)))))))


(def-area-subclass (<sequencer-left-part> :gui :x1 :y1 :x2 :y2)
  (define num-seqtracks (<ra> :get-num-seqtracks))

  (define seqtrack0-y1 (<ra> :get-seqtrack-y1 0))
  
  (define b 0)
  (define b/2 (/ b 2))

  (define dragger-height (myfloor (+ ((<ra> :get-box seqtimeline-area) :height)
                                     (if (not (<ra> :seqtempo-visible))
                                         0
                                         ((<ra> :get-box seqtempo-area) :height)))))
  
  (define ty1-height (myfloor (- (<ra> :get-seqtrack-y1 0)
                                 (<ra> :get-seqtimeline-area-y1))))
  
  (define ty1 (+ y1 ty1-height))    
  (define ty2 (- y2 (myfloor ((<ra> :get-box seqnav) :height))))

  ;;(c-display "       ___:" x1 y1 x2 y2 ty1 ty2)

  (add-sub-area-plain! (<new> :sequencer-height-dragger gui x1 y1 x2 (+ y1 dragger-height)))

  (define use-two-rows (> (/ (- ty2 ty1) num-seqtracks)
                          (* 2.5 (get-fontheight))))

  (define show-panner (> (/ (- ty2 ty1) num-seqtracks)
                         (* 3.5 (get-fontheight))))

  (let loop ((seqtracknum 0))
    (when (< seqtracknum num-seqtracks)
      (define seqtrack-box (<ra> :get-box seqtrack seqtracknum))
      (define sy1 (+ ty1 (- (seqtrack-box :y1) seqtrack0-y1)))
      (define sy2 (+ ty1 (- (seqtrack-box :y2) seqtrack0-y1)))

      ;;(set! sy1 (scale seqtracknum 0 num-seqtracks ty1 ty2))
      ;;(set! sy2 (scale (1+ seqtracknum) 0 num-seqtracks ty1 ty2))

      (set! sy1 (+ sy1
                   (if (= 0 seqtracknum)
                       0
                       b/2)))
      (set! sy2 (- sy2
                   (if (= (1- num-seqtracks) seqtracknum)
                       0
                       b/2)))

      (if (or (not (<ra> :seqtrack-for-audiofiles seqtracknum))
              (>= (<ra> :get-seqtrack-instrument seqtracknum) 0))
          (add-sub-area-plain! (<new> :seqtrack-header gui x1 sy1 x2 sy2 use-two-rows show-panner seqtracknum)))
      
      (loop (1+ seqtracknum))))

  (add-sub-area-plain! (<new> :sequencer-left-part-buttons gui x1 ty2 x2 y2))

  (define background-color (<gui> :get-background-color gui))

  (define-override (paint)
    ;;(c-display "   Scheme: Painting left part")
    (<gui> :filled-box gui background-color x1 y1 x2 y2))
  
  )

'(def-area-subclass (<sequencer-left-part> :gui :x1 :y1 :x2 :y2)
  #t)


(define *testarea* (and *use-testgui* (<new> :area *testgui* 20 30 2000 2000)))

(if *use-testgui*
    (<gui> :add-paint-callback *testgui*
           (lambda (width height)
             (try-finally :try (lambda ()
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
      (<gui> :add-paint-callback (<gui> :get-sequencer-gui)
             (lambda (width height)
               (get-sequencer-left-part-area)
               (try-finally :try (lambda ()
                                   (if (not *sequencer-left-part-area*)
                                       (c-display "*sequencer-left-part-area* is false")
                                       (*sequencer-left-part-area* :paint-internal 0 0 width height))))))
      (<gui> :add-mouse-callback (<gui> :get-sequencer-gui)
             (lambda (button state x y)
               (get-sequencer-left-part-area)
               ;;(c-display "asd" x y)
               (*sequencer-left-part-area* :mouse-callback-internal button state x y)
               (if (*sequencer-left-part-area* :has-mouse)
                   #t
                   #f)))
      ;(<gui> :add-resize-callback (<gui> :get-sequencer-gui) ;; TODO: I think this resize callback can be removed.
      ;       (lambda (width height)
      ;         (try-finally :try FROM_C-reconfigure-sequencer-left-part)))

      ))
  
  *sequencer-left-part-area*)

(define (FROM_C-reconfigure-sequencer-left-part)
  (c-display "   Scheme: Reconfiguring left part")

  (get-sequencer-left-part-area)

  (define gui (if *use-testgui*
                  *testgui*
                  (<gui> :get-sequencer-gui)))

  (define height ((<ra> :get-box sequencer) :height))
  ;;(define height (- (<gui> :height *testgui*) 60))

  (<gui> :remove-all-vertical-audio-meters gui)

  (if *use-testgui*
      (begin
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
           (try-finally :try FROM_C-reconfigure-sequencer-left-part)))

  (FROM_C-reconfigure-sequencer-left-part))

#!!
(FROM_C-reconfigure-sequencer-left-part)
!!#


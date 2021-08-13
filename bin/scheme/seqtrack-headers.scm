
(provide 'seqtrack-headers.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)
(my-require 'sequencer.scm)
(my-require 'main_layout.scm)
(my-require 'sequencer_upper_part.scm)

(<declare-variable> get-mutesolo-width)



;;(define *curr-seqtrack-color* "#7c3a3a")
;;(define *curr-seqtrack-color* "#776757")
;;(define *curr-seqtrack-color* (ra:gui_mix-colors *current-mixer-strip-border-color* "black" 0.6))
;;(define *curr-seqtrack-color* (<gui> :mix-colors *current-mixer-strip-border-color* "white" 0.92))


(define (get-seqtrack-background-color gui seqtracknum)
  (let ((color (if (not (<ra> :seqtrack-for-audiofiles seqtracknum))
                   "sequencer_editor_seqtrack"
                   (begin
                     (define instrument-id (<ra> :get-seqtrack-instrument seqtracknum))
                     (<ra> :get-instrument-color instrument-id)))))
;;                     (get-instrument-background-color gui instrument-id)))))

    (define is-curr-seqtrack (= seqtracknum (<ra> :get-curr-seqtrack)))
    (define is-curr-seqtrack-under-mouse (= seqtracknum (<ra> :get-curr-seqtrack-under-mouse)))
    (cond ((and is-curr-seqtrack
                is-curr-seqtrack-under-mouse)
           (<gui> :make-color-lighter color 1.4))
          ((or is-curr-seqtrack
               is-curr-seqtrack-under-mouse)
           (<gui> :make-color-lighter color 1.2))
          (else
           color))))

(define (get-sequencer-header-popup-menu-entries seqtracknum instrument-id effect-name parentgui)
  (list
   (and effect-name
        (get-effect-popup-entries instrument-id effect-name))

   (and effect-name
        "--------Instrument")
   (get-instrument-popup-entries instrument-id parentgui :include-replace #f :put-in-submenu effect-name)
   
   "------------Seqtrack"
   (list (<-> "\"" (<ra> :get-seqtrack-name seqtracknum) "\"")
         (get-seqtrack-popup-menu-entries seqtracknum))))
   
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


(define (get-num-recording-channels seqtracknum)
  (define ret 0)
  (for-each (lambda (input-channel)
              (for-each (lambda (soundfile-channel)
                          (if (<ra> :get-seqtrack-recording-matrix seqtracknum input-channel soundfile-channel)
                              (set! ret (max ret (1+ soundfile-channel)))))
                        (iota 8)))
            (iota 8))
  ret)



(delafina (get-mutesolo-width :for-audiofiles
                              :include-rec-button #t)
  (myfloor (* 1.8 (<gui> :text-width (if for-audiofiles
                                         (if include-rec-button
                                             "H R M S "
                                             "H M S ")
                                         "M H ")))))

;; There's a lot of copy-paste code from mixer-strip.scm:create-mixer-strip-mutesolo here, but I hope this code will eventually replace mixer-strip.scm:create-mixer-strip-mutesolo some day
(def-area-subclass (<mute-solo-buttons> :gui :x1 :y1 :x2 :y2
                                        :instrument-id
                                        :use-single-letters 
                                        :stack-horizontally
                                        :seqtracknum
                                        :include-rec-button #t)
  
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
                (when (and (not (equal? instrument-id except))
                           (< (<ra> :get-instrument-effect instrument-id volume-on-off-name) 0.5))
                  (<ra> :undo-instrument-effect instrument-id volume-on-off-name)
                  (<ra> :set-instrument-effect instrument-id volume-on-off-name 1)
                  ))
              (get-all-audio-instruments)))
  
  (define (turn-off-all-solo except)
    (for-each (lambda (instrument-id)
                (if (not (equal? instrument-id except))
                    (<ra> :set-instrument-solo #f instrument-id)))
              (get-all-audio-instruments)))
  
  (define last-drawn-implicitly-muted (and for-audiofiles
                                           (<ra> :instrument-is-implicitly-muted instrument-id)))

  (when for-audiofiles
    (add-update-gui-effect-monitor gui instrument-id volume-on-off-name #t #t)
    (add-update-gui-effect-monitor gui instrument-id "System Solo On/Off" #t #t))
    
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
                   (if include-rec-button
                       '(record mute solo height)
                       '(mute solo height))
                   '(mute height))
               :spacing 1
               :callback
               (lambda (type x1 y1 x2 y2)
                 (define-optional-func box (methodname . args))
                 (set! box (<new> :checkbox gui x1 y1 x2 y2
                                    (lambda ()
                                      (get-selected type))
                                    (lambda (is-selected)
                                      (undo-block
                                       (lambda ()
                                         (cond ((eq? type 'height)
                                                (show-seqtrack-height-gui seqtracknum #t))
                                               ((eq? type 'record)
                                                (cond ((= 0 (get-num-recording-channels seqtracknum))
                                                       (<ra> :show-async-message "Must select at least one recording channel"))
                                                      ((and (<ra> :release-mode)
                                                            (not (<ra> :has-session)))
                                                       (<ra> :show-async-message "Session is required to record audio files. Please save song first."))
                                                      (else
                                                       (<ra> :set-seqtrack-is-recording seqtracknum is-selected)))
                                                )
                                               ((eq? type 'solo)
                                                (<ra> :set-instrument-solo is-selected instrument-id)
                                                (if (<ra> :control-pressed)
                                                    (turn-off-all-solo instrument-id)))
                                               ((eq? type 'mute)
                                                (if for-audiofiles
                                                    (<ra> :set-instrument-mute is-selected instrument-id)
                                                    (<ra> :set-editor-seqtrack-muted is-selected seqtracknum))
                                                ;;(c-display "mute: " is-muted)
                                                (if (and for-audiofiles
                                                         (<ra> :control-pressed))
                                                    (turn-off-all-mute instrument-id)))
                                               (else
                                                (assert #f))))))
                                    :paint-func
                                    (lambda (gui x1 y1 x2 y2 is-selected is-hovering)
                                      ;;(c-display "is-hovering/type:" is-hovering type)
                                      ;;(c-display "DRAWING mutesolo" instrument-id type (* 1.0 x1) (* 1.0 y1) (* 1.0 x2) (* 1.0 y2))
                                      (if (eq? type 'solo)
                                          (set! last-drawn-implicitly-muted (<ra> :instrument-is-implicitly-muted instrument-id)))
                                      (draw-mutesolo gui
                                                     type
                                                     instrument-id
                                                     x1 y1 x2 y2
                                                     is-selected
                                                     use-single-letters
                                                     :is-hovering is-hovering
                                                     :background-color (get-seqtrack-background-color gui seqtracknum)
                                                     :border 0
                                                     :implicit-border 3
                                                     :seqtracknum seqtracknum
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
                                                                                             (<ra> :set-editor-seqtrack-muted #f seqtracknum)))))
                                                                          (else
                                                                           (assert #f))))))

                 (if (eq? type 'height)
                     (box :override-method! :mouse-wheel-moved
                          (lambda (is-up x y)
                            (define height-type (<ra> :get-seqtrack-min-height-type seqtracknum))
                            ;;(c-display "HEIGHT-type:" height-type is-up)
                            (if is-up
                                (if (= height-type 1)
                                    (<ra> :set-seqtrack-min-height-type 4 seqtracknum)
                                    (if (not (= height-type 4))
                                        (<ra> :set-seqtrack-min-height-type (- height-type 1) seqtracknum)))
                                (if (= height-type 4)
                                    (<ra> :set-seqtrack-min-height-type 1 seqtracknum)
                                    (if (not (= height-type 3))
                                        (<ra> :set-seqtrack-min-height-type (+ height-type 1) seqtracknum))))
                            #t)))

                 (cond ((eq? type 'record)
                        (box :add-statusbar-text-handler "Record audio. Right-click to configure recording options."))
                       ((eq? type 'height)
                        (box :add-statusbar-text-handler "Set seqtrack height (Tip: use scroll wheel)"))
                       ((eq? type 'solo)
                        (box :add-statusbar-text-handler "Enable/disable Solo"))
                       ((eq? type 'mute)
                        (box :add-statusbar-text-handler "Enable/disable Mute"))
                       (else
                        (assert #f)))
                 
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
                                                (get-seqtrack-background-color gui seqtracknum))
                            :callback (lambda (new-name)
                                        (if (string=? "" new-name)
                                            #f
                                            (begin
                                              (set! last-painted-name new-name)
                                              (<ra> :set-seqtrack-name new-name seqtracknum)
                                              new-name)))))

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

  (if (<ra> :is-legal-instrument instrument-id)
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
  
  (detect-hovering!)
  
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
                                        is-hovering
                                        )
    ;;(if is-hovering
    ;;    (draw-hovering-overlay gui x1 y1 x2 y2))
    )

  
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
    (<-> "Volume: " (get-volume-slider-text (get-radium-normalized))))
  
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

  (detect-hovering!)
  
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
                             :is-hovering is-hovering
                             )

    ;;(if is-hovering
    ;;    (draw-hovering-overlay gui x1 y1 x2 y2))
    )

  
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
    (<-> "Volume: "(get-volume-slider-text (get-gain))))

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

  (detect-hovering!)
  
  (define-override (paint)
    (define value (get-pan))
    (set! last-painted-normalized-pan (scale value -90 90 0 1))
    (define is-on (pan-enabled? instrument-id))
    ;;(<gui> :filled-box gui (get-color) x1 y1 x2 y2)
    ;;(define background-color (get-color))
    
    (define used-background-color (paint-pan-slider gui x1 y1 x2 y2 value is-on (get-color)
                                                    :is-hovering is-hovering
                                                    :automation-slider-value automation-slider-value
                                                    :automation-color pan-automation-color))

    ;;(if is-hovering
    ;;    (draw-hovering-overlay gui x1 y1 x2 y2))

    (when (<ra> :instrument-effect-has-midi-learn instrument-id "System Pan")
      (define midi-learn-color (<gui> :mix-colors *text-color* used-background-color 0.2))
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
    (when (not (eq? onoff (pan-enabled? instrument-id)))
      (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
      (<ra> :set-instrument-effect instrument-id "System Pan On/Off" (if onoff 1.0 0.0))))

  (define start-mouse-value #f)

  (define (show-popup)
    (<ra> :schedule 0 ;; Workaround. Opening a popup menu causes Qt to skip the drag and release mouse events.
          (lambda ()
            (define pan-enabled (pan-enabled? instrument-id))
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

(define *hovering-seqtrack-number* -1)

(def-area-subclass (<seqtrack-number> :gui :x1 :y1 :x2 :y2
                                      :seqtracknum
                                      :get-color)

  (add-raw-mouse-cycle!
   :enter-func (lambda (button x y)
                 (set! *hovering-seqtrack-number* seqtracknum)
                 (update-me!)
                 #t)
   :leave-func (lambda (button x y)
                 (if (not (has-mouse))
                     (set! *hovering-seqtrack-number* -1))
                 (update-me!)
                 #f))

  (add-sub-area-plain! (<new> :text-area gui x1 y1 x2 y2
                              (<-> seqtracknum)
                              :background-color (lambda ()
                                                  (if (= seqtracknum *hovering-seqtrack-number*)
                                                      (<gui> :make-color-lighter (get-color) 1.7)
                                                      (get-color)))
                              :text-color "black"
                              :border-rounding 108 ;0 ;;(if use-two-rows 1 8)
                              ))

  (define has-made-undo #f)

  (define (maybe-make-undo)
    (when (not has-made-undo)
      (<ra> :undo-sequencer)
      (set! has-made-undo #t)
      ))

  (define (get-seqtrack-y1 seqtracknum)
    (define s-y1 (<ra> :get-sequencer-y1))
    (- (<ra> :get-seqtrack-y1 seqtracknum) s-y1))
  
  (define (get-seqtrack-y2 seqtracknum)
    (define s-y1 (<ra> :get-sequencer-y1))
    (- (<ra> :get-seqtrack-y2 seqtracknum) s-y1))

  (define (get-new-seqtracknum y)
    (cond ((< y (- (get-seqtrack-y1 seqtracknum) 2))
           (let loop ((seqtracknum2 (find-prev-visible-seqtrack seqtracknum))
                      (safety 0))                                    
             ;;(c-display "a" seqtracknum seqtracknum2)
             (cond ((or (<= seqtracknum2 0)
                        (> safety 1000))
                    0)
                   ((< y (- (get-seqtrack-y1 seqtracknum2) 2))
                    (loop (find-prev-visible-seqtrack seqtracknum2)
                          (+ safety 1)))
                   (else
                    seqtracknum2))))
          ((> y (+ 2 (get-seqtrack-y2 seqtracknum)))
           (let loop ((seqtracknum2 (find-next-visible-seqtrack seqtracknum))
                      (safety 0))
             ;;(c-display "b" seqtracknum seqtracknum2)
             (cond ((or (>= seqtracknum2 (- (<ra> :get-num-seqtracks) 1))
                        (> safety 1000))
                    (- (<ra> :get-num-seqtracks) 1))
                   ((> y (+ 2 (get-seqtrack-y2 seqtracknum2)))
                    (loop (find-next-visible-seqtrack seqtracknum2)
                          (+ safety 1)
                          ))
                   (else
                    seqtracknum2))))
          (else
           seqtracknum)))

  (define (get-statusbar-text)
    "Drag number to change seqtrack order")
  
  (add-statusbar-text-handler get-statusbar-text)

  (define was-using-sequencer-timing #f)
  (define first-seqtrack-was-audio #f)

  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set-statusbar-text! (get-statusbar-text))
     (set! has-made-undo #f)
     (set! first-seqtrack-was-audio (<ra> :seqtrack-for-audiofiles 0))
     (set! was-using-sequencer-timing (<ra> :is-using-sequencer-timing))
     (if (= button *left-button*)
         (begin
           (set! *hovering-seqtrack-number* seqtracknum)
           #t)
         (begin
          #f)))
   (lambda (button x* y* dx dy)
     (define new-seqtracknum (get-new-seqtracknum y*))
     (cond ((< new-seqtracknum seqtracknum)
            (let loop ((seqtracknum2 seqtracknum)
                       (safety 0))                                     
              (when (and (> seqtracknum2 new-seqtracknum)
                         (< safety 1000))
                (maybe-make-undo)
                (define prev-seqtracknum (find-prev-visible-seqtrack seqtracknum2))
                (<ra> :swap-seqtracks prev-seqtracknum seqtracknum2)
                (loop prev-seqtracknum
                      (+ safety 1)))))
           ((> new-seqtracknum seqtracknum)
            (let loop ((seqtracknum2 seqtracknum)
                       (safety 0))
              (when (and (< seqtracknum2 new-seqtracknum)
                         (< safety 1000))
                (maybe-make-undo)
                (define next-seqtracknum (find-next-visible-seqtrack seqtracknum2))
                (<ra> :swap-seqtracks seqtracknum2 next-seqtracknum)
                (loop next-seqtracknum
                      (+ safety 1)
                      )))))
     (set! *hovering-seqtrack-number* new-seqtracknum)
     (set! seqtracknum new-seqtracknum)
     (<ra> :set-curr-seqtrack seqtracknum #f)
     )
   (lambda (button x* y* dx dy)
     ;;(c-display (not was-using-sequencer-timing)
     ;;           (not first-seqtrack-was-audio)
     ;;           (<ra> :seqtrack-for-audiofiles 0))
     (set! *hovering-seqtrack-number* -1)
     (update-me-and-all-parents-and-siblings!) ;; Update hovered background color of all numbers. (this area might not correspond to the area of the actual current seqtrack anymore)
     (when (and (not was-using-sequencer-timing)
                (not first-seqtrack-was-audio)
                (<ra> :seqtrack-for-audiofiles 0))
       (ask-user-about-first-audio-seqtrack2
        (lambda (res)
          (if (string=? res "No")
              (<ra> :undo))))))
   )
  )

#!!
(list (<gui> :height (<gui> :get-sequencer-gui))
      ((<ra> :get-box sequencer) :height))
!!#

(def-area-subclass (<seqtrack-header> :gui :x1 :y1 :x2 :y2
                                      :use-two-rows
                                      :show-panner
                                      :bottom-visible-y2
                                      :seqtracknum)

  (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define is-bus (and for-audiofiles (<ra> :seqtrack-is-bus seqtracknum)))
  (define for-blocks (not for-audiofiles))
  (define instrument-id (if for-blocks
                            (<ra> :create-illegal-instrument)
                            (<ra> :get-seqtrack-instrument seqtracknum)))

  (if for-blocks
      (set! show-panner #f))
  
  ;;(<gui> :set-background-color gui "blue")

  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define mutesolo-width (get-mutesolo-width for-audiofiles
                                             (and for-audiofiles
                                                  (not is-bus))))
  (define meter-width (if for-audiofiles
                          (max 4 (myfloor (/ fontheight 2)))
                          0))

  (define name-height (if use-two-rows
                          (* fontheight 1.3)
                          height))

  (define b (max 0.1 (myfloor (/ fontheight 10)))) ;; border between areas.
  
  (define x-meter-split (- x2 (+ b meter-width)))
  (define x1-split (+ x1 (myfloor (* 1.8 (<gui> :text-width "9:")))))
  (define x2-split (- x-meter-split (+ b mutesolo-width)))
  (define y-split (myfloor (+ y1 name-height)))

  (delafina (get-background-color :gakk #f)
    (define is-current (or (= seqtracknum (<ra> :get-curr-seqtrack))
                           (= seqtracknum (<ra> :get-curr-seqtrack-under-mouse))))
    (cond (for-blocks
           (get-seqtrack-background-color gui seqtracknum))
          (is-current
           (get-seqtrack-background-color gui seqtracknum))
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

  (add-sub-area-plain! (<new> :seqtrack-number gui
                              x1 (+ y1 0)
                              (- x1-split 1) y-split
                              seqtracknum
                              (lambda () (<gui> :mix-colors "white" (get-background-color (<ra> :create-illegal-instrument)) 0.02))))
  
  (define panner-x2 (if use-two-rows x-meter-split x2-split))
  (define panner-y1 (if use-two-rows
                        (+ 0 y-split)
                        y1))
  (define panner-y2 (min (+ panner-y1 fontheight)
                         (- y2 fontheight)))
  
  
  (define vol-x2 panner-x2)
  (define vol-y1 (if show-panner
                     (+ 0 panner-y2)
                     panner-y1))
  (define vol-y2 y2)
    
  (add-sub-area-plain! (<new> :mute-solo-buttons gui
                              (+ b x2-split) y1
                              x-meter-split y-split
                              instrument-id
                              #t ;; use single letters
                              #t ;; stack horizontally
                              seqtracknum
                              (and for-audiofiles (not is-bus)))) ;; include-rec-button

  
  (if show-panner
      (add-sub-area-plain! (<new> :instrument-pan-slider gui
                                  x1 panner-y1
                                  panner-x2 panner-y2
                                  instrument-id
                                  :get-color get-background-color
                                  :seqtracknum seqtracknum
                                  )))

  (if for-audiofiles
      (add-sub-area-plain! (<new> :instrument-volume-slider gui
                                  (if (not use-two-rows) x1-split x1) vol-y1
                                  vol-x2 vol-y2
                                  instrument-id
                                  :effect-name "System Volume"
                                  :display-instrument-name (not use-two-rows)
                                  :get-color get-background-color
                                  :seqtracknum seqtracknum
                                  ))
      (add-sub-area-plain! (<new> :editor-seqtrack-volume-slider gui
                                  (if (not use-two-rows) x1-split x1) vol-y1
                                  vol-x2 vol-y2
                                  :display-instrument-name (not use-two-rows)
                                  :get-color get-background-color
                                  :seqtracknum seqtracknum
                                  )))

  (when for-audiofiles
    (define vam (<gui> :add-vertical-audio-meter gui (find-meter-instrument-id instrument-id) (+ b x-meter-split) y1 x2 y2))
    (when (> y2 bottom-visible-y2)
      (<gui> :set-vertical-audio-meter-clip-rect vam x1 y1 x2 bottom-visible-y2))
    )

  ;;(define vam2 (<gui> :add-vertical-audio-meter gui instrument-id (- x2 8) y1 x2 y2))
  
  ;;(<gui> :remove-vertical-audio-meter vam)
  
  (define-override (paint)
    (if (= seqtracknum (<ra> :get-curr-seqtrack))
        (<gui> :filled-box gui (get-seqtrack-background-color gui seqtracknum) x1 y1 x2 y2)))
  
  (define-override (paint?)
    (or for-blocks
        (<ra> :instrument-is-open-and-audio instrument-id)))
  
  '(set! paint (lambda ()
                 ;;(define background-color (get-instrument-background-color gui instrument-id))
                 (define background-color (<gui> :get-background-color gui))
                 ;;(define background-color (<ra> :generate-new-color))
                 (<gui> :filled-box gui background-color x1 y1 x2 y2)
                 )
         )
  
  '(set! post-paint (lambda ()
                     (define background-color (get-instrument-background-color gui instrument-id))
                     (define background-color (<ra> :generate-new-color))
                     (<gui> :do-alpha gui 0.65
                            (lambda ()
                              (<gui> :filled-box gui background-color x1 y1 x2 y2)))
                     )
        )

  (add-raw-mouse-cycle!
   :enter-func (lambda (button x* y)
                 (if (not (= button *middle-button*))
                     (maybe-autoselect-curr-seqtrack seqtracknum))
                 (if (= 0 button)
                     (set-mouse-pointer ra:set-normal-mouse-pointer gui))
                 #f))

  (define-override (get-mouse-cycle button x* y*)
    (when (inside? x* y*)
      ;;(c-display "2222222222222____HEADER seqtracknum:" seqtracknum)
      (<ra> :set-curr-seqtrack seqtracknum #f #t))
    (super:get-mouse-cycle button x* y*))

  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (if (and (= button *right-button*)
                                           (<ra> :shift-pressed)
                                           (> (<ra> :get-num-seqtracks) 1))
                                      (begin
                                        (delete-seqtrack)
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

(define *sequencer-height-dragging* #f)
(define *sequencer-height-hovering* #f)
  
(def-area-subclass (<sequencer-height-dragger> :gui :x1 :y1 :x2 :y2)
  (define background-color (<gui> :get-background-color gui))
  (define border-color (get-default-button-color gui))

  (define (is-active)
    (and (<ra> :upper-part-of-main-window-is-visible)
         (not *sequencer-window-gui-active*)))
  
  (define-override (paint)
    (draw-button gui "=" *sequencer-height-dragging*
                 x1 y1 x2 y2
                 :selected-color "button_pressed_v2"
                 :background-color "button_v2"
                 :is-hovering *sequencer-height-hovering*))

    ;;(<gui> :filled-box gui background-color x1 y1 x2 y2)
    ;;(when (is-active)
    ;;  (<gui> :draw-text gui *text-color* "=" x1 y1 x2 y2
    ;;         #f ;; wrap-lines
   ;;          #f ;; align top
   ;;          #f)
   ;;   ;;(<gui> :draw-box gui "black" x1 y1 x2 y2 1.1 3 3)
   ;;   )
   ;; )
;      (<gui> :draw-box gui border-color
;             (1+ x1) (1+ y1)
;             (1- x2) (1- y2)
;             1.3 0 0))) ;; align left

  ;; TODO: Add area::add-mouse-pointer-shape-handler
  (add-raw-mouse-cycle!
   :enter-func (lambda (button x* y)
                 ;;(c-display "ENTER DRAGGER" class-name)
                 (<ra> :set-statusbar-text "Change sequencer height")
                 (if (is-active)
                     (begin
                       (set-mouse-pointer ra:set-vertical-split-mouse-pointer gui)
                       (set! *sequencer-height-hovering* #t)
                       (update-me!)
                       #t)
                     #f))
   :leave-func (lambda (button x y)
                 ;;(c-display "LEAVE DRAGGER")
                 (<ra> :set-statusbar-text "")
                 (if (= 0 button)
                     (set-mouse-pointer ra:set-normal-mouse-pointer gui))
                 (set! *sequencer-height-hovering* #f)
                 (update-me!)))

  (add-delta-mouse-cycle!
   :press-func (lambda (button x* y*)
                 (if (is-active)
                     (begin
                       (set-mouse-pointer ra:set-vertical-split-mouse-pointer gui)
                       (set! *sequencer-height-dragging* #t)
                       (update-me!)
                       #t)
                     #f))
   :drag-func
   (lambda (button x* y* dx dy)

     (define ysplitter (if (<ra> :sequencer-in-mixer)
                           (<gui> :get-mixer-y-splitter)
                           *ysplitter*))
     
     (define size0 ((<gui> :get-splitter-sizes ysplitter) 0))
     (define size1 ((<gui> :get-splitter-sizes ysplitter) 1))
     (define size (+ size0 size1))
     
     (define new-size0 (max 10 (+ size0 dy)))     
     (define new-size1 (max 10 (- size1 dy)))
     (define new-size (+ new-size0 new-size1))
     
     ;(define size-diff (- new-size
     ;                     size))
    ; 
    ; (when (not (= size-diff 0))
    ;   (set! new-size0 (max 10 (+ new-size0 (round (/ size-diff 2)))))
    ;   (set! new-size1 (max 10 (- size new-size0))))

                          
     ;;(c-display "Y:" y ", start-y:" start-y ". DY:" (- y start-y))
     (<gui> :set-splitter-sizes ysplitter (list new-size0 new-size1))
     ;;(FROM_C-reconfigure-sequencer-left-part)
     )

   :release-func
   (lambda (button x* y* dx dy)
     (set! *sequencer-height-dragging* #f)
     (update-me!)
     (if (= 0 button)
         (set-mouse-pointer ra:set-normal-mouse-pointer gui))
     )
   )

  ;; doesn't work. must fix nonpress-mouse-cycle in area.scm
  (add-statusbar-text-handler (lambda ()
                                (if (is-active)
                                    "Change sequencer height"
                                    "")))
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

(def-area-subclass (<sequencer-left-part-buttons> :gui :x1 :y1 :x2 :y2)

  (define (callback type)
    (cond ((eq? type 'InsertE)
           (<ra> :insert-seqtrack #f))
          ((or (eq? type 'InsertA)
               (eq? type 'InsertB))
           (<ra> :insert-seqtrack #t -1 (eq? type 'InsertB)))
          ((eq? type '-)
           (if (> (<ra> :get-num-seqtracks) 1)
               (delete-seqtrack)))
          ((eq? type 'AppendE)
           (<ra> :append-seqtrack #f))
          ((eq? type 'AppendA)
           (<ra> :append-seqtrack #t))
          ((eq? type 'AppendB)
           (<ra> :append-seqtrack #t #t))
          (else
           (assert #f))))
  
  (add-raw-mouse-cycle!
   :enter-func (lambda (button x* y)
                 (if (= 0 button)
                     (set-mouse-pointer ra:set-normal-mouse-pointer gui))
                 #f))

  (define b (max 0 (/ (get-fontheight) 6)))
  (horizontally-layout-areas x1 y1 (- x2 2) y2
                             (list 'InsertE 'InsertA 'InsertB '- 'AppendE 'AppendA 'AppendB)
                             :y1-border 0 ;;(1+ b)
                             :y2-border 0 ;;(1+ b)
                             :spacing b
                             :callback
                             (lambda (type x1 y1 x2 y2)
                               (add-sub-area-plain! (<new> :button gui x1 y1 x2 y2
                                                           :text (cond ((eq? type 'InsertE) "E+")
                                                                       ((eq? type 'InsertA) "A+")
                                                                       ((eq? type 'InsertB) "B+")
                                                                       ((eq? type '-) "-")
                                                                       ((eq? type 'AppendE) "+E")
                                                                       ((eq? type 'AppendA) "+A")
                                                                       ((eq? type 'AppendB) "+B")
                                                                       (else
                                                                        (assert #f)))
                                                           :statusbar-text (list #t
                                                                                 (cond ((eq? type 'InsertE) "Insert editor seqtrack")
                                                                                       ((eq? type 'InsertA ) "Insert audio seqtrack")
                                                                                       ((eq? type 'InsertB ) "Insert audio bus")
                                                                                       ((eq? type '-) "Delete current seqtrack")
                                                                                       ((eq? type 'AppendE) "Append editor seqtrack")
                                                                                       ((eq? type 'AppendA) "Append audio seqtrack")
                                                                                       ((eq? type 'AppendB) "Append audio bus")
                                                                                       (else
                                                                                        (assert #f))))
                                                           :callback-release (lambda ()
                                                                               (callback type))
                                                           :right-mouse-clicked-callback
                                                           (lambda ()
                                                             (define func-and-args
                                                               (cond ((eq? type 'InsertE)
                                                                      (list "ra:insert-editor-seqtrack"))
                                                                     ((eq? type 'InsertA)
                                                                      (list "ra:insert-audio-seqtrack"))
                                                                     ((eq? type 'InsertB)
                                                                      (list "ra:insert-bus-seqtrack"))
                                                                     ((eq? type '-)
                                                                      (list "delete-seqtrack"))
                                                                     ((eq? type 'AppendE)
                                                                      (list "ra:append-editor-seqtrack"))
                                                                     ((eq? type 'AppendA)
                                                                      (list "ra:append-audio-seqtrack"))
                                                                     ((eq? type 'AppendB)
                                                                      (list "ra:append-bus-seqtrack"))
                                                                     (else
                                                                      (assert #f))))
                                                             (popup-menu
                                                              (get-keybinding-configuration-popup-menu-entries
                                                               :ra-funcname (car func-and-args)
                                                               :args (cdr func-and-args)
                                                               :focus-keybinding "FOCUS_SEQUENCER")
                                                              "-------------"
                                                              "Help keybindings" show-keybinding-help-window
                                                              )
                                                             )

                                                           :id (<_> 'sequencer-left-part-buttons type)))))

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
  (define show-dragger (and (not in-window)
                            (<ra> :upper-part-of-main-window-is-visible)))

  (define spacing 2)
  
  (define gridbutton-width (if show-dragger
                               (myfloor (* 1.3 (<gui> :text-width "Grid: Beat")))
                               (/ (- x2 x1)
                                  2)))
  
  (define buttons-width (myfloor (* 1.8 (<gui> :text-width "W | F |"))))
    
  (define buttons-x1 (if show-dragger
                         (- x2 buttons-width)
                         (+ gridbutton-width x1)))

  (define gridbutton-x1 (- buttons-x1 gridbutton-width spacing))

  (when show-dragger
    (define dragger (<new> :sequencer-height-dragger gui x1 y1 (- gridbutton-x1 spacing) y2))
    (add-sub-area-plain! dragger))

  (define (get-curr-grid)
    (define grid (<ra> :get-sequencer-grid-type))
    ;;(c-display "GRID:" grid)
    (cond ((string=? (<ra> :get-sequencer-grid-type) "line")
           "Line")
          ((string=? (<ra> :get-sequencer-grid-type) "beat")
           "Beat")
          (else
           "Bar")))
  
  (define-optional-func grid-checkbox (key . rest))
  
  (set! grid-checkbox
        (<new> :checkbox gui gridbutton-x1 y1 (+ gridbutton-x1 gridbutton-width) y2
               (lambda ()
                 (<ra> :sequencer-grid-enabled))
               (lambda (new-value)
                 (<ra> :set-sequencer-grid-enabled new-value))
               :text (lambda ()
                       (<-> "Grid: " (get-curr-grid)))
               :prepend-checked-marker #t
               :right-mouse-clicked-callback
               (lambda ()
                 (define grid (get-curr-grid))
                 (popup-menu
                  (list
                   :radio-buttons
                   (list "Line grid"
                         :check (string=? grid "Line")
                         :shortcut (list ra:set-sequencer-grid-type "line")
                         (lambda (ison)
                           (when ison
                             (c-display "setting ot line")
                             (<ra> :set-sequencer-grid-type "line")
                             (update-me!))))
                   (list "Beat grid"
                         :check (string=? grid "Beat")
                         :shortcut (list ra:set-sequencer-grid-type "beat")
                         (lambda (ison)
                           (when ison
                             (<ra> :set-sequencer-grid-type "beat")
                             (update-me!))))
                   (list "Bar grid"
                         :check (string=? grid "Bar")
                         :shortcut (list ra:set-sequencer-grid-type "bar")
                         (lambda (ison)
                           when ison
                           (<ra> :set-sequencer-grid-type "bar")
                           (update-me!)))
                   "--------------"
                   (list "Keybindings"
                         (list
                          "-----------------Grid on/off keybinding"
                          (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-sequencer-grid-enabled"
                                                                           :args '()
                                                                           :focus-keybinding "FOCUS_SEQUENCER"
                                                                           :gui-or-area grid-checkbox)
                          "-------------"
                          "Help keybindings" show-keybinding-help-window)))))))
                  
  (add-sub-area-plain! grid-checkbox)
  (grid-checkbox :add-statusbar-text-handler "Grid. Right-click to change type")


  (horizontally-layout-areas
   buttons-x1 y1 x2 y2
   (if (or in-window
           (<ra> :sequencer-in-mixer))
       '(window)
       '(window full))
   :spacing spacing
   :callback
   (lambda (type x1 y1 x2 y2)
     (define checkbox
       (<new> :checkbox gui x1 y1 x2 y2
              (lambda ()
                (cond ((eq? type 'window)
                       *sequencer-window-gui-active*)
                      ((eq? type 'full)
                       (not (<ra> :upper-part-of-main-window-is-visible)))
                      (else
                       (assert #f))))
              (lambda (new-value)
                ;;(c-display type "new-value:" new-value)
                (cond ((eq? type 'window)
                       (<ra> :configure-sequencer-widget new-value))
                      ((eq? type 'full)
                       (<ra> :set-sequencer-in-full-mode new-value))
                      (else
                       (assert #f)))
                (<gui> :update gui)) ;; Necessary if the dragger is at topmost position. Then the dragger gfx won't be updated.
              :text (cond ((eq? type 'window)
                           "W")
                          ((eq? type 'full)
                           "F")
                          (else
                           (assert #f)))
              :right-mouse-clicked-callback
              (lambda ()
                (popup-menu
                 (if (eq? type 'window)
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:show-hide-sequencer-in-window"
                                                                      :args '()
                                                                      :focus-keybinding "FOCUS_SEQUENCER")
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-sequencer-in-full-mode"
                                                                      :args '()
                                                                      :focus-keybinding "FOCUS_SEQUENCER"))
                 "-------------"
                 "Help keybindings" show-keybinding-help-window))))
     
     
     (checkbox :add-statusbar-text-handler
               (cond ((eq? type 'window)
                      "Window mode")
                     ((eq? type 'full)
                      "Full mode. Use all available space in main window")
                     (else
                      (assert #f))))
     
     (add-sub-area-plain! checkbox)
     )
   )
                                 
  )

#!!
(map (lambda (seqtracknum)
       (list seqtracknum
             (<ra> :get-seqtrack-y1 seqtracknum)
             (<ra> :get-seqtrack-y2 seqtracknum)))
     (iota (<ra> :get-num-seqtracks)))
!!#


(define *scrollbar-is-moving* #f)

#!!
(<ra> :get-topmost-visible-seqtrack)
(<ra> :get-lowest-possible-topmost-visible-seqtrack)
!!#

  

(def-area-subclass (<sequencer-left-part> :gui :x1 :y1 :x2 :y2 :seqtrack-x1)
  (define num-seqtracks (<ra> :get-num-seqtracks))

  (define topmost-seqtrack (<ra> :get-topmost-visible-seqtrack))

  (define seqtrack0-y1 (<ra> :get-seqtracks-y1)) ;;(<ra> :get-seqtrack-y1 topmost-seqtrack))
  
  (define b 0)
  (define b/2 (/ b 2))

  (define topbar-height (myfloor (+ ((<ra> :get-box seqtimeline-area) :height)
                                     (if (not (<ra> :seqtempo-visible))
                                         0
                                         ((<ra> :get-box seqtempo-area) :height)))))
  
  (define ty1 (get-sequencer-left-part-seqtracks-y1))
  (define ty2 (get-sequencer-left-part-seqtracks-y2))

  ;;(c-display "       ___:" x1 y1 x2 y2 ty1 ty2 seqtrack-x1)

  (define topbar-y2 (+ y1 topbar-height))
  (define seqtracks-y2 (get-sequencer-left-part-seqtracks-y2))
  
  (add-sub-area-plain! (<new> :sequencer-left-part-top-bar gui x1 (+ y1 2) x2 (- topbar-y2 2)))

  (define show-tempos (<ra> :show-tempos-sequencer-lane))
  (define show-signatures (<ra> :show-signatures-sequencer-lane))
  (define show-markers (<ra> :show-markers-sequencer-lane))

  (if (or show-tempos show-signatures show-markers)
      (let* ((timeline-height (- (<ra> :get-seqtimeline-area-y2) (<ra> :get-sequencer-y1)))
             (y1 (+ y1 timeline-height))
             (header-height (- seqtrack0-y1 (<ra> :get-seqtimeline-area-y2)))
             (y2 (+ y1 header-height)))
        ;;(c-display "timeline-height:" timeline-height ". header-height:" header-height ". topmost seqtrack:" topmost-seqtrack". stuff: " seqtrack0-y1 (<ra> :get-seqtimeline-area-y2))
        (add-sub-area-plain! (<new> :sequencer-timeline-headers gui x1 y1 x2 y2))))
  
  (define header-area (<new> :area gui seqtrack-x1 topbar-y2 x2 (- ty2 1)))
  (add-sub-area-plain! header-area)

  (define lowest-seqtrack 0)

  (define topmost-seqtrack-y1 0)
  
  (let loop ((seqtracknum topmost-seqtrack))
    (set! lowest-seqtrack seqtracknum)
      
    (when (< seqtracknum num-seqtracks)
      (define seqtrack-box (<ra> :get-box seqtrack seqtracknum))
      (define sy1 (+ ty1 (- (seqtrack-box :y1) seqtrack0-y1)))
      (define sy2 (+ ty1 (- (seqtrack-box :y2) seqtrack0-y1)))

      (if (or (< sy2 topbar-y2)
              (not (<ra> :get-seqtrack-visible seqtracknum)))
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

            (if (= 1 num-seqtracks)
                (set! sy2 (max sy2 seqtracks-y2)))
            
            (if (= topmost-seqtrack seqtracknum)
                (set! topmost-seqtrack-y1 sy1))
            
            (define use-two-rows (> (seqtrack-box :height)
                                    (* 2.5 (get-fontheight))))
            
            (define show-panner (> (seqtrack-box :height)
                                   (* 4.0 (get-fontheight))))
            
            (if (or (not (<ra> :seqtrack-for-audiofiles seqtracknum))
                    (<ra> :is-legal-instrument (<ra> :get-seqtrack-instrument seqtracknum)))
                (header-area :add-sub-area-plain! (<new> :seqtrack-header gui seqtrack-x1 sy1 x2 sy2 use-two-rows show-panner seqtracks-y2 seqtracknum)))
            
            (loop (1+ seqtracknum))))))

  (if (> (<ra> :get-lowest-possible-topmost-visible-seqtrack)
         0)
      (let ()
        (define scrollbar-x1 x1)
        (define scrollbar-x2 (- seqtrack-x1 (<ra> :get-seqtrack-border-width)))
        
        (define slider-length (/ (- seqtracks-y2 topmost-seqtrack-y1)
                                 (get-actual-total-seqtracks-height)))

        (define first-visible-seqtrack (find-first-visible-seqtrack))
        (define last-visible-seqtrack (find-last-visible-seqtrack))
        
        (define org-slider-pos (scale (<ra> :get-seqtrack-y1 topmost-seqtrack)
                                      (<ra> :get-seqtrack-y1 first-visible-seqtrack)
                                      (+ (<ra> :get-seqtrack-y1 first-visible-seqtrack)
                                         (get-actual-total-seqtracks-height))
                                      0 1))

        (c-display "visible height:" (- seqtracks-y2 topmost-seqtrack-y1)
                   "y1/y2:" (<ra> :get-seqtrack-y1 first-visible-seqtrack) (<ra> :get-seqtrack-y2 last-visible-seqtrack)
                   ". total height:" (get-actual-total-seqtracks-height) ;;(- (<ra> :get-seqtrack-y2 (- (<ra> :get-num-seqtracks) 1)) (<ra> :get-seqtrack-y1 0))
                   ". slider-length:" slider-length
                   ". org-slider-pos:" org-slider-pos
                   ". topmost-seqtrack-y1:" topmost-seqtrack-y1)

        (add-sub-area-plain! (<new> :scrollbar gui scrollbar-x1 topmost-seqtrack-y1 scrollbar-x2 seqtracks-y2
                                    :callback (lambda (slider-pos slider-len)
                                                '(c-display "callback:" slider-pos slider-len ". dist:" (- slider-len slider-pos))
                                                '(c-display "dy:" (- slider-pos org-slider-pos)
                                                           (scale (- slider-pos org-slider-pos)
                                                                  0 1
                                                                  0 (get-actual-total-seqtracks-height)))
                                                (c-display "len/length:" slider-pos slider-len)
                                                (set-topmost-visible-seqtrack-from-percentage slider-pos)
                                                )
                                    :slider-pos org-slider-pos
                                    :slider-length slider-length
                                    ;;:slider-length (scale (- (+ 1 lowest-seqtrack) topmost-seqtrack)
                                    ;;                     0 (<ra> :get-num-seqtracks)
                                    ;;                    0 1)
                                    :vertical #t
                                    :mouse-press-callback (lambda ()
                                                            (set! *scrollbar-is-moving* #t))
                                    :mouse-release-callback (lambda ()
                                                              (set! *scrollbar-is-moving* #f))
                                    :is-moving (lambda ()
                                                 *scrollbar-is-moving*)
                                    ))))
  
  (add-sub-area-plain! (<new> :sequencer-left-part-buttons gui x1 ty2 x2 y2))

  (define background-color (<gui> :get-background-color gui))

  (define-override (paint)
    ;;(c-display "   Scheme: Painting left part")
    ;;(define-override (paint)
    (<gui> :filled-box gui "high_background" x1 y1 x2 y2)


    ;;(<gui> :filled-box gui background-color x1 y1 x2 y2)
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
             (*testarea* :about-to-be-removed-internal!))))


(if *use-testgui*
    (<gui> :show *testgui*))
  

(define (get-sequencer-left-part-position kont)
  (define header-box (<ra> :get-box sequencer-left-part))

  (when (not (<ra> :release-mode))
    (assert (= (<gui> :height (<gui> :get-sequencer-gui))
               ((<ra> :get-box sequencer) :height))))

  (kont (header-box :x1) (header-box :y1)
        (header-box :x2) (header-box :y2)))

(define *had-sequencer-paint-callback* (defined? '*seqtrack-headers-has-run*))
(define *seqtrack-headers-has-run* #t)

(define (get-sequencer-left-part-area)
  (when (not *sequencer-left-part-area*)
    (set! *sequencer-left-part-area* (if *use-testgui*
                                         *testarea*
                                         (get-sequencer-left-part-position
                                          (lambda (x1 y1 x2 y2)
                                            (<new> :area (<gui> :get-sequencer-gui)
                                                   0 y1 x2 y2)))))
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
                   (let ((ret (*sequencer-timing-area* :mouse-callback-internal button state x y)))
                     ;;(c-display "timing area mouse ret:" ret)
                     ret))
               (if (or (*sequencer-left-part-area* :has-mouse)
                       (and *sequencer-right-part-area*
                            (*sequencer-right-part-area* :has-mouse))
                       (and *sequencer-timing-area*
                            (*sequencer-timing-area* :has-mouse)))
                   (begin
                     ;;(c-display "----cancel 2. left/right/timing:"
                     ;;           (*sequencer-left-part-area* :has-mouse)
                     ;;           (and *sequencer-right-part-area*
                     ;;                (*sequencer-right-part-area* :has-mouse))
                     ;;           (and *sequencer-timing-area*
                     ;;                (*sequencer-timing-area* :has-mouse)))
                     (if (or (*sequencer-left-part-area* :has-mouse)
                             (and *sequencer-right-part-area*
                                  (*sequencer-right-part-area* :has-mouse)))
                         (<ra> :cancel-seq-indicator))
                     #t)
                   #f)))
      ;(<gui> :add-resize-callback gui ;; TODO: I think this resize callback can be removed.
      ;       (lambda (width height)
      ;         (eat-errors :try FROM_C-reconfigure-sequencer-left-part)))

      ))
  
  *sequencer-left-part-area*)

(define (FROM_C-reconfigure-sequencer-left-part)
  ;;(c-display "---------------------   Scheme: Reconfiguring left part")

  (get-sequencer-left-part-area)

  (define gui (if *use-testgui*
                  *testgui*
                  (<gui> :get-sequencer-gui)))

  (<gui> :remove-all-vertical-audio-meters gui)

  (<declare-variable> recreate-seqtracks-config-area)
  
  (if (and (defined? 'recreate-seqtracks-config-area)
           recreate-seqtracks-config-area)
      (recreate-seqtracks-config-area))
  
  (if *use-testgui*
      (begin

        (define height ((<ra> :get-box sequencer) :height))
        ;;(define height (- (<gui> :height *testgui*) 60))

        (if (not (<ra> :release-mode))
            (assert (= (<gui> :height (<gui> :get-sequencer-gui))
                       ((<ra> :get-box sequencer) :height))))
        
        ;;(define *testarea* (<new> :horizontal-instrument-slider *testgui* 20 30 110 120 (car (get-all-audio-instruments))))
        ;;(define *testarea* (<new> :mute-solo-buttons *testgui* 20 30 110 120 (car (get-all-audio-instruments)) #t #t))
        (*sequencer-left-part-area* :reset! 20 30 210 (+ 30 height))
        
        (*sequencer-left-part-area* :add-sub-area-plain! (<new> :sequencer-left-part gui
                                                                20 30 
                                                                210 (+ 30 height)
                                                                10
                                                                )))
      (get-sequencer-left-part-position
       (lambda (x1 y1 x2 y2)
         (when (not (<ra> :release-mode))
           (assert (= (<gui> :height (<gui> :get-sequencer-gui))
                      ((<ra> :get-box sequencer) :height)))
           (assert (= (- y2 y1)
                      (<gui> :height (<gui> :get-sequencer-gui)))))
         (*sequencer-left-part-area* :reset! 0 y1 x2 y2)        
         (*sequencer-left-part-area* :add-sub-area-plain! (<new> :sequencer-left-part gui
                                                                 0 y1 x2 y2
                                                                 x1
                                                                 )))))
  )


(when *use-testgui*
  (<gui> :add-resize-callback *testgui*
         (lambda (width height)
           (eat-errors :try FROM_C-reconfigure-sequencer-left-part)))

  (FROM_C-reconfigure-sequencer-left-part))

#!!
(FROM_C-reconfigure-sequencer-left-part)
!!#


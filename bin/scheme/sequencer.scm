
(provide 'sequencer.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)


;;(define *curr-seqtrack-color* "#7c3a3a")
;;(define *curr-seqtrack-color* "#776757")
(define *curr-seqtrack-color* (ra:gui_mix-colors *current-mixer-strip-border-color* "black" 0.6))
;;(define *curr-seqtrack-color* (<gui> :mix-colors *current-mixer-strip-border-color* "white" 0.92))

(define (show-sequencer-header-popup-menu instrument-id parentgui)
  (popup-menu
   (list "Reset volume"
         (lambda ()
           (<ra> :undo-instrument-effect instrument-id "System Volume")
           (<ra> :set-instrument-effect instrument-id "System Volume" (db-to-radium-normalized 0.0))))
   "------------"
   (get-instrument-popup-entries instrument-id parentgui)))


(define *has-shown-record-message* #t)
(define (maybe-show-record-message)
  (when (not *has-shown-record-message*)
    (show-async-message :text "Recording audio is a technology preview. It seems to work fine, but it could have some bugs.<p>Current limitations:<UL><LI>You can only record from the input connections of the seqtrack instrument,<br>not from the main inputs of the program. (For now you have to manually<br>connect a \"System In\" object to the seqtrack instrument.)<LI>You can only record stereo files.<LI>There is no punch in and punch out yet.</UL>")
    (set! *has-shown-record-message* #t)))



(define (show-record-popup-menu seqtracknum)
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
                                                                            (c-display (<-> input-channel " -> " soundfile-channel ": " ison)))))
                                                                 (iota 8))))
                                                   (iota 8)))))
                           matrix))
                  
                  (<gui> :group "Use custom settings for this seqtrack?"
                         (<gui> :vertical-layout
                                (<gui> :radiobutton "Yes. (These settings applies to this seqtrack only)"
                                       (<ra> :get-seqtrack-use-custom-recording-config seqtracknum)
                                       (lambda (ison)
                                         (<ra> :set-seqtrack-use-custom-recording-config seqtracknum ison)))                                         
                                (<gui> :radiobutton "No. (These settings applies to all seqtracks with non-custom settings)"
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
                                                     :background-color (if (= seqtracknum (<ra> :get-curr-seqtrack))
                                                                           *curr-seqtrack-color*
                                                                           (get-mixer-strip-background-color gui instrument-id))
                                                     :border 0
                                                     :implicit-border 1
                                                     ))
                                    (lambda (_)
                                      (define is-selected (not (get-selected type)))
                                      (undo-block
                                       (lambda ()
                                         (cond ((eq? type 'record)
                                                (if (not (get-recording))
                                                    (maybe-show-record-message))
                                                (<ra> :set-seqtrack-is-recording seqtracknum is-selected)
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
                                    :right-mouse-clicked-callback (if (eq? type 'record)
                                                                      (lambda ()
                                                                        (show-record-popup-menu seqtracknum))
                                                                      #f)))

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
          (<gui> :filled-box gui *curr-seqtrack-color* x1 y1 x2 y2)
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


(def-area-subclass (<seqtrack-header> :gui :x1 :y1 :x2 :y2
                                      :use-two-rows
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
                                  x1 y1 x1-split
                                  y-split
                                  instrument-id
                                  seqtracknum)))

  (if for-audiofiles
      (add-sub-area-plain! (<new> :mute-solo-buttons gui
                                  (+ b x1-split) y1
                                  x-meter-split y-split
                                  instrument-id #t #t seqtracknum)))

  (if for-audiofiles
      (add-sub-area-plain! (<new> :horizontal-instrument-slider gui
                                  x1 (if use-two-rows (+ b y-split) y1)
                                  (if use-two-rows x-meter-split x1-split) y2
                                  instrument-id
                                  :use-two-rows use-two-rows
                                  :get-color (lambda ()
                                               (if (= seqtracknum (<ra> :get-curr-seqtrack))
                                                   *curr-seqtrack-color*
                                                   (<ra> :get-instrument-color instrument-id)))
                                  )))

  (if for-audiofiles
      (<gui> :add-vertical-audio-meter gui instrument-id (+ b x-meter-split) y1 x2 y2))

  ;;(define vam2 (<gui> :add-vertical-audio-meter gui instrument-id (- x2 8) y1 x2 y2))
  
  ;;(<gui> :remove-vertical-audio-meter vam)

  (define-override (paint)
    (if (= seqtracknum (<ra> :get-curr-seqtrack))
        (<gui> :filled-box gui *curr-seqtrack-color* x1 y1 x2 y2)))
  
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

  (if for-audiofiles
      (add-mouse-cycle! :press-func (lambda (button x* y*)
                                      (if (= button *right-button*)
                                          (begin
                                            (<ra> :schedule 0 ;; Workaround. Opening a popup menu causes Qt to skip the drag and release mouse events.
                                                  (lambda ()
                                                    (show-sequencer-header-popup-menu instrument-id gui)
                                                    #t))
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

(def-area-subclass (<sequencer-left-part-buttons> :gui :x1 :y1 :x2 :y2)

  (define (callback type)
    (cond ((eq? type '+E)
           (<ra> :insert-seqtrack #f))
          ((eq? type '+A)
           (<ra> :insert-seqtrack #t))
          ((eq? type '-)
           (when (> (<ra> :get-num-seqtracks) 1)
             (define seqtracknum (<ra> :get-curr-seqtrack))
             (set! *current-seqblock-info* #f)
             (<ra> :delete-seqtrack seqtracknum)))
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
  (horizontally-layout-areas x1 y1 x2 y2
                             (list '+E '+A '- 'AppendE 'AppendA)
                             :y1-border (1+ b)
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
                                                           :statusbar-text (cond ((eq? type '+E) "Insert editor seqtrack")
                                                                                 ((eq? type '+A ) "Insert audio seqtrack")
                                                                                 ((eq? type '-) "Delete current seqtrack")
                                                                                 ((eq? type 'AppendE) "Append editor seqtrack")
                                                                                 ((eq? type 'AppendA) "Append audio seqtrack")
                                                                                 (else
                                                                                  (assert #f)))
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
          (add-sub-area-plain! (<new> :seqtrack-header gui x1 sy1 x2 sy2 use-two-rows seqtracknum)))
      
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



;;(<gui> :height *testgui*)

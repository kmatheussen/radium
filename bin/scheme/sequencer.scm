
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
    (if (eq? type 'solo)
        (get-soloed)
        (get-muted)))

  (define layout-func (if stack-horizontally
                          horizontally-layout-areas
                          vertically-layout-areas))

  (layout-func x1 y1 x2 y2
               (list 'mute 'solo)
               :callback
               (lambda (type x1 y1 x2 y2)
                 (add-sub-area-plain! (<new> :checkbox gui x1 y1 x2 y2
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
                                                  (if (eq? type 'solo)
                                                      (begin
                                                        (<ra> :set-instrument-solo instrument-id is-selected)
                                                        (if (<ra> :control-pressed)
                                                            (turn-off-all-solo instrument-id)))
                                                      (begin
                                                        (<ra> :set-instrument-mute instrument-id is-selected)
                                                        ;;(c-display "mute: " is-muted)
                                                        (if (<ra> :control-pressed)
                                                            (turn-off-all-mute instrument-id)))))))))))
  )

(def-area-subclass (<instrument-name> :gui :x1 :y1 :x2 :y2
                                      :instrument-id
                                      :seqtracknum)
  
  (define last-painted-name "")

  (define (paint)
                ;;(<gui> :set-clip-rect gui x1 y1 x2 y2)
                (let* ((b 0)
                       (x1 (+ x1 b))
                       (y1 (+ y1 b))
                       (x2 (- x2 b))
                       (y2 (- y2 b)))

                  (if (= seqtracknum (<ra> :get-curr-seqtrack))
                      (<gui> :filled-box gui *curr-seqtrack-color* x1 y1 x2 y2)
                      (paint-instrument-background-color gui x1 y1 x2 y2 instrument-id))

                  (define instrument-name (<ra> :get-instrument-name instrument-id))
                  (set! last-painted-name instrument-name)
                  (<gui> :draw-text gui *text-color* (<-> seqtracknum ": " instrument-name)
                         (+ 4 x1) y1 x2 y2
                         #f ;; wrap-lines
                         #f ;; align top
                         #t) ;; align left
                  (define background-color (<gui> :get-background-color gui))
                  (<gui> :draw-box gui background-color (+ 0 x1) (+ 0 y1) (- x2 0) (- y2 0) 2 0 0)
                  (<gui> :draw-box gui *mixer-strip-border-color* x1 y1 x2 y2 1.5 5 5))
                ;;(<gui> :cancel-clip-rect gui)
                )

  (<ra> :schedule (random 1000)
        (lambda ()
          (and is-alive (<gui> :is-open gui) (<ra> :instrument-is-open-and-audio instrument-id)
               (begin
                 (if (not (string=? last-painted-name (<ra> :get-instrument-name instrument-id)))
                     (update-me!))
                 300))))
  
  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (= button *left-button*))
                    :release-func
                    (lambda (button x* y*)
                      (and (= button *left-button*)
                           (let* ((old-name (<ra> :get-instrument-name instrument-id))
                                  (new-name (<ra> :request-string "New name:" #t old-name)))
                             (c-display "GAKKKGAKK_________ NEWNAME" (<-> "-" new-name "-"))
                             (when (and (not (string=? new-name ""))
                                        (not (string=? new-name old-name)))
                               (<ra> :set-instrument-name new-name instrument-id)
                               (update-me!))
                             #f)))) ;; Mouse cycle is screwed up when focus is switeched to a different widget. #f fixes this.
  )


(def-area-subclass (<seqtrack-header> :gui :x1 :y1 :x2 :y2
                                      :instrument-id
                                      :use-two-rows
                                      :seqtracknum)

  ;;(<gui> :set-background-color gui "blue")

  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define mutesolo-width (myfloor (* 1.8 (<gui> :text-width "M S "))))
  (define meter-width (max 4 (myfloor (/ fontheight 2))))

  (define name-height (if use-two-rows
                          (* fontheight 1.3)
                          height))

  (define b (max 1 (myfloor (/ fontheight 6)))) ;; border between areas.
  
  (define x-meter-split (- x2 (+ b meter-width)))
  (define x1-split (- x-meter-split (+ b mutesolo-width)))
  (define y-split (myfloor (+ y1 name-height)))


  (if use-two-rows
      (add-sub-area-plain! (<new> :instrument-name gui
                                  x1 y1 x1-split
                                  y-split
                                  instrument-id
                                  seqtracknum)))

  (add-sub-area-plain! (<new> :mute-solo-buttons gui
                              (+ b x1-split) y1
                              x-meter-split y-split
                              instrument-id #t #t seqtracknum))
  (add-sub-area-plain! (<new> :horizontal-instrument-slider gui
                              x1 (if use-two-rows (+ b y-split) y1)
                              (if use-two-rows x-meter-split x1-split) y2
                              instrument-id
                              :use-two-rows use-two-rows
                              :get-color (lambda ()
                                           (if (= seqtracknum (<ra> :get-curr-seqtrack))
                                               *curr-seqtrack-color*
                                               (<ra> :get-instrument-color instrument-id)))
                              ))

  (define vam (<gui> :add-vertical-audio-meter gui instrument-id (+ b x-meter-split) y1 x2 y2))
  ;;(define vam2 (<gui> :add-vertical-audio-meter gui instrument-id (- x2 8) y1 x2 y2))
  
  ;;(<gui> :remove-vertical-audio-meter vam)

  (define (paint)
    (if (= seqtracknum (<ra> :get-curr-seqtrack))
        (<gui> :filled-box gui *curr-seqtrack-color* x1 y1 x2 y2)))

  (set! paint? (lambda ()
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

  (define get-mouse-cycle-org get-mouse-cycle)
  (define (get-mouse-cycle button x* y*)
    (when (inside? x* y*)
      ;;(c-display "____HEADER seqtracknum:" seqtracknum)
      (<ra> :select-seqtrack seqtracknum))
    (get-mouse-cycle-org button x* y*))
  
  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (if (= button *right-button*)
                                      (begin
                                        (show-sequencer-header-popup-menu instrument-id gui)
                                        #t)
                                      #f)))  
  )


(def-area-subclass (<sequencer-height-dragger> :gui :x1 :y1 :x2 :y2)
  (define background-color (<gui> :get-background-color gui))
  (define border-color (<gui> :mix-colors background-color "black" 0.5))

  (define (paint)
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

  (add-statusbar-text-handler (lambda ()
                                "Change sequencer height"))
  
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
    (cond ((eq? type '+)
           (define seqtracknum (<ra> :get-curr-seqtrack))
           (<ra> :insert-seqtrack seqtracknum))
          ((eq? type '-)
           (when (> (<ra> :get-num-seqtracks) 1)
             (define seqtracknum (<ra> :get-curr-seqtrack))
             (set! *current-seqblock-info* #f)
             (<ra> :delete-seqtrack seqtracknum)))
          ((eq? type 'Append)
           (<ra> :append-seqtrack))
          (else
           (assert #f))))

  (define b (max 0 (/ (get-fontheight) 6)))
  (horizontally-layout-areas x1 y1 x2 y2
                             (list '+ '- 'Append)
                             :y1-border (1+ b)
                             :spacing b
                             :callback
                             (lambda (type x1 y1 x2 y2)
                               (add-sub-area-plain! (<new> :button gui x1 y1 x2 y2
                                                           :text (to-string type)
                                                           :statusbar-text (cond ((eq? type '+) "Insert seqtrack")
                                                                                 ((eq? type '-) "Delete current seqtrack")
                                                                                 ((eq? type 'Append) "Append seqtrack")
                                                                                 (else
                                                                                  (assert #f)))                                                                                  
                                                           :callback (lambda ()
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

      (define instrument-id (<ra> :get-seqtrack-instrument seqtracknum))
      (if (>= instrument-id 0)
          (add-sub-area-plain! (<new> :seqtrack-header gui x1 sy1 x2 sy2 instrument-id use-two-rows seqtracknum)))
      
      (loop (1+ seqtracknum))))

  (add-sub-area-plain! (<new> :sequencer-left-part-buttons gui x1 ty2 x2 y2))

  (define background-color (<gui> :get-background-color gui))

  (define (paint)
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

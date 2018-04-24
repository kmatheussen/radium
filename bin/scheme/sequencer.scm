
(provide 'sequencer.scm)



(define (show-sequencer-header-popup-menu instrument-id parentgui)
  (popup-menu
   (list "Reset volume"
         (lambda ()
           (<ra> :undo-instrument-effect instrument-id "System Volume")
           (<ra> :set-instrument-effect instrument-id "System Volume" (db-to-radium-normalized 0.0))))
   "------------"
   (get-instrument-popup-entries instrument-id parentgui)))
  

(define (create-seqtrack-volume-slider instrument-id instrument-name use-two-rows)
  (define (get-db-value)
    (radium-normalized-to-db (<ra> :get-stored-instrument-effect instrument-id "System Volume")))
  (define (set-db-value db)    
    (<ra> :set-instrument-effect instrument-id "System Volume" (db-to-radium-normalized db)))

  (define (get-scaled-value)
    (db-to-slider (get-db-value)))
  
  (define has-made-undo #f)

  (define (maybe-make-undo)
    (when (not has-made-undo)
      (set! has-made-undo #t)
      (<ra> :undo-instrument-effect instrument-id "System Volume")))

  (define automation-value #f)
  (define automation-color (<ra> :get-instrument-effect-color instrument-id "System Volume"))
  (define (get-automation-data kont)
    (if automation-value
        (kont automation-value automation-color)))

  (define last-value -20000)
  (define volume-slider #f)
  (set! volume-slider (<gui> :horizontal-slider "" 0 (get-scaled-value) 1.0
                             (lambda (val)
                               ;;(<ra> :set-instrument-effect instrument-id effect-name val)
                               (when volume-slider

                                 (define db (slider-to-db val))
                                 ;;(c-display "new-db:" db ", old-db:" last-value)
                                 (when (not (= last-value db))
                                   (set! last-value db)
                                   (maybe-make-undo)
                                   (set-db-value db))
                               
                                 ;;(set-value val)
                                 (<gui> :update volume-slider)
                                 ;;(<ra> :set-current-instrument first-instrument-id)
                                 ))))
  
  (add-gui-effect-monitor volume-slider instrument-id "System Volume" #t #t
                          (lambda (radium-normalized automation)
                            (when radium-normalized
                              (define new-value (radium-normalized-to-slider radium-normalized))
                              (<gui> :set-value volume-slider new-value))
                            (when automation
                              (set! automation-value (if (< automation 0)
                                                         #f
                                                         automation))
                              (<gui> :update volume-slider))))

  (define (get-volume-slider-value-text value)
    (db-to-text (slider-to-db value) #t))

  (define (get-volume-slider-text)
    (let ((volume-text (get-volume-slider-value-text (get-scaled-value))))
      (if use-two-rows
          (<-> "  " volume-text)
          (<-> instrument-name ": " volume-text))))
    
  (add-safe-paint-callback volume-slider
                           (lambda (width height)
                             (paint-horizontal-instrument-slider volume-slider
                                                                 instrument-id
                                                                 (get-scaled-value)
                                                                 (get-volume-slider-text)
                                                                 #t
                                                                 #f
                                                                 get-automation-data
                                                                 0
                                                                 width height)))

  (define is-dragging #f)

  (add-safe-mouse-callback volume-slider                           
                           (lambda (button state x y)
                             (when (and (= state *is-pressing*)
                                        (= button *right-button*))
                               (show-sequencer-header-popup-menu instrument-id volume-slider))
                             (when (and (= state *is-pressing*)
                                        (= button *left-button*))
                               (set! has-made-undo #f)
                               (set! is-dragging #t))
                             (if (= state *is-releasing*)
                                 (set! is-dragging #f))
                             (let ((status-text (get-volume-slider-text)))
                               (if is-dragging 
                                   (set-tooltip-and-statusbar status-text)
                                   (<ra> :set-statusbar-text status-text)))
                             (when (= state *is-releasing*)
                               (c-display "RELEASE")
                               (<gui> :tool-tip ""))
                             #f))

  volume-slider)

(define (create-seqtrack-header-gui instrument-id use-two-rows)
  (define gui (<gui> :horizontal-layout))

  (define instrument-name (<ra> :get-instrument-name instrument-id))

  ;;(define system-background-color (<gui> :get-background-color gui))
  (define background-color (get-mixer-strip-background-color gui instrument-id))
  (define instrument-color (<ra> :get-instrument-color instrument-id))

  (define name-gui (and use-two-rows
                        (let ((name-gui (<gui> :line instrument-name
                                               (lambda (new-name)
                                                 (c-display "NEWNAME" new-name)
                                                 (<ra> :set-instrument-name new-name instrument-id)))))
                          (<gui> :set-background-color name-gui background-color)
                          name-gui)))

  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  ;;(define pan-height fontheight-and-borders)
  (define mutesolo-height fontheight-and-borders)

                               
  (define volume-slider (create-seqtrack-volume-slider instrument-id instrument-name use-two-rows))

  ;;(define pan-gui (create-mixer-strip-pan instrument-id
  ;;                                        strips-config
  ;;                                        system-background-color
  ;;                                        background-color
  ;;                                        pan-height))

  (define mutesolo-gui (create-mixer-strip-mutesolo instrument-id 
                                                    #f
                                                    background-color
                                                    mutesolo-height
                                                    #t
                                                    #t
                                                    :set-fixed-size #f))
  (<gui> :set-layout-spacing mutesolo-gui 2 0 0 0 0)

  (define meter-instrument-id (find-meter-instrument-id instrument-id))
  (define meter-gui (<gui> :vertical-audio-meter meter-instrument-id))

  (define upper-row (and use-two-rows
                         (let ((upper-row (<gui> :horizontal-layout
                                                 name-gui
                                                 mutesolo-gui)))
                           (<gui> :set-size-policy upper-row #t #f)
                           (<gui> :set-layout-spacing upper-row 2 2 0 2 0)
                           upper-row)))

  (if use-two-rows
      (<gui> :add gui
             (let ((rows (<gui> :vertical-layout
                                upper-row
                                volume-slider)))
               (<gui> :set-layout-spacing rows 2 2 0 2 0)
               rows))
      (begin
        (<gui> :add gui volume-slider)
        (<gui> :add gui mutesolo-gui)))

  (<gui> :add gui
         meter-gui)
  
  (<gui> :set-layout-spacing gui 0 2 2 2 2)
  
  ;(<gui> :set-size-policy volume-slider #t #t)
  ;(<gui> :set-size-policy mutesolo-gui #t #t)
  ;;(<gui> :set-size-policy gui #t #t)
  
  ;;(set-fixed-height volume-slider (<gui> :height mutesolo-gui))
  (<gui> :set-size-policy volume-slider #t #t)
  (<gui> :set-size-policy mutesolo-gui #t #t)

  (set-fixed-width meter-gui (max 4 (floor (/ fontheight 2))))
  (<gui> :set-size-policy meter-gui #f #t)

  ;;(if name-gui
  ;;    (<gui> :set-min-height name-gui (floor (* fontheight 1.5))))

  (set-fixed-width mutesolo-gui (floor (* 1.8 (<gui> :text-width "M S"))))

  (<gui> :set-size-policy gui #t #t)

  gui)


(if *is-initializing*
    (let ((num-rows 1)
          (last-showed-seqtempo #f))
      (<ra> :schedule 100
            (lambda ()
              (define num-seqtracks (<ra> :get-num-seqtracks))
              (define show-seqtempo (<ra> :seqtempo-visible))
              ;;(c-display "num-rows/num-seqtracks:" num-rows num-seqtracks)
              (when (or (not (= num-rows num-seqtracks))
                        (not (eq? last-showed-seqtempo show-seqtempo)))
                (set! num-rows num-seqtracks)
                (set! last-showed-seqtempo show-seqtempo)
                (recreate-left-part-gui))
              70))))

#!!    
(define testgui (create-seqtrack-header-gui (<ra> :get-seqtrack-instrument seqtracknum) #t))
(<gui> :show testgui)
!##

(define (add-seqtrack-header gui seqtracknum use-two-rows)
  (define instrument-id (<ra> :get-seqtrack-instrument seqtracknum))  

  (define sequencer-box (<ra> :get-box sequencer))
  (define seqtrack-box (<ra> :get-box seqtrack seqtracknum))
  (define y1 (floor (- (seqtrack-box :y1) (sequencer-box :y1))))
  (define y2 (floor (- (seqtrack-box :y2) (sequencer-box :y1))))

  ;;(define mixer-strip (create-standalone-mixer-strip instrument-id width (floor height)))
  (define seqheader-gui (if (>= instrument-id 0)
                            (create-seqtrack-header-gui instrument-id use-two-rows)
                            (let ((gui (<gui> :widget)))
                              (<gui> :set-size-policy gui #t #t)
                              gui)))
    
  (<gui> :add gui seqheader-gui);; 0 y1 width y2)
  ;;(set-fixed-size mixer-strip width height)

  ;;(c-display "   " seqtracknum ": ADDed TO: " 0 y1 100 y2)

  (add-safe-mouse-callback seqheader-gui
                           (lambda (button state x y)
                             (when (and (= state *is-pressing*)
                                        (= button *right-button*))
                               (show-sequencer-header-popup-menu instrument-id seqheader-gui))
                             #f))

  seqheader-gui)


(define *g-sequencer-left-part-gui* (if *is-initializing*
                                        #f
                                        *g-sequencer-left-part-gui*))

(delafina (create-sequencer-left-part-gui :use-two-rows 'undefined)
  (define gui (<gui> :vertical-layout));;widget))
  (<gui> :set-layout-spacing gui 0 0 0 0 0)

  (define sequencer-box (<ra> :get-box sequencer))
  (set-fixed-width gui (floor (* 1.5 (<gui> :text-width "S Seqtrack 0.... And M+S|"))))
  ;;(<gui> :set-background-color gui "red")

  (define upper-left (<gui> :widget))
  (if (not (<ra> :release-mode))
      (<gui> :set-background-color upper-left "red"))
  (set-fixed-height upper-left (floor (+ ((<ra> :get-box seqtimeline-area) :height)
                                         (if (not (<ra> :seqtempo-visible))
                                             0
                                             ((<ra> :get-box seqtempo-area) :height)))))
  (<gui> :add gui upper-left)

  (define (use-two-rows?)
    (let* ((seqtrack-box (<ra> :get-box seqtrack 0))
           (height (seqtrack-box :height)))
      ;;(c-display "seqbox/seqtrackbox\n" sequencer-box "\n" seqtrack-box "\ny1/y2:" y1 y2)
      (cond ((eq? 'undefined use-two-rows)
             (> height (* 2.5 (get-fontheight))))
            (use-two-rows
             (> height (* 2.5 (get-fontheight))))
            (else
             (> height (* 2.7 (get-fontheight)))))))

  (if (eq? 'undefined use-two-rows)
      (set! use-two-rows (use-two-rows?)))

  (let ((scroll-area (<gui> :scroll-area #t #t)))
    (<gui> :set-layout-spacing scroll-area 0 0 0 0 0)
    
    (define num-seqtracks (<ra> :get-num-seqtracks))
    
    (let loop ((seqtracknum 0))
      (when (< seqtracknum num-seqtracks)
        (add-seqtrack-header scroll-area seqtracknum use-two-rows)
        (loop (1+ seqtracknum))))

    (<gui> :set-size-policy scroll-area #t #t)
    (<gui> :set-min-height scroll-area 10)
  
    (<gui> :add gui scroll-area))

  
  (define lower-left (<gui> :horizontal-layout)) ;;

  (let ()
    (if (not (<ra> :release-mode))
        (<gui> :set-background-color lower-left "green"))

    (<gui> :set-layout-spacing lower-left 2 2 2 2 2)
    (set-fixed-height lower-left (floor ((<ra> :get-box seqnav) :height)))

    (define pressed-color (<gui> :get-background-color lower-left))
    (define background-color (<gui> :mix-colors pressed-color "black" 0.5))
    (define text-color "white")
    
    (define button1 (mybutton "+" background-color pressed-color text-color
                              (lambda ()
                                (define seqtracknum (<ra> :get-curr-seqtrack))
                                (<ra> :insert-seqtrack seqtracknum))))
    (define button2 (mybutton "-" background-color pressed-color text-color
                              (lambda ()
                                (define seqtracknum (<ra> :get-curr-seqtrack))
                                (set! *current-seqblock-info* #f)
                                (<ra> :delete-seqtrack seqtracknum))))
    (define button3 (mybutton "Append" background-color pressed-color text-color
                              (lambda ()
                                (<ra> :append-seqtrack))))

    (if (= 1 (<ra> :get-num-seqtracks))
        (<gui> :set-enabled button2 #f))
    
    (for-each (lambda (button)
                (<gui> :set-size-policy button #t #t)
                ;;(<gui> :set-background-color button "white")
                (<gui> :add lower-left button))
              (list button1 button2 button3))
    
    (<gui> :add gui lower-left))

  ;;(<gui> :set-size-policy gui #f #f)

  (define is-resizing #f)
  (define is-creating #t)

  ;;(<ra> :schedule 100
  ;;      (lambda ()
  ;;        (c-display "is-visible:" (FROM-C-sequencer-gui-is-visible) gui)
  ;;        (if (<gui> :is-open gui)
  ;;            100
  ;;            #f)))

  (<gui> :add-resize-callback gui
         (lambda (width height)
           (define new-use-two-rows (use-two-rows?))
           (when (not (eq? new-use-two-rows use-two-rows))
             (when (and (not is-resizing) ;; Unfortunately, remake triggers a new resize, and we get a recursive call here. TODO: Fix this. Resize callback should never call itself.
                        (not is-creating))
               (set! is-resizing #t)             
               ;;(<gui> :disable-updates gui)
               ;;(c-display "  Calling remake from resize callback")
               (recreate-left-part-gui new-use-two-rows)
               ;;(<gui> :enable-updates gui)
               (set! is-resizing #f)))))
  
  (set! is-creating #f)

  gui)

#!!
(<gui> :height *g-sequencer-left-part-gui*)
!!#

(define *g-full-sequencer-gui* (if *is-initializing*
                                   #f
                                   *g-full-sequencer-gui*))

(delafina (create-full-sequencer-gui :use-two-rows 'undefined)
  (define gui (<gui> :horizontal-layout))

  (define left-part (create-sequencer-left-part-gui use-two-rows))
  
  (define sequencer-gui (<gui> :get-sequencer-gui))
  ;;(<gui> :set-size-policy left-part #t #t)
  
  (<gui> :add gui left-part)

  (<gui> :remove-parent sequencer-gui)
  (<gui> :add gui sequencer-gui)

  (<gui> :show sequencer-gui)

  (set! *g-full-sequencer-gui* gui)
  (set! *g-sequencer-left-part-gui* left-part)

  gui)

(delafina (recreate-left-part-gui :use-two-rows 'undefined)
  (define old-left-part *g-sequencer-left-part-gui*)
  (assert old-left-part)
  (define new-left-part (create-sequencer-left-part-gui use-two-rows))
  (try-finally :try (lambda ()
                      (<gui> :replace *g-full-sequencer-gui* old-left-part new-left-part))
               :failure (lambda ()
                          (c-display "FAILURE!")
                          (<gui> :add *g-full-sequencer-gui* new-left-part)))
  (set! *g-sequencer-left-part-gui* new-left-part)

  (<gui> :close old-left-part))


;;(define full (create-full-sequencer-gui))

;;(<gui> :add *g-full-sequencer-gui* (<gui> :get-sequencer-gui))

(if (not *is-initializing*)
    (recreate-left-part-gui))


#!!
(recreate-left-part-gui #f)
(recreate-left-part-gui)

(begin *g-sequencer-left-part-gui*)

(define seq-gui (<gui> :get-sequencer-gui))

(c-display seq-gui *g-full-sequencer-gui*)
(<gui> :show seq-gui)
(<gui> :remove-parent seq-gui)

(<gui> :show *g-full-sequencer-gui*)

(define new-seq-gui (create-full-sequencer-gui))
(<gui> :show new-seq-gui)

(begin *g-sequencer-left-part-gui*)
(begin *g-full-sequencer-gui*)
!!#



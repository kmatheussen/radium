
(define (get-fontheight)
  (+ 4 (<gui> :get-system-fontheight)))

(define *text-color* "#cccccc")

(define (create-mixer-gui)
  (<gui> :widget 800 400))


(define (create-mixer-strip-name gui instrument-id x1 y1 x2 y2)
  (define name (<gui> :line (<ra> :get-instrument-name instrument-id) (lambda (edited)
                                                                        ;;(c-display "edited to" edited)
                                                                        #t
                                                                        )))
  (<gui> :set-background-color name (<ra> :get-instrument-color instrument-id))
  (<gui> :add gui name x1 y1 x2 y2))


(define (strip-slider instrument-id effect-name)
  (define instrument-name (<ra> :get-instrument-name instrument-id))
  ;;(define widget (<gui> :widget 100 (get-fontheight)))
  (define widget #f)
  
  (define (paintit width height)
    (define color (<ra> :get-instrument-color instrument-id))
    (define value (<ra> :get-instrument-effect instrument-id effect-name))
    ;;(c-display "value: " value)
    (define pos (scale value 0 1 0 width))
    (<gui> :filled-box widget (<gui> :get-background-color widget) 0 0 width height)
    (<gui> :filled-box widget "black" 1 1 (1- width) (1- height) 5 5)
    (<gui> :filled-box widget color 0 0 pos height 5 5)
    (<gui> :draw-box widget "gray" 0 0 width height 0.8)
    (<gui> :draw-text widget *text-color* (<-> instrument-name ": " (floor (scale value 0 1 0 100)))
           4 2 width height))

  (set! widget (<gui> :horizontal-slider "" 0 0.5 1.0
                      (lambda (val)
                        ;;(<ra> :set-instrument-effect instrument-id effect-name val)
                        (when widget
                          (<ra> :set-instrument-effect instrument-id effect-name val)
                          (paintit (<gui> :width widget)
                                   (<gui> :height widget))))))
  
  (<gui> :set-min-height widget (get-fontheight))

  (<gui> :add-resize-callback widget paintit)

  '(<gui> :add-mouse-callback widget (lambda (button state x y)
                                      (when (= button *left-button*)
                                        ;;(c-display "  m" button x y (scale x 0 (<gui> :width widget) 0 1.0))
                                        (<ra> :set-instrument-effect instrument-id effect-name (scale x 0 (<gui> :width widget) 0 1))
                                        (paintit (<gui> :width widget)
                                                 (<gui> :height widget)))
                                      #t))

  (paintit (<gui> :width widget)
           (<gui> :height widget))

  widget)

(define (create-mixer-strip-plugin gui instrument-id send-id)
  (define fontheight (get-fontheight))
  '(define slider (<gui> :horizontal-int-slider
                        (<-> (<ra> :get-instrument-name send-id) ": ")
                        0 0 100
                        (lambda (percentage)
                          ;;(c-display "moved" percentage)
                          )))
  (define slider (strip-slider send-id "System Volume"))
  (<gui> :set-size-policy slider #t #t)
  ;;(<gui> :set-background-color slider (<ra> :get-instrument-color send-id))
  (<gui> :add gui slider))

(define (create-mixer-strip-send gui instrument-id send-id)
  (define horiz (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing horiz 1 1 0 1 0)

  (define text (<gui> :text "↳"))
  (<gui> :set-size-policy text #f #t)
  (<gui> :add horiz text)

  (<gui> :add gui horiz)
  (create-mixer-strip-plugin horiz instrument-id send-id))


;; Returns a list of parallel plugins that needs their own mixer strip.
(define (get-returned-plugin-buses instrument-id)
  (define returned-plugin-buses '())

  (define out-instruments (sort-instruments-by-mixer-position ;; Needs to be sorted.
                           (get-instruments-connecting-from-instrument instrument-id)))

  (define plugin-instrument #f)

  (define ret (keep (lambda (out-instrument)
                      (define inputs (get-instruments-connecting-to-instrument out-instrument))
                      (if (and (not plugin-instrument)
                               (= 1 (length inputs)))
                          (begin
                            (set! plugin-instrument out-instrument)
                            #f)
                          (= 1 (length inputs))))
                    out-instruments))

  (if plugin-instrument
      (append ret 
              (get-returned-plugin-buses plugin-instrument))
      ret))


(define (create-mixer-strip-path gui instrument-id)
  (define fontheight (get-fontheight))
  (define send-height fontheight)

  (define (add-bus-sends instrument-id)
    (if (not (<ra> :instrument-is-bus-descendant instrument-id))
        (for-each (lambda (send-id)
                    (create-mixer-strip-send gui
                                             instrument-id
                                             send-id))              
                  (get-buses-connecting-from-instrument instrument-id))))

  (add-bus-sends instrument-id)
    
  (define out-instruments (sort-instruments-by-mixer-position
                           (get-instruments-connecting-from-instrument instrument-id)))
  (define plugin-instrument #f)
  
  (for-each (lambda (out-instrument)
              (define inputs (get-instruments-connecting-to-instrument out-instrument))
              (if (and (not plugin-instrument)
                       (= 1 (length inputs)))
                  (set! plugin-instrument out-instrument)
                  (begin
                    (create-mixer-strip-send gui
                                             out-instrument
                                             out-instrument))))
            out-instruments)
  
  (when plugin-instrument
    (create-mixer-strip-plugin gui
                               plugin-instrument
                               plugin-instrument)
    (create-mixer-strip-path gui plugin-instrument)))


(define (create-mixer-strip-pan gui system-background-color instrument-id x1 y1 x2 y2)
  (define (pan-enabled?)
    (>= (<ra> :get-instrument-effect instrument-id "System Pan On/Off") 0.5))
  
  (define (get-pan)
    (floor (scale (<ra> :get-instrument-effect instrument-id "System Pan")
                  0 1
                  -90 90)))
  (define doit #t)

  (define paint #f)

  (define last-slider-val (get-pan))
  (define slider (<gui> :horizontal-int-slider
                        "pan: "
                        -90 (get-pan) 90
                        (lambda (degree)
                          (when (and doit (not (= last-slider-val degree)) (pan-enabled?))
                            (set! last-slider-val degree)
                            ;;(<ra> :set-instrument-effect instrument-id "System Pan On/Off" 1.0)
                            (<ra> :set-instrument-effect instrument-id "System Pan" (scale degree -90 90 0 1))
                            (if paint
                                (paint))))))

  (set! paint
        (lambda ()
          (define width (<gui> :width slider))
          (define height (<gui> :height slider))
          (define value (get-pan))
          (define is-on (pan-enabled?))
          (<gui> :filled-box slider system-background-color 0 0 width height)
          (define background (if is-on
                                 (<gui> :mix-colors (<gui> :get-background-color gui) "black" 0.39)
                                 (<gui> :mix-colors (<gui> :get-background-color gui) "white" 0.95)))
          (<gui> :filled-box slider background 0 0 width height 5 5)
          (define col1 (<gui> :mix-colors "white" background 0.4))
          (define col2 (<gui> :mix-colors "#010101" background 0.5))

          (define inner-width/2 (scale 1 0 18 0 (get-fontheight)))
          (define outer-width/2 (* inner-width/2 2))

          (define middle (scale value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))

          (<gui> :filled-box slider col1 (- middle inner-width/2) 2 (+ middle inner-width/2) (- height 3))
          (<gui> :filled-box slider col2 (- middle inner-width/2 outer-width/2) 2 (- middle inner-width/2) (- height 3))
          (<gui> :filled-box slider col2 (+ middle inner-width/2) 2 (+ middle inner-width/2 outer-width/2) (- height 3))
          ;;(<gui> :draw-text slider "white" (<-> value "o") 0 0 width height #t)
          (<gui> :draw-box slider "#404040" 0 0 width height 2)
          ))

  (<gui> :add-resize-callback slider (lambda x (paint)))

  (paint)

  (define effect-monitor (<ra> :add-effect-monitor "System Pan" instrument-id
                               (lambda ()
                                 (set! doit #f)
                                 (<gui> :set-value slider (get-pan))
                                 (paint)
                                 (set! doit #t))))
  (define effect-monitor2 (<ra> :add-effect-monitor "System Pan On/Off" instrument-id paint))

  (<gui> :add-close-callback slider
         (lambda ()
           (<ra> :remove-effect-monitor effect-monitor)
           (<ra> :remove-effect-monitor effect-monitor2)))

  (<gui> :add-mouse-callback slider
         (lambda (button state x y)
           (cond ((and (= button *left-button*)
                       (= state *is-pressing*))
                  (<ra> :undo-instrument-effect instrument-id "System Pan"))
                 ((and (= button *right-button*)
                       (= state *is-releasing*))
                  
                  (popup-menu "Reset" (lambda ()
                                        (<ra> :undo-instrument-effect instrument-id "System Pan")
                                        (<ra> :set-instrument-effect instrument-id "System Pan" 0.5))
                              (list "Enabled"
                                    :check (pan-enabled?)
                                    (lambda (onoff)
                                      (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
                                      (<ra> :set-instrument-effect instrument-id "System Pan On/Off" (if onoff 1.0 0.0)))))))
           #f))

  (<gui> :add gui slider x1 y1 x2 y2))

;;(define (create-mixer-strip-checkbox text sel-color unsel-color width height callback)
;;  (define button (<gui> :widget width height))

(define (custom-checkbox paint-func value-changed is-selected)
  (define checkbox (<gui> :widget))
  (define width (<gui> :width checkbox))
  (define height (<gui> :height checkbox))
  (define (repaint)
    (paint-func checkbox is-selected width height))
  (<gui> :add-mouse-callback checkbox (lambda (button state x y)
                                        ;;(c-display "state" state)
                                        (when (and (= button *left-button*)
                                                   (= state *is-pressing*))
                                          (set! is-selected (not is-selected))
                                          (value-changed is-selected)
                                          (repaint))
                                        #t))
  (<gui> :add-resize-callback checkbox
         (lambda (newwidth newheight)
           (set! width newwidth)
           (set! height newheight)
           (repaint)))

  (repaint)

  (list (lambda (new-value)
          (set! is-selected new-value)
          (repaint))
        checkbox))

  

(define (create-mixer-strip-mutesolo gui instrument-id x1 y1 x2 y2)
  (define background-color (<gui> :get-background-color gui));(<ra> :get-instrument-color instrument-id))

  (define middle (floor (average x1 x2)))
  
  (define (draw-mutesolo checkbox is-selected text color width height)
    (<gui> :filled-box
           checkbox
           background-color
           0 0 width height)
    (<gui> :filled-box
           checkbox
           (if is-selected
               color
               background-color)
           2 2 (- width 2) (- height 2)
           5 5)
    (<gui> :draw-text
           checkbox
           "black"
           text
           0 0 width height
           #f)
    (<gui> :draw-box
           checkbox
           "#404040"
           2 2 (- width 2) (- height 2)
           1.0
           0 0))

  (define (get-muted)
    (< (<ra> :get-instrument-effect instrument-id "System Volume On/Off") 0.5))
  (define (get-soloed)
    (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
           
  (define mute (custom-checkbox (lambda (mute is-muted width height)
                                  (draw-mutesolo mute is-muted "Mute" "green" width height))
                                (lambda (is-muted)
                                  (<ra> :undo-instrument-effect instrument-id "System Volume On/Off")
                                  (<ra> :set-instrument-effect instrument-id "System Volume On/Off" (if is-muted 0.0 1.0))
                                  ;;(c-display "mute: " is-muted)
                                  )
                                (get-muted)))
                               
  (define solo (custom-checkbox (lambda (solo is-soloed width height)
                                  (draw-mutesolo solo is-soloed "Solo" "yellow" width height))
                                (lambda (is-selected)
                                  (<ra> :undo-instrument-effect instrument-id "System Solo On/Off")
                                  (<ra> :set-instrument-effect instrument-id "System Solo On/Off" (if is-selected 1.0 0.0))
                                  ;;(c-display "solo: " is-selected)
                                  )
                                (get-soloed)))

  (define mute-monitor (<ra> :add-effect-monitor "System Volume On/Off" instrument-id
                             (lambda ()
                               ((car mute) (get-muted)))))

  (define solo-monitor (<ra> :add-effect-monitor "System Solo On/Off" instrument-id
                             (lambda ()
                               ((car solo) (get-soloed)))))

  (<gui> :add-close-callback (cadr mute)
         (lambda ()
           (<ra> :remove-effect-monitor mute-monitor)))
  
  (<gui> :add-close-callback (cadr solo)
         (lambda ()
           (<ra> :remove-effect-monitor solo-monitor)))

  (<gui> :add gui (cadr mute) x1 y1 middle y2)
  (<gui> :add gui (cadr solo) middle y1 x2 y2)
  )

(define (create-mixer-strip-volume gui instrument-id x1 y1 x2 y2)
  (define fontheight (get-fontheight))
  (define middle (floor (average x1 x2)))

  (define border-size (scale 0.05 0 1 0 (- x2 x1)))
  (define border-size/2 (round (/ border-size 2)))

  (define voltext_x2 (+ x1 middle))
  (define voltext_y1 (round (+ y1 border-size/2)))
  (define voltext_y2 (+ voltext_y1 fontheight))

  (define peaktext_y1 voltext_y1)
  (define peaktext_x1 middle)
  (define peaktext_x2 x2)
  (define peaktext_y2 voltext_y2)

  (define volslider_x1 x1)
  (define volslider_y1 (round (+ border-size/2 voltext_y2)))
  (define volslider_x2 middle)
  (define volslider_y2 (round (- y2 border-size)))

  (define peak_x1 peaktext_x1)
  (define peak_y1 volslider_y1)
  (define peak_x2 peaktext_x2)
  (define peak_y2 volslider_y2)

  (define peaktexttext "-inf")

  (define (get-volume)
    ;(c-display "           got"
    ;           (<ra> :get-instrument-effect instrument-id "System Volume")
    ;           (scale (<ra> :get-instrument-effect instrument-id "System Volume")
    ;                  0 1
    ;                  *min-db* *max-db*)
    ;           " for " (<ra> :get-instrument-name instrument-id))
    (scale (<ra> :get-instrument-effect instrument-id "System Volume")
           0 1
           *min-db* *max-db*))

  (define doit #t)

  (define paint-voltext #f)
  (define paint-peaktext #f)

  (define last-voltext (get-volume))
  (define voltext (<gui> :widget))
  (define peaktext (<gui> :widget))

  (define paint-slider #f)

  (define (db-to-slider db)
    (define scaled (scale db *min-db* *max-mixer-db* 0 1))
    (* scaled scaled))

  (define (slider-to-db slider)
    (define scaled (sqrt slider))
    (scale scaled 0 1 *min-db* *max-mixer-db*))

  (define last-vol-slider (get-volume))
  (define volslider (<gui> :vertical-slider
                           ""
                           0 (db-to-slider (get-volume)) 1
                           (lambda (val)
                             (define db (slider-to-db val))
                             (when (and doit (not (= last-vol-slider db)))
                               (set! last-vol-slider db)
                               (<ra> :set-instrument-effect instrument-id "System Volume" (scale db *min-db* *max-db* 0 1))
                               (if paint-voltext
                                   (paint-voltext))
                               ;;(<gui> :set-value voltext val)
                               (if paint-slider
                                   (paint-slider))))))
    
  (define background (<gui> :get-background-color gui))

  (define (paint-text gui text)
    (define width (<gui> :width gui))
    (define height (<gui> :height gui))
    
    (define col1 (<gui> :mix-colors "#010101" background 0.7))
    
    ;; background
    (<gui> :filled-box gui background 0 0 width height)
    
    ;; rounded
    (<gui> :filled-box gui col1 border-size 0 (- width border-size) height 5 5)
    
    ;; text
    (<gui> :draw-text gui *text-color* text 0 0 width height))

    
  (set! paint-voltext
        (lambda ()
          (paint-text voltext (one-decimal-string (get-volume)))))

  (<gui> :add-resize-callback voltext (lambda x (paint-voltext)))
  (paint-voltext)

  (set! paint-peaktext
        (lambda ()
          (paint-text peaktext peaktexttext)))

  (<gui> :add-resize-callback peaktext (lambda x (paint-peaktext)))
  (paint-peaktext)

  (set! paint-slider
        (lambda ()
          (define width (<gui> :width volslider))
          (define height (<gui> :height volslider))
          (define x1 border-size) ;;(scale 0.1 0 1 0 width))
          (define x2 (- width border-size)) ;;(scale 0.9 0 1 0 width))
          (define middle_y (scale (db-to-slider (get-volume)) 0 1 height 0))
          
          ;; background
          (<gui> :filled-box volslider background 0 0 width height)
          
          (define col1 (<gui> :mix-colors "#010101" background 0.2)) ;; down
          (define col2 (<gui> :mix-colors "#010101" background 0.9)) ;; up

          ;; slider
          (<gui> :filled-box volslider col2 x1 0 x2 height 5 5) ;; up (fill everything)
          (<gui> :filled-box volslider col1 x1 middle_y x2 height 5 5) ;; down

          ;; slider border
          (<gui> :draw-box volslider "black" x1 0 x2 height 1.0)

          ;; slider 0db, white line
          (define middle_0db (scale (db-to-slider 0) 0 1 height 0))
          (<gui> :draw-line volslider "#eeeeee" (1+ x1) middle_0db (1- x2) middle_0db 0.3)
          ))

  (<gui> :add-resize-callback volslider (lambda x (paint-slider)))


  (define volmeter (<gui> :vertical-audio-meter instrument-id))
  
  (<gui> :add gui voltext x1 voltext_y1 middle voltext_y2)
  (<gui> :add gui peaktext peaktext_x1 peaktext_y1 peaktext_x2 peaktext_y2)

  (define effect-monitor (<ra> :add-effect-monitor "System Volume" instrument-id
                               (lambda ()
                                 (set! doit #f)
                                 (<gui> :set-value volslider (db-to-slider (get-volume)))
                                 ;;(<gui> :set-value voltext (get-volume))
                                 (paint-voltext)
                                 (paint-slider)
                                 (set! doit #t))))

  (<gui> :add-close-callback volslider
         (lambda ()
           (<ra> :remove-effect-monitor effect-monitor)))

  (<gui> :add-mouse-callback volslider
         (lambda (button state x y)
           (cond ((and (= button *left-button*)
                       (= state *is-pressing*))
                  (<ra> :undo-instrument-effect instrument-id "System Volume"))
                 ((and (= button *right-button*)
                       (= state *is-releasing*))
                  (popup-menu "Reset" (lambda ()
                                        (<ra> :undo-instrument-effect instrument-id "System Volume")
                                        (<ra> :set-instrument-effect instrument-id "System Volume" (scale 0 *min-db* *max-db* 0 1))))))
           #f))

  (<gui> :add-mouse-callback voltext (lambda (button state x y)
                                        (when (and (= button *left-button*)
                                                   (= state *is-pressing*))
                                          (let ((maybe (<ra> :request-float "" *min-db* *max-db* #t)))
                                            (when (>= maybe *min-db*)
                                              (<ra> :undo-instrument-effect instrument-id "System Volume")
                                              (<ra> :set-instrument-effect instrument-id "System Volume" (scale maybe *min-db* *max-db* 0 1)))))
                                        #t))
                                                

  (<gui> :add-audio-meter-peak-callback volmeter (lambda (text)
                                                   (set! peaktexttext text)
                                                   (paint-peaktext)))

  (<gui> :add-mouse-callback peaktext (lambda (button state x y)
                                        (when (and (= button *left-button*)
                                                   (= state *is-pressing*))
                                          (set! peaktexttext "-inf")
                                          (<gui> :reset-audio-meter-peak volmeter)
                                          (paint-peaktext))
                                        #t))
  
  (<gui> :add gui volslider volslider_x1 volslider_y1 volslider_x2 volslider_y2)
  (<gui> :add gui volmeter peak_x1 peak_y1 peak_x2 peak_y2)
  )


(define (get-mixer-strip-background-color gui instrument-id)
  (<gui> :mix-colors
         (<ra> :get-instrument-color instrument-id)
         (<gui> :get-background-color gui)
         0.3))


(define (create-mixer-strip-comment gui instrument-id x1 y1 x2 y2)
  (define comment-edit (<gui> :line "Comment"))
  (<gui> :set-background-color comment-edit (<ra> :get-instrument-color instrument-id))
  (<gui> :add gui comment-edit x1 y1 x2 y2))


(define (create-mixer-strip instrument-id width height)
  (define gui (<gui> :widget width height))
  (<gui> :set-min-width gui width)
  (<gui> :set-max-width gui width)
  (<gui> :set-size-policy gui #f #t)
  (define system-background-color (<gui> :get-background-color gui))
  ;;(c-display "               backgroudn: " (<gui> :get-background-color gui))
  (<gui> :set-background-color gui (get-mixer-strip-background-color gui instrument-id)) ;;(<ra> :get-instrument-color instrument-id))
  
  (define y1 0)
  (define x1 0)
  (define x2 width)
  (define y2 height)
  (define top-y y1)
  
  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define name-height (floor (* 1.5 fontheight-and-borders)))
  (define pan-height fontheight-and-borders)
  (define mutesolo-height fontheight-and-borders)
  (define comment-height fontheight-and-borders)

  (define min-send-height (* 2 fontheight-and-borders))
  (define min-volume-height (* 3 fontheight-and-borders))

  (define height-left (- height
                         (+ name-height pan-height mutesolo-height comment-height)))

  (define sends-height (max min-send-height
                            (floor (/ height-left 2))))
  (define volume-height (max min-volume-height
                             (1+ (floor (/ height-left 2)))))


  (define name_y1 y1)
  (define name_y2 (+ y1 name-height))

  (define sends_y1 name_y2)
  (define sends_y2 (+ sends_y1 sends-height))

  (define pan_y1 sends_y2)
  (define pan_y2 (+ pan_y1 pan-height))

  (define mutesolo_y1 pan_y2)
  (define mutesolo_y2 (+ mutesolo_y1 mutesolo-height))

  (define volume_y1 mutesolo_y2)
  (define volume_y2 (+ volume_y1 volume-height))

  (define comment_y1 volume_y2)
  (define comment_y2 (+ comment_y1 comment-height))

  (create-mixer-strip-name gui instrument-id x1 name_y1 x2 name_y2)

  (define mixer-strip-path-gui (<gui> :vertical-scroll))
  (<gui> :set-layout-spacing mixer-strip-path-gui 5 5 5 5 5)
  (<gui> :add gui mixer-strip-path-gui x1 sends_y1 x2 sends_y2)
  
  (create-mixer-strip-path mixer-strip-path-gui instrument-id)

  (create-mixer-strip-pan gui system-background-color instrument-id x1 pan_y1 x2 pan_y2)
  (create-mixer-strip-mutesolo gui instrument-id x1 mutesolo_y1 x2 mutesolo_y2)
  (create-mixer-strip-volume gui instrument-id x1 volume_y1 x2 volume_y2)
  (create-mixer-strip-comment gui instrument-id x1 comment_y1 x2 comment_y2)

  gui)


#!
(begin
  (define pos-x (or (and (defined? 'mixer-strips) (<gui> :get-x mixer-strips))
                    400))
  (define pos-y (or (and (defined? 'mixer-strips) (<gui> :get-y mixer-strips))
                    50))
  (if (defined? 'mixer-strips)
      (<gui> :close mixer-strips))
  (define mixer-strips (<gui> :widget 300 800))
  ;;(<gui> :draw-box mixer-strips "black" (- 20 1) (- 20 1) (1+ 220) (1+ 700) 1.0)
  ;;(<gui> :filled-box mixer-strips "black" (- 20 1) (- 20 1) (1+ 220) (1+ 700))
  (create-mixer-strip mixer-strips (get-instrument-from-name "Sample Player 1") 20 20 220 700)
  (<gui> :show mixer-strips)

  (<gui> :set-pos mixer-strips pos-x pos-y)
  )
!#

#!
(define mixer-strips (<gui> :widget 300 800))
!#

;;!#

(define (create-mixer-strips width height)

  (define strip-separator-width 5)

  ;;(define mixer-strips (<gui> :widget 800 800))
  (define mixer-strips (<gui> :horizontal-scroll)) ;;widget 800 800))
  (<gui> :set-layout-spacing mixer-strips strip-separator-width 0 0 0 0)
  
  ;;(define x1 0)
  (define mixer-strip-width 140)
  (define instruments-buses-separator-width (* (get-fontheight) 2))

  (define instruments (get-all-instruments-with-no-input-connections))

  (define instrument-plugin-buses (apply append (map (lambda (instrument-id)
                                                       (get-returned-plugin-buses instrument-id))
                                                     instruments)))

  (define buses (append (get-all-instruments-with-at-least-two-input-connections)
                        (get-buses)))

  (define buses-plugin-buses (apply append (map (lambda (instrument-id)
                                                  (get-returned-plugin-buses instrument-id))
                                                (append buses
                                                        instrument-plugin-buses))))

  (define all-buses (append instrument-plugin-buses
                            buses
                            buses-plugin-buses))

  (define fit-vertically? (<= (+ 0
                                 (* (+ (length instruments)
                                       (length all-buses))
                                   strip-separator-width)
                                 (* mixer-strip-width (+ (length instruments)
                                                         (length all-buses)))
                                 instruments-buses-separator-width)
                              width))

  (when (not fit-vertically?)
    (define scroll-bar-height (<gui> :height
                                     (<gui> :child mixer-strips "horizontalScrollBar")))
    (set! height (- height
                    (1+ (floor (/ scroll-bar-height 2))))))

  '(c-display "       buses-plugin-buses:"
             (map (lambda (i)
                    (<ra> :get-instrument-name i))
                  buses-plugin-buses))

  ;; no-input instruments
  (for-each (lambda (instrument-id)
              (<gui> :add mixer-strips (create-mixer-strip instrument-id mixer-strip-width height)))
            (sort-instruments-by-mixer-position
             instruments))

  ;;(set! x1 (+ x1 40))

  (<gui> :add-layout-space mixer-strips instruments-buses-separator-width 10)
  
  ;; buses
  (for-each (lambda (instrument-id)
              (<gui> :add mixer-strips (create-mixer-strip instrument-id mixer-strip-width height)))
            (sort-instruments-by-mixer-position
             all-buses))

  mixer-strips
  )

#!
(<gui> :show (create-mixer-strips 1000 800))
!#

(define (create-mixer-strips-gui)
  (define parent (<gui> :horizontal-layout))

  (<gui> :set-size parent 1000 800)
  (<gui> :set-pos parent 600 50)
  (<gui> :show parent)

  (<gui> :set-layout-spacing parent 0 0 0 0 0)

  (define mixer-strips (create-mixer-strips (<gui> :width parent) (<gui> :height parent)))

  (<gui> :add parent mixer-strips)

  (<gui> :add-resize-callback parent
         (lambda (width height)
           (catch #t
                  (lambda ()
                    (<gui> :disable-updates parent)
                    (define new-mixer-strips (create-mixer-strips width height))
                    (<gui> :close mixer-strips)
                    (<gui> :add parent new-mixer-strips)
                    (<gui> :show new-mixer-strips)
                    (set! mixer-strips new-mixer-strips))
                  (lambda args
                    (display (ow!))))
           (<gui> :enable-updates parent)))


  )


(let ((start (time)))
  (create-mixer-strips-gui)
  (c-display "   Time used to open mixer:" (- (time) start)))




#!

(get-instrument-from-name "Sample Player 1")
(get-buses-connecting-from-instrument (get-instrument-from-name "Sample Player 1"))


(get-all-audio-instruments)
(define widget (<gui> :widget 200 200))
(<gui> :show widget)
!#

;; Option to have two rows
;; Options to turn on/off instruments
;;option to select +35 or +6


;; Background color should probably be a mix between system background color and instrument color.
;; Color type should probably be int64_t, not string. Don't see any advantages using string.

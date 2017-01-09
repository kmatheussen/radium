
(define (get-fontheight)
  (+ 4 (<gui> :get-system-fontheight)))


(define (create-mixer-gui)
  (<gui> :widget 800 400))


(define (create-mixer-strip-name gui instrument-id x1 y1 x2 y2)
  (define name (<gui> :line (<ra> :get-instrument-name instrument-id) (lambda (edited)
                                                                        (c-display "edited to" edited))))
  (<gui> :set-background-color name (<ra> :get-instrument-color instrument-id))
  (<gui> :add gui name x1 y1 x2 y2))


(define (strip-slider instrument-id effect-name)
  (define instrument-name (<ra> :get-instrument-name instrument-id))
  ;;(define widget (<gui> :widget 100 (get-fontheight)))
  (define widget #f)
  
  (define (paintit width height)
    (define color (<ra> :get-instrument-color instrument-id))
    (define value (<ra> :get-instrument-effect instrument-id effect-name))
    (c-display "value: " value)
    (define pos (scale value 0 1 0 width))
    (<gui> :filled-box widget color 0 0 pos height)
    (<gui> :filled-box widget "black" pos 0 width height)
    (<gui> :draw-box widget "gray" 0 0 width height 0.8)
    (<gui> :draw-text widget "white" (<-> instrument-name ": " (floor (scale value 0 1 0 100)))
           4 2 width height))

  (set! widget (<gui> :horizontal-slider "" 0 0.5 1.0
                      (lambda (val)
                        (<ra> :set-instrument-effect instrument-id effect-name val)
                        (if widget
                            (paintit (<gui> :width widget)
                                     (<gui> :height widget))))))
  
  (<gui> :set-min-height widget (get-fontheight))

  (<gui> :add-resize-callback widget paintit)

  '(<gui> :add-mouse-callback widget (lambda (button x y)
                                      (when (= button *left-button*)
                                        (c-display "  m" button x y (scale x 0 (<gui> :width widget) 0 1.0))
                                        (<ra> :set-instrument-effect instrument-id effect-name (scale x 0 (<gui> :width widget) 0 1))
                                        (paintit (<gui> :width widget)
                                                 (<gui> :height widget)))))

  (paintit (<gui> :width widget)
           (<gui> :height widget))

  widget)

(define (create-mixer-strip-plugin gui instrument-id send-id)
  (define fontheight (get-fontheight))
  '(define slider (<gui> :horizontal-int-slider
                        (<-> (<ra> :get-instrument-name send-id) ": ")
                        0 0 100
                        (lambda (percentage)
                          (c-display "moved" percentage))))
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
(define (create-mixer-strip-path gui instrument-id)
  (define fontheight (get-fontheight))
  (define send-height fontheight)

  (define returned-plugin-buses '())

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
                    (if (= 1 (length inputs))
                        (push-back! returned-plugin-buses out-instrument))
                    (create-mixer-strip-send gui
                                             out-instrument
                                             out-instrument))))
            out-instruments)
  
  (when plugin-instrument
    (create-mixer-strip-plugin gui
                               plugin-instrument
                               plugin-instrument)
    (create-mixer-strip-path gui plugin-instrument))

  returned-plugin-buses)


(define (create-mixer-strip-pan gui instrument-id x1 y1 x2 y2)
  (define slider (<gui> :horizontal-int-slider
                        "pan: "
                        -90 0 90
                        (lambda (degree)
                          (c-display "pan moved" degree))))
  (<gui> :add gui slider x1 y1 x2 y2))

(define (create-mixer-strip-mutesolo gui instrument-id x1 y1 x2 y2)
  (define middle (floor (average x1 x2)))
  (define mute (<gui> :checkbox 
                      "M"
                      #f
                      (lambda (val)
                        (c-display "mute?" val))))
  (define solo (<gui> :checkbox 
                      "S"
                      #f
                      (lambda (val)
                        (c-display "solo?" val))))
  (<gui> :add gui mute x1 y1 middle y2)
  (<gui> :add gui solo middle y1 x2 y2))

(define (create-mixer-strip-volume gui instrument-id x1 y1 x2 y2)
  (define min-volume -40)
  (define max-volume 35)

  (define fontheight (get-fontheight))
  (define middle (floor (average x1 x2)))

  (define voltext_x2 (+ x1 middle))
  (define voltext_y2 (+ y1 fontheight))

  (define peaktext_x1 middle)
  (define peaktext_x2 x2)
  (define peaktext_y2 voltext_y2)

  (define volslider_x1 x1)
  (define volslider_y1 voltext_y2)
  (define volslider_x2 middle)
  (define volslider_y2 y2)

  (define peak_x1 peaktext_x1)
  (define peak_y1 peaktext_y2)
  (define peak_x2 peaktext_x2)
  (define peak_y2 y2)

  (define voltext (<gui> :float-text 
                         min-volume 0 max-volume
                         (lambda (val)
                           (c-display "new-vol" val))))
  (define peaktext (<gui> :text
                          "-inf"))

  (define volslider (<gui> :vertical-slider
                           ""
                           min-volume 0 max-volume
                           (lambda (val)
                             (c-display "volslider set to" val)
                             (<gui> :set-value voltext val))))

  (define volmeter (<gui> :vertical-audio-meter instrument-id))
  
  (<gui> :add gui voltext x1 y1 middle voltext_y2)
  (<gui> :add gui peaktext peaktext_x1 y1 peaktext_x2 peaktext_y2)

  (<gui> :add gui volslider volslider_x1 volslider_y1 volslider_x2 volslider_y2)
  (<gui> :add gui volmeter peak_x1 peak_y1 peak_x2 peak_y2)
  )
  

(define (create-mixer-strip-comment gui instrument-id x1 y1 x2 y2)
  (define comment-edit (<gui> :line "Comment"))
  (<gui> :add gui comment-edit x1 y1 x2 y2))


(define (create-mixer-strip instrument-id width height)
  (define gui (<gui> :widget width height))
  (<gui> :set-min-width gui width)
  (<gui> :set-max-width gui width)
  (<gui> :set-size-policy gui #f #t)
  
  (define y1 0)
  (define x1 0)
  (define x2 width)
  (define y2 height)
  (define top-y y1)
  
  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))
  (define pan-height fontheight-and-borders)

  (define name_y1 y1)
  (define name_y2 (+ y1 fontheight-and-borders))

  (define comment_y1 (- y2 fontheight-and-borders))
  (define comment_y2 y2)

  (define mutesolo_y1 (floor (- (average name_y2 comment_y1) (/ fontheight-and-borders 2))))
  (define mutesolo_y2 (+ mutesolo_y1 fontheight-and-borders))

  (define pan_y1 (- mutesolo_y1 pan-height))
  (define pan_y2 mutesolo_y1)

  (define sends_y1 name_y2)
  (define sends_y2 pan_y1)

  (define volume_y1 mutesolo_y2)
  (define volume_y2 comment_y1)

  (create-mixer-strip-name gui instrument-id x1 name_y1 x2 name_y2)

  (define mixer-strip-path-gui (<gui> :vertical-scroll))
  (<gui> :set-layout-spacing mixer-strip-path-gui 5 1 0 1 0)
  (<gui> :add gui mixer-strip-path-gui x1 sends_y1 x2 sends_y2)
  (let ((returned-plugin-buses (create-mixer-strip-path mixer-strip-path-gui instrument-id)))

    (create-mixer-strip-pan gui instrument-id x1 pan_y1 x2 pan_y2)
    (create-mixer-strip-mutesolo gui instrument-id x1 mutesolo_y1 x2 mutesolo_y2)
    (create-mixer-strip-volume gui instrument-id x1 volume_y1 x2 volume_y2)
    (create-mixer-strip-comment gui instrument-id x1 comment_y1 x2 comment_y2)
    
    ;;(<gui> :draw-box gui "#010101" x1 top-y (1- x2) y2 0.2)
    
    (list gui returned-plugin-buses)
    ))

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


(define (create-mixer-strips height)
  ;;(define mixer-strips (<gui> :widget 800 800))
  (define mixer-strips (<gui> :horizontal-scroll)) ;;widget 800 800))
  (<gui> :set-layout-spacing mixer-strips 5 0 0 0 0)
  
  ;;(define x1 0)
  (define mixer-strip-width 160)
  (define plugin-buses '())

  ;; no-input instruments
  (for-each (lambda (instrument-id)
              (let* ((gakk (create-mixer-strip instrument-id mixer-strip-width height))
                     (mixer-strip (car gakk))
                     (returned-plugin-buses (cadr gakk)))
                (<gui> :add mixer-strips mixer-strip)
                (set! plugin-buses (append plugin-buses returned-plugin-buses))))
            (sort-instruments-by-mixer-position
             (get-all-instruments-with-no-input-connections)))

  ;;(set! x1 (+ x1 40))

  (<gui> :add-layout-space mixer-strips (* (get-fontheight) 2) 10)
  
  ;; buses
  (let loop ((bus-instruments (sort-instruments-by-mixer-position
                               (append plugin-buses
                                       (get-all-instruments-with-at-least-two-input-connections)
                                       (get-buses)))))
    (when (not (null? bus-instruments))
      (let* ((instrument-id (car bus-instruments))
             (gakk (create-mixer-strip instrument-id mixer-strip-width height))
             (mixer-strip (car gakk))
             (returned-plugin-buses (cadr gakk)))
        (<gui> :add mixer-strips mixer-strip)
        (if (null? returned-plugin-buses)
            (loop (cdr bus-instruments))
            (loop (sort-instruments-by-mixer-position (append (cdr bus-instruments)
                                                              returned-plugin-buses)))))))
  

  mixer-strips
  )

#!
(create-mixer-strips)
!#

(define (create-mixer-strips-gui)
  (define parent (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing parent 0 0 0 0 0)

  (define mixer-strips (create-mixer-strips (<gui> :height parent)))

  (<gui> :add-resize-callback parent
         (lambda (width height)
           (<gui> :disable-updates parent)
           (define new-mixer-strips (create-mixer-strips height))
           (<gui> :close mixer-strips)
           (<gui> :add parent new-mixer-strips)
           (<gui> :show new-mixer-strips)
           (<gui> :enable-updates parent)
           (set! mixer-strips new-mixer-strips)))

  (<gui> :add parent mixer-strips)
  (<gui> :show parent))


(create-mixer-strips-gui)




#!

(get-instrument-from-name "Sample Player 1")
(get-buses-connecting-from-instrument (get-instrument-from-name "Sample Player 1"))


(get-all-audio-instruments)
(define widget (<gui> :widget 200 200))
(<gui> :show widget)
!#

;;option to select +35 or +6




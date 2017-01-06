
(define (get-fontheight)
  35)



(define (create-mixer-gui)
  (<gui> :canvas 800 400))


(define (create-mixer-strip-name gui instrument-id x1 y1 x2 y2)
  (define name (<gui> :line (<ra> :get-instrument-name instrument-id) (lambda (edited)
                                                                        (c-display "edited to" edited))))
  (<gui> :set-background-color name (<ra> :get-instrument-color instrument-id))
  (<gui> :add gui name x1 y1 x2 y2))



(define (create-mixer-strip-plugin gui instrument-id send-id)
  (define fontheight (get-fontheight))
  (define slider (<gui> :horizontal-int-slider
                        (<-> (<ra> :get-instrument-name send-id) ": ")
                        0 0 100
                        (lambda (percentage)
                          (c-display "moved" percentage))))
  (<gui> :set-size-policy slider #t #t)
  (<gui> :set-background-color slider (<ra> :get-instrument-color send-id))
  (<gui> :add gui slider))

(define (create-mixer-strip-send gui instrument-id send-id)
  (define horiz (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing horiz 1 1 0 1 0)

  (define text (<gui> :text "↳"))
  (<gui> :set-size-policy text #f #t)
  (<gui> :add horiz text)

  (<gui> :add gui horiz)
  (create-mixer-strip-plugin horiz instrument-id send-id))



(define (create-mixer-strip-path gui instrument-id)
  (define fontheight (get-fontheight))
  (define send-height fontheight)

  (define (add-bus-sends instrument-id)
    (for-each (lambda (send-id)
                (create-mixer-strip-send gui
                                         instrument-id
                                         send-id))
              (get-buses-connecting-from-instrument instrument-id)))

  (add-bus-sends instrument-id)
    
  (define out-instruments (get-instruments-connecting-from-instrument instrument-id))
  (define plugin-instrument #f)
  
  (for-each (lambda (out-instrument)
              (define inputs (get-instruments-connecting-to-instrument out-instrument))
              (if (and (not plugin-instrument)
                       (= 1 (length inputs)))
                  (set! plugin-instrument out-instrument)
                  (create-mixer-strip-send gui
                                           out-instrument
                                           out-instrument)))
            out-instruments)
  
  (when plugin-instrument
    (create-mixer-strip-plugin gui
                               plugin-instrument
                               plugin-instrument)
    (create-mixer-strip-path gui plugin-instrument)))


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

  (<gui> :add gui voltext x1 y1 middle voltext_y2)
  (<gui> :add gui peaktext peaktext_x1 y1 peaktext_x2 peaktext_y2)

  (<gui> :add gui volslider volslider_x1 volslider_y1 volslider_x2 volslider_y2)
  )
  

(define (create-mixer-strip-comment gui instrument-id x1 y1 x2 y2)
  (define comment-edit (<gui> :line "Comment"))
  (<gui> :add gui comment-edit x1 y1 x2 y2))

(define (create-mixer-strip gui instrument-id x1 y1 x2 y2)
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
  (<gui> :set-layout-spacing mixer-strip-path-gui 1 1 0 1 0)
  (<gui> :add gui mixer-strip-path-gui x1 sends_y1 x2 sends_y2)
  (create-mixer-strip-path mixer-strip-path-gui instrument-id)

  (create-mixer-strip-pan gui instrument-id x1 pan_y1 x2 pan_y2)
  (create-mixer-strip-mutesolo gui instrument-id x1 mutesolo_y1 x2 mutesolo_y2)
  (create-mixer-strip-volume gui instrument-id x1 volume_y1 x2 volume_y2)
  (create-mixer-strip-comment gui instrument-id x1 comment_y1 x2 comment_y2)

  (<gui> :draw-box gui "black" x1 y1 x2 y2 1.0)
  )

;;#!
(begin
  (define mixer-strips (<gui> :canvas 300 800))
  (create-mixer-strip mixer-strips (get-instrument-from-name "Sample Player 1") 20 20 220 700)
  (<gui> :show mixer-strips))
;;!#



(define (create-mixer-strips gui)
  #f
  )


#!

(get-instrument-from-name "Sample Player 1")
(get-buses-connecting-from-instrument (get-instrument-from-name "Sample Player 1"))


(get-all-audio-instruments)
(define canvas (<gui> :canvas 200 200))
(<gui> :show canvas)
!#




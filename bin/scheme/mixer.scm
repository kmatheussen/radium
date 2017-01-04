


(define (create-mixer-gui)
  (<gui> :canvas 800 400))

(define (paint-mixer-strip-name gui instrument-id x1 y1 x2 y2)
  (<gui> :filled-box
         gui
         (<ra> :get-instrument-color instrument-id)
         x1 y1
         x2 y2))

  (<gui> :draw-text
         gui
         (<ra> :get-instrument-name instrument-id)
         x1 y1
         x2 y2))

(define (paint-mixer-strip-sends gui instrument-id x1 y1 x2 y2)
  #t)
(define (paint-mixer-strip-pan gui instrument-id x1 y1 x2 y2)
  #t)
(define (paint-mixer-strip-mutesolo gui instrument-id x1 y1 x2 y2)
  #t)
(define (paint-mixer-strip-volume gui instrument-id x1 y1 x2 y2)
  #t)
(define (paint-mixer-strip-comment gui instrument-id x1 y1 x2 y2)
  #t)

(define (paint-mixer-strip gui instrument-id x1 y1 x2 y2)
  (define fontheight 20)
  (define fontheight-and-borders (+ 4 fontheight))
  (define pan-height fontheight-and-borders)

  (define name_y1 (+ y1 2))
  (define name_y2 (+ y1 fontheight-and-borders))

  (define comment_y1 (- y2 fontheight-and-borders))
  (define comment_y2 (- y2 2))

  (define solomute_y1 (- (average name_y2 comment_y1) (/ fontheight-and-borders 2)))
  (define solomute_y2 (+ solomute_y1 fontheight-and-borders))

  (define pan_y1 (- solomute_y1 pan-height))
  (define pan_y2 solomute_y1)

  (define sends_y1 name_y2)
  (define sends_y2 pan_y1)

  (define volume_y1 solomute_y2)
  (define volume_y2 comment_y1)

  (paint-mixer-strip-name gui instrument-id x1 name_y1 x2 name_y2)
  (paint-mixer-strip-sends gui instrument-id x1 sends_y1 x2 sends_y2)
  (paint-mixer-strip-pan gui instrument-id x1 pan_y1 x2 pan_y2)
  (paint-mixer-strip-mutesolo gui instrument-id x1 mutesolo_y1 x2 mutesolo_y2)
  (paint-mixer-strip-volume gui instrument-id x1 volum_y1 x2 volume_y2)
  (paint-mixer-strip-comment gui instrument-id x1 comment_y1 x2 comment_y2)

  )



(define (paint-mixer-gui gui)
  )




(define canvas (<gui> :canvas 200 200))
(<gui> :show canvas)



(provide 'seqblock-paint.scm)

(my-require 'sequencer.scm)


(define (paint-seqblock-box gui seqtracknum seqblocknum
                            box is-current-box
                            text
                            background-color)
  ;; box content (filled color)
  ;;
  (<gui> :do-alpha gui (if is-current-box 0.7 0.1)
         (lambda ()
           (<gui> :filled-box
                  gui
                  background-color
                  (box :x1) (box :y1) (box :x2) (box :y2)
                  10 10)))

  (define border 2)

  ;; box border
  ;;
  (<gui> :do-alpha gui 0.3
         (lambda ()
           (<gui> :draw-box
                  gui
                  background-color
                  (+ border (box :x1)) (+ border (box :y1)) (- (box :x2) border) (- (box :y2) border)
                  border
                  5 5)))
  
  ;; text
  ;;
  (<ra> :gui_draw-text gui
        "black"
        ;;"white"
        ;;*text-color*
        text
        (+ (box :x1) 5) (box :y1) (- (box :x2) 5) (box :y2)
        #f ;; wrap-lines
        #f ;; align top
        #t ;; align left
        0 ;; rotate
        #f ;; cut text to fit
        #t ;; scale font size
        )
  )


;; Sequencer painting function. Try to minimize gc.

(define-expansion (call-maybe-paint-box)
  `(let ((selected-box (<ra> :get-seqblock-selected-box)))
     ,@(map (lambda (box boxnum touched? get-text background-color)
              (assert (eq? (pair? touched?) (pair? get-text)))
              (define use-seqblockid (pair? touched?))
              (define args (if use-seqblockid
                               '(seqblockid)
                               '(seqblocknum seqtracknum)))
              (set! touched? (if use-seqblockid
                                 (car touched?)
                                 touched?))
              (set! get-text (if use-seqblockid
                                 (car get-text)
                                 get-text))
              `(if (and (or ,(if (memv boxnum '(1 2 7 8)) #t #f)
                            is-sample)
                        (or is-current-seqblock
                            (,touched? ,@args)))
                   (paint-seqblock-box gui seqtracknum seqblocknum
                                       (map-box ,box)
                                       (and is-current-seqblock
                                            (= ,boxnum selected-box))
                                       (,get-text ,@args)
                                       ,background-color
                                       )))
            (list '(<ra> :get-box seqblock-left-fade seqblocknum seqtracknum)
                  '(<ra> :get-box seqblock-right-fade seqblocknum seqtracknum)
                  '(<ra> :get-box seqblock-left-interior seqblocknum seqtracknum)
                  '(<ra> :get-box seqblock-right-interior seqblocknum seqtracknum)
                  '(<ra> :get-box seqblock-left-speed seqblocknum seqtracknum)
                  '(<ra> :get-box seqblock-right-speed seqblocknum seqtracknum)
                  '(<ra> :get-box seqblock-left-stretch seqblocknum seqtracknum)
                  '(<ra> :get-box seqblock-right-stretch seqblocknum seqtracknum))
            '(1 2 3 4 5 6 7 8)
            '(fade-left-touched? fade-right-touched?
              left-interior-touched? right-interior-touched?
              (speed-touched?) (speed-touched?)
              (stretch-touched?) (stretch-touched?))
            '(get-fade-string-left get-fade-string-right
              get-left-interior-string get-right-interior-string
              (get-speed-string) (get-speed-string)
              (get-stretch-string) (get-stretch-string))
            '("green" "green"
              "blue" "blue"
              "red" "red"
              "yellow" "yellow")
            )))


#!!
(pretty-print (macroexpand (call-maybe-paint-box)))
!!#
(define (FROM_C-paint-seqblock-stuff seqtracknum seqblocknum seqblockid)
  (define gui (<gui> :get-sequencer-gui))
  (define map-x
    (let ((dx (<gui> :get-editor-distance-x gui)))
      (lambda (x)
        (- x dx))))
  (define map-y
    (let ((dy (<gui> :get-editor-distance-y gui)))
      (lambda (y)
        (- y dy))))
  (define (map-box box)
    (make-box2 (map-x (box :x1)) (map-y (box :y1))
               (map-x (box :x2)) (map-y (box :y2))))

  ;;(c-display seqblockid (<ra> :get-curr-seqblock-id-under-mouse))
  (define is-current-seqblock (= seqblockid (<ra> :get-curr-seqblock-id-under-mouse)))
  
  (define is-sample (<ra> :seqblock-holds-sample seqblocknum seqtracknum #t))

  (call-maybe-paint-box)
)


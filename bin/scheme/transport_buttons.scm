(provide 'transport_buttons.scm)

(my-require 'gui.scm)
(my-require 'area.scm)

(define *transport-background-color* "#364636")
(define *transport-play-color* "#2ecc71")
(define *transport-inactive-color* "#cfcfcf")
(define *transport-stop-idle-color* "#1f6f3f")
(define *transport-icon-border-color* "#40000000")
(define *transport-button-color* "#803a303a")
(define *transport-border-color* "#20ffffff");000000")
(define *transport-inner-border-color* "#22aaaaaa")
(define *transport-border-padding* 3)
(define *transport-button-gap* 1)
(define *transport-icon-border-width* 2)


(define (transport-draw-outlined-box gui color x1 y1 x2 y2 round-x round-y)
  (define b *transport-icon-border-width*)
  (<gui> :filled-box gui *transport-icon-border-color*
         (- x1 b) (- y1 b) (+ x2 b) (+ y2 b)
         (+ round-x b) (+ round-y b) *no-gradient* 0.0)
  (<gui> :filled-box gui color x1 y1 x2 y2 round-x round-y)
  )


(define (transport-draw-play-or-pause-icon gui color x1 y1 x2 y2 state)
  (define mid-y (/ (+ y1 y2) 2))
  (if (eq? state 'playing)
      (let* ((w (- x2 x1))
             (bar-width (max 2 (round (* w 0.28))))
             (gap (max 1 (round (* w 0.16))))
             (total (+ (* bar-width 2) gap))
             (bx1 (- (+ x1 (/ w 2)) (/ total 2))))
        (transport-draw-outlined-box gui color
                                     bx1 y1 (+ bx1 bar-width) y2 1 1)
        (transport-draw-outlined-box gui color
                                     (+ bx1 bar-width gap) y1 (+ bx1 bar-width gap bar-width) y2 1 1))
      (let ((points (vector x1 y1
                            x1 y2
                            x2 mid-y)))
        (<gui> :draw-polygon gui *transport-icon-border-color* points 4.0)
        (<gui> :filled-polygon gui color points))))


(define (transport-draw-start-bar gui color x1 y1 x2 y2)
  (define w (- x2 x1))
  (define bar-width (max 1 (round (* w 0.18))))
  (define gap (max 1 (round (* w 0.08))))
  (transport-draw-outlined-box gui color
                               x1 y1 (+ x1 bar-width) y2 1 1)
  (+ x1 bar-width gap))


(define (transport-draw-stop-icon gui color x1 y1 x2 y2)
  (define size (min (* (- x2 x1) 0.7)
                    (* (- y2 y1) 0.7)))
  (define mid-x (/ (+ x1 x2) 2))
  (define mid-y (/ (+ y1 y2) 2))
  (transport-draw-outlined-box gui color
                               (- mid-x (/ size 2)) (- mid-y (/ size 2))
                               (+ mid-x (/ size 2)) (+ mid-y (/ size 2))
                               2 2))


(define (transport-graphic-color kind)
  (cond ((eq? kind 'stop)
         (if (<ra> :is-playing)
             *transport-inactive-color*
             *transport-stop-idle-color*))
        ((memq kind '(block-start block-current))
         (if (<ra> :is-playing-block)
             *transport-play-color*
             *transport-inactive-color*))
        ((memq kind '(song-start song-current))
         (if (<ra> :is-playing-song)
             *transport-play-color*
             *transport-inactive-color*))
        (else
         *transport-inactive-color*)))


(define (transport-draw-icon gui x1 y1 x2 y2 kind state)
  (define box-width 1.2)
  (<gui> :draw-box gui *transport-icon-border-color*
         (+ x1 box-width) (+ y1 box-width) (- x2 box-width) (- y2 box-width)
         0.9 2 2)
  (define width (- x2 x1))
  (define height (- y2 y1))
  (define pad (max 2 (round (* height 0.16))))
  (define icon-width (max 4 (min (* height 0.9)
                                 (* width 0.6))))
  (define icon-height (* 0.75 (- height (* 2 pad))))
  (define stop-height (* 0.75 height))
  (define icon-x1 (+ x1 (/ (- width icon-width) 2)))
  (define icon-y1 (+ y1 (/ (- height icon-height) 2)))
  (define icon-x2 (+ icon-x1 icon-width))
  (define icon-y2 (+ icon-y1 icon-height))
  (define stop-y1 (+ y1 (/ (- height stop-height) 2)))
  (define stop-y2 (+ stop-y1 stop-height))
  (define graphic-color (transport-graphic-color kind))
  (define (draw-play from-start)
    (define play-x1 (if from-start
                        (transport-draw-start-bar gui graphic-color icon-x1 icon-y1 icon-x2 icon-y2)
                        icon-x1))
    (transport-draw-play-or-pause-icon gui graphic-color play-x1 icon-y1 icon-x2 icon-y2 state))
  (cond ((eq? kind 'block-start)
         (draw-play #t))
        ((eq? kind 'block-current)
         (draw-play #f))
        ((eq? kind 'song-start)
         (draw-play #t))
        ((eq? kind 'song-current)
         (draw-play #f))
        ((eq? kind 'stop)
         (transport-draw-stop-icon gui graphic-color x1 stop-y1 x2 stop-y2))
        (else
         (assert #f))))


(def-area-subclass (<transport-buttons-area> :gui :x1 :y1 :x2 :y2
                                             :paint-border #f
                                             :background-color *transport-background-color*)

  (define grid-columns 2)
  (define grid-rows 3)
  (define gap *transport-button-gap*)
  (define border-padding (if paint-border *transport-border-padding* 0))
  (define label-width (max 16 (+ 6 (<gui> :text-width "Block:" gui))))
  (define button-shrink 2)
  (define play-button-y2-extra 2)
  (define label-x1 (+ x1 border-padding))
  (define label-x2 (+ label-x1 label-width))
  (define button-area-x1 (+ label-x2 gap))
  (define button-area-x2 (- x2 border-padding))
  (define stop-x1 (+ x1 border-padding button-shrink))
  (define stop-x2 (- x2 border-padding button-shrink))
  (define button-width (/ (- button-area-x2
                             button-area-x1
                             gap)
                          grid-columns))
  (define button-height (/ (- height
                              (* 2 border-padding)
                              (* gap (- grid-rows 1)))
                           grid-rows))

  (define (cell-x1 col)
    (+ button-area-x1 (* col (+ button-width gap))))

  (define (cell-x2 col)
    (+ (cell-x1 col) button-width))

  (define (get-x1 col)
    (+ (cell-x1 col) button-shrink))

  (define (get-x2 col)
    (- (cell-x2 col) button-shrink))

  (define (cell-y1 row)
    (+ y1 border-padding (* row (+ button-height gap))))

  (define (cell-y2 row)
    (+ (cell-y1 row) button-height))

  (define (get-y1 row)
    (+ (cell-y1 row) button-shrink))

  (define (get-y2 row)
    (- (cell-y2 row) button-shrink))

  (define (button-state kind)
    (cond ((eq? kind 'block-current)
           (cond ((<ra> :is-playing-block) 'playing)
                 ((<ra> :is-paused-block) 'paused)
                 (else 'stopped)))
          ((eq? kind 'song-current)
           (cond ((<ra> :is-playing-song) 'playing)
                 ((<ra> :is-paused-song) 'paused)
                 (else 'stopped)))
          (else
           'stopped)))

  (define (block-current-statusbar-text)
    (list #t
          (cond ((<ra> :is-playing-block) "Pause block")
                ((<ra> :is-paused-block) "Continue block")
                (else "Play block from current position"))))

  (define (song-current-statusbar-text)
    (list #t
          (cond ((<ra> :is-playing-song) "Pause song")
                ((<ra> :is-paused-song) "Continue song")
                (else "Play Song from current position"))))

  (define (click kind)
    (cond ((eq? kind 'stop)
           (<ra> :play-stop))
          ((eq? kind 'block-start)
           (<ra> :play-block-from-start))
          ((eq? kind 'song-start)
           (<ra> :play-song-from-start))
          ((eq? kind 'block-current)
           (cond ((<ra> :is-playing-block)
                  (<ra> :pause-playing))
                 ((<ra> :is-paused-block)
                  (<ra> :continue-playing))
                 (else
                  (<ra> :play-block-from-current))))
          ((eq? kind 'song-current)
           (cond ((<ra> :is-playing-song)
                  (<ra> :pause-playing))
                 ((<ra> :is-paused-song)
                  (<ra> :continue-playing))
                 (else
                  (<ra> :play-song-from-current-sequencer-position))))
          (else
           (assert #f)))
    (<ra> :schedule 0
          (lambda ()
            (update-me!)
            #f)))

  (define (create-button bx1 by1 bx2 by2 kind statusbar-text)
    (define transport-button
      (<new> :button gui
             bx1 by1
             bx2 by2
             :icon-paint-func (lambda (gui bx1 by1 bx2 by2 is-hovering is-pressing)
                                (transport-draw-icon gui bx1 by1 bx2 by2 kind (button-state kind)))
             :text ""
             :background-color *transport-button-color*
             :statusbar-text statusbar-text
             :callback-release (lambda ()
                                 (click kind))))
    (add-sub-area-plain! transport-button)
    transport-button)

  (create-button (get-x1 0) (get-y1 0) (get-x2 0) (+ (get-y2 0) play-button-y2-extra)
                 'block-start
                 (list #t "Play block from start"))
  (create-button (get-x1 1) (get-y1 0) (get-x2 1) (+ (get-y2 0) play-button-y2-extra)
                 'block-current
                 block-current-statusbar-text)
  (create-button (get-x1 0) (get-y1 1) (get-x2 0) (+ (get-y2 1) play-button-y2-extra)
                 'song-start
                 (list #t "Play Song from start"))
  (create-button (get-x1 1) (get-y1 1) (get-x2 1) (+ (get-y2 1) play-button-y2-extra)
                 'song-current
                 song-current-statusbar-text)
  (create-button stop-x1 (get-y1 2) stop-x2 (get-y2 2)
                 'stop
                 (list #t "Stop"))

  (define last-state #f)

  (define (get-state-key)
    (list (<ra> :is-playing-block)
          (<ra> :is-playing-song)
          (<ra> :is-paused-block)
          (<ra> :is-paused-song)))

  (define (update-loop)
    (when is-alive
      (<ra> :schedule 100
            (lambda ()
              (when is-alive
                (let ((state (get-state-key)))
                  (when (not (equal? state last-state))
                    (set! last-state state)
                    (update-me!)))
                (update-loop))
              #f))))

  (set! last-state (get-state-key))
  (update-loop)

  (define-override (paint)
    (when paint-border
      (define rounding 3)
      (define border-width 1)
      '(<gui> :filled-box gui *transport-border-color*
              x1 y1 x2 y2 rounding rounding *no-gradient* 0.0)
      (<gui> :filled-box gui *transport-inner-border-color*
              (+ x1 0) (+ y1 0) (- x2 0) (- y2 0)
              (max 1 (- rounding 1)) (max 1 (- rounding 1))
             *no-gradient* 0.0)
      (<gui> :filled-box gui background-color
              (+ x1 border-width) (+ y1 border-width) (- x2 border-width) (- y2 border-width)
              (max 1 (- rounding 2)) (max 1 (- rounding 2))
              *gradient-diagonal-light-upper-left* 0.12)
      )
    (<gui> :draw-text gui "buttons_text" "Block:"
           label-x1 (get-y1 0) label-x2 (get-y2 0)
           #f #f #f 0 #t #t)
    (<gui> :draw-text gui "buttons_text" "Song:"
           label-x1 (get-y1 1) label-x2 (get-y2 1)
           #f #f #f 0 #t #t)))

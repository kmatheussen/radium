(provide 'mouse.scm)

(define *left-button* 1)
(define *middle-button* 3)
(define *right-button* 5)

(define (left-or-right-button Button)
  (or (= *left-button* Button)
      (= *right-button* Button)))


(define (set-statusbar-value val)
  (ra:set-statusbar-text (<-> val)))

;; Quantitize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match quantitize
  Place Q :> (* (roundup (/ Place Q))
                Q))

#||
(begin
  (test (quantitize 0 0.5)
        0.5)
  (test (quantitize 0.5 0.5)
        0.5)
  (test (quantitize 1.0 0.5)
        1.0)
  (test (quantitize 10.0 0.5)
        10)
  (test (quantitize 10.3 0.5)
        10.5)
  (test (quantitize 10.5 0.5)
        10.5)
  (test (quantitize 10.6 0.5)
        10.5)
  (test (quantitize 10.9 0.5)
        11))
||#


;;; Distance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

;; shortest distance between a point and a non-infinite line. Can be optimized (a lot).
(define (get-distance x y x1 y1 x2 y2)
  (define dist-1-to-2 (sqrt (+ (square (- x2 x1))
                               (square (- y2 y1)))))
  (define dist-0-to-1 (sqrt (+ (square (- x1 x))
                               (square (- y1 y)))))
  (define dist-0-to-2 (sqrt (+ (square (- x2 x))
                               (square (- y2 y)))))                       
  (cond ((= 0 dist-1-to-2)
         dist-0-to-1)
        ((= 0 dist-0-to-1)
         0)
        ((= 0 dist-0-to-2)
         0)
        (else
         (define angle-1 (acos (/ (- (+ (square dist-1-to-2)
                                        (square dist-0-to-1))
                                     (square dist-0-to-2))
                                  (* 2
                                     dist-1-to-2
                                     dist-0-to-1))))
         
         (define angle-2 (acos (/ (- (+ (square dist-1-to-2)
                                        (square dist-0-to-2))
                                     (square dist-0-to-1))
                                  (* 2
                                     dist-1-to-2
                                     dist-0-to-2))))
         ;;(c-display "angle-1" (/ (* 180 angle-1) pi))
         ;;(c-display "angle-2" (/ (* 180 angle-2) pi))
         
         (if (or (>= angle-1 (/ pi 2))
                 (>= angle-2 (/ pi 2)))
             (min dist-0-to-1
                  dist-0-to-2)
             (/ (abs (- (* (- x2 x1) ;; http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
                           (- y1 y))
                        (* (- x1 x)
                           (- y2 y1))))
                dist-1-to-2)))))

#||
(test (get-distance 0 0
                    10 0
                    20 0)
      10)

(test (get-distance 30 0
                    10 0
                    20 0)
      10)

(test (get-distance 0 30
                    0 10
                    0 20)
      10)

(test (get-distance 4 10
                    5 0              
                    5 10)
      1)

(test (get-distance 0 0
                    0 0
                    5 5)
      0.0)
(test (get-distance 0 0
                    0 0
                    0 0)
      0.0)
(test (get-distance 0 0
                    5 0
                    5 0)
      5.0)
||#


(define (get-quantitized-place-from-y Button Y)
  (define place (ra:get-place-from-y Y))
  (if (= Button *right-button*)
      (quantitize place (ra:get-quantitize))
      place))

(define (get-place-from-y Button Y)
  (if (ra:ctrl-pressed)
      (ra:get-place-from-y Y)
      (ra:get-place-in-grid-from-y Y)))


;; Mouse move handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *mouse-move-handlers* '())

(delafina (add-mouse-move-handler :move)
  (push-back! *mouse-move-handlers* move))


(define (run-mouse-move-handlers button x y)
  (for-each (lambda (move-handler)
              (move-handler button x y))
            *mouse-move-handlers*))


;; Mouse cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct mouse-cycle
  :press-func 
  :drag-func (lambda _ #f)
  :release-func (lambda _ #f)
  )

(define *mouse-cycles* '())
(define *current-mouse-cycle* #f)

(define (add-mouse-cycle $mouse-cycle)
  (push-back! *mouse-cycles*
              $mouse-cycle))

(define (get-mouse-cycle $button $x $y)
  (find-if (lambda (cycle)
             ((cycle :press-func) $button $x $y))
           *mouse-cycles*))

(define (only-y-direction)
  (ra:shift-pressed))

(define (only-x-direction)
  (ra:left-extra-pressed))

(delafina (add-delta-mouse-handler :press :move-and-release :release #f :mouse-pointer-is-hidden #f)
  (define prev-x #f)
  (define prev-y #f)
  (define value #f)

  (define (call-move-and-release $button $x $y)
    (if (and (morally-equal? $x prev-x)
             (morally-equal? $y prev-y)
             (not value))
        value
        (begin

          (define dx (cond ((only-y-direction)
                            0)
                           ((ra:ctrl-pressed)
                            (/ (- $x prev-x)
                               10.0))
                           (else
                            (- $x prev-x))))
          (define dy (cond ((only-x-direction)
                            0)
                           ((ra:ctrl-pressed)
                            (/ (- $y prev-y)
                               10.0))
                           (else
                            (- $y prev-y))))

          (set! prev-x $x)
          (set! prev-y $y)
          
          ;; dirty trick to avoid the screen edges
          (when mouse-pointer-is-hidden
            (when (or (< (ra:get-mouse-pointer-x) 16)
                      (< (ra:get-mouse-pointer-y) 16)
                      (> (ra:get-mouse-pointer-x) 500)
                      (> (ra:get-mouse-pointer-y) 500))
              (ra:move-mouse-pointer 100 100)
              ;;(c-display "x/y" (ra:get-mouse-pointer-x) (ra:get-mouse-pointer-y))
              (set! prev-x 100)
              (set! prev-y 100)))
          
          (set! value (move-and-release $button
                                        dx
                                        dy
                                        value)))))
  
  (add-mouse-cycle (make-mouse-cycle
                    :press-func (lambda ($button $x $y)
                                  (set! value (press $button $x $y))
                                  (if value
                                      (begin
                                        (set! prev-x $x)
                                        (set! prev-y $y)
                                        #t)
                                      #f))
                    :drag-func  call-move-and-release
                    :release-func (lambda ($button $x $y)
                                    (call-move-and-release $button $x $y)
                                    (if release
                                        (release $button $x $y value))
                                    (set! prev-x #f)
                                    (set! prev-y #f)))))
  


;; Functions called from radium
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mouse-fx-has-been-set #f)
(define (set-mouse-fx fxnum tracknum)
  (set! mouse-fx-has-been-set #t)
  (ra:set-mouse-fx fxnum tracknum))

(define mouse-track-has-been-set #f)
(define (set-mouse-track tracknum)
  (set! mouse-track-has-been-set #t)
  (ra:set-mouse-track tracknum))
(define (set-mouse-track-to-reltempo)
  (set! mouse-track-has-been-set #t)
  (ra:set-mouse-track-to-reltempo))

(define mouse-note-has-been-set #f)
(define (set-mouse-note notenum tracknum)
  (set! mouse-note-has-been-set #t)
  (ra:set-mouse-note notenum tracknum))

(define indicator-node-has-been-set #f)
(define (set-indicator-temponode num)
  (set! indicator-node-has-been-set #t)
  (ra:set-indicator-temponode num))
(define (set-indicator-pitch num tracknum)
  (set! indicator-node-has-been-set #t)
  (ra:set-indicator-pitch num tracknum))
(define (set-indicator-velocity-node velocitynum notenum tracknum)
  (set! indicator-node-has-been-set #t)
  (ra:set-indicator-velocity-node velocitynum notenum tracknum))
(define (set-indicator-fxnode fxnodenum notenum tracknum)
  (set! indicator-node-has-been-set #t)
  (ra:set-indicator-fxnode fxnodenum notenum tracknum))

(define current-node-has-been-set #f)
(define (set-current-temponode num)
  (set! current-node-has-been-set #t)
  (ra:set-current-temponode num))
(define (set-current-velocity-node velnum notenum tracknum)
  (set! current-node-has-been-set #t)
  (ra:set-statusbar-text (<-> "Velocity: " (two-decimal-string (ra:get-velocity-value velnum notenum tracknum))))
  (ra:set-current-velocity-node velnum notenum tracknum))
(define (set-current-fxnode fxnodenum fxnum tracknum)
  (set! current-node-has-been-set #t)
  (ra:set-statusbar-text (ra:get-fx-string fxnodenum fxnum tracknum))
  (ra:set-current-fxnode fxnodenum fxnum tracknum))
(define (set-current-pitch pitchnum tracknum)
  (set! current-node-has-been-set #t)
  (ra:set-current-pitch pitchnum tracknum)
  (ra:set-statusbar-text (<-> "Pitch: " (two-decimal-string (ra:get-pitch-value pitchnum tracknum)))))

(define mouse-pointer-has-been-set #f)
(define (set-mouse-pointer func)
  (set! mouse-pointer-has-been-set #t)
  (func))

;; TODO: block->is_dirty is set unnecessarily often to true this way.
(define (cancel-current-stuff)
  (ra:set-no-mouse-fx)
  (ra:set-no-mouse-note)
  (ra:set-no-mouse-track)
  (ra:cancel-current-node)
  (ra:cancel-indicator-node)
  )


(define (handling-nodes thunk)
  (set! mouse-fx-has-been-set #f)
  (set! mouse-track-has-been-set #f)
  (set! mouse-note-has-been-set #f)
  (set! indicator-node-has-been-set #f)
  (set! current-node-has-been-set #f)
  (set! mouse-pointer-has-been-set #f)

  (ra:set-statusbar-text "")

  (define ret (thunk))

  (if (not mouse-fx-has-been-set)
      (ra:set-no-mouse-fx))

  (if (not mouse-track-has-been-set)
      (ra:set-no-mouse-track))

  (if (not mouse-note-has-been-set)
      (ra:set-no-mouse-note))

  (if (not indicator-node-has-been-set)
      (ra:cancel-indicator-node))

  (if (not current-node-has-been-set)
      (ra:cancel-current-node))

  ;;(if (not mouse-pointer-has-been-set)
  ;;    (ra:set-normal-mouse-pointer))

  ret)


(define (radium-mouse-press $button $x $y)
  (handling-nodes
   (lambda()
     (c-display "mouse press" $button $x $y)
     ;;(cancel-current-stuff)
     (if (not *current-mouse-cycle*)
         (set! *current-mouse-cycle* (get-mouse-cycle $button $x $y)))
     *current-mouse-cycle*)))

(define (radium-mouse-move $button $x $y)
  (handling-nodes
   (lambda()
     ;;(c-display "mouse move" $button $x $y (ra:ctrl-pressed) (ra:shift-pressed))
     ;;(cancel-current-stuff)
     (if *current-mouse-cycle*
         (begin 
           ((*current-mouse-cycle* :drag-func) $button $x $y)
           #t)
         (begin
           (run-mouse-move-handlers $button $x $y)
           #f)))))

(define (radium-mouse-release $button $x $y)
  (handling-nodes
   (lambda()
     ;;(c-display "mouse release" $button $x $y)
     (if *current-mouse-cycle*
         (begin
           ((*current-mouse-cycle* :release-func) $button $x $y)
           (set! *current-mouse-cycle* #f)
           (run-mouse-move-handlers $button $x $y)
           (cancel-current-stuff)
           (ra:set-normal-mouse-pointer)
           #t)
         #f))))


#||
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda ($button $x $y)
                                (c-display "pressed it" $x $y)
                                #t)
                  :drag-func  (lambda ($button $x $y)
                                (c-display "moved it" $x $y))
                  :release-func (lambda ($button $x $y)
                                  (c-display "released it" $x $y))))
||#


(define-match get-track-num-0
  X _ ___ X1 __ __________ :> #f  :where (< X X1)
  X _ Num X1 X2 __________ :> Num :where (and (>= X X1)
                                              (< X X2))
  _ _ Num __ __ Num-tracks :> #f  :where (= (1+ Num) Num-tracks)
  X Y Num X1 X2 Num-tracks :> (get-track-num-0 X
                                               Y
                                               (1+ Num)
                                               X2
                                               (if (= Num (- Num-tracks 2))
                                                   (ra:get-track-x2 (1+ Num))
                                                   (ra:get-track-x1 (+ 2 Num)))
                                               Num-tracks))

(define-match get-track-num
  X Y :> (get-track-num-0 X Y 0
                          (ra:get-track-x1 0)
                          (ra:get-track-x1 1)
                          (ra:get-num-tracks)))
                                                   
  
#||
(get-track-num 650 50)
||#

(define *current-track-num* #f)

;; Set current track and mouse track
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (set! *current-track-num* (get-track-num X Y))
         (cond (*current-track-num*
                (set-mouse-track *current-track-num*))
               ((inside-box (ra:get-box temponode-area) X Y)
                (set-mouse-track-to-reltempo)))))

(define *current-subtrack-num* #f)

(define-match get-subtrack-from-x-0
  __ _ Num Num   ________ :> #f  
  X1 X Num Total Tracknum :> (let ((X2 (if (= Num (1- Total))
                                           (ra:get-subtrack-x2 Num Tracknum)                                           
                                           (ra:get-subtrack-x1 (1+ Num) Tracknum))))
                               (if (and (>= X X1)
                                        (<  X X2))
                                   Num
                                   (get-subtrack-from-x-0 X2
                                                          X
                                                          (1+ Num)
                                                          Total
                                                          Tracknum))))

(define-match get-subtrack-from-x
  X Tracknum :> (get-subtrack-from-x-0 (ra:get-subtrack-x1 0 Tracknum)
                                       X
                                       0
                                       (ra:get-num-subtracks Tracknum) Tracknum))

(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (set! *current-subtrack-num* (and *current-track-num*
                                           (inside-box (ra:get-box track-fx *current-track-num*) $x $y)
                                           (get-subtrack-from-x $x *current-track-num*)))))


;;;;;;;;;;;;;;;;;;;;;;

#||
(ra:set-reltempo 0.2)
||#

(define (get-common-node-box $x $y)
  (define width/2 (ra:get-half-of-node-width))
  (define x1 (- $x width/2))
  (define y1 (- $y width/2))
  (define x2 (+ $x width/2))
  (define y2 (+ $y width/2))
  (make-box2 x1 y1 x2 y2))
  

(delafina (find-node :$x
                     :$y
                     :$get-node-box
                     :$num-nodes
                     :$num 0
                     )

  (cond ((= 0 $num-nodes)
         (list 'new-box 0))
        (else
         (define box ($get-node-box $num))
         (cond ((inside-box box $x $y)
                (list 'existing-box $num box))
               ((and (= 0 $num)
                     (< $y (box :y1)))
                'before)
               ((> (box :y1) $y)
                (list 'new-box $num))
               ((= $num (1- $num-nodes))
                'after)
               (else
                (find-node $x $y $get-node-box $num-nodes (1+ $num)))))))


(define-struct move-node-handler
  :move
  :release
  :node
  )

(define-struct node-mouse-cycle
  :press
  :move-and-release
  :release
  )

(define *move-existing-node-mouse-cycles* '())
(define *create-new-node-mouse-cycles* '())

(define-match get-cycle-and-node
  ______ _ _ ()             :> #f
  Button X Y (Cycle . Rest) :> (let ((Node ((Cycle :press) Button X Y)))
                                 (if Node
                                     (make-move-node-handler :move (Cycle :move-and-release)
                                                             :release (Cycle :release)
                                                             :node Node)
                                     (get-cycle-and-node Button X Y Rest))))


                         
;; This cycle handler makes sure all move-existing-node cycles are given a chance to run before the create-new-node cycles.
(add-delta-mouse-handler
 :press (lambda (Button X Y)
          (get-cycle-and-node Button
                              X
                              Y
                              (append *move-existing-node-mouse-cycles*
                                      *create-new-node-mouse-cycles*)))
 
 :move-and-release (lambda (Button Dx Dy Cycle-and-node)
                     (define Move (Cycle-and-node :move))
                     (define Release (Cycle-and-node :release))
                     (define Old-node (Cycle-and-node :node))
                     (define New-node (Move Button Dx Dy Old-node))
                     (make-move-node-handler :move Move
                                             :release Release
                                             :node New-node))
 
 :release (lambda (Button X Y Cycle-and-node)
            (define Release (Cycle-and-node :release))
            (define node (Cycle-and-node :node))
            (Release Button X Y node))

 :mouse-pointer-is-hidden #t)


   
(delafina (add-node-mouse-handler :Get-area-box
                                  :Get-existing-node-info
                                  :Get-min-value
                                  :Get-max-value
                                  :Get-x
                                  :Get-y
                                  :Make-undo
                                  :Create-new-node
                                  :Move-node
                                  :Publicize
                                  :Get-pixels-per-value-unit #f
                                  :Create-button #f
                                  )
  
  (define-struct node
    :node-info
    :value
    :y)

  (define (press-existing-node Button X Y)
    (and (left-or-right-button Button)
         (Get-existing-node-info X
                                 Y
                                 (lambda (Node-info Value Node-y)
                                   (Make-undo Node-info)
                                   (Publicize Node-info)
                                   (set-mouse-pointer ra:set-blank-mouse-pointer)
                                   (make-node :node-info Node-info
                                              :value Value
                                              :y Node-y
                                              )))))

  (define (can-create Button X Y)
    (and (or (and Create-button (= Button Create-button))
             (and (not Create-button) (left-or-right-button Button)))
         (let ((area-box (Get-area-box)))
           (and area-box
                (inside-box area-box X Y)))))
    
  (define (press-and-create-new-node Button X Y)
    (and (can-create Button X Y)
         (Create-new-node X
                          (get-place-from-y Button Y)
                          (lambda (Node-info Value)
                            (Publicize Node-info)
                            (set-mouse-pointer ra:set-blank-mouse-pointer)
                            (make-node :node-info Node-info
                                       :value Value
                                       :y Y)))))
  
  (define (move-and-release Button Dx Dy Node)
    (define node-info (Node :node-info))
    (define min (Get-min-value node-info))
    (define max (Get-max-value node-info))
    (define area-box (Get-area-box))
    (define node-area-width (area-box :width))
    (define pixels-per-value-unit (if Get-pixels-per-value-unit
                                      (Get-pixels-per-value-unit node-info)
                                      (/ node-area-width
                                         (- max min))))
    (define new-value (let ((try-it (+ (Node :value)
                                       (/ Dx
                                          pixels-per-value-unit))))
                        (between min try-it max)))
    
    ;;(c-display "num" ($node :num) ($get-num-nodes-func) "value" $dx ($node :value) (node-area :x1) (node-area :x2) ($get-node-value-func ($node :num)))
    (define new-y (and (not (= 0 Dy))
                       (let ((try-it (+ (Node :y)
                                        Dy)))
                         (between (1- (ra:get-top-visible-y))
                                  try-it
                                  (+ 2 (ra:get-bot-visible-y))))))
                                   
    (Move-node node-info new-value (and new-y (get-place-from-y Button new-y)))
    (Publicize node-info)
    (make-node :node-info node-info
               :value new-value
               :y (or new-y (Node :y))))

  (define (release Button Dx Dy Node)
    (define node-info (Node :node-info))
    (ra:move-mouse-pointer (Get-x node-info)
                           (Get-y node-info))
    )

  (define move-existing-node-mouse-cycle (make-node-mouse-cycle :press press-existing-node
                                                                :move-and-release move-and-release
                                                                :release release))
  
  (define create-new-node-mouse-cycle (make-node-mouse-cycle :press press-and-create-new-node
                                                             :move-and-release move-and-release
                                                             :release release))

  (push-back! *move-existing-node-mouse-cycles* move-existing-node-mouse-cycle)
  (push-back! *create-new-node-mouse-cycles* create-new-node-mouse-cycle)

  )

      

;; Used for sliders and track width
(delafina (add-horizontal-handler :Get-handler-data
                                  :Get-x1
                                  :Get-x2
                                  :Get-min-value
                                  :Get-max-value
                                  :Get-x
                                  :Get-value
                                  :Make-undo
                                  :Move
                                  :Publicize)
 
  (define-struct info
    :handler-data
    :y)

  (add-node-mouse-handler :Get-area-box (lambda () (make-box2 0 0 10000 1))
                          :Get-existing-node-info (lambda (X Y callback)
                                                    (define handler-data (Get-handler-data X Y))
                                                    (and handler-data
                                                         (let ((info (make-info :handler-data handler-data
                                                                                :y Y)))
                                                           (callback info
                                                                     (Get-value handler-data)
                                                                     0))))
                          :Get-min-value (lambda (Info)
                                           (Get-min-value (Info :handler-data)))
                          :Get-max-value (lambda (Info)
                                           (Get-max-value (Info :handler-data)))
                          :Get-x (lambda (Info)
                                   (Get-x (Info :handler-data)))
                          :Get-y (lambda (Info)
                                   (Info :y))
                          :Make-undo (lambda (Info)
                                       (Make-undo (Info :handler-data)))
                          :Create-new-node (lambda (Value Place callback)
                                             #f)
                          :Move-node (lambda (Info Value Place)
                                       (Move (Info :handler-data)
                                             Value))
                          :Publicize (lambda (Info)
                                       (Publicize (Info :handler-data)))
                          :Get-pixels-per-value-unit (lambda (Info)
                                                       (/ (- (Get-x2 (Info :handler-data))
                                                             (Get-x1 (Info :handler-data)))
                                                          (- (Get-max-value (Info :handler-data))
                                                             (Get-min-value (Info :handler-data)))))
                          ))
                                  



;; status bar and mouse pointer for block and track sliders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-move-handler
 :move (lambda ($button X Y)
         (cond ((and *current-track-num*
                     (inside-box (ra:get-box track-pan-slider *current-track-num*) X Y))
                (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer)
                (show-track-pan-in-statusbar *current-track-num*))
               ((and *current-track-num*
                     (inside-box (ra:get-box track-volume-slider *current-track-num*) X Y))
                (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer)
                (show-track-volume-in-statusbar *current-track-num*))
               ((inside-box (ra:get-box reltempo-slider) X Y)
                (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer)
                (show-reltempo-in-statusbar))
               (else
                (ra:set-normal-mouse-pointer)))))



;; block tempo multiplier slider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-reltempo-in-statusbar)
  (ra:set-statusbar-text (<-> "Block tempo multiplied by " (two-decimal-string (ra:get-reltempo)))))


(define (get-reltemposlider-x)
  (define box (ra:get-box reltempo-slider))
  (scale (ra:get-reltempo)
         (ra:get-min-reltempo)
         (ra:get-max-reltempo)
         (box :x1)
         (box :x2)))

;; slider
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (define box (ra:get-box reltempo-slider))
                                            (and (inside-box box X Y)
                                                 (ra:get-reltempo)))
                        :Get-x1 (lambda (_)
                                  (ra:get-reltempo-slider-x1))
                        :Get-x2 (lambda (_)
                                  (ra:get-reltempo-slider-x2))
                        :Get-min-value (lambda (_)
                                         (ra:get-min-reltempo))
                        :Get-max-value (lambda (_)
                                         (ra:get-max-reltempo))
                        :Get-x (lambda (_)
                                 (get-reltemposlider-x))
                        :Get-value (lambda (Value)
                                     Value)
                        :Make-undo (lambda (_)
                                     (ra:undo-reltempo))
                        :Move (lambda (_ Value)
                                (ra:set-reltempo Value))
                        :Publicize (lambda (_)
                                     (show-reltempo-in-statusbar))
                        )



;; track pan on/off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)
                                (and *current-track-num*
                                     (inside-box (ra:get-box track-pan-on-off *current-track-num*) X Y)
                                     (ra:set-track-pan-on-off (not (ra:get-track-pan-on-off *current-track-num*))
                                                              *current-track-num*)
                                     #t))
                  :drag-func  (lambda (Button X Y)
                                #f)
                  :release-func (lambda ($button $x $y)
                                  #f)))



;; track volume on/off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)
                                (and *current-track-num*
                                     (inside-box (ra:get-box track-volume-on-off *current-track-num*) X Y)
                                     (ra:set-track-volume-on-off (not (ra:get-track-volume-on-off *current-track-num*))
                                                                 *current-track-num*)
                                     #t))
                  :drag-func  (lambda (Button X Y)
                                #f)
                  :release-func (lambda ($button $x $y)
                                  #f)))



;; track pan sliders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-track-pan-in-statusbar Tracknum)
  (ra:set-statusbar-text (<-> "Track pan " (two-decimal-string (ra:get-track-pan Tracknum)))))

(define (get-trackpan-x Tracknum)
  (scale (ra:get-track-pan Tracknum)
         -1
         1
         (ra:get-track-pan-slider-x1 Tracknum)
         (ra:get-track-pan-slider-x2 Tracknum)))

;; slider
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (and *current-track-num*
                                                 (inside-box (ra:get-box track-pan-slider *current-track-num*) X Y)
                                                 *current-track-num*))
                        :Get-x1 ra:get-track-pan-slider-x1
                        :Get-x2 ra:get-track-pan-slider-x2
                        :Get-min-value (lambda (_)
                                         -1.0)
                        :Get-max-value (lambda (_)
                                         1.0)
                        :Get-x (lambda (Tracknum)
                                 (get-trackpan-x Tracknum))
                        :Get-value ra:get-track-pan
                        :Make-undo ra:undo-track-pan
                        :Move (lambda (Tracknum Value)
                                (c-display Tracknum Value)
                                (ra:set-track-pan Value Tracknum))
                        :Publicize (lambda (Tracknum)
                                     (show-track-pan-in-statusbar Tracknum))
                        )


;; track volume sliders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-track-volume-in-statusbar Tracknum)
  (ra:set-statusbar-text (<-> "Track volume " (two-decimal-string (ra:get-track-volume Tracknum)))))

(define (get-trackvolume-x Tracknum)
  (scale (ra:get-track-volume Tracknum)
         0
         1
         (ra:get-track-volume-slider-x1 Tracknum)
         (ra:get-track-volume-slider-x2 Tracknum)))

;; slider
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (and *current-track-num*
                                                 (inside-box (ra:get-box track-volume-slider *current-track-num*) X Y)
                                                 *current-track-num*))
                        :Get-x1 ra:get-track-volume-slider-x1
                        :Get-x2 ra:get-track-volume-slider-x2
                        :Get-min-value (lambda (_)
                                         0.0)
                        :Get-max-value (lambda (_)
                                         1.0)
                        :Get-x (lambda (Tracknum)
                                 (get-trackvolume-x Tracknum))
                        :Get-value ra:get-track-volume
                        :Make-undo ra:undo-track-volume
                        :Move (lambda (Tracknum Value)
                                (c-display Tracknum Value)
                                (ra:set-track-volume Value Tracknum))
                        :Publicize (lambda (Tracknum)
                                     (show-track-volume-in-statusbar Tracknum))
                        )


;; temponodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (if (inside-box (ra:get-box temponode-area) $x $y)
             (c-display "inside" $x $y))))
||#

(define (show-temponode-in-statusbar value)
  (define actual-value (if (< value 0) ;; see reltempo.c
                           (/ 1
                              (- 1 value))
                           (1+ value)))
  (ra:set-statusbar-text (<-> "Tempo multiplied by " (two-decimal-string actual-value))))

(define (get-temponode-box $num)
  (get-common-node-box (ra:get-temponode-x $num)
                       (ra:get-temponode-y $num)))

(define (temponodeval->01 value)
  (scale value
         (- (1- (ra:get-temponode-max)))
         (1- (ra:get-temponode-max))
         0
         1))

(define (01->temponodeval O1)
  (scale O1
         0
         1
         (- (1- (ra:get-temponode-max)))
         (1- (ra:get-temponode-max))))
         
(add-node-mouse-handler :Get-area-box (lambda () (ra:get-box temponode-area))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (and (inside-box-forgiving (ra:get-box temponode-area) X Y)
                                                       (match (list (find-node X Y get-temponode-box (ra:get-num-temponodes)))
                                                              (existing-box Num Box) :> (callback Num (temponodeval->01 (ra:get-temponode-value Num)) (Box :y))
                                                              _                      :> #f)))
                        :Get-min-value (lambda (_) 0);(- (1- (ra:get-temponode-max))))
                        :Get-max-value (lambda (_) 1);(1- (ra:get-temponode-max)))
                        :Get-x (lambda (Num) (ra:get-temponode-x Num))
                        :Get-y (lambda (Num) (ra:get-temponode-y Num))
                        :Make-undo (lambda (_) ra:undo-temponodes)
                        :Create-new-node (lambda (X Place callback)
                                           (define Value (scale X (ra:get-temponode-area-x1) (ra:get-temponode-area-x2) 0 1))
                                           (define Num (ra:create-temponode (01->temponodeval Value) Place))
                                           (if (= -1 Num)
                                               #f
                                               (callback Num (temponodeval->01 (ra:get-temponode-value Num)))))
                        :Move-node (lambda (Num Value Place)
                                     (ra:set-temponode Num (01->temponodeval Value) (or Place -1))
                                     (define new-value (ra:get-temponode-value Num)) ;; might differ from Value
                                     ;;(c-display "Place/New:" Place (ra:get-temponode-value Num))
                                     (temponodeval->01 new-value)
                                     )
                        :Publicize (lambda (Num) ;; this version works though. They are, or at least, should be, 100% functionally similar.
                                     (set-indicator-temponode Num)
                                     (show-temponode-in-statusbar (ra:get-temponode-value Num)))
                        :Get-pixels-per-value-unit #f
                        )                        

;; delete temponode
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *middle-button*)
                     (inside-box (ra:get-box temponode-area) $x $y)                                     
                     (match (list (find-node $x $y get-temponode-box (ra:get-num-temponodes)))
                            (existing-box Num Box) :> (begin
                                                        (ra:undo-temponodes)
                                                        (ra:delete-temponode Num)
                                                        #t)
                            _                      :> #f)))))

;; highlight current temponode
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and (inside-box-forgiving (ra:get-box temponode-area) $x $y)
              (match (list (find-node $x $y get-temponode-box (ra:get-num-temponodes)))
                     (existing-box Num Box) :> (begin
                                                 (set-mouse-track-to-reltempo)
                                                 (set-current-temponode Num)
                                                 (set-indicator-temponode Num)
                                                 (show-temponode-in-statusbar (ra:get-temponode-value Num))
                                                 #t)
                     _                      :> #f))))





;; pitches
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-pitch-box $num)
  ;;(c-display "get-pitch-box" $num)
  (make-box2 (ra:get-pitch-x1 $num *current-track-num*)
             (ra:get-pitch-y1 $num *current-track-num*)
             (ra:get-pitch-x2 $num *current-track-num*)
             (ra:get-pitch-y2 $num *current-track-num*)))

(define (todofunc funcname . $returnvalue)
  (lambda x
    (c-display "\"" funcname "\" not implemented. Arguments: " x)
    (if (null? $returnvalue)
        'no-return-value
        (car $returnvalue))))
  
#||
(set! *current-track-num* 0)
(box-to-string (get-pitch-box 1))
(ra:get-num-pitches 0)
(ra:get-pitch-value 1 0)
||#

#||
(add-delta-mouse-handler
 :press (lambda ($button $x $y)
          (c-display $x $y)
          #f))
||#


(define-match get-min-pitch-in-current-track-0
  N N   #f           :> 0
  N N   Least-So-Far :> Least-So-Far
  N Max #f           :> (get-min-pitch-in-current-track-0 (1+ N)
                                                          Max
                                                          (ra:get-pitch-value N *current-track-num*))
  N Max Least-So-Far :> (get-min-pitch-in-current-track-0 (1+ N)
                                                          Max
                                                          (min Least-So-Far
                                                               (ra:get-pitch-value N *current-track-num*))))
  
(define (get-min-pitch-in-current-track)
  (1- (get-min-pitch-in-current-track-0 0
                                        (ra:get-num-pitches *current-track-num*)
                                        #f)))
       
(define-match get-max-pitch-in-current-track-0
  N N   #f           :> 127
  N N   Least-So-Far :> Least-So-Far
  N Max #f           :> (get-max-pitch-in-current-track-0 (1+ N)
                                                          Max
                                                          (ra:get-pitch-value N *current-track-num*))
  N Max Least-So-Far :> (get-max-pitch-in-current-track-0 (1+ N)
                                                          Max
                                                          (max Least-So-Far
                                                               (ra:get-pitch-value N *current-track-num*))))
  
(define (get-max-pitch-in-current-track)
  (1+ (get-max-pitch-in-current-track-0 0
                                        (ra:get-num-pitches *current-track-num*)
                                        #f)))

;; add and move pitch
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (ra:get-box track-notes *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (and *current-track-num*
                                                       (not (may-be-a-resize-point-in-track X Y *current-track-num*))
                                                       (match (list (find-node X Y get-pitch-box (ra:get-num-pitches *current-track-num*)))
                                                              (existing-box Num Box) :> (callback Num (ra:get-pitch-value Num *current-track-num*) (Box :y))
                                                              _                      :> #f)))
                        :Get-min-value (lambda (_)
                                         (get-min-pitch-in-current-track))
                        :Get-max-value (lambda (_)
                                         (get-max-pitch-in-current-track))
                        :Get-x (lambda (Num)
                                 (ra:get-pitch-x Num *current-track-num*))
                        :Get-y (lambda (Num)
                                 (ra:get-pitch-y Num *current-track-num*))
                        :Make-undo (lambda (_) (ra:undo-notes *current-track-num*))
                        :Create-new-node (lambda (X Place callback)
                                           (define Value (scale X
                                                                (ra:get-track-notes-x1 *current-track-num*) (ra:get-track-notes-x2 *current-track-num*) 
                                                                (get-min-pitch-in-current-track) (get-max-pitch-in-current-track)))
                                           (define Num (ra:create-pitch Value Place *current-track-num*))
                                           (if (= -1 Num)
                                               #f
                                               (callback Num (ra:get-pitch-value Num *current-track-num*))))
                        :Move-node (lambda (Num Value Place)                                     
                                     (ra:set-pitch Num Value (or Place -1) *current-track-num*))
                        :Publicize (lambda (Num)
                                     (set-indicator-pitch Num *current-track-num*)
                                     (ra:set-statusbar-text (<-> "Pitch: " (two-decimal-string (ra:get-pitch-value Num *current-track-num*)))))
                        :Get-pixels-per-value-unit (lambda (_)
                                                     5.0)
                        )


;; delete pitch
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *middle-button*)
                     *current-track-num*
                     (inside-box (ra:get-box track-notes *current-track-num*) $x $y)
                     (match (list (find-node $x $y get-pitch-box (ra:get-num-pitches *current-track-num*)))
                            (existing-box Num Box) :> (begin
                                                        (ra:undo-notes *current-track-num*)
                                                        (ra:delete-pitch Num *current-track-num*)
                                                        #t)
                            _                      :> #f)))))



;; highlight current pitch
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and *current-track-num*
              (inside-box (ra:get-box track-notes *current-track-num*) $x $y)
              (match (list (find-node $x $y get-pitch-box (ra:get-num-pitches *current-track-num*)))
                     (existing-box Num Box) :> (begin
                                                 (set-indicator-pitch Num *current-track-num*)
                                                 (set-current-pitch Num  *current-track-num*)
                                                 #t)
                     _                      :> #f))))




;; velocities
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-velocity-box Tracknum Notenum Velocitynum)
  (get-common-node-box (ra:get-velocity-x Velocitynum Notenum Tracknum)
                       (ra:get-velocity-y Velocitynum Notenum Tracknum)))

(define-struct velocity-info
  :tracknum
  :notenum
  :velocitynum
  :value
  :y  
  )

(define (velocity-info-rating Y Vi)
  (define velocity-y (ra:get-velocity-y (Vi :velocitynum) (Vi :notenum) (Vi :tracknum)))
  (cond ((and (= 0
                 (Vi :velocitynum))
              (> Y
                 velocity-y))
         10)
        ((and (= (1- (ra:get-num-velocities (Vi :notenum) (Vi :tracknum)))
                 (Vi :velocitynum))
              (< Y
                 velocity-y))
         10)
        (else
         0)))

(define (highest-rated-velocity-info-0 Y A B)
  (if (> (velocity-info-rating Y A)
         (velocity-info-rating Y B))
      A
      B))
         
(define-match highest-rated-velocity-info
  _ (#f       ) :> #f
  Y (#f . Rest) :> (highest-rated-velocity-info Y Rest)
  _ (A        ) :> A
  Y (A  . Rest) :> (let ((B (highest-rated-velocity-info Y Rest)))
                     (if B
                         (highest-rated-velocity-info-0 Y A B)
                         A)))

#||
(highest-rated-velocity-info 'b )
(highest-rated-velocity-info 'b 2)
(highest-rated-velocity-info 'b #f 3)
(highest-rated-velocity-info 'b 4 #f)
||#

(define-match get-velocity-2
  X Y Tracknum Notenum Velocitynum Velocitynum      :> #f
  X Y Tracknum Notenum Velocitynum Total-Velocities :> (begin                                                                     
                                                         (define box (get-velocity-box Tracknum Notenum Velocitynum))
                                                         (if (> (box :y1) Y)
                                                             #f
                                                             (highest-rated-velocity-info Y
                                                                                          (list (get-velocity-2 X Y Tracknum Notenum (1+ Velocitynum) Total-Velocities)
                                                                                                (and (inside-box box X Y)
                                                                                                     (make-velocity-info :velocitynum Velocitynum
                                                                                                                         :notenum Notenum
                                                                                                                         :tracknum Tracknum
                                                                                                                         :value (ra:get-velocity-value Velocitynum Notenum Tracknum)
                                                                                                                         :y (box :y)
                                                                                                                         )))))))

(define-match get-velocity-1
  X Y Tracknum Notenum Notenum     :> #f
  X Y Tracknum Notenum Total-Notes :> (highest-rated-velocity-info Y
                                                                   (list (get-velocity-1 X Y Tracknum (1+ Notenum) Total-Notes)
                                                                         (get-velocity-2 X Y Tracknum Notenum 0 (ra:get-num-velocities Notenum Tracknum)))))
                                   
  
(define-match get-velocity-0
  X Y -1       :> #f
  X Y Tracknum :> #f :where (>= Tracknum (ra:get-num-tracks))
  X Y Tracknum :> (get-velocity-1 X Y Tracknum 0 (ra:get-num-notes Tracknum)))
  
(define-match get-velocity-info
  X Y #f       :> (get-velocity-info X Y 0)
  X Y Tracknum :> (highest-rated-velocity-info Y
                                               (list (get-velocity-0 X Y (1- Tracknum)) ;; node in the prev track can overlap into the current track
                                                     (get-velocity-0 X Y Tracknum))))


#||
(let ((node (get-velocity-info 319 169 0)))
  (c-display (node :velocitynum) (node :notenum) (node :tracknum)))
        

(ra:get-velocity-x 1 0 0)
(ra:get-velocity-y 1 0 0)
||#

(define *current-note-num* #f)


(define-match get-note-num-0
  _____ ________ Num Num   :> #f
  Place Subtrack Num Total :> (if (and (>= Place
                                           (ra:get-note-start Num *current-track-num*))
                                       (<  Place
                                           (ra:get-note-end Num *current-track-num*))
                                       (=  Subtrack
                                           (ra:get-note-subtrack Num *current-track-num*)))
                                  Num
                                  (get-note-num-0 Place
                                                  Subtrack
                                                  (1+ Num)
                                                  Total)))
                                                 
(define-match get-note-num
  X Y :> (get-note-num-0 (ra:get-place-from-y Y)
                         *current-subtrack-num*
                         0
                         (ra:get-num-notes *current-track-num*)))

;; Set *current-note-num*
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (set! *current-note-num* (and *current-subtrack-num*
                                       (get-note-num $x $y)))))

         
(define-match get-shortest-velocity-distance-0
  ___ _________ X Y X1 Y1 X2 Y2 :> (get-distance X Y X1 Y1 X2 Y2) :where (and (>= Y Y1)
                                                                              (< Y Y2))
  Vel Vel       _ _ __ __ __ __ :> #f
  Vel Total-vel X Y __ __ X2 Y2 :> (get-shortest-velocity-distance-0 (1+ Vel)
                                                                     Total-vel
                                                                     X Y
                                                                     X2 Y2
                                                                     (ra:get-velocity-x Vel *current-note-num* *current-track-num*)
                                                                     (ra:get-velocity-y Vel *current-note-num* *current-track-num*)))

(define (get-shortest-velocity-distance X Y)
  (if (not *current-note-num*)
      #f
      (get-shortest-velocity-distance-0 2
                                        (ra:get-num-velocities *current-note-num* *current-track-num*)
                                        X Y
                                        (ra:get-velocity-x 0 *current-note-num* *current-track-num*)
                                        (ra:get-velocity-y 0 *current-note-num* *current-track-num*)
                                        (ra:get-velocity-x 1 *current-note-num* *current-track-num*)
                                        (ra:get-velocity-y 1 *current-note-num* *current-track-num*))))




;; add and move velocity
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (ra:get-box track-fx *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (and *current-track-num*
                                                       (let ((velocity-info (get-velocity-info X Y *current-track-num*)))
                                                         (and velocity-info
                                                              (callback velocity-info (velocity-info :value) (velocity-info :y))))))
                        :Get-min-value (lambda (_) 0.0)
                        :Get-max-value (lambda (_) 1.0)
                        :Get-x (lambda (info) (ra:get-velocity-x (info :velocitynum)
                                                                 (info :notenum)
                                                                 (info :tracknum)))
                        :Get-y (lambda (info) (ra:get-velocity-y (info :velocitynum)
                                                                 (info :notenum)
                                                                 (info :tracknum)))
                        :Make-undo (lambda (_) (ra:undo-notes *current-track-num*))
                        :Create-new-node (lambda (X Place callback)
                                           (and *current-note-num*
                                                (not (get-current-fxnum))
                                                (begin
                                                  (define Value (scale X
                                                                       (ra:get-track-fx-x1 *current-track-num*) (ra:get-track-fx-x2 *current-track-num*) 
                                                                       0 1))
                                                  (define Num (ra:create-velocity Value Place *current-note-num* *current-track-num*))
                                                  (if (= -1 Num)
                                                      #f
                                                      (callback (make-velocity-info :tracknum *current-track-num*
                                                                                    :notenum *current-note-num*
                                                                                    :velocitynum Num
                                                                                    :value Value
                                                                                    :y #f ;; dont need it.
                                                                                    )
                                                                (ra:get-velocity-value Num *current-note-num* *current-track-num*))))))
                        :Publicize (lambda (velocity-info)
                                              (set-indicator-velocity-node (velocity-info :velocitynum)
                                                                           (velocity-info :notenum)
                                                                           (velocity-info :tracknum))
                                              (define value (ra:get-velocity-value (velocity-info :velocitynum)
                                                                                   (velocity-info :notenum)
                                                                                   (velocity-info :tracknum)))
                                              (ra:set-statusbar-text (<-> "Velocity: " (two-decimal-string value))))
                        :Move-node (lambda (velocity-info Value Place)
                                     (ra:set-velocity (velocity-info :velocitynum) Value (or Place -1) (velocity-info :notenum) (velocity-info :tracknum)))
                        )

;; delete velocity
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *middle-button*)
                     *current-track-num*
                     (inside-box-forgiving (ra:get-box track *current-track-num*) X Y)
                     (begin
                       (define velocity-info (get-velocity-info X Y *current-track-num*))
                       ;;(c-display "got velocity info " velocity-info)
                       (if velocity-info
                           (begin
                             (ra:undo-notes (velocity-info :tracknum))
                             (ra:delete-velocity (velocity-info :velocitynum)
                                                 (velocity-info :notenum)
                                                 (velocity-info :tracknum))
                             #t)
                           #f))))))


#||
;; show current velocity
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (and *current-track-num*
              (inside-box-forgiving (ra:get-box track *current-track-num*) X Y)
              (begin
                (define velocity-info (get-velocity-info X Y *current-track-num*))
                (c-display "got velocity info " velocity-info)
                (if velocity-info
                    (begin
                      (set-mouse-note (velocity-info :notenum) (velocity-info :tracknum))
                      (c-display "setting current to " (velocity-info :velocitynum))
                      (set-indicator-velocity-node (velocity-info :velocitynum)
                                                   (velocity-info :notenum)
                                                   (velocity-info :tracknum))
                      (set-current-velocity-node (velocity-info :velocitynum) (velocity-info :notenum) (velocity-info :tracknum)))
                    (c-display "no current"))))))
||#

#||
(ra:get-num-velocities 0 0)

(ra:get-velocitynode-y 0 0)
(ra:get-velocitynode-y 2 0)
(ra:get-velocity-value 7 1)



||#


;; track borders
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct trackwidth-info
  :tracknum
  :width
  :y)

(define (may-be-a-resize-point-in-track X Y Tracknum)
  (and (>= X (- (ra:get-track-notes-x1 Tracknum)
                2))
       (<= X (+ (ra:get-track-notes-x1 Tracknum)
                (ra:get-half-of-node-width)))))

(define-match get-resize-point-track
  X _ Tracknum :> (and (>= X (- (ra:get-track-fx-x2 (1- Tracknum))
                                2))
                       Tracknum) :where (= Tracknum (ra:get-num-tracks)) ;; i.e. to the right of the rightmost track
  X _ Tracknum :> #f       :where (> (ra:get-track-notes-x1 Tracknum) X)
  X Y Tracknum :> Tracknum :where (may-be-a-resize-point-in-track X Y Tracknum)
  X Y Tracknum :> (get-resize-point-track X Y (1+ Tracknum)))

(define (get-trackwidth-info X Y)
  (define resize-point-track (get-resize-point-track X Y 0))
  (and resize-point-track
       (let ((tracknum (1- resize-point-track)))
         (make-trackwidth-info :tracknum tracknum
                               :width    (ra:get-track-width tracknum)
                               :y        Y))))

#||
(add-delta-mouse-handler
 :press (lambda (Button X Y)
          (and (= Button *left-button*)
               (get-trackwidth-info X Y)))

 :move-and-release (lambda (Button DX DY trackwidth-info)
                     (c-display "hepp")
                     (define tracknum (trackwidth-info :tracknum))
                     (define new-width (+ DX
                                          (trackwidth-info :width)))
                     (ra:set-track-width new-width tracknum)
                     (make-trackwidth-info :tracknum tracknum
                                             :width    new-width)))

||#

(add-horizontal-handler :Get-handler-data get-trackwidth-info
                        :Get-x1 (lambda (Trackwidth-info)
                                  (ra:get-track-fx-x1 (Trackwidth-info :tracknum)))
                        :Get-x2 (lambda (Trackwidth-info)
                                  (+ 10000
                                     (ra:get-track-fx-x1 (Trackwidth-info :tracknum))))
                        :Get-min-value (lambda (_)
                                         0)
                        :Get-max-value (lambda (_)
                                         10000)
                        :Get-x (lambda (Trackwidth-info)
                                 (define tracknum (Trackwidth-info :tracknum))
                                 (if (= tracknum (1- (ra:get-num-tracks)))
                                     (ra:get-track-fx-x2 tracknum)
                                     (ra:get-track-notes-x1 (1+ tracknum))))
                        :Get-value (lambda (Trackwidth-info)
                                     (Trackwidth-info :width))
                        :Make-undo (lambda (_) #f)
                        :Move (lambda (Trackwidth-info Value)
                                (define tracknum (Trackwidth-info :tracknum))
                                (ra:set-track-width Value tracknum))
                        :Publicize (lambda (_)
                                     #f))
                        
                        



;; fxnodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-fxnode-box Tracknum FX-num FX-nodenum)
  (get-common-node-box (ra:get-fxnode-x FX-nodenum FX-num Tracknum)
                       (ra:get-fxnode-y FX-nodenum FX-num Tracknum)))

#||
(box-to-string (get-fxnode-box 0 0 1))
||#

(define-struct fxnode-info
  :tracknum
  :fxnum
  :fxnodenum
  :value
  :y  
  )

(define (fxnode-info-rating Y Fi)
  (define fxnode-y (ra:get-fxnode-y (Fi :fxnodenum) (Fi :fxnum) (Fi :tracknum)))
  (cond ((and (= 0
                 (Fi :fxnodenum))
              (> Y
                 fxnode-y))
         10)
        ((and (= (1- (ra:get-num-fxnodes (Fi :fxnum) (Fi :tracknum)))
                 (Fi :fxnodenum))
              (< Y
                 fxnode-y))
         10)
        (else
         0)))

(define (highest-rated-fxnode-info-0 Y A B)
  (if (> (fxnode-info-rating Y A)
         (fxnode-info-rating Y B))
      A
      B))
         
(define-match highest-rated-fxnode-info
  _ (#f       ) :> #f
  Y (#f . Rest) :> (highest-rated-fxnode-info Y Rest)
  _ (A        ) :> A
  Y (A  . Rest) :> (let ((B (highest-rated-fxnode-info Y Rest)))
                     (if B
                         (highest-rated-fxnode-info-0 Y A B)
                         A)))

(define-match get-fxnode-2
  X Y Tracknum Fxnum Fxnodenum Fxnodenum      :> #f
  X Y Tracknum Fxnum Fxnodenum Total-Fxnodes :> (begin                                                                     
                                                  (define box (get-fxnode-box Tracknum Fxnum Fxnodenum))
                                                  (if (> (box :y1) Y)
                                                      #f
                                                      (highest-rated-fxnode-info Y
                                                                                 (list (get-fxnode-2 X Y Tracknum Fxnum (1+ Fxnodenum) Total-Fxnodes)
                                                                                       (and (inside-box box X Y)
                                                                                            (make-fxnode-info :fxnodenum Fxnodenum
                                                                                                              :fxnum Fxnum
                                                                                                              :tracknum Tracknum
                                                                                                              :value (ra:get-fxnode-value Fxnodenum Fxnum Tracknum)
                                                                                                              :y (box :y)
                                                                                                              )))))))

(define-match get-fxnode-1
  X Y Tracknum Fxnum Fxnum     :> #f
  X Y Tracknum Fxnum Total-Fxs :> (highest-rated-fxnode-info Y
                                                             (list (get-fxnode-1 X Y Tracknum (1+ Fxnum) Total-Fxs)
                                                                   (get-fxnode-2 X Y Tracknum Fxnum 0 (ra:get-num-fxnodes Fxnum Tracknum)))))


(define-match get-fxnode-0
  X Y -1       :> #f
  X Y Tracknum :> #f :where (>= Tracknum (ra:get-num-tracks))
  X Y Tracknum :> (get-fxnode-1 X Y Tracknum 0 (ra:get-num-fxes Tracknum)))

(define-match get-fxnode-info
  X Y #f       :> (get-fxnode-info X Y 0)
  X Y Tracknum :> (highest-rated-fxnode-info Y
                                             (list (get-fxnode-0 X Y (1- Tracknum)) ;; node in the prev track can overlap into the current track
                                                   (get-fxnode-0 X Y Tracknum))))


#||
(ra:get-num-fxes 0)
(let ((node (get-fxnode-info 347 211 0)))
  (c-display "hm?" node)
  (if node
      (c-display "node:" (node :fxnodenum) "value:" (node :value))))
        
(ra:get-fxnode-x 0 0 0)
(ra:get-fxnode-y 0 0 0)
(ra:get-fxnode-value 0 0 0)
||#


(define-struct fx/distance
  :fx
  :distance)

(define *current-fx/distance* #f)

(define (get-current-fxnum)
  (and *current-fx/distance*
       (*current-fx/distance* :fx)))
(define (get-current-fx-distance)
  (and *current-fx/distance*
       (*current-fx/distance* :distance)))
  
  
(define (min-fx/distance A B)
  (cond ((not A)
         B)
        ((not B)
         A)
        ((<= (A :distance) (B :distance))
         A)
        (else
         B)))
          
(define-match get-closest-fx-1
  _______ ___________ Fx X Y X1 Y1 X2 Y2 :> (make-fx/distance :fx Fx
                                                              :distance (get-distance X Y X1 Y1 X2 Y2))
                                            :where (and (>= Y Y1)
                                                        (< Y Y2))
  Nodenum Nodenum     __ _ _ __ __ __ __ :> #f
  Nodenum Total-Nodes Fx X Y __ __ X2 Y2 :> (get-closest-fx-1 (1+ Nodenum)
                                                              Total-Nodes
                                                              Fx
                                                              X Y
                                                              X2 Y2
                                                              (ra:get-fxnode-x Nodenum Fx *current-track-num*)
                                                              (ra:get-fxnode-y Nodenum Fx *current-track-num*)))

(define-match get-closest-fx-0
  Fx Fx        _ _ :> #f
  Fx Total-Fxs X Y :> (min-fx/distance (get-closest-fx-1 2
                                                         (ra:get-num-fxnodes Fx *current-track-num*)
                                                         Fx
                                                         X Y
                                                         (ra:get-fxnode-x 0 Fx *current-track-num*)
                                                         (ra:get-fxnode-y 0 Fx *current-track-num*)
                                                         (ra:get-fxnode-x 1 Fx *current-track-num*)
                                                         (ra:get-fxnode-y 1 Fx *current-track-num*))
                                       (get-closest-fx-0 (1+ Fx)
                                                         Total-Fxs
                                                         X
                                                         Y)))
                                                                


(define (get-closest-fx X Y)
  (get-closest-fx-0 0 (ra:get-num-fxes *current-track-num*) X Y))

#||
(ra:get-num-fxes 0)
||#

;; add and move fxnode
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (ra:get-box track-fx *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (and *current-track-num*
                                                       (let ((fxnode-info (get-fxnode-info X Y *current-track-num*)))
                                                         (and fxnode-info
                                                              (callback fxnode-info (fxnode-info :value) (fxnode-info :y))))))
                        :Get-min-value (lambda (fxnode-info)
                                         (ra:get-fx-min-value (fxnode-info :fxnum)))
                        :Get-max-value (lambda (fxnode-info)
                                         (ra:get-fx-max-value (fxnode-info :fxnum)))
                        :Get-x (lambda (info) (ra:get-fxnode-x (info :fxnodenum)
                                                               (info :fxnum)
                                                               (info :tracknum)))
                        :Get-y (lambda (info) (ra:get-fxnode-y (info :fxnodenum)
                                                               (info :fxnum)
                                                               (info :tracknum)))
                        :Make-undo (lambda (_) (ra:undo-fxs *current-track-num*))
                        :Create-new-node (lambda (X Place callback)
                                           (define Fxnum (get-current-fxnum))
                                           (and Fxnum
                                                (begin
                                                  (define Value (scale X
                                                                       (ra:get-track-fx-x1 *current-track-num*) (ra:get-track-fx-x2 *current-track-num*)
                                                                       (ra:get-fx-min-value Fxnum) (ra:get-fx-max-value Fxnum)))
                                                  (define Nodenum (ra:create-fxnode Value Place Fxnum *current-track-num*))
                                                  (if (= -1 Nodenum)
                                                      #f
                                                      (callback (make-fxnode-info :tracknum *current-track-num*
                                                                                  :fxnum Fxnum
                                                                                  :fxnodenum Nodenum
                                                                                  :value Value
                                                                                  :y #f ;; dont need it.
                                                                                  )
                                                                (ra:get-fxnode-value Nodenum Fxnum *current-track-num*))))))
                        :Publicize (lambda (fxnode-info)
                                     (set-indicator-fxnode (fxnode-info :fxnodenum)
                                                           (fxnode-info :fxnum)
                                                           (fxnode-info :tracknum))
                                     (ra:set-statusbar-text (ra:get-fx-string (fxnode-info :fxnodenum) (fxnode-info :fxnum) (fxnode-info :tracknum))))

                        :Move-node (lambda (fxnode-info Value Place)                                     
                                     (ra:set-fxnode (fxnode-info :fxnodenum) Value (or Place -1) (fxnode-info :fxnum) (fxnode-info :tracknum)))
                        )

;; delete fx
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *middle-button*)
                     *current-track-num*
                     (inside-box-forgiving (ra:get-box track *current-track-num*) X Y)
                     (begin
                       (define fxnode-info (get-fxnode-info X Y *current-track-num*))
                       ;;(c-display "got fx info " fxnode-info)
                       (if fxnode-info
                           (begin
                             (ra:undo-fxs *current-track-num*)
                             (ra:delete-fxnode (fxnode-info :fxnodenum)
                                               (fxnode-info :fxnum)
                                               (fxnode-info :tracknum))
                             #t)
                           #f))))))

;; Show and set:
;;  1. current fx or current note, depending on which nodeline is closest to the mouse pointer
;;  2. current velocity node, or current fxnode
;;
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (define resize-mouse-pointer-is-set #f)
         (define result
           (and *current-track-num*
                (inside-box-forgiving (ra:get-box track *current-track-num*) X Y)
                (lazy
                 (define-lazy velocity-info (get-velocity-info X Y *current-track-num*))
                 (define-lazy fxnode-info (get-fxnode-info X Y *current-track-num*))

                (define-lazy velocity-dist (get-shortest-velocity-distance X Y))
                (define-lazy fx-dist (get-closest-fx X Y))

                (define-lazy is-in-fx-area (inside-box (ra:get-box track-fx *current-track-num*) X Y))
                  
                (define-lazy velocity-dist-is-shortest
                  (cond ((not velocity-dist)
                         #f)
                        ((not fx-dist)
                         #t)
                        (else
                         ;;(c-display "dist:" fx-dist) ;; :distance))
                         (<= velocity-dist
                             (fx-dist :distance)))))

                (define-lazy fx-dist-is-shortest
                  (cond ((not fx-dist)
                         #f)
                        ((not velocity-dist)
                         #t)
                        (else
                         (<= (fx-dist :distance)
                             velocity-dist))))


                (define-lazy trackwidth-info (get-trackwidth-info X Y))
                (set! *current-fx/distance* #f)
                
                (cond (velocity-info
                       (set-mouse-note (velocity-info :notenum) (velocity-info :tracknum))
                       ;;(c-display "setting current to " (velocity-info :velocitynum) (velocity-info :dir))
                       (set-indicator-velocity-node (velocity-info :velocitynum)
                                                    (velocity-info :notenum)
                                                    (velocity-info :tracknum))
                       (set-current-velocity-node (velocity-info :velocitynum) (velocity-info :notenum) (velocity-info :tracknum)))
                      
                      (fxnode-info
                       (set-mouse-fx (fxnode-info :fxnum) (fxnode-info :tracknum))
                       (set-indicator-fxnode (fxnode-info :fxnodenum)
                                             (fxnode-info :fxnum)
                                             (fxnode-info :tracknum))
                       (set-current-fxnode  (fxnode-info :fxnodenum)
                                            (fxnode-info :fxnum)
                                            (fxnode-info :tracknum))
                       )

                      (trackwidth-info
                       (set! resize-mouse-pointer-is-set #t)
                       (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer))

                      ((and is-in-fx-area velocity-dist-is-shortest)
                       (set-mouse-note *current-note-num* *current-track-num*))

                      ((and is-in-fx-area fx-dist-is-shortest)
                       (set! *current-fx/distance* fx-dist)
                       (ra:set-statusbar-text (ra:get-fx-name (fx-dist :fx) *current-track-num*)) ;; TODO: Also write fx value at mouse position.
                       (set-mouse-fx (fx-dist :fx) *current-track-num*)
                       )
                      
                      (else
                       #f)))))
         (if (or (not result)
                 (not resize-mouse-pointer-is-set))
             (if (and (> Y (ra:get-block-header-y2))
                      (< Y (ra:get-reltempo-slider-y1)))
                 (ra:set-normal-mouse-pointer)))
         result))




#||
(load "lint.scm")
(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #t)
(define *report-multiply-defined-top-level-functions* #f) ; same name defined at top level in more than one file
(define *report-shadowed-variables* #t)
(define *report-minor-stuff* #t)                          ; let*, docstring checks, (= 1.5 x), numerical and boolean simplification
(lint "/home/kjetil/radium/bin/scheme/mouse/mouse.scm")

(c-display (ra:create-temponode 2.1 -5.0))

(box-to-string (find-temponode 210 1210))

(ra:set-temponode 1 65/2 8.0)
(ra:set-temponode 1 0.01 8.0)
(ra:set-temponode 3 100.01 8.0)

(ra:ctrl-pressed)

(define (mouse-press button x* y*)
  (if (not curr-mouse-cycle)
      (set! curr-mouse-cycle (get-mouse-cycle button x* y*))))

(define (mouse-drag button x* y*)
  (if curr-mouse-cycle
      ((cadr curr-mouse-cycle) button x* y*)))

(define (mouse-release button x* y*)
  (let ((mouse-cycle curr-mouse-cycle))
    (set! curr-mouse-cycle #f)
    (if mouse-cycle
        ((caddr mouse-cycle) button x* y*))))


||#



#||

;; testing backtracing. Haven't been able to get longer backtrace than 1/2, plus that there is only line number for the last place.
(define (d)
  (e))
(define (c)
  (d))
(define (b)
  (c))
(define (a)
  (b))
(a)

(set! (*s7* 'undefined-identifier-warnings) #t)
(*s7* 'undefined-identifier-warnings)

(define (happ2)
  (+ 2 aiai6))

(*s7* 'symbol-table)
(*s7* 'rootlet-size)

(define (get-func)
  (define (get-inner)
    __func__)
  (get-inner))

(<-> "name: " (symbol->string (get-func)) )




(begin *stacktrace*)
(a)
(stacktrace)
(set! (*s7* 'maximum-stack-size) 3134251345)
(set! (*s7* 'max-frames) 30)
(*s7*)

(let ((x 1))
  (catch #t
	 (lambda ()
           (set! (*s7* 'stack-size) 50)
	   (let ((y 2))
             (a)))
	 (lambda args
           (c-display "stacktrace" (stacktrace 5) "2")
           (c-display "args" args)
           (c-display "owlet" (owlet))
           (with-let (owlet)
                     (c-display "stack-top" (pretty-print (*s7* 'stack))))
           )))
(load "/home/kjetil/radium3.0/bin/scheme/mouse/bug.scm")

(ra:move-mouse-pointer 50 50)
||#


(provide 'mouse.scm)

(define-constant *left-button* 1) ;; TR_LEFTMOUSE
(define-constant *middle-button* 3) ;; TR_MIDDLEMOUSE
(define-constant *right-button* 5) ;; TR_RIGHTMOUSE

(define-constant *is-pressing* 1) ;; API_MOUSE_PRESSING
(define-constant *is-moving* 2) ;; API_MOUSE_MOVING
(define-constant *is-releasing* 3)  ;; API_MOUSE_RELEASING
(define-constant *is-leaving* 4)  ;; API_MOUSE_LEAVING (mouse has left the qt widget)

(define (select-button Button)
  (= *left-button* Button))

(define (left-or-right-button Button)
  (or (= *left-button* Button)
      (= *right-button* Button)))


(define *last-statusbar-id* -1)

(define (set-editor-statusbar text)
  (set! *last-statusbar-id* (<ra> :set-statusbar-text text)))

(define (set-statusbar-value val)
  (set-editor-statusbar (<-> val)))

(define (set-velocity-statusbar-text value)
  (set-editor-statusbar (<-> "Velocity: " (one-decimal-percentage-string value) "%")))


;; Quantitize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quantitize Place Q)
  (* (roundup (/ Place Q))
     Q))

#||
(quantitize 18341/2134 1/3)

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

#||
;; Todo: use this one instead of the 'get-distance' function below, since this one is much faster. (function originally created for ab, but not used)
(define (distance-to-vector x y vector)
  (define point  (create-point x y))
  (define point1 (vector-point1 vector))
  (define point2 (vector-point2 vector))
  
  (define x1 (point-x point1))
  (define x2 (point-x point2))
  (define y1 (point-y point1))
  (define y2 (point-y point2))
  
  (define distance12 (distance point1 point2))
  (define distance1  (distance point point1))
  (define distance2  (distance point point2))
  
  (if (= 0 distance12)
      distance1
      (begin
        (define distance   (/ (abs (+ (* x  (- y2 y1))
                                      (* (- y) (- x2 x1))
                                      (* y1 x2)
                                      (* (- x1) y2)))
                              distance12))

        (min distance distance1 distance2))))


(define (distance point1 point2)
  (sqrt (+ (square (- (point-x point1)
                      (point-x point2)))
           (square (- (point-y point1)
                      (point-y point2))))))
(define point-x car)
(define point-y cadr)
(define create-point list)
(define vector-point1 car)
(define vector-point2 cadr)
(define (get-distance2 x y x1 y1 x2 y2)
  (distance-to-vector x y (list (create-point x1 y1) (create-point x2 y2))))

||#


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

(define (get-distance-vertical x y x1 y1 x2 y2 logtype)
  (if (= logtype *logtype-hold*)
      (min (get-distance y x y1 x1 y1 x2)
           (get-distance y x y1 x2 y2 x2))
      (get-distance y x y1 x1 y2 x2)))

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


#||
(define (get-quantitized-place-from-y Button Y)
  (define place (<ra> :get-place-from-y Y))
  (quantitize place (<ra> :get-quantitize)))
||#

(define (get-place-from-y Button Y)
  (if (<ra> :control-pressed)
      (<ra> :get-place-from-y Y)
      (<ra> :get-place-in-grid-from-y Y)))


(define (get-next-place-from-y Button Y)
  (if (<ra> :control-pressed)
      (<ra> :get-place-from-y (+ Y 1))
      (<ra> :get-next-place-in-grid-from-y Y)))


;; Mouse move handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define2 *mouse-move-handlers* list? '())

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
  :drag-func #f
  :release-func #f
  )

(define2 *mouse-cycles* list? '())
(define2 *current-mouse-cycle* (curry-or not hash-table?) #f)

(define (add-mouse-cycle $mouse-cycle)
  (push-back! *mouse-cycles*
              $mouse-cycle))

(define (get-mouse-cycle $button $x $y)
  (find-if (lambda (cycle)
             ((cycle :press-func) $button $x $y))
           *mouse-cycles*))

(define *check-mouse-shift-key* #t)
(define (only-y-direction)
  (and *check-mouse-shift-key*
       (<ra> :shift-pressed)))

(define (only-x-direction)
  (<ra> :control2-pressed))

(delafina (add-delta-mouse-handler :press :move-and-release :release #f :mouse-pointer-is-hidden-func #f)
  (define prev-x #f)
  (define prev-y #f)
  (define instance #f)

  (define next-mouse-x-set-time 0)
  (define next-mouse-x #f)
  (define next-mouse-y #f)
  
  (define (call-move-and-release $button $x $y)
    ;;(set! $x (<ra> :get-mouse-pointer-x))
    ;;(set! $y (<ra> :get-mouse-pointer-y))

    (define mouse-pointer-is-hidden (and mouse-pointer-is-hidden-func
                                         (mouse-pointer-is-hidden-func)))
    
    ;; Delta-movements are not always exactly pixel-accurate (especially if user has not disabled acceleration), so we only use it if the pointer is hidden.
    (define use-delta (and mouse-pointer-is-hidden
                           (<ra> :has-delta-mouse)))

    ;;(c-display "use-delta:" use-delta "hidden:" mouse-pointer-is-hidden "has-delta:" (<ra> :has-delta-mouse) "can-move-pointer:" (<ra> :can-move-pointer))
    
    ;;(c-display "call-move-and-release" $x $y ", next-mouse-x:" next-mouse-x)
    ;; Ignore all $x and $y values that was already queued when we sat a new mouse pointer position. (needed in qt5)
    (when next-mouse-x
      (if (or (> (- (time) next-mouse-x-set-time) 0.1) ;; We give up after 0.1 seconds.
              (and (< (abs (- $x next-mouse-x)) 100)  ;; Need some buffer, unfortunately we don't always get the same mouse event back when calling (<ra> :move-mouse-pointer).
                   (< (abs (- $y next-mouse-y)) 100))) ;; same here.
          (begin
            (set! prev-x next-mouse-x)
            (set! prev-y next-mouse-y)
            (set! next-mouse-x #f))
          (begin            
            (set! $x prev-x)
            (set! $y prev-y))))

    (define raw-dx (if use-delta
                       (<ra> :get-delta-mouse-x)
                       (- $x prev-x)))
    (define raw-dy (if use-delta
                       (<ra> :get-delta-mouse-y)
                       (- $y prev-y)))
      
    (if (and (not (= 0 raw-dx))
             (not (= 0 raw-dy))
             (not instance))
        instance
        (begin
          (define dx (cond ((only-y-direction)
                            0)
                           ((<ra> :control-pressed)
                            (/ raw-dx
                               10))
                           (else
                            raw-dx)))
          (define dy (cond ((only-x-direction)
                            0)
                           ((<ra> :control-pressed)
                            (/ raw-dy
                               10))
                           (else
                            raw-dy)))

          ;;(c-display "               $x:" $x ", prev-x:" prev-x)
          
          (set! prev-x $x)
          (set! prev-y $y)
          
          ;; dirty tricks to avoid the screen edges
          ;;
          (when (<ra> :can-move-pointer)
            
            (define (set-mouse x y)
              ;;(c-display " ... SET MOUSE " x y)
              (<ra> :move-mouse-pointer x y)
              (set! next-mouse-x-set-time (time))
              (set! next-mouse-x x)
              (set! next-mouse-y y))
            
            (define limit (if mouse-pointer-is-hidden
                              100
                              0))

            (define limit-x2 (if mouse-pointer-is-hidden
                                 100
                                 20))

            (let ((x (<ra> :get-mouse-pointer-x))
                  (y (<ra> :get-mouse-pointer-y))
                  (x2 (- (<ra> :get-seqnav-x2) 1))
                  (y2 (- (<ra> :get-seqnav-y2) 1)))
              
              (define new-x x)
              (define new-y y)

              (if (<= x limit)
                  (set! new-x limit)
                  (if (>= x (- x2 limit-x2))
                      (set! new-x (- x2 limit-x2))))
              
              (if (<= y limit)
                  (set! new-y limit)
                  (if (>= y (- y2 limit))
                      (set! new-y (- y2 limit))))

              (if (or (not (= new-x x))
                      (not (= new-y y)))
                  (set-mouse new-x new-y))))
              

          (set! instance (move-and-release $button
                                        dx
                                        dy
                                        instance)))))
  
  (add-mouse-cycle (make-mouse-cycle
                    :press-func (lambda ($button $x $y)
                                  (set! instance (press $button $x $y))
                                  (if instance
                                      (begin
                                        (set! prev-x $x)
                                        (set! prev-y $y)
                                        (<ra> :get-delta-mouse-x) ;; reset delta
                                        (<ra> :get-delta-mouse-y) ;; reset delta
                                        #t)
                                      #f))
                    :drag-func  call-move-and-release
                    :release-func (lambda ($button $x $y)
                                    (call-move-and-release $button $x $y)
                                    (if release
                                        (release $button $x $y instance))
                                    (set! prev-x #f)
                                    (set! prev-y #f)))))


;; Functions called from radium
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define2 mouse-fx-has-been-set boolean? #f)
(define (set-mouse-fx fxnum tracknum)
  (set! mouse-fx-has-been-set #t)
  (<ra> :set-mouse-fx fxnum tracknum))

(define2 mouse-track-has-been-set boolean? #f)
(define (set-mouse-track tracknum)
  (set! mouse-track-has-been-set #t)
  ;;(c-display "set mouse track to" tracknum)
  (<ra> :set-mouse-track tracknum))
(define (set-mouse-track-to-reltempo)
  (set! mouse-track-has-been-set #t)
  ;;(c-display "set mouse track to reltempo")
  (<ra> :set-mouse-track-to-reltempo)
  )

(define2 mouse-note-has-been-set boolean? #f)
(define (set-mouse-note notenum tracknum)
  (set! mouse-note-has-been-set #t)
  (<ra> :set-mouse-note notenum tracknum))

(define2 indicator-node-has-been-set boolean? #f)
(define (set-indicator-temponode num)
  (set! indicator-node-has-been-set #t)
  (<ra> :set-indicator-temponode num))
(define (set-indicator-pitchnum num tracknum)
  (set! indicator-node-has-been-set #t)
  (<ra> :set-indicator-pitchnum num tracknum))
(define (set-indicator-velocity-node velocitynum notenum tracknum)
  (set! indicator-node-has-been-set #t)
  (<ra> :set-indicator-velocity-node velocitynum notenum tracknum))
(define (set-indicator-fxnode fxnodenum notenum tracknum)
  (set! indicator-node-has-been-set #t)
  (<ra> :set-indicator-fxnode fxnodenum notenum tracknum))

(define2 current-node-has-been-set boolean? #f)
(define (set-current-temponode num)
  (set! current-node-has-been-set #t)
  (<ra> :set-current-temponode num)
  )
(define (set-current-velocity-node velnum notenum tracknum)
  (set! current-node-has-been-set #t)
  (set-velocity-statusbar-text (<ra> :get-velocity-value velnum notenum tracknum))
  (<ra> :set-current-velocity-node velnum notenum tracknum))
(define (set-current-fxnode fxnodenum fxnum tracknum)
  (set! current-node-has-been-set #t)
  (set-editor-statusbar (<ra> :get-fx-string fxnodenum fxnum tracknum))
  (<ra> :set-current-fxnode fxnodenum fxnum tracknum))
(define (set-current-pitchnum pitchnum tracknum)
  (set! current-node-has-been-set #t)
  (<ra> :set-current-pitchnum pitchnum tracknum)
  (set-editor-statusbar (<-> "Pitch: " (two-decimal-string (<ra> :get-pitchnum-value pitchnum tracknum)))))

(define2 current-pianonote-has-been-set boolean? #f)
(define (set-current-pianonote pianonotenum notenum tracknum)
  (set! current-pianonote-has-been-set #t)
  (<ra> :set-current-pianonote pianonotenum notenum tracknum))
;;  (set-editor-statusbar (<-> "Pitch: " (two-decimal-string (<ra> :get-pitchnum-value pianonotenum tracknum)))))

(define2 mouse-pointer-has-been-set boolean? #f)
(define2 mouse-pointer-guinum number? -4)
(define (set-mouse-pointer func guinum)
  ;;(c-display "  setting mouse func to" func)
  (set! mouse-pointer-has-been-set #t)
  (set! mouse-pointer-guinum guinum)
  ;;(c-display "  setting mouse func to" func)
  (func guinum)
  )

;; TODO: block->is_dirty is set unnecessarily often to true this way.
(define (cancel-current-stuff)
  (<ra> :set-no-mouse-fx)
  (<ra> :set-no-mouse-note)
  (<ra> :set-no-mouse-track)
  (<ra> :cancel-current-node)
  (<ra> :cancel-current-pianonote)
  (<ra> :cancel-indicator-node)
  )


(define (handling-nodes thunk)
  (set! mouse-fx-has-been-set #f)
  (set! mouse-track-has-been-set #f)
  (set! mouse-note-has-been-set #f)
  (set! indicator-node-has-been-set #f)
  (set! current-node-has-been-set #f)
  (set! current-pianonote-has-been-set #f)
  (set! mouse-pointer-has-been-set #f)
  
  ;;(set-editor-statusbar "")
  ;;(c-display "   LAST_ID:" *last-statusbar-id*)
  (<ra> :remove-statusbar-text *last-statusbar-id*)
  
  (try-finally :try thunk
               :failure (lambda ()
                          
                          (set! *current-mouse-cycle* #f)
                          (set! *check-mouse-shift-key* #t)

                          (when *current-seqblock-info*
                            (c-display "\n\n\n  ************** CANCELLING **********\n\n\n\n")
                            (<ra> :cancel-gfx-seqblocks (*current-seqblock-info* :seqtracknum))
                            (delete-all-gfx-gfx-seqblocks)
                            (set! *current-seqblock-info* #f))
                          
                          ;; We also used to rethrow the error here. Don't know why. Seems like the wrong thing to do.
                          
                          #f
                          )
               :finally (lambda ()
                          (if (not mouse-fx-has-been-set)
                              (<ra> :set-no-mouse-fx))
                          
                          (if (not mouse-track-has-been-set)
                              (<ra> :set-no-mouse-track))
                          
                          (if (not mouse-note-has-been-set)
                              (<ra> :set-no-mouse-note))
                          
                          (if (not indicator-node-has-been-set)
                              (<ra> :cancel-indicator-node))
                          
                          (if (not current-node-has-been-set)
                              (<ra> :cancel-current-node))
                          
                          (if (not current-pianonote-has-been-set)
                              (<ra> :cancel-current-pianonote))
                          ;;(if (not mouse-pointer-has-been-set)
                          ;;    (<ra> :set-normal-mouse-pointer))
                          )))


(define (radium-mouse-press $button $x $y)  
  (handling-nodes
   (lambda()
     ;;(c-display "%%%%%%%%%%%%%%%%% >> mouse press" $button $x $y *current-mouse-cycle*)
     ;;(cancel-current-stuff)
     (if (not *current-mouse-cycle*)
         (let ((new-mouse-cycle (get-mouse-cycle $button $x $y)))
           (if (and new-mouse-cycle
                    (new-mouse-cycle :drag-func))
               (set! *current-mouse-cycle* new-mouse-cycle))))
     (get-bool *current-mouse-cycle*))))

(define (radium-mouse-move $button $x $y)
  ;;(c-display "X:" $x ". seq_x1/x2:" (<ra> :get-sequencer-x1) (<ra> :get-sequencer-x2))
  (handling-nodes
   (lambda()
     ;;(c-display "mouse move2" $button $x $y (<ra> :control-pressed) (<ra> :shift-pressed))
     ;;(cancel-current-stuff)
     (if *current-mouse-cycle*
         (begin
           ;;(c-display "           1. Running current mouse cycle" $x $y)
           ((*current-mouse-cycle* :drag-func) $button $x $y)
           #t)
         (begin
           ;;(c-display "           2. Mouse-move-handlers        " $x $y)
           (run-mouse-move-handlers $button $x $y)
           #f)))))

(define (radium-mouse-release $button $x $y)
  (handling-nodes
   (lambda()
     ;;(c-display "%%%%%%%%%%%%%%%%% << mouse release" $button $x $y)
     (if *current-mouse-cycle*
         (begin
           ((*current-mouse-cycle* :release-func) $button $x $y)
           (set! *current-mouse-cycle* #f)
           (run-mouse-move-handlers $button $x $y)
           (cancel-current-stuff)
           ;;(<ra> :set-normal-mouse-pointer)
           (set! *check-mouse-shift-key* #t)
           (set-grid-type #f)
           #t)
         (begin
           (set! *check-mouse-shift-key* #t)
           #f)))))


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
                                                   (<ra> :get-track-x2 (1+ Num))
                                                   (<ra> :get-track-x1 (+ 2 Num)))
                                               Num-tracks))

(define-match get-track-num
  X Y :> (let ((Num-tracks (<ra> :get-num-tracks)))
           (get-track-num-0 X Y (<ra> :get-leftmost-track-num)
                            (<ra> :get-track-x1 (<ra> :get-leftmost-track-num))
                            (<ra> :get-track-x2 (<ra> :get-leftmost-track-num))
                            Num-tracks)))
                                                   
  
#||
(get-track-num 650 50)
||#

(define2 *current-track-num-all-tracks* (curry-or not integer?) #f) ;; Includes the time tracks, linenumbers, and so forth. (see nsmtracker.h)
(define2 *current-track-num* (curry-or not integer?) #f)

(define (set-current-track-num! X Y)
  (if (>= Y (<ra> :get-editor-y2))
      (begin
        (set! *current-track-num* #f)
        (set! *current-track-num-all-tracks* #f))
      (begin
        (define track-num (get-track-num X Y))
        ;;(c-display "track-num:" track-num)
        (set! *current-track-num-all-tracks* track-num)
        (if (and track-num
                 (>= track-num 0))
            (set! *current-track-num* track-num)
            (set! *current-track-num* #f))
        ;;(c-display "track-num:" track-num *current-track-num*)
        (cond (*current-track-num*
               (set-mouse-track *current-track-num*))
              ((and (<ra> :reltempo-track-visible)
                    *current-track-num-all-tracks*
                    (= *current-track-num-all-tracks* (<ra> :get-rel-tempo-track-num)))
               (set-mouse-track-to-reltempo))))))

;; Set current track and mouse track
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (set-current-track-num! X Y)))

(define2 *current-subtrack-num* (curry-or not integer?) #f)

(define-match get-subtrack-from-x-0
  __ _ Num Num   ________ :> #f  
  X1 X Num Total Tracknum :> (let ((X2 (if (= Num (1- Total))
                                           (<ra> :get-subtrack-x2 Num Tracknum)                                           
                                           (<ra> :get-subtrack-x1 (1+ Num) Tracknum))))
                               (if (and (>= X X1)
                                        (<  X X2))
                                   Num
                                   (get-subtrack-from-x-0 X2
                                                          X
                                                          (1+ Num)
                                                          Total
                                                          Tracknum))))

(define-match get-subtrack-from-x
  X Tracknum :> (get-subtrack-from-x-0 (<ra> :get-subtrack-x1 0 Tracknum)
                                       X
                                       0
                                       (<ra> :get-num-subtracks Tracknum) Tracknum))

(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (set! *current-subtrack-num* (and *current-track-num*
                                           (inside-box (<ra> :get-box track-fx *current-track-num*) $x $y)
                                           (get-subtrack-from-x $x *current-track-num*)))))


;;;;;;;;;;;;;;;;;;;;;;

#||
(<ra> :set-reltempo 0.2)
||#

(define (get-common-node-box $x $y)
  (define width/2 (<ra> :get-half-of-node-width))
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

(delafina (find-node-horizontal :$x
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
                     (< $x (box :x1)))
                'before)
               ((> (box :x1) $x)
                (list 'new-box $num))
               ((= $num (1- $num-nodes))
                'after)
               (else
                (find-node-horizontal $x $y $get-node-box $num-nodes (1+ $num)))))))


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

(define2 *move-existing-node-mouse-cycles* list? '())
(define2 *create-new-node-mouse-cycles* list? '())

(define-match get-cycle-and-node
  ______ _ _ ()             :> #f
  Button X Y (Cycle . Rest) :> (let ((Node ((Cycle :press) Button X Y)))
                                 (if Node
                                     (make-move-node-handler :move (Cycle :move-and-release)
                                                             :release (Cycle :release)
                                                             :node Node)
                                     (get-cycle-and-node Button X Y Rest))))



(define2 *mouse-pointer-is-currently-hidden* boolean? #t)

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
 
 :mouse-pointer-is-hidden-func (lambda () *mouse-pointer-is-currently-hidden*)
 ) 

   
(delafina (add-node-mouse-handler :Get-area-box
                                  :Get-existing-node-info
                                  :Get-min-value
                                  :Get-max-value
                                  :Get-x #f ;; Only used when releasing mouse button
                                  :Get-y #f ;; Only used when releasing mouse button
                                  :Make-undo 
                                  :Create-new-node
                                  :Move-node
                                  :Release-node #f
                                  :Publicize
                                  :Get-pixels-per-value-unit #f
                                  :Create-button #f
                                  :Use-Place #t
                                  :Mouse-pointer-func #f
                                  :Get-guinum (lambda () (<gui> :get-editor-gui))
                                  :Forgiving-box #t
                                  :Check-shift #t
                                  )
  
  (define-struct node
    :node-info
    :value    
    :y)

  (define (press-existing-node Button X Y)
    (set! *check-mouse-shift-key* Check-shift)
    (and (select-button Button)
         (let ((area-box (Get-area-box)))
           ;;(c-display X Y "area-box" (and area-box (box-to-string area-box)) (and area-box (inside-box-forgiving area-box X Y)) (box-to-string (<ra> :get-box reltempo-slider)))
           (and area-box
                (if Forgiving-box
                    (inside-box-forgiving area-box X Y)
                    (inside-box area-box X Y))))
         (Get-existing-node-info X
                                 Y
                                 (lambda (Node-info Value Node-y)
                                   (Make-undo Node-info)
                                   (Publicize Node-info)
                                   (if Mouse-pointer-func
                                       (set! *mouse-pointer-is-currently-hidden* #f)
                                       (set! *mouse-pointer-is-currently-hidden* #t))
                                   (set-mouse-pointer (or Mouse-pointer-func ra:set-blank-mouse-pointer) (Get-guinum))
                                   (make-node :node-info Node-info
                                              :value Value
                                              :y Node-y
                                              )))))

  (define (can-create Button X Y)
    (and (or (and Create-button (= Button Create-button))
             (and (not Create-button) (select-button Button)))
         (let ((area-box (Get-area-box)))
           (and area-box
                (inside-box area-box X Y)))))
    
  (define (press-and-create-new-node Button X Y)
    (set! *check-mouse-shift-key* Check-shift)
    (and (can-create Button X Y)
         (Create-new-node X
                          (if Use-Place
                              (get-place-from-y Button Y)
                              Y)
                          (lambda (Node-info Value)
                            (Publicize Node-info)
                            (if Mouse-pointer-func
                                (set! *mouse-pointer-is-currently-hidden* #f)
                                (set! *mouse-pointer-is-currently-hidden* #t))
                            (set-mouse-pointer (or Mouse-pointer-func ra:set-blank-mouse-pointer) (Get-guinum))
                            (make-node :node-info Node-info
                                       :value Value
                                       :y Y)))))

  (define (move-or-release Button Dx Dy Node)
    (define node-info (Node :node-info))
    (define min-value (and Get-min-value (Get-min-value node-info)))
    (define max-value (and Get-max-value (Get-max-value node-info)))
    (if (or (not min-value)
            (not max-value))
        (assert Get-pixels-per-value-unit))
    (define area-box (Get-area-box))
    (define node-area-width (area-box :width))
    (define pixels-per-value-unit (if Get-pixels-per-value-unit
                                      (Get-pixels-per-value-unit node-info)
                                      (/ node-area-width
                                         (- max-value min-value))))
    (define new-value (let ((try-it (+ (Node :value)
                                       (/ Dx
                                          pixels-per-value-unit))))
                        (if min-value
                            (set! try-it (max min-value try-it)))
                        (if max-value
                            (set! try-it (min max-value try-it)))
                        try-it))

    ;;(c-display "Dy/Y" Dy (Node :y))
    ;;(c-display "num" ($node :num) ($get-num-nodes-func) "value" $dx ($node :value) (node-area :x1) (node-area :x2) ($get-node-value-func ($node :num)))
    (define new-y (if Use-Place
                      (and (not (= 0 Dy))                           
                           (+ (Node :y) Dy))
                      (+ Dy (Node :y))))

    (define same-pos (and (morally-equal? new-y (Node :y))
                          (morally-equal? new-value (Node :value))))
    
    ;;(c-display "Dx:" Dx ", Dy:" Dy ", same-pos:" same-pos "new-y:" new-y ". Place:" (and new-y (get-place-from-y Button new-y)) "(Node :y):" (Node :y) "new-value:" new-value "(Node :value):" (Node :value))

    (if same-pos
        (begin
          (Publicize node-info)
          Node)
        (let ((node-info (Move-node node-info new-value
                                    (if Use-Place
                                        (if new-y
                                            (get-place-from-y Button new-y)
                                            'same-place)
                                        new-y))))
          (Publicize node-info)
          (make-node :node-info node-info
                     :value new-value
                     :y (or new-y (Node :y))))))
  
  (define (move-and-release Button Dx Dy Node)
    (move-or-release Button Dx Dy Node))
  
  (define (release Button Dx Dy Node)
    (define node-info (Node :node-info))
    (if Release-node
        (Release-node node-info))
    (if (and (not Mouse-pointer-func)
             Get-x Get-y)
        (let ((x (Get-x node-info))
              (y (Get-y node-info)))
          (and x y
               (<ra> :move-mouse-pointer x y)))))

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
                                  :Get-x #f
                                  :Get-value
                                  :Make-undo
                                  :Release #f
                                  :Move
                                  :Publicize
                                  :Mouse-pointer-func #f
                                  :Get-guinum (lambda () (<gui> :get-editor-gui)))
 
  (define-struct info
    :handler-data
    :y)

  (add-node-mouse-handler :Get-area-box (lambda () (make-box2 -1000000 -100000 100000 100000))
                          :Get-existing-node-info (lambda (X Y callback)
                                                    ;;(c-display "  horiz: " X Y)
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
                                   (and Get-x
                                        (Get-x (Info :handler-data))))
                          :Get-y (lambda (Info)
                                   (and Get-x
                                        (Info :y)))
                          :Make-undo (lambda (Info)
                                       (Make-undo (Info :handler-data)))
                          :Create-new-node (lambda (Value Place callback)
                                             #f)
                          :Release-node (lambda x
                                          (if Release
                                              (Release)))
                          :Move-node (lambda (Info Value Place)
                                       (Move (Info :handler-data)
                                             Value)
                                       Info)
                          :Publicize (lambda (Info)
                                       (Publicize (Info :handler-data)))
                          :Get-pixels-per-value-unit (lambda (Info)
                                                       (/ (- (Get-x2 (Info :handler-data))
                                                             (Get-x1 (Info :handler-data)))
                                                          (- (Get-max-value (Info :handler-data))
                                                             (Get-min-value (Info :handler-data)))))
                          :Mouse-pointer-func Mouse-pointer-func
                          :Get-guinum Get-guinum
                          :Forgiving-box #f
                          ))
                                  

(delafina (add-vertical-handler :Get-handler-data
                                :Get-y1
                                :Get-y2
                                :Get-min-value
                                :Get-max-value
                                :Get-y #f
                                :Get-value
                                :Make-undo #f
                                :Release #f
                                :Move
                                :Publicize #f
                                :Mouse-pointer-func #f
                                :Get-guinum (lambda () (<gui> :get-editor-gui)))
  
  (define-struct info
    :handler-data
    :x)

  (add-node-mouse-handler :Get-area-box (lambda () (make-box2 -1000000 -100000 100000 100000))
                          :Get-existing-node-info (lambda (X Y callback)
                                                    (c-display "  vertic: " X Y)
                                                    (define handler-data (Get-handler-data X Y))
                                                    (and handler-data
                                                         (let ((info (make-info :handler-data handler-data
                                                                                :x X)))
                                                           (callback info
                                                                     (Get-value handler-data)
                                                                     0))))
                          :Get-min-value (lambda (Info)
                                           (Get-min-value (Info :handler-data)))
                          :Get-max-value (lambda (Info)
                                           (Get-max-value (Info :handler-data)))
                          :Get-x (lambda (Info)
                                   (and Get-y
                                        (Info :y)
                                        (Get-y (Info :handler-data))))
                          :Get-y (lambda (Info)
                                   (and Get-y
                                        (Get-y (Info :handler-data))))
                          :Make-undo (lambda (Info)
                                       (if Make-undo
                                           (Make-undo (Info :handler-data))))
                          :Create-new-node (lambda (Value Place callback)
                                             #f)
                          :Release-node (lambda x
                                          (if Release
                                              (Release)))
                          :Move-node (lambda (Info Value Place)
                                       (Move (Info :handler-data)
                                             Value)
                                       Info)
                          :Publicize (lambda (Info)
                                       (if Publicize
                                           (Publicize (Info :handler-data))))
                          :Get-pixels-per-value-unit (lambda (Info)
                                                       (/ (- (Get-x2 (Info :handler-data))
                                                             (Get-x1 (Info :handler-data)))
                                                          (- (Get-max-value (Info :handler-data))
                                                             (Get-min-value (Info :handler-data)))))
                          :Mouse-pointer-func Mouse-pointer-func
                          :Get-guinum Get-guinum
                          :Forgiving-box #f
                          ))
                                  



;; Update current seqblock info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;             
(define-struct seqblock-info
  :seqtracknum
  :seqblocknum
  :stretch
  :samples-per-pixel)

(define (make-seqblock-info2 seqtracknum seqblocknum)
  (define stretch (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
  (define x1 (<ra> :get-seqblock-x1 seqblocknum seqtracknum))
  (define x2 (<ra> :get-seqblock-x2 seqblocknum seqtracknum))
  (define start-time (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
  (define end-time (<ra> :get-seqblock-end-time seqblocknum seqtracknum))
  (define duration (- end-time start-time))

  (define samples-per-pixel (/ (/ duration
                                  (- x2 x1))
                               stretch))

  (make-seqblock-info :seqtracknum seqtracknum
                      :seqblocknum seqblocknum
                      :stretch stretch
                      :samples-per-pixel samples-per-pixel))
    
(define (get-selected-seqblock-infos)
  (define ret '())
  (for-each-selected-seqblock (lambda (seqtracknum seqblocknum)
                                (push-back! ret (make-seqblock-info2 seqtracknum seqblocknum))))
  ret)

(define (get-seqblock seqtracknum X Y)
  (let loop ((seqblocknums (to-list (<ra> :get-seqblocknum-z-order seqtracknum))))
    (cond ((null? seqblocknums)
           #f)
          ((inside-box (ra:get-box2 seqblock (car seqblocknums) seqtracknum) X Y)
           (make-seqblock-info2 seqtracknum (car seqblocknums)))
          (else
           (loop (cdr seqblocknums))))))

(define (get-seqblock-info X Y)
  (let ((seqtracknum *current-seqtrack-num*))
    (and seqtracknum
         (inside-box (<ra> :get-box sequencer) X Y)
         ;;(begin (c-display "seqtracknum:" seqtracknum X Y (inside-box (ra:get-box2 seqtrack 1) X Y)) #t)
         (get-seqblock seqtracknum X Y))))


(define2 *current-seqblock-info* (curry-or not hash-table?) #f)

(set! FROM_C-call-me-when-num-seqtracks-might-have-changed
      (lambda (new-num-seqtracks)
        (if (and *current-seqtrack-num*
                 (>= *current-seqtrack-num* new-num-seqtracks))
            (set! *current-seqtrack-num* #f))))

;;(define (update-current-seqblock-info!) )
  
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (let ((old *current-seqblock-info*)
               (new (get-seqblock-info X Y)))
           ;;(c-display "old/new seqblock-info" old new)
           (cond ((and old (not new))
                  (<ra> :cancel-curr-seqblock-under-mouse)
                  (set! *current-seqblock-info* #f))
                 ((or (and new (not old))
                      (not (morally-equal? new old)))
                  ;;(c-display "set-normal")
                  ;;(<ra> :set-normal-mouse-pointer)
                  (<ra> :set-curr-seqblock-under-mouse (new :seqblocknum) (new :seqtracknum))                  
                  (set! *current-seqblock-info* new))
                 (else
                  #f)))))


;; status bar and Update mouse pointer shape when moved above various things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-interior-displayable-string value)
  (if (= value 0)
      "0.00s"
      (let ((seconds (/ value
                        (<ra> :get-sample-rate))))
        (if (< seconds 0.01)
            (let* ((ms (* 1000 seconds))                         
                   (sms (two-decimal-string ms)))
              (if (string=? sms "0.00")
                  "0.01s"
                  (<-> sms "ms")))
            (<-> (two-decimal-string seconds) "s")))))


(define (set-left-interior-status-bar2 interior-start)
  (set-editor-statusbar (<-> "----|: " (get-interior-displayable-string interior-start))))

(define (set-left-interior-status-bar seqblocknum seqtracknum)
  ;;(c-display "setting" seqblocknum seqtracknum)
  (set-seqblock-selected-box 5 seqblocknum seqtracknum)
  (set-left-interior-status-bar2 (<ra> :get-seqblock-interior-start seqblocknum seqtracknum)))

(define (set-right-interior-status-bar seqblocknum seqtracknum)
  (set-seqblock-selected-box 6 seqblocknum seqtracknum)

  (set-editor-statusbar (<-> "|----: " (get-interior-displayable-string (- (get-original-seqblock-duration seqblocknum seqtracknum)
                                                                               (<ra> :get-seqblock-interior-end seqblocknum seqtracknum))))))

(define *old-selected-box-seqblocknum* -1)
(define *old-selected-box-seqtracknum* -1)
(define (set-seqblock-selected-box which-one seqblocknum seqtracknum)
  ;;(c-display "   setting " which-one seqblocknum seqtracknum " old: " *old-selected-box-seqblocknum* *old-selected-box-seqtracknum*)
  (when (and (>= *old-selected-box-seqtracknum* 0)
             (< *old-selected-box-seqtracknum* (<ra> :get-num-seqtracks))
             (>= *old-selected-box-seqblocknum* 0)
             (< *old-selected-box-seqblocknum* (<ra> :get-num-seqblocks *old-selected-box-seqtracknum*)))    
    ;;(c-display "UNSETTING")
    (<ra> :set-seqblock-selected-box 0 *old-selected-box-seqblocknum* *old-selected-box-seqtracknum*))
  
  (set! *old-selected-box-seqblocknum* seqblocknum)
  (set! *old-selected-box-seqtracknum* seqtracknum)

  (if (>= seqblocknum 0)
      (<ra> :set-seqblock-selected-box which-one seqblocknum seqtracknum)))

(define (set-fade-status-bar is-left seqblocknum seqtracknum)
  (define (get-displayable-string value)
    (<-> (if (= value 0)
             "0.00"
             (let* ((ms (* 1000
                           (/ (* value (- (<ra> :get-seqblock-end-time seqblocknum seqtracknum)
                                          (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                              (<ra> :get-sample-rate))))
                    (sms (two-decimal-string ms)))
               (if (string=? sms "0.00")
                   "0.01"
                   sms)))
         "ms"))

  (if is-left
      (begin
        (set-seqblock-selected-box 1 seqblocknum seqtracknum)
        (set-editor-statusbar (<-> "Fade in: " (get-displayable-string (<ra> :get-seqblock-fade-in seqblocknum seqtracknum)))))
      (begin
        (set-seqblock-selected-box 2 seqblocknum seqtracknum)
        (set-editor-statusbar (<-> "Fade out: " (get-displayable-string (<ra> :get-seqblock-fade-out seqblocknum seqtracknum)))))))


(add-mouse-move-handler
 :move (lambda ($button X Y)
         ;;(c-display X Y (box-to-string (get-seqnav-move-box)))
         (cond ((and *current-track-num*
                     (inside-box (<ra> :get-box track-pan-slider *current-track-num*) X Y))
                (set-mouse-pointer ra:set-horizontal-split-mouse-pointer (<gui> :get-editor-gui))
                (show-track-pan-in-statusbar *current-track-num*))
               
               ((and *current-track-num*
                     (inside-box (<ra> :get-box track-volume-slider *current-track-num*) X Y))
                (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer (<gui> :get-editor-gui))
                (show-track-volume-in-statusbar *current-track-num*))
               
               ((inside-box (<ra> :get-box reltempo-slider) X Y)
                (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer (<gui> :get-editor-gui))
                (show-reltempo-in-statusbar))
               
               ((and *current-track-num*
                     (inside-box (<ra> :get-box track-pan-on-off *current-track-num*) X Y))
                (set-mouse-pointer ra:set-pointing-mouse-pointer (<gui> :get-editor-gui))
                (set-editor-statusbar (<-> "Track panning slider " (if (<ra> :get-track-pan-on-off *current-track-num*) "on" "off"))))
               
               ((and *current-track-num*
                     (inside-box (<ra> :get-box track-volume-on-off *current-track-num*) X Y))
                (set-mouse-pointer ra:set-pointing-mouse-pointer (<gui> :get-editor-gui))
                (set-editor-statusbar (<-> "Track volume slider " (if (<ra> :get-track-volume-on-off *current-track-num*) "on" "off"))))

               ((and *current-track-num*
                     (< Y (<ra> :get-track-pan-on-off-y1)))
                (set-mouse-pointer ra:set-pointing-mouse-pointer (<gui> :get-editor-gui))
                (set-editor-statusbar (<-> "Select instrument for track " *current-track-num*)))

               ((and (inside-box (<ra> :get-box sequencer) X Y)
                     (not *current-seqautomation/distance*))
                (if (not *current-seqblock-info*)
                    (set-seqblock-selected-box 0 -1 -1))
                (cond (*current-seqblock-info*
                       (define seqblock-info *current-seqblock-info*)
                       (define seqtracknum (seqblock-info :seqtracknum))
                       (define seqblocknum (seqblock-info :seqblocknum))
                       (define holds-block (<ra> :seqblock-holds-block seqblocknum seqtracknum))
                       (define holds-sample (not holds-block))
                       (cond ((inside-box (<ra> :get-box seqblock-left-fade seqblocknum seqtracknum) X Y)
                              (set-fade-status-bar #t seqblocknum seqtracknum)
                              (ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))
                             
                             ((inside-box (<ra> :get-box seqblock-right-fade seqblocknum seqtracknum) X Y)
                              (set-fade-status-bar #f seqblocknum seqtracknum)
                              (ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))

                             ((inside-box (<ra> :get-box seqblock-right-stretch seqblocknum seqtracknum) X Y)
                              (set-editor-statusbar (<-> "Stretch: " (two-decimal-string (<ra> :get-seqblock-stretch seqblocknum seqtracknum))))
                              (set-seqblock-selected-box 4 seqblocknum seqtracknum)
                              (ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))
                             
                             ((inside-box (<ra> :get-box seqblock-left-stretch seqblocknum seqtracknum) X Y)
                              (set-editor-statusbar (<-> "Stretch: " (two-decimal-string (<ra> :get-seqblock-stretch seqblocknum seqtracknum))))
                              (set-seqblock-selected-box 3 seqblocknum seqtracknum)
                              (ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))

                             ((and holds-sample
                                   (inside-box (<ra> :get-box seqblock-left-interior seqblocknum seqtracknum) X Y))
                              (set-left-interior-status-bar seqblocknum seqtracknum)
                              (ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))
                             
                             ((and holds-sample
                                   (inside-box (<ra> :get-box seqblock-right-interior seqblocknum seqtracknum) X Y))
                              (set-right-interior-status-bar seqblocknum seqtracknum)
                              (ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))
                             
                             (else                              
                              (set-editor-statusbar (two-decimal-string (/ (<ra> :get-seqblock-start-time seqblocknum seqtracknum)
                                                                               (<ra> :get-sample-rate))))
                              (set-seqblock-selected-box 0 seqblocknum seqtracknum)
                              ;;(c-display "setting open hand")
                              (set-mouse-pointer ra:set-open-hand-mouse-pointer (<gui> :get-sequencer-gui))))
                       ;;(c-display "hepp")
                       )
                      ((inside-box (get-seqnav-move-box) X Y)
                       (set-mouse-pointer ra:set-open-hand-mouse-pointer (<gui> :get-sequencer-gui))
                       )
                      ((inside-box (<ra> :get-box seqnav-left-size-handle) X Y)
                       (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))
                      ((inside-box (<ra> :get-box seqnav-right-size-handle) X Y)
                       (set-mouse-pointer ra:set-horizontal-resize-mouse-pointer (<gui> :get-sequencer-gui)))
                      (else
                       ;;(c-display "normal1" *current-seqblock-info*)
                       (<ra> :set-normal-mouse-pointer (<gui> :get-sequencer-gui)))))
               
               ((inside-box (<ra> :get-box track-slider) X Y)
                (<ra> :set-open-hand-mouse-pointer (<gui> :get-editor-gui)))
               
               ((inside-box (<ra> :get-box editor-scrollbar) X Y)
                (<ra> :set-open-hand-mouse-pointer (<gui> :get-editor-gui)))
               
               ((not *current-track-num*)
                (set-mouse-pointer ra:set-pointing-mouse-pointer (<gui> :get-editor-gui)))

               (else
                ;;(<ra> :set-normal-mouse-pointer)
                ))))



;; block tempo multiplier slider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-reltempo-in-statusbar)
  (set-editor-statusbar (<-> "Block tempo multiplied by " (two-decimal-string (<ra> :get-reltempo)))))


(define (get-BPMs)
  (map (lambda (bpmnum)
         (list (<ra> :get-bpm-place bpmnum)
               (<ra> :get-bpm bpmnum)))
       (iota (<ra> :num-bpms))))

#!!
(get-BPMs)
!!#

(define (get-temponodes)
  (map (lambda (num)
         (list (<ra> :get-temponode-place num)
               (<ra> :get-temponode-value num)))
       (iota (<ra> :get-num-temponodes))))
#!!
(get-temponodes)
!!#


(define (reset-tempo-multiplier)
  (<ra> :undo-reltempo)
  (<ra> :set-reltempo 1.0))

(define (apply-tempo-multiplier-to-block)
  (undo-block
   (lambda ()
     (let* ((reltempo (<ra> :get-reltempo))
            (bpms (get-BPMs))
            (scale-bpm (lambda (bpm)
                         (round (* reltempo bpm)))))
       (for-each (lambda (place-and-bpm)
                   (let ((place (car place-and-bpm))
                         (bpm (cadr place-and-bpm)))
                     (<ra> :add-bpm (scale-bpm bpm) place)))
                 bpms)
       (if (or (null? bpms)
               (> (car (car bpms)) 0))
           (<ra> :add-bpm (scale-bpm (<ra> :get-main-bpm)) 0))
       (reset-tempo-multiplier)))))

(define (apply-bpm-glide bpmnum)
  (undo-block
   (lambda ()
     (define bpms (get-BPMs))
     (define bpm1 (list-ref bpms bpmnum))
     (define bpm2 (list-ref bpms (1+ bpmnum)))
     (define temponodes (get-temponodes))
     (set! temponodes (nodelist-add-same-value-at-place (car bpm1) 0))
     (set! temponodes (nodelist-add-same-value-at-place (car bpm2) 0))
     (set! temponodes (nodelist-mix temponodes (list (create-node (car bpm1) 0)
                                                     (create-node (-line (car bpm2)) 2)))) ;; Fix 2.
     )))


(define (get-reltemposlider-x)
  (define box (<ra> :get-box reltempo-slider))
  (scale (<ra> :get-reltempo)
         (<ra> :get-min-reltempo)
         (<ra> :get-max-reltempo)
         (box :x1)
         (box :x2)))

;; slider
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (define box (<ra> :get-box reltempo-slider))
                                            (and (inside-box box X Y)
                                                 (<ra> :get-reltempo)))
                        :Get-x1 (lambda (_)
                                  (<ra> :get-reltempo-slider-x1))
                        :Get-x2 (lambda (_)
                                  (<ra> :get-reltempo-slider-x2))
                        :Get-min-value (lambda (_)
                                         (<ra> :get-min-reltempo))
                        :Get-max-value (lambda (_)
                                         (<ra> :get-max-reltempo))
                        :Get-x (lambda (_)
                                 (get-reltemposlider-x))
                        :Get-value (lambda (Value)
                                     Value)
                        :Make-undo (lambda (_)
                                     (<ra> :undo-reltempo))
                        :Move (lambda (_ Value)
                                ;;(c-display "Value:" Value)
                                (<ra> :set-reltempo Value))
                        :Publicize (lambda (_)
                                     (show-reltempo-in-statusbar))
                        )

;; reset slider value
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)                                
                                (if (and (= Button *right-button*)
                                         (inside-box (<ra> :get-box reltempo-slider) X Y))
                                    (begin
                                      (popup-menu "Reset" reset-tempo-multiplier
                                                  "Apply tempo" apply-tempo-multiplier-to-block
                                                  (list
                                                   "Add MIDI learn"
                                                   :enabled (not (<ra> :has-block-multiplier-midi-learn))
                                                   ra:add-block-multiplier-midi-learn)
                                                  (list
                                                   "Remove MIDI learn"
                                                   :enabled (<ra> :has-block-multiplier-midi-learn)
                                                   ra:remove-block-multiplier-midi-learn))
                                      #t)
                                    #f))))

;; track slider
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (<ra> :get-box track-slider))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  ;;(c-display "hep" X Y (box-to-string (<ra> :get-box editor-scrollbar-scroller)) (inside-box (<ra> :get-box editor-scrollbar-scroller) X Y))
                                                  (<ra> :set-track-slider-is-moving #t)
                                                  (callback (<ra> :get-track-slider-pos)
                                                            (<ra> :get-track-slider-pos)
                                                            0))
                        :Get-min-value (lambda (_) 0)
                        :Get-max-value (lambda (_) 1)
                                        ;:Get-x (lambda (Num) (average (<ra> :get-editor-scrollbar-scroller-x1) (<ra> :get-editor-scrollbar-scroller-x2)))
                                        ;:Get-y (lambda (Num) (average (<ra> :get-editor-scrollbar-scroller-y1) (<ra> :get-editor-scrollbar-scroller-y2)))
                        :Make-undo (lambda (_) 50)
                        :Create-new-node (lambda (X Place callback)
                                           #f)
                        :Move-node (lambda (Pos Value Y)
                                     (define newpos (scale Value
                                                           0 1
                                                           0 1))
                                     ;;(c-display "Pos/Value/Y/newpos" Pos Value Y newpos)
                                     (<ra> :set-track-slider-pos newpos)
                                     (<ra> :get-track-slider-pos)
                                     )
                        :Release-node (lambda (Num)
                                        (<ra> :set-track-slider-is-moving #f)
                                        #f
                                        )
                        :Publicize (lambda (Num)
                                     #f)
                        :Use-Place #f
                        :Mouse-pointer-func ra:set-closed-hand-mouse-pointer
                        :Get-pixels-per-value-unit #f ;;(lambda (_)
                        :Forgiving-box #f
                        ;;1)
                        )

;;(<ra> :set-track-slider-pos 0)
;;(<ra> :set-track-slider-pos -100)

;; Editor scrollbar
;;
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (<ra> :get-box editor-scrollbar))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  ;;(c-display "hep" X Y (box-to-string (<ra> :get-box editor-scrollbar-scroller)) (inside-box (<ra> :get-box editor-scrollbar-scroller) X Y))
                                                  (<ra> :set-editor-scrollbar-is-moving #t)
                                                  (callback (<ra> :get-curr-realline)
                                                            (<ra> :get-curr-realline)
                                                            0))
                        :Get-min-value (lambda (_) 0)
                        :Get-max-value (lambda (_) 1)
                        ;:Get-x (lambda (Num) (average (<ra> :get-editor-scrollbar-scroller-x1) (<ra> :get-editor-scrollbar-scroller-x2)))
                                        ;:Get-y (lambda (Num) (average (<ra> :get-editor-scrollbar-scroller-y1) (<ra> :get-editor-scrollbar-scroller-y2)))
                        :Make-undo (lambda (_) 50)
                        :Create-new-node (lambda (X Place callback)
                                           #f)
                        :Move-node (lambda (Num Value Y)
                                     (define scroller-height (- (<ra> :get-editor-scrollbar-scroller-y2) (<ra> :get-editor-scrollbar-scroller-y1)))
                                     (define scrollbar-height (- (<ra> :get-editor-scrollbar-y2) (<ra> :get-editor-scrollbar-y1)))
                                     (define num-reallines (<ra> :get-num-reallines))
                                     (define drealline (scale Y 0 (- scrollbar-height scroller-height) 0 num-reallines))
                                     (define new-realline (between 0
                                                                   (+ Num
                                                                      (to-integer drealline))
                                                                   ;;(to-integer (/ Y 5)))
                                                                   (1- (<ra> :get-num-reallines))))
                                     (<ra> :set-curr-realline new-realline)
                                     Num
                                     )
                        :Release-node (lambda (Num)
                                        (<ra> :set-editor-scrollbar-is-moving #f))
                        :Publicize (lambda (Num)
                                     #f)
                        :Use-Place #f
                        :Mouse-pointer-func ra:set-closed-hand-mouse-pointer
                        :Get-pixels-per-value-unit #f ;;(lambda (_)
                        ;;1)
                        :Forgiving-box #f
                        )                        


(define (show-instrument-color-dialog parentgui . instrument-ids)
  (<ra> :color-dialog (<ra> :get-instrument-color (car instrument-ids)) parentgui
        (lambda (color)
          (for-each (lambda (instrument-id)
                      (<ra> :set-instrument-color color instrument-id))
                    instrument-ids))))

(define (swingtext-popup-elements)
  (list (list "Swing text" :check (<ra> :swingtext-visible *current-track-num*) (lambda (onoff)
                                                                                  (<ra> :show-swingtext onoff *current-track-num*)))
        (list "Swing help" (lambda ()
                             (<ra> :show-swing-help-window)))))

(define (centtext-popup-elements)
  (list (list "Cents text"
              :check (<ra> :centtext-visible *current-track-num*)
              :enabled (<ra> :centtext-can-be-turned-off *current-track-num*)
              (lambda (onoff)
                (<ra> :show-centtext  onoff *current-track-num*)))
        '()))

(define (chancetext-popup-elements)
  (list (list "Chance text" :check (<ra> :chancetext-visible *current-track-num*) (lambda (onoff)
                                                                                    (<ra> :show-chancetext onoff *current-track-num*)))
        (list "Help Chance text" (lambda ()
                                   (<ra> :show-chance-help-window)))))

(define (velocitytext-popup-elements)
  (list (list "Velocity text (left alt + y)" :check (<ra> :veltext-visible *current-track-num*) (lambda (onoff)
                                                                                                  (<ra> :show-veltext onoff *current-track-num*)))
        (list "Help Velocity text" (lambda ()
                                     (<ra> :show-velocity-help-window)))))
        
(define (fxtext-popup-elements)
  (list (list "FX text" :check (<ra> :fxtext-visible *current-track-num*)   (lambda (onoff)
                                                                              (<ra> :show-fxtext onoff *current-track-num*)))
        (list "Help FX text" (lambda ()
                               (<ra> :show-fx-help-window)))))

        
(define (request-midi-channel now callback)
  (define ready #f)
  (define main-layout (<gui> :vertical-layout))

  (for-each (lambda (rownum)
              (define layout (<gui> :horizontal-layout))
              (<gui> :add main-layout layout)
              (for-each (lambda (columnnum)
                          (define channelnum (+ (* rownum 4) columnnum))
                          (<gui> :add
                                 layout
                                 (<gui> :radiobutton
                                        (<-> (1+ channelnum))
                                        (= now channelnum)
                                        (lambda (pressed)
                                          (when (and pressed ready)
                                            (<gui> :close main-layout)
                                            ;;(c-display "channelnum:" channelnum)
                                            (callback channelnum))))))
                        (iota 4)))
            (iota 4))
  #||
  (<gui> :set-size layout
         (* (<gui> :get-system-fontheight) 8)
         (* (<gui> :get-system-fontheight) 4))
  (<gui> :minimize-as-much-as-possible layout)
  ||#
  (set! ready #t)
  (<gui> :show main-layout)
  )
#!!
(request-midi-channel 5 c-display)
!!#
;;(<ra> :request-integer "MIDI channel (1-16):" 1 16))

(define (track-configuration-popup-async X Y)
  (c-display "TRACK " *current-track-num*)
  (popup-menu (car (swingtext-popup-elements))
              "Pianoroll     (left alt + p)" :check (<ra> :pianoroll-visible *current-track-num*) (lambda (onoff)
                                                                                                    (<ra> :show-pianoroll onoff *current-track-num*))
              "Note text     (left alt + n)" :check (<ra> :note-track-visible *current-track-num*) (lambda (onoff)
                                                                                                     (<ra> :show-note-track onoff *current-track-num*))
              (car (centtext-popup-elements))
              (car (chancetext-popup-elements))
              (car (velocitytext-popup-elements))
              (car (fxtext-popup-elements))

              "-------"
              "Copy Track     (left alt + c)" (lambda ()
                                                (<ra> :copy-track *current-track-num*))
              "Cut Track      (left alt + x)"     (lambda ()
                                                    (<ra> :cut-track *current-track-num*))
              "Paste Track    (left alt + v)" (lambda ()
                                                (<ra> :paste-track *current-track-num*))
              "-------"
              "Insert Track     (left alt + i)" (lambda ()
                                                  (<ra> :insert-track *current-track-num*)
                                                  (set-current-track-num! X Y))
              "Delete Track     (left alt + r)" (lambda ()
                                                  (<ra> :delete-track *current-track-num*)
                                                  (set-current-track-num! X Y))
              "----------"
              "Load Track (BETA!)" (lambda ()
                                     (<ra> :load-track "" *current-track-num*))
              "Save Track" (lambda ()
                             (<ra> :save-track "" *current-track-num*))       
              "-------"
              "Set Instrument     (F12)" (lambda ()
                                           (select-track-instrument *current-track-num*))
              (let* ((tracknum *current-track-num*)
                     (curr-midi-channel (<ra> :get-track-midi-channel tracknum))
                     (instrument-id (<ra> :get-instrument-for-track tracknum))
                     (is-midi-instrument (and (>= instrument-id 0)
                                              (string=? (<ra> :get-instrument-type-name instrument-id)
                                                        "MIDI"))))
                (list (<-> "Set MIDI channel" (if is-midi-instrument "" (<-> "(now: " (1+ curr-midi-channel) ")")))
                      :enabled (not is-midi-instrument)
                      (lambda ()
                        (c-display "CURETNTE TRSCKN NUM: " tracknum)
                        (request-midi-channel curr-midi-channel
                                              (lambda (channelnum)
                                                (c-display "channelnum2:" channelnum tracknum)
                                                (<ra> :set-track-midi-channel channelnum tracknum))))))
              (let ((instrument-id (<ra> :get-instrument-for-track  *current-track-num*)))
                (list "Configure instrument color"
                      :enabled (>= instrument-id 0)
                      (lambda ()
                        (show-instrument-color-dialog -1 instrument-id))))
              "-------"
              (cadr (swingtext-popup-elements))
              (cadr (centtext-popup-elements))
              (cadr (chancetext-popup-elements))
              (cadr (velocitytext-popup-elements))
              (cadr (fxtext-popup-elements))
              ))

#||        
  (popup-menu "Show/hide Velocity text" (lambda ()
                                          (<ra> :show-hide-veltext *current-track-num*))
              "Show/hide Pianoroll"     (lambda ()
                                          (<ra> :show-hide-pianoroll *current-track-num*))
              "Show/hide Notes"         (lambda ()
                                          (<ra> :show-hide-note-track *current-track-num*))
              ))
||#


;; select instrument for track
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)
                                (cond ((and *current-track-num*
                                            (>= X (<ra> :get-track-x1 0))
                                            (< Y (<ra> :get-track-pan-on-off-y1)))
                                       (if (= Button *right-button*)
                                           (if (<ra> :shift-pressed)
                                               (<ra> :delete-track *current-track-num*)
                                               (track-configuration-popup-async X Y))
                                           (select-track-instrument *current-track-num*))
                                       #t)
                                      (else
                                       #f)))))


;; track pan on/off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)
                                (cond ((and *current-track-num*
                                            (inside-box (<ra> :get-box track-pan-on-off *current-track-num*) X Y))
                                       (<ra> :undo-track-pan *current-track-num*)
                                       (<ra> :set-track-pan-on-off (not (<ra> :get-track-pan-on-off *current-track-num*))
                                                                *current-track-num*)
                                       #t)
                                      (else
                                       #f)))))


;; track volume on/off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)
                                (cond ((and *current-track-num*
                                            (inside-box (<ra> :get-box track-volume-on-off *current-track-num*) X Y))
                                       (<ra> :undo-track-volume *current-track-num*)
                                       (<ra> :set-track-volume-on-off (not (<ra> :get-track-volume-on-off *current-track-num*))
                                                                   *current-track-num*)
                                       #t)
                                      (else
                                       #f)))))




;; track pan sliders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-track-pan-in-statusbar Tracknum)
  (set-editor-statusbar (<-> "Track pan " (two-decimal-string (<ra> :get-track-pan Tracknum)))))

(define (get-trackpan-x Tracknum)
  (scale (<ra> :get-track-pan Tracknum)
         -1
         1
         (<ra> :get-track-pan-slider-x1 Tracknum)
         (<ra> :get-track-pan-slider-x2 Tracknum)))

;; slider
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (and *current-track-num*
                                                 (inside-box (<ra> :get-box track-pan-slider *current-track-num*) X Y)
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
                                ;;(c-display Tracknum Value)
                                (<ra> :set-track-pan Value Tracknum))
                        :Publicize (lambda (Tracknum)
                                     (show-track-pan-in-statusbar Tracknum))
                        )

;; reset slider value
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)
                                (cond ((and *current-track-num*
                                            (inside-box (<ra> :get-box track-pan-slider *current-track-num*) X Y))
                                       (<ra> :undo-track-pan *current-track-num*)
                                       (<ra> :set-track-pan 0.0 *current-track-num*)
                                       #t)
                                      (else
                                       #f)))))

     

;; track volume sliders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-track-volume-in-statusbar Tracknum)
  (set-editor-statusbar (<-> "Track volume " (two-decimal-string (<ra> :get-track-volume Tracknum)))))

(define (get-trackvolume-x Tracknum)
  (scale (<ra> :get-track-volume Tracknum)
         0
         1
         (<ra> :get-track-volume-slider-x1 Tracknum)
         (<ra> :get-track-volume-slider-x2 Tracknum)))

;; slider
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (and *current-track-num*
                                                 (inside-box (<ra> :get-box track-volume-slider *current-track-num*) X Y)
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
                                ;;(c-display Tracknum Value)
                                (<ra> :set-track-volume Value Tracknum))
                        :Publicize (lambda (Tracknum)
                                     (show-track-volume-in-statusbar Tracknum))
                        )


;; reset slider value
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)
                                (cond ((and *current-track-num*
                                            (inside-box (<ra> :get-box track-volume-slider *current-track-num*) X Y))
                                       (<ra> :undo-track-volume *current-track-num*)
                                       (<ra> :set-track-volume 0.8 *current-track-num*)
                                       #t)
                                      (else
                                       #f)))))

     

;; temponodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (if (inside-box (<ra> :get-box temponode-area) $x $y)
             (c-display "inside" $x $y))))
||#

(define (show-temponode-in-statusbar value)
  (define actual-value (if (< value 0) ;; see reltempo.c
                           (/ 1
                              (- 1 value))
                           (1+ value)))
  (set-editor-statusbar (<-> "Tempo multiplied by " (two-decimal-string actual-value))))

(define (get-temponode-box $num)
  (get-common-node-box (<ra> :get-temponode-x $num)
                       (<ra> :get-temponode-y $num)))

(define (temponodeval->01 value)
  (scale value
         (- (1- (<ra> :get-temponode-max)))
         (1- (<ra> :get-temponode-max))
         0
         1))

(define (01->temponodeval O1)
  (scale O1
         0
         1
         (- (1- (<ra> :get-temponode-max)))
         (1- (<ra> :get-temponode-max))))
         
(add-node-mouse-handler :Get-area-box (lambda () (and (<ra> :reltempo-track-visible) (<ra> :get-box temponode-area)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (match (list (find-node X Y get-temponode-box (<ra> :get-num-temponodes)))
                                                         (existing-box Num Box) :> (callback Num (temponodeval->01 (<ra> :get-temponode-value Num)) (Box :y))
                                                         _                      :> #f))
                        :Get-min-value (lambda (_) 0);(- (1- (<ra> :get-temponode-max))))
                        :Get-max-value (lambda (_) 1);(1- (<ra> :get-temponode-max)))
                        :Get-x (lambda (Num)
                                 (<ra> :get-temponode-x Num))
                        :Get-y (lambda (Num) (<ra> :get-temponode-y Num))
                        :Make-undo (lambda (_) (ra:undo-temponodes))
                        :Create-new-node (lambda (X Place callback)
                                           (define Value (scale X (<ra> :get-temponode-area-x1) (<ra> :get-temponode-area-x2) 0 1))
                                           (define Num (<ra> :add-temponode (01->temponodeval Value) Place))
                                           (if (= -1 Num)
                                               #f
                                               (callback Num (temponodeval->01 (<ra> :get-temponode-value Num)))))
                        :Move-node (lambda (Num Value Place)
                                     (<ra> :set-temponode Num (01->temponodeval Value) Place)
                                     (define new-value (<ra> :get-temponode-value Num)) ;; might differ from Value
                                     ;;(c-display "Place/New:" Place (<ra> :get-temponode-value Num))
                                     (temponodeval->01 new-value)
                                     Num
                                     )
                        :Publicize (lambda (Num) ;; this version works though. They are, or at least, should be, 100% functionally similar.
                                     (set-indicator-temponode Num)
                                     (show-temponode-in-statusbar (<ra> :get-temponode-value Num)))
                        :Get-pixels-per-value-unit #f
                        )                        

;; delete temponode
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     (<ra> :reltempo-track-visible)
                     (inside-box (<ra> :get-box temponode-area) $x $y)                                     
                     (match (list (find-node $x $y get-temponode-box (<ra> :get-num-temponodes)))
                            (existing-box Num Box) :> (begin
                                                        (<ra> :undo-temponodes)
                                                        (<ra> :delete-temponode Num)
                                                        #t)
                            _                      :> #f)))))

;; highlight current temponode
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and (<ra> :reltempo-track-visible)
              (inside-box-forgiving (<ra> :get-box temponode-area) $x $y)
              (match (list (find-node $x $y get-temponode-box (<ra> :get-num-temponodes)))
                     (existing-box Num Box) :> (begin
                                                 (set-mouse-track-to-reltempo)
                                                 (set-current-temponode Num)
                                                 (set-indicator-temponode Num)
                                                 (show-temponode-in-statusbar (<ra> :get-temponode-value Num))
                                                 #t)
                     _                      :> #f))))



;; notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (place-is-last-place place)
  (= (-line (<ra> :get-num-lines))
     place))

(define (note-spans-last-place notenum tracknum)
  (define num-nodes (<ra> :get-num-velocities notenum tracknum))
  (place-is-last-place (<ra> :get-velocity-place
                             (1- num-nodes)
                             notenum
                             tracknum)))

;; pitches
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-pitchnum-box $num)
  ;;(c-display "get-pitchnum-box" $num)
  (make-box2 (<ra> :get-pitchnum-x1 $num *current-track-num*)
             (<ra> :get-pitchnum-y1 $num *current-track-num*)
             (<ra> :get-pitchnum-x2 $num *current-track-num*)
             (<ra> :get-pitchnum-y2 $num *current-track-num*)))

(define (todofunc funcname . $returnvalue)
  (lambda x
    (c-display "\"" funcname "\" not implemented. Arguments: " x)
    (if (null? $returnvalue)
        'no-return-value
        (car $returnvalue))))
  
#||
(set! *current-track-num* 0)
(box-to-string (get-pitchnum-box 1))
(<ra> :get-num-pitchnum 0)
(<ra> :get-pitchnum-value 1 0)
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
                                                          (<ra> :get-pitchnum-value N *current-track-num*))
  N Max Least-So-Far :> (get-min-pitch-in-current-track-0 (1+ N)
                                                          Max
                                                          (min Least-So-Far
                                                               (<ra> :get-pitchnum-value N *current-track-num*))))

(define (get-min-pitch-in-current-track)
  (1- (get-min-pitch-in-current-track-0 0
                                        (<ra> :get-num-pitchnums *current-track-num*)
                                        #f)))
       
(define-match get-max-pitch-in-current-track-0
  N N   #f           :> 127
  N N   Least-So-Far :> Least-So-Far
  N Max #f           :> (get-max-pitch-in-current-track-0 (1+ N)
                                                          Max
                                                          (<ra> :get-pitchnum-value N *current-track-num*))
  N Max Least-So-Far :> (get-max-pitch-in-current-track-0 (1+ N)
                                                          Max
                                                          (max Least-So-Far
                                                               (<ra> :get-pitchnum-value N *current-track-num*))))
  
(define (get-max-pitch-in-current-track)
  (1+ (get-max-pitch-in-current-track-0 0
                                        (<ra> :get-num-pitchnums *current-track-num*)
                                        #f)))

;; add and move pitch
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (<ra> :get-box track-notes *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  '(c-display "hepp"
                                                              (may-be-a-resize-point-in-track X Y *current-track-num*)
                                                              (list (find-node X Y get-pitchnum-box (<ra> :get-num-pitchnums *current-track-num*))))
                                                  (and *current-track-num*
                                                       (not (may-be-a-resize-point-in-track X Y *current-track-num*))
                                                       (match (list (find-node X Y get-pitchnum-box (<ra> :get-num-pitchnums *current-track-num*)))
                                                              (existing-box Num Box) :> (callback Num (<ra> :get-pitchnum-value Num *current-track-num*) (Box :y))
                                                              _                      :> #f)))
                        :Get-min-value (lambda (_)
                                         (get-min-pitch-in-current-track))
                        :Get-max-value (lambda (_)
                                         (get-max-pitch-in-current-track))
                        :Get-x (lambda (Num)
                                 ;;(c-display "    NUM----> " Num)
                                 (<ra> :get-pitchnum-x Num *current-track-num*))
                        :Get-y (lambda (Num)
                                 (<ra> :get-pitchnum-y Num *current-track-num*))
                        :Make-undo (lambda (_) (<ra> :undo-notes *current-track-num*))
                        :Create-new-node (lambda (X Place callback)
                                           (if (place-is-last-place Place)
                                               #f
                                               (begin
                                                 (define Value (scale X
                                                                      (<ra> :get-track-notes-x1 *current-track-num*) (<ra> :get-track-notes-x2 *current-track-num*) 
                                                                      (get-min-pitch-in-current-track) (get-max-pitch-in-current-track)))
                                                 (if (not (<ra> :control-pressed))
                                                     (set! Value (round Value)))
                                                 (define Num (<ra> :add-pitchnum Value Place *current-track-num*))
                                                 (if (= -1 Num)
                                                     #f
                                                     (callback Num (<ra> :get-pitchnum-value Num *current-track-num*))))))
                        :Move-node (lambda (Num Value Place)                                     
                                     (<ra> :set-pitchnum Num
                                                        (if (<ra> :control-pressed)
                                                            Value
                                                            (round Value))
                                                        Place
                                                        *current-track-num*))
                        :Publicize (lambda (Num)
                                     (set-indicator-pitchnum Num *current-track-num*)
                                     (set-editor-statusbar (<-> "Pitch: " (two-decimal-string (<ra> :get-pitchnum-value Num *current-track-num*)))))
                        :Get-pixels-per-value-unit (lambda (_)
                                                     5.0)
                        )


;; delete pitch
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     (<ra> :shift-pressed)
                     *current-track-num*
                     (inside-box (<ra> :get-box track-notes *current-track-num*) $x $y)
                     (match (list (find-node $x $y get-pitchnum-box (<ra> :get-num-pitchnums *current-track-num*)))
                            (existing-box Num Box) :> (begin
                                                        (<ra> :undo-notes *current-track-num*)
                                                        (<ra> :delete-pitchnum Num *current-track-num*)
                                                        #t)
                            _                      :> #f)))))

#||
;; pitch popup menu
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     *current-track-num*
                     (inside-box (<ra> :get-box track-notes *current-track-num*) $x $y)
                     (match (list (find-node $x $y get-pitchnum-box (<ra> :get-num-pitchnums *current-track-num*)))
                            (existing-box Num Box) :> (begin
                                                        (define (delete-pitch)
                                                          (<ra> :undo-notes *current-track-num*)
                                                          (<ra> :delete-pitchnum Num *current-track-num*))

                                                        (popup-menu "Delete pitch" delete-pitch)
                                                        (list "Glide to next pitch"
                                                              :check )
                                                        #t)
                            _                      :> #f)))))
||#


;; highlight current pitch
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and *current-track-num*
              (inside-box (<ra> :get-box track-notes *current-track-num*) $x $y)
              (match (list (find-node $x $y get-pitchnum-box (<ra> :get-num-pitchnums *current-track-num*)))
                     (existing-box Num Box) :> (begin
                                                 (set-indicator-pitchnum Num *current-track-num*)
                                                 (set-current-pitchnum Num  *current-track-num*)
                                                 #t)
                     _                      :> #f))))




;; pianoroll
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant *pianonote-move-start* 'move-start)
(define-constant *pianonote-move-all* 'move-all)
(define-constant *pianonote-move-end* 'move-end)

(define-struct pianonote-info
  :tracknum
  :notenum
  :pianonotenum
  :move-type     ;; A "*pianonote-move-<...>*" value
  :mouse-delta
  :note-id
  )


(define (get-pianonote-y pianonotenum notenum tracknum move-type)
  (define y1 (<ra> :get-pianonote-y1 pianonotenum
                                  notenum
                                  tracknum))
  (define y2 (<ra> :get-pianonote-y2 pianonotenum
                                  notenum
                                  tracknum))

  (cond ((eq? move-type
              *pianonote-move-start*)
         y1)
        ((eq? move-type
              *pianonote-move-end*)
         y2)
        (else
         (/ (+ y1 y2) 2))))

         
(define (get-pianonote-box $tracknum $notenum $num)
  ;;(c-display "get-pitchnum-box" $num)
  (make-box2 (<ra> :get-pianonote-x1 $num $notenum $tracknum)
             (<ra> :get-pianonote-y1 $num $notenum $tracknum)
             (+ 2 (<ra> :get-pianonote-x2 $num $notenum $tracknum))
             (+ 2 (<ra> :get-pianonote-y2 $num $notenum $tracknum))))

(define (get-pianonote-move-type $y $y1 $y2)
  (define height (- $y2 $y1))
  (define h (if (< height
                   (* 3 (<ra> :get-half-of-node-width)))
                (/ height
                   3)
                (<ra> :get-half-of-node-width)))                   
  (cond ((< $y (+ $y1 h))
         *pianonote-move-start*)
        ((> $y (- $y2 h))
         *pianonote-move-end*)
        (else
         *pianonote-move-all*)))

(define (get-pianonote-info4 $x $y $tracknum $notenum $pianonotenum)
  (define box (get-pianonote-box $tracknum $notenum $pianonotenum))
  (and (inside-box box $x $y)
       (let ((move-type (get-pianonote-move-type $y (box :y1) (box :y2))))
         (make-pianonote-info :tracknum $tracknum
                              :notenum $notenum
                              :pianonotenum $pianonotenum
                              :move-type move-type
                              :mouse-delta (- $y (get-pianonote-y $pianonotenum $notenum $tracknum move-type))
                              :note-id -1
                              ))))
  
(define-match get-pianonote-info3
  _ _ ________ _______ Num-pianonotes Num-pianonotes :> #f
  X Y Tracknum Notenum Pianonotenum   Num-pianonotes :> (or (get-pianonote-info4 X Y
                                                                                 Tracknum
                                                                                 Notenum
                                                                                 Pianonotenum)
                                                            (get-pianonote-info3 X Y
                                                                                 Tracknum
                                                                                 Notenum
                                                                                 (1+ Pianonotenum)
                                                                                 Num-pianonotes)))

(define-match get-pianonote-info2
  _ _ ________ Num-notes Num-notes :> #f
  X Y Tracknum Notenum   Num-notes :> (or (get-pianonote-info3 X Y
                                                               Tracknum
                                                               Notenum
                                                               0
                                                               (<ra> :get-num-pianonotes Notenum Tracknum))
                                          (get-pianonote-info2 X Y
                                                               Tracknum
                                                               (1+ Notenum)
                                                               Num-notes)))
  
(define (get-pianonote-info $x $y $tracknum)
  (get-pianonote-info2 $x $y $tracknum 0 (<ra> :get-num-notes $tracknum)))


(define (call-get-existing-node-info-callbacks callback info)

  (define num-pianonotes (<ra> :get-num-pianonotes (info :notenum) (info :tracknum)))
  (define pianonotenum (info :pianonotenum))

  (define logtype-holding (= *logtype-hold* (<ra> :get-pianonote-logtype (info :pianonotenum)
                                                                         (info :notenum)
                                                                         (info :tracknum))))


  (define portamento-enabled (<ra> :portamento-enabled (info :notenum)
                                                    (info :tracknum)))
  
  (define is-end-pitch (and portamento-enabled
                            (not logtype-holding)
                            (eq? (info :move-type) *pianonote-move-end*)
                            (= (1- num-pianonotes) pianonotenum)))
  

  (define value-pianonote-num (if (and (not logtype-holding)
                                       (eq? (info :move-type) *pianonote-move-end*))
                                  (1+ (info :pianonotenum))
                                  (info :pianonotenum)))
                                  
  (callback info
            (if is-end-pitch 
                (<ra> :get-note-end-pitch (info :notenum)
                                       (info :tracknum))
                (<ra> :get-pianonote-value (if portamento-enabled
                                            value-pianonote-num
                                            0)
                                        (info :notenum)
                                        (info :tracknum)))
            (if (eq? *pianonote-move-end* (info :move-type))
                (<ra> :get-pianonote-y2 (info :pianonotenum)
                                     (info :notenum)
                                     (info :tracknum))
                (<ra> :get-pianonote-y1 (info :pianonotenum)
                                     (info :notenum)
                                     (info :tracknum)))))
  

(define (create-play-pianonote note-id pianonote-id)
  (let ((instrument-id (<ra> :get-instrument-for-track  *current-track-num*)))
    (if (< instrument-id 0)
        -1
        (<ra> :play-note
              (<ra> :get-pianonote-value pianonote-id note-id *current-track-num*)
              (if (<ra> :get-track-volume-on-off *current-track-num*)
                  (<ra> :get-track-volume *current-track-num*)
                  1.0)
              (if (<ra> :get-track-pan-on-off *current-track-num*)
                  (<ra> :get-track-pan *current-track-num*)
                  0.0)
              (<ra> :get-track-midi-channel *current-track-num*)
              instrument-id))))

(define (get-pianoroll-key X)
  (scale X
         (<ra> :get-track-pianoroll-x1 *current-track-num*)
         (<ra> :get-track-pianoroll-x2 *current-track-num*)
         (<ra> :get-pianoroll-low-key *current-track-num*)
         (<ra> :get-pianoroll-high-key *current-track-num*)))

(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (<ra> :pianoroll-visible *current-track-num*)
                                             (<ra> :get-box track-pianoroll *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (and *current-track-num*
                                                       (let ((info (get-pianonote-info X Y *current-track-num*)))
                                                         ;;(and info
                                                         ;;     (c-display "        NUM " (info :pianonotenum) " type: " (info :move-type)))
                                                         (and info
                                                              (let ((info (<copy-pianonote-info> info
                                                                                                 :note-id (create-play-pianonote (info :notenum)
                                                                                                                                 (info :pianonotenum)))))
                                                                (call-get-existing-node-info-callbacks callback info))))))
                        :Get-min-value (lambda (_) 1)
                        :Get-max-value (lambda (_) 127)
                        :Get-x (lambda (info) (/ (+ (<ra> :get-pianonote-x1 (info :pianonotenum)
                                                                         (info :notenum)
                                                                         (info :tracknum))
                                                    (<ra> :get-pianonote-x2 (info :pianonotenum)
                                                                         (info :notenum)
                                                                         (info :tracknum)))
                                                 2))
                        :Get-y (lambda (info)
                                 (+ (info :mouse-delta)
                                    (get-pianonote-y (info :pianonotenum)
                                                     (info :notenum)
                                                     (info :tracknum)
                                                     (info :move-type))))
                        :Make-undo (lambda (_) (<ra> :undo-notes *current-track-num*))
                        :Create-new-node (lambda (X Place callback)
                                           (define Value (get-pianoroll-key X))
                                           (define Next-Place (get-next-place-from-y *left-button* (<ra> :get-mouse-pointer-y)))
                                           (define noteid (<ra> :add-pianonote Value Place Next-Place *current-track-num*))
                                           (if (and (number? noteid) (= -1 noteid))
                                               #f
                                               (callback (make-pianonote-info :tracknum *current-track-num*
                                                                              :notenum noteid
                                                                              :pianonotenum 0
                                                                              :move-type *pianonote-move-end*
                                                                              :mouse-delta 0
                                                                              :note-id (create-play-pianonote noteid 0))
                                                         Value)))
                        :Publicize (lambda (pianonote-info)
                                     (set-current-pianonote (pianonote-info :pianonotenum)
                                                            (pianonote-info :notenum)
                                                            (pianonote-info :tracknum)))
                        :Move-node (lambda (pianonote-info Value Place)
                                     ;;(c-display "moving to. Value: " Value ", Place: " Place " type: " (pianonote-info :move-type) " pianonotenum:" (pianonote-info :pianonotenum))
                                     (define func
                                       (cond ((eq? (pianonote-info :move-type)
                                                   *pianonote-move-start*)
                                              ra:move-pianonote-start)
                                             ((eq? (pianonote-info :move-type)
                                                   *pianonote-move-end*)
                                              ra:move-pianonote-end)
                                             ((eq? (pianonote-info :move-type)
                                                   *pianonote-move-all*)
                                              ra:move-pianonote)
                                             (else
                                              (c-display "UNKNOWN pianonote-info type: " (pianonote-info :move-type))
                                              #f)))
                                     ;(c-display "value:" (<ra> :control-pressed) (if (<ra> :control-pressed)
                                     ;                                             Value
                                     ;                                             (round Value))
                                     ;           Value)
                                     (define new-notenum
                                       (func (pianonote-info :pianonotenum)
                                             (if (<ra> :control-pressed)
                                                 Value
                                                 (round Value))
                                             Place
                                             (pianonote-info :notenum)
                                             (pianonote-info :tracknum)))

                                     (if (not (and (number? (pianonote-info :note-id))
                                                   (= -1 (pianonote-info :note-id))))
                                         (let ((instrument-id (<ra> :get-instrument-for-track  *current-track-num*)))
                                           (if (>= instrument-id 0)
                                               (<ra> :change-note-pitch
                                                     (<ra> :get-pianonote-value (pianonote-info :pianonotenum) new-notenum *current-track-num*)
                                                     (pianonote-info :note-id)
                                                     (<ra> :get-track-midi-channel *current-track-num*)
                                                     instrument-id))))
                                           
                                     (make-pianonote-info :tracknum (pianonote-info :tracknum)
                                                          :notenum new-notenum
                                                          :pianonotenum (pianonote-info :pianonotenum)
                                                          :move-type (pianonote-info :move-type)
                                                          :mouse-delta (pianonote-info :mouse-delta)
                                                          :note-id (pianonote-info :note-id)
                                                          ))

                        :Release-node (lambda (pianonote-info)
                                        (if (not (and (number? (pianonote-info :note-id))
                                                      (= -1 (pianonote-info :note-id))))
                                            (let ((instrument-id (<ra> :get-instrument-for-track  *current-track-num*)))
                                              (if (>= instrument-id 0)
                                                  (<ra> :stop-note (pianonote-info :note-id)
                                                                   (<ra> :get-track-midi-channel *current-track-num*)
                                                                   instrument-id)))))
                                     
                        :Get-pixels-per-value-unit (lambda (_)
                                                     (<ra> :get-pianoroll-low-key *current-track-num*)
                                                     (<ra> :get-pianoroll-high-key *current-track-num*)
                                                     (<ra> :get-half-of-node-width))

                        :Forgiving-box #f
                        )



;; highlight current pianonote
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and *current-track-num*
              (<ra> :pianoroll-visible *current-track-num*)
              (inside-box (<ra> :get-box track-pianoroll *current-track-num*) $x $y)
              ;;(c-display "current-tracknum:" *current-track-num*)
              (let ((pianonote-info (get-pianonote-info $x $y *current-track-num*)))
                '(c-display $x $y pianonote-info
                            (box-to-string (get-pianonote-box 0 1 0)))
                (if (and pianonote-info
                         (let ((pianonote-info pianonote-info)) ;;;(copy-pianonote-info :note-id (create-play-pianonote (pianonote-info :notenum)
                                                                ;;;                                    (pianonote-info :pianonotenum)))))
                           ;;(c-display "type: " (pianonote-info :move-type))
                           (set-current-pianonote (pianonote-info :pianonotenum)
                                                  (pianonote-info :notenum)
                                                  (pianonote-info :tracknum))
                           ;;(c-display "hello:" (pianonote-info :dir))
                           (cond ((eq? (pianonote-info :move-type)
                                       *pianonote-move-start*)
                                  #t)
                                 ((eq? (pianonote-info :move-type)
                                       *pianonote-move-end*)
                                  #t)
                                 ((eq? (pianonote-info :move-type)
                                       *pianonote-move-all*)
                                  #f)))
                         )
                    (set-mouse-pointer ra:set-vertical-resize-mouse-pointer (<gui> :get-editor-gui))
                    (set-mouse-pointer ra:set-pointing-mouse-pointer (<gui> :get-editor-gui)))))))

;; Delete pianonote (shift + right mouse)
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     (<ra> :shift-pressed)
                     *current-track-num*
                     (<ra> :pianoroll-visible *current-track-num*)
                     (inside-box (<ra> :get-box track-pianoroll *current-track-num*) $x $y)
                     (let ((pianonote-info (get-pianonote-info $x $y *current-track-num*)))
                       (and pianonote-info
                            (begin
                              (<ra> :undo-notes (pianonote-info :tracknum))
                              (<ra> :delete-pianonote 0
                                    (pianonote-info :notenum)
                                    (pianonote-info :tracknum))
                              #t)))))))

                               
;; delete note / add pitch / delete pitch
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     *current-track-num*
                     (<ra> :pianoroll-visible *current-track-num*)
                     (inside-box (<ra> :get-box track-pianoroll *current-track-num*) $x $y)
                     (let ((pianonote-info (get-pianonote-info $x $y *current-track-num*)))
                       (if pianonote-info
                           (begin
                             (define (delete-note)
                               (<ra> :undo-notes (pianonote-info :tracknum))
                               (<ra> :delete-pianonote
                                     0
                                     (pianonote-info :notenum)
                                     (pianonote-info :tracknum))
                               #f)
                             (define (cut-note)
                               (<ra> :undo-notes (pianonote-info :tracknum))
                               (define Place (get-place-from-y $button $y))
                               (<ra> :cut-note Place
                                               (pianonote-info :notenum)
                                               (pianonote-info :tracknum))
                               #f)
                             (define (delete-pitch)
                               (<ra> :undo-notes (pianonote-info :tracknum))
                               (<ra> :delete-pianonote (if (= 0 (pianonote-info :pianonotenum))
                                                        1
                                                        (pianonote-info :pianonotenum))
                                                    (pianonote-info :notenum)
                                                    (pianonote-info :tracknum))
                               #f)
                             (define (enable-portamento)
                               (<ra> :undo-notes (pianonote-info :tracknum))
                               (<ra> :enable-portamento (pianonote-info :notenum)
                                                     (pianonote-info :tracknum))
                               #f)
                             (define (disable-portamento)
                               (<ra> :undo-notes (pianonote-info :tracknum))
                               (<ra> :disable-portamento (pianonote-info :notenum)
                                                      (pianonote-info :tracknum))
                               #f)
                             (define (set-linear!)
                               (<ra> :set-pianonote-logtype *logtype-linear*
                                                            (pianonote-info :pianonotenum)
                                                            (pianonote-info :notenum)
                                                            (pianonote-info :tracknum))
                               #f
                               )
                             (define (set-hold!)
                               (<ra> :set-pianonote-logtype *logtype-hold*
                                                            (pianonote-info :pianonotenum)
                                                            (pianonote-info :notenum)
                                                            (pianonote-info :tracknum))
                               #f
                               )
                             (define (add-pitch)
                               (<ra> :undo-notes (pianonote-info :tracknum))
                               (define Place (get-place-from-y $button $y))
                               (define Value (<ra> :get-note-value (pianonote-info :notenum) (pianonote-info :tracknum)))
                               (<ra> :add-pianonote-pitch Value Place (pianonote-info :notenum) (pianonote-info :tracknum))
                               #f)

                             (define (glide-from-here-to-next-note)
                               (<ra> :undo-notes (pianonote-info :tracknum))
                               (define Place (get-place-from-y $button $y))
                               (define Value (<ra> :get-note-value (pianonote-info :notenum) (pianonote-info :tracknum)))
                               (define next-pitch (<ra> :get-note-value (1+ (pianonote-info :notenum)) (pianonote-info :tracknum)))                                 
                               (<ra> :add-pianonote-pitch Value Place (pianonote-info :notenum) (pianonote-info :tracknum))
                               (<ra> :set-note-end-pitch next-pitch (pianonote-info :notenum) (pianonote-info :tracknum))
                               #f)
                             
                             (define (can-glide-from-here-to-next-note?)
                               (and (< (pianonote-info :notenum)
                                       (1- (<ra> :get-num-notes (pianonote-info :tracknum))))
                                    (= (pianonote-info :pianonotenum)
                                       (1- (<ra> :get-num-pianonotes (pianonote-info :notenum)
                                                                     (pianonote-info :tracknum))))))
                               
                             (define num-pianonotes (<ra> :get-num-pianonotes (pianonote-info :notenum)
                                                                           (pianonote-info :tracknum)))
                             (let ((portamento-enabled (<ra> :portamento-enabled
                                                             (pianonote-info :notenum)
                                                             (pianonote-info :tracknum)))
                                   (is-holding (= *logtype-hold* (<ra> :get-pianonote-logtype
                                                                       (pianonote-info :pianonotenum)
                                                                       (pianonote-info :notenum)
                                                                       (pianonote-info :tracknum)))))
                               
                               (popup-menu "Cut Note at mouse position" cut-note
                                           "Glide to next note at mouse position" :enabled (can-glide-from-here-to-next-note?) glide-from-here-to-next-note
                                           "Delete Note" delete-note
                                           "--------"
                                           "Delete break point" :enabled (> num-pianonotes 1) delete-pitch
                                           "Add break point at mouse position" add-pitch
                                           (list "Glide to next break point"
                                                 :check (if (< num-pianonotes 2)
                                                            portamento-enabled
                                                            (not is-holding))
                                                 ;;:enabled (> num-pianonotes 1)
                                                 (lambda (maybe)
                                                   (c-display "   ______________________________   Glide1 called " maybe)
                                                   (if (< num-pianonotes 2)
                                                       (if maybe
                                                           (enable-portamento)
                                                           (disable-portamento))
                                                       (if maybe
                                                           (set-linear!)
                                                           (set-hold!)))))
                                           (let ((note-spans-last-place (note-spans-last-place (pianonote-info :notenum)
                                                                                               (pianonote-info :tracknum))))
                                             (list "continue playing note into next block"
                                                   :check (and note-spans-last-place
                                                               (<ra> :note-continues-next-block (pianonote-info :notenum) (pianonote-info :tracknum)))
                                                   :enabled note-spans-last-place
                                                   (lambda (maybe)
                                                     (<ra> :set-note-continue-next-block maybe (pianonote-info :notenum) (pianonote-info :tracknum)))))


                                           ;;"--------"
                                           ;;"Glide to end position" :check portamento-enabled :enabled (< num-pianonotes 2) (lambda (ison)
                                           ;;                                                                                  (c-display "   ______________________________   Glide2 called " ison)
                                           ;;                                                                                  (if ison
                                           ;;                                                                                      (enable-portamento)
                                           ;;                                                                                      (disable-portamento)))
                                           
                                           ;; "Stop note here" stop-note
                                           ))))
                       #f)))))


;; Eraser

(define (get-pianoroll-eraser-pitches-and-places $button $x $y kont)
  (let* ((pitch (if (<ra> :control-pressed)
                    (get-pianoroll-key $x)
                    (round (get-pianoroll-key $x))))
         (place1 (get-place-from-y $button $y))
         (place2 (+ place1 (/ (<ra> :get-grid)
                              (<ra> :get-line-zoom-block-ratio))))
         )
    (if (<ra> :control-pressed)
        (kont (- pitch 0.5) (+ pitch 0.5) place1 place2)
        (kont pitch (1+ pitch) place1 place2))))


(define (move-pianoroll-eraser $button $dx $dy $instance)
  (define $x (+ ($instance :x) $dx))
  (define $y (+ ($instance :y) $dy))
  (define has-made-undo ($instance :has-made-undo))
  (get-pianoroll-eraser-pitches-and-places $button $x $y
                                           (lambda (pitch1 pitch2 place1 place2)
                                             (<ra> :show-pianoroll-eraser pitch1 pitch2 place1 place2 *current-track-num*)
                                             (define made-undo-now (pr-erase! -1 *current-track-num*
                                                                              (min pitch1 ($instance :pitch1))
                                                                              (max pitch2 ($instance :pitch2))
                                                                              (min place1 ($instance :place1))
                                                                              (max place2 ($instance :place2))
                                                                              (not has-made-undo)))
                                             (hash-table* :x $x
                                                          :y $y
                                                          :has-made-undo (or made-undo-now
                                                                             has-made-undo)
                                                          :pitch1 pitch1
                                                          :pitch2 pitch2
                                                          :place1 place1
                                                          :place2 place2))))

(add-delta-mouse-handler :press (lambda ($button $x $y)
                                  (and (= $button *right-button*)
                                       *current-track-num*
                                       (<ra> :pianoroll-visible *current-track-num*)
                                       (inside-box (<ra> :get-box track-pianoroll *current-track-num*) $x $y)
                                       (not (get-pianonote-info $x $y *current-track-num*))
                                       (set-mouse-pointer ra:set-blank-mouse-pointer (<gui> :get-editor-gui))
                                       (move-pianoroll-eraser $button 0 0 (hash-table* :x $x
                                                                                       :y $y
                                                                                       :has-made-undo #f
                                                                                       :pitch1 100000
                                                                                       :pitch2 -100000
                                                                                       :place1 1000000
                                                                                       :place2 -10000000))))
                         :move-and-release move-pianoroll-eraser

                         :release (lambda ($button $x $y x-and-y)
                                    (set-mouse-pointer ra:set-normal-mouse-pointer (<gui> :get-editor-gui))
                                    (<ra> :hide-pianoroll-eraser)
                                    ;;(c-display "RELEASING")
                                    )
                         :mouse-pointer-is-hidden-func (lambda () #t)
                         )



;; velocities
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-velocity-box Tracknum Notenum Velocitynum)
  (get-common-node-box (<ra> :get-velocity-x Velocitynum Notenum Tracknum)
                       (<ra> :get-velocity-y Velocitynum Notenum Tracknum)))

(define-struct velocity-info
  :tracknum
  :notenum
  :velocitynum
  :value
  :y  
  )

(define (velocity-info-rating Y Vi)
  (define velocity-y (<ra> :get-velocity-y (Vi :velocitynum) (Vi :notenum) (Vi :tracknum)))
  (cond ((and (= 0
                 (Vi :velocitynum))
              (> Y
                 velocity-y))
         10)
        ((and (= (1- (<ra> :get-num-velocities (Vi :notenum) (Vi :tracknum)))
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
                                                                                                                         :value (<ra> :get-velocity-value Velocitynum Notenum Tracknum)
                                                                                                                         :y (box :y)
                                                                                                                         )))))))

(define-match get-velocity-1
  X Y Tracknum Notenum Notenum     :> #f
  X Y Tracknum Notenum Total-Notes :> (highest-rated-velocity-info Y
                                                                   (list (get-velocity-1 X Y Tracknum (1+ Notenum) Total-Notes)
                                                                         (get-velocity-2 X Y Tracknum Notenum 0 (<ra> :get-num-velocities Notenum Tracknum)))))
                                   
  
(define-match get-velocity-0
  X Y -1       :> #f
  X Y Tracknum :> #f :where (>= Tracknum (<ra> :get-num-tracks))
  X Y Tracknum :> (get-velocity-1 X Y Tracknum 0 (<ra> :get-num-notes Tracknum)))
  
(define-match get-velocity-info
  X Y #f       :> (get-velocity-info X Y 0)
  X Y Tracknum :> (highest-rated-velocity-info Y
                                               (list (get-velocity-0 X Y (1- Tracknum)) ;; node in the prev track can overlap into the current track
                                                     (get-velocity-0 X Y Tracknum))))


#||
(let ((node (get-velocity-info 319 169 0)))
  (c-display (node :velocitynum) (node :notenum) (node :tracknum)))
        

(<ra> :get-velocity-x 1 0 0)
(<ra> :get-velocity-y 1 0 0)
||#

(define *current-note-num* #f)


(define-match get-note-num-0
  _____ ________ Num Num   :> #f
  Place Subtrack Num Total :> (if (and (>= Place
                                           (<ra> :get-note-start Num *current-track-num*))
                                       (<  Place
                                           (<ra> :get-note-end Num *current-track-num*))
                                       (=  Subtrack
                                           (<ra> :get-note-subtrack Num *current-track-num*)))
                                  Num
                                  (get-note-num-0 Place
                                                  Subtrack
                                                  (1+ Num)
                                                  Total)))
                                                 
(define-match get-note-num
  X Y :> (get-note-num-0 (<ra> :get-place-from-y Y)
                         *current-subtrack-num*
                         0
                         (<ra> :get-num-notes *current-track-num*)))

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
                                                                     (<ra> :get-velocity-x Vel *current-note-num* *current-track-num*)
                                                                     (<ra> :get-velocity-y Vel *current-note-num* *current-track-num*)))

(define (get-shortest-velocity-distance X Y)
  (if (not *current-note-num*)
      #f
      (get-shortest-velocity-distance-0 2
                                        (<ra> :get-num-velocities *current-note-num* *current-track-num*)
                                        X Y
                                        (<ra> :get-velocity-x 0 *current-note-num* *current-track-num*)
                                        (<ra> :get-velocity-y 0 *current-note-num* *current-track-num*)
                                        (<ra> :get-velocity-x 1 *current-note-num* *current-track-num*)
                                        (<ra> :get-velocity-y 1 *current-note-num* *current-track-num*))))



;; add and move velocity
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (<ra> :get-box track-fx *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (let ((velocity-info (get-velocity-info X Y *current-track-num*)))
                                                    ;;(c-display "vel-info:" velocity-info)
                                                    (and velocity-info
                                                         (callback velocity-info
                                                                   (velocity-info :value)
                                                                   (velocity-info :y)))))
                        :Get-min-value (lambda (_) 0.0)
                        :Get-max-value (lambda (_) 1.0)
                        :Get-x (lambda (info)
                                 (<ra> :get-velocity-x (info :velocitynum)
                                       (info :notenum)
                                       (info :tracknum)))
                        :Get-y (lambda (info) (<ra> :get-velocity-y (info :velocitynum)
                                                    (info :notenum)
                                                    (info :tracknum)))
                        :Make-undo (lambda (info) (<ra> :undo-notes (info :tracknum)))
                        :Create-new-node (lambda (X Place callback)
                                           ;;(c-display "a" Place)
                                           (and *current-note-num*
                                                (not (get-current-fxnum))
                                                (begin
                                                  ;;(c-display "b" Place)
                                                  (define Value (scale X
                                                                       (<ra> :get-subtrack-x1 *current-subtrack-num* *current-track-num*)
                                                                       (<ra> :get-subtrack-x2 *current-subtrack-num* *current-track-num*)
                                                                       0 1))
                                                  (<ra> :undo-notes *current-track-num*)
                                                  (define Num (<ra> :add-velocity-dont-display-errors Value Place *current-note-num* *current-track-num*))
                                                  (if (= -1 Num)
                                                      #f
                                                      (callback (make-velocity-info :tracknum *current-track-num*
                                                                                    :notenum *current-note-num*
                                                                                    :velocitynum Num
                                                                                    :value Value
                                                                                    :y #f ;; dont need it.
                                                                                    )
                                                                (<ra> :get-velocity-value Num *current-note-num* *current-track-num*))))))
                        :Publicize (lambda (velocity-info)
                                              (set-indicator-velocity-node (velocity-info :velocitynum)
                                                                           (velocity-info :notenum)
                                                                           (velocity-info :tracknum))
                                              (define value (<ra> :get-velocity-value (velocity-info :velocitynum)
                                                                                   (velocity-info :notenum)
                                                                                   (velocity-info :tracknum)))
                                              (set-velocity-statusbar-text value))
                        :Move-node (lambda (velocity-info Value Place)
                                     (define note-num (<ra> :set-velocity Value Place (velocity-info :velocitynum) (velocity-info :notenum) (velocity-info :tracknum)))
                                     (make-velocity-info :tracknum (velocity-info :tracknum)
                                                         :notenum note-num
                                                         :velocitynum (velocity-info :velocitynum)
                                                         :value (velocity-info :value)
                                                         :y (velocity-info :y)))
                        )

;; delete velocity (shift + right mouse)
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     (<ra> :shift-pressed)
                     *current-track-num*
                     (inside-box-forgiving (<ra> :get-box track *current-track-num*) X Y)
                     (begin
                       (define velocity-info (get-velocity-info X Y *current-track-num*))
                       ;;(c-display "got velocity info " velocity-info)
                       (and velocity-info
                            (begin
                              (<ra> :undo-notes (velocity-info :tracknum))
                              (<ra> :delete-velocity
                                    (velocity-info :velocitynum)
                                    (velocity-info :notenum)
                                    (velocity-info :tracknum))
                              #t)))))))

;; velocity popup
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     *current-track-num*
                     (inside-box-forgiving (<ra> :get-box track *current-track-num*) X Y)
                     (begin
                       (define velocity-info (get-velocity-info X Y *current-track-num*))
                       ;;(c-display "got velocity info " velocity-info)
                       (if velocity-info
                           (begin
                             (define (delete-velocity!)
                               (<ra> :undo-notes (velocity-info :tracknum))
                               (<ra> :delete-velocity
                                     (velocity-info :velocitynum)
                                     (velocity-info :notenum)
                                     (velocity-info :tracknum)))
                             (define (set-hold!)
                               (<ra> :undo-notes (velocity-info :tracknum))
                               (<ra> :set-velocity-logtype
                                     *logtype-hold*
                                     (velocity-info :velocitynum)
                                     (velocity-info :notenum)
                                     (velocity-info :tracknum)))
                             (define (set-linear!)
                               (<ra> :undo-notes (velocity-info :tracknum))
                               (<ra> :set-velocity-logtype
                                     *logtype-linear*
                                     (velocity-info :velocitynum)
                                     (velocity-info :notenum)
                                     (velocity-info :tracknum)))
                             (let* ((is-holding (= (<ra> :get-velocity-logtype
                                                         (velocity-info :velocitynum)
                                                         (velocity-info :notenum)
                                                         (velocity-info :tracknum))
                                                   *logtype-hold*))
                                    (num-nodes (<ra> :get-num-velocities (velocity-info :notenum) (velocity-info :tracknum)))
                                    (is-last-node (= (velocity-info :velocitynum)
                                                     (1- num-nodes)))
                                    (note-spans-last-place (note-spans-last-place (velocity-info :notenum)
                                                                                  (velocity-info :tracknum))))
                               '(c-display "place" (<ra> :get-velocity-place
                                                        (velocity-info :velocitynum)
                                                        (velocity-info :notenum)
                                                        (velocity-info :tracknum))
                                          (-line (<ra> :get-num-lines)))
                               (popup-menu "Delete Velocity" delete-velocity!
                                           (list "glide"
                                                 :check (and (not is-holding)
                                                             (not is-last-node))
                                                 :enabled (not is-last-node)
                                                 (lambda (maybe)
                                                   (if maybe
                                                       (set-linear!)
                                                       (set-hold!))))
                                           (list "continue playing note into next block"
                                                 :check (and note-spans-last-place
                                                             (<ra> :note-continues-next-block (velocity-info :notenum) (velocity-info :tracknum)))
                                                 :enabled note-spans-last-place
                                                 (lambda (maybe)
                                                   (<ra> :set-note-continue-next-block maybe (velocity-info :notenum) (velocity-info :tracknum))))))
                             #t)
                           #f))))))


#||
;; show current velocity
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (and *current-track-num*
              (inside-box-forgiving (<ra> :get-box track *current-track-num*) X Y)
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
(<ra> :get-num-velocities 0 0)

(<ra> :get-velocitynode-y 0 0)
(<ra> :get-velocitynode-y 2 0)
(<ra> :get-velocity-value 7 1)



||#


;; track borders
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct trackwidth-info
  :tracknum
  :width
  :y)

(define (may-be-a-resize-point-in-track X Y Tracknum)
  (and (>= X (- (<ra> :get-track-x1 Tracknum)
                2))
       (<= X (+ (<ra> :get-track-x1 Tracknum)
                (<ra> :get-half-of-node-width)))
       (>= Y (+ 4 (<ra> :get-track-volume-on-off-y2)))))
                

(define-match get-resize-point-track
  X _ Tracknum :> (and (>= X (- (<ra> :get-track-fx-x2 (1- Tracknum))
                                2))
                       Tracknum) :where (= Tracknum (<ra> :get-num-tracks)) ;; i.e. to the right of the rightmost track
  X _ Tracknum :> #f       :where (> (<ra> :get-track-x1 Tracknum) X)
  X Y Tracknum :> Tracknum :where (may-be-a-resize-point-in-track X Y Tracknum)
  X Y Tracknum :> (get-resize-point-track X Y (1+ Tracknum)))

(define (get-trackwidth-info X Y)
  (and (inside-box (<ra> :get-box editor) X Y)
       (begin
         (define resize-point-track (get-resize-point-track X Y 0))
         (and resize-point-track
              (let ((tracknum (1- resize-point-track)))
                (make-trackwidth-info :tracknum tracknum
                                      :width    (<ra> :get-track-width tracknum)
                                      :y        Y))))))

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
                     (<ra> :set-track-width new-width tracknum)
                     (make-trackwidth-info :tracknum tracknum
                                             :width    new-width)))

||#

(add-horizontal-handler :Get-handler-data get-trackwidth-info
                        :Get-x1 (lambda (Trackwidth-info)
                                  (<ra> :get-track-fx-x1 (Trackwidth-info :tracknum)))
                        :Get-x2 (lambda (Trackwidth-info)
                                  (+ 10000
                                     (<ra> :get-track-fx-x1 (Trackwidth-info :tracknum))))
                        :Get-min-value (lambda (_)
                                         0)
                        :Get-max-value (lambda (_)
                                         10000)
                        :Get-x (lambda (Trackwidth-info)
                                 (define tracknum (Trackwidth-info :tracknum))
                                 (if (= tracknum (1- (<ra> :get-num-tracks)))
                                     (<ra> :get-track-fx-x2 tracknum)
                                     (<ra> :get-track-x1 (1+ tracknum))))
                        :Get-value (lambda (Trackwidth-info)
                                     (Trackwidth-info :width))
                        :Make-undo (lambda (_)
                                     (<ra> :undo-track-width))
                        :Move (lambda (Trackwidth-info Value)
                                (define tracknum (Trackwidth-info :tracknum))
                                (<ra> :set-track-width Value tracknum))
                        :Publicize (lambda (_)
                                     #f))
                        
                        

;; switch track note area width
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     *current-track-num*
                     (inside-box (<ra> :get-box track-notes *current-track-num*) X Y)
                     (<ra> :change-track-note-area-width *current-track-num*)
                     (<ra> :select-track *current-track-num*)
                     #f))))


;; fxnodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-fxnode-box Tracknum FX-num FX-nodenum)
  (get-common-node-box (<ra> :get-fxnode-x FX-nodenum FX-num Tracknum)
                       (<ra> :get-fxnode-y FX-nodenum FX-num Tracknum)))

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
  (define fxnode-y (<ra> :get-fxnode-y (Fi :fxnodenum) (Fi :fxnum) (Fi :tracknum)))
  (cond ((and (= 0
                 (Fi :fxnodenum))
              (> Y
                 fxnode-y))
         10)
        ((and (= (1- (<ra> :get-num-fxnodes (Fi :fxnum) (Fi :tracknum)))
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
                                                                                                              :value (<ra> :get-fxnode-value Fxnodenum Fxnum Tracknum)
                                                                                                              :y (box :y)
                                                                                                              )))))))

(define-match get-fxnode-1
  X Y Tracknum Fxnum Fxnum     :> #f
  X Y Tracknum Fxnum Total-Fxs :> (highest-rated-fxnode-info Y
                                                             (list (get-fxnode-1 X Y Tracknum (1+ Fxnum) Total-Fxs)
                                                                   (get-fxnode-2 X Y Tracknum Fxnum 0 (<ra> :get-num-fxnodes Fxnum Tracknum)))))


(define-match get-fxnode-0
  X Y -1       :> #f
  X Y Tracknum :> #f :where (>= Tracknum (<ra> :get-num-tracks))
  X Y Tracknum :> (get-fxnode-1 X Y Tracknum 0 (<ra> :get-num-fxs Tracknum)))

(define-match get-fxnode-info
  X Y #f       :> (get-fxnode-info X Y 0)
  X Y Tracknum :> (highest-rated-fxnode-info Y
                                             (list (get-fxnode-0 X Y (1- Tracknum)) ;; node in the prev track can overlap into the current track
                                                   (get-fxnode-0 X Y Tracknum))))


#||
(<ra> :get-num-fxs 0)
(let ((node (get-fxnode-info 347 211 0)))
  (c-display "hm?" node)
  (if node
      (c-display "node:" (node :fxnodenum) "value:" (node :value))))
        
(<ra> :get-fxnode-x 0 0 0)
(<ra> :get-fxnode-y 0 0 0)
(<ra> :get-fxnode-value 0 0 0)
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
                                                              (<ra> :get-fxnode-x Nodenum Fx *current-track-num*)
                                                              (<ra> :get-fxnode-y Nodenum Fx *current-track-num*)))

(define-match get-closest-fx-0
  Fx Fx        _ _ :> #f
  Fx Total-Fxs X Y :> (min-fx/distance (get-closest-fx-1 2
                                                         (<ra> :get-num-fxnodes Fx *current-track-num*)
                                                         Fx
                                                         X Y
                                                         (<ra> :get-fxnode-x 0 Fx *current-track-num*)
                                                         (<ra> :get-fxnode-y 0 Fx *current-track-num*)
                                                         (<ra> :get-fxnode-x 1 Fx *current-track-num*)
                                                         (<ra> :get-fxnode-y 1 Fx *current-track-num*))
                                       (get-closest-fx-0 (1+ Fx)
                                                         Total-Fxs
                                                         X
                                                         Y)))
                                                                


(define (get-closest-fx X Y)
  (get-closest-fx-0 0 (<ra> :get-num-fxs *current-track-num*) X Y))

#||
(<ra> :get-num-fxs 0)
||#

;; add and move fxnode
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (<ra> :get-box track-fx *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (let ((fxnode-info (get-fxnode-info X Y *current-track-num*)))
                                                    (and fxnode-info
                                                         (callback fxnode-info (fxnode-info :value) (fxnode-info :y)))))
                        :Get-min-value (lambda (fxnode-info)
                                         (<ra> :get-fx-min-value (fxnode-info :fxnum)))
                        :Get-max-value (lambda (fxnode-info)
                                         (<ra> :get-fx-max-value (fxnode-info :fxnum)))
                        :Get-x (lambda (info) (<ra> :get-fxnode-x (info :fxnodenum)
                                                               (info :fxnum)
                                                               (info :tracknum)))
                        :Get-y (lambda (info) (<ra> :get-fxnode-y (info :fxnodenum)
                                                               (info :fxnum)
                                                               (info :tracknum)))
                        :Make-undo (lambda (_) (<ra> :undo-fxs *current-track-num*))
                        :Create-new-node (lambda (X Place callback)
                                           (define Fxnum (get-current-fxnum))
                                           (and Fxnum
                                                (begin
                                                  (define Value (scale X
                                                                       (<ra> :get-track-fx-x1 *current-track-num*) (<ra> :get-track-fx-x2 *current-track-num*)
                                                                       (<ra> :get-fx-min-value Fxnum) (<ra> :get-fx-max-value Fxnum)))
                                                  (define Nodenum (<ra> :add-fxnode Value Place Fxnum *current-track-num*))
                                                  (if (= -1 Nodenum)
                                                      #f
                                                      (callback (make-fxnode-info :tracknum *current-track-num*
                                                                                  :fxnum Fxnum
                                                                                  :fxnodenum Nodenum
                                                                                  :value Value
                                                                                  :y #f ;; dont need it.
                                                                                  )
                                                                (<ra> :get-fxnode-value Nodenum Fxnum *current-track-num*))))))
                        :Publicize (lambda (fxnode-info)
                                     (set-indicator-fxnode (fxnode-info :fxnodenum)
                                                           (fxnode-info :fxnum)
                                                           (fxnode-info :tracknum))
                                     (set-editor-statusbar (<ra> :get-fx-string (fxnode-info :fxnodenum) (fxnode-info :fxnum) (fxnode-info :tracknum))))

                        :Move-node (lambda (fxnode-info Value Place)                                     
                                     (<ra> :set-fxnode (fxnode-info :fxnodenum) Value Place (fxnode-info :fxnum) (fxnode-info :tracknum))
                                     fxnode-info)
                        )

;; Delete fx node (shift + right mouse)
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button X Y)
                (and (= $button *right-button*)
                     (<ra> :shift-pressed)
                     *current-track-num*
                     (inside-box-forgiving (<ra> :get-box track *current-track-num*) X Y)
                     (begin
                       (define fxnode-info (get-fxnode-info X Y *current-track-num*))
                       (and fxnode-info
                            (begin
                              (<ra> :undo-fxs *current-track-num*)
                              (<ra> :delete-fxnode
                                    (fxnode-info :fxnodenum)
                                    (fxnode-info :fxnum)
                                    (fxnode-info :tracknum))
                              #t)))))))

;; fx popup
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     *current-track-num*
                     (inside-box-forgiving (<ra> :get-box track *current-track-num*) X Y)
                     (begin
                       (define fxnode-info (get-fxnode-info X Y *current-track-num*))
                       ;;(c-display "got fx info " fxnode-info)
                       (if fxnode-info
                           (begin
                             (define (delete-node!)
                               (<ra> :undo-fxs *current-track-num*)
                               (<ra> :delete-fxnode
                                     (fxnode-info :fxnodenum)
                                     (fxnode-info :fxnum)
                                     (fxnode-info :tracknum)))
                             (define (set-hold!)
                               (<ra> :undo-fxs *current-track-num*)
                               (<ra> :set-fxnode-logtype
                                     *logtype-hold*
                                     (fxnode-info :fxnodenum)
                                     (fxnode-info :fxnum)
                                     (fxnode-info :tracknum)))
                             (define (set-linear!)
                               (<ra> :undo-fxs *current-track-num*)
                               (<ra> :set-fxnode-logtype
                                     *logtype-linear*
                                     (fxnode-info :fxnodenum)
                                     (fxnode-info :fxnum)
                                     (fxnode-info :tracknum)))
                             (let* ((is-holding (= (<ra> :get-fxnode-logtype
                                                         (fxnode-info :fxnodenum)
                                                         (fxnode-info :fxnum)
                                                         (fxnode-info :tracknum))
                                                  *logtype-hold*))
                                    (num-nodes (<ra> :get-num-fxnodes (fxnode-info :fxnum) (fxnode-info :tracknum)))
                                    (is-last (= (fxnode-info :fxnodenum)
                                                (1- num-nodes))))
                               (popup-menu "Delete Node" delete-node!
                                           (list "glide"
                                                 :check (and (not is-holding) (not is-last))
                                                 :enabled (not is-last)
                                                 (lambda (maybe)
                                                   (if maybe
                                                       (set-linear!)
                                                       (set-hold!)))))
                               )
                             #t)
                           #f))))))

;; add fx
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     *current-track-num*
                     (inside-box (<ra> :get-box track-fx *current-track-num*) X Y)
                     (<ra> :select-track *current-track-num*)
                     (<ra> :request-fx *current-track-num*)
                     #t))))


(define (get-full-fx-name fxnum tracknum)
  (define fxname (<ra> :get-fx-name fxnum tracknum))
  (define trackinstrument_id (<ra> :get-instrument-for-track tracknum))
  (define fxinstrument_id (<ra> :get-fx-instrument fxnum tracknum))
  (if (= trackinstrument_id fxinstrument_id)
      fxname
      (<-> fxname " (" (<ra> :get-instrument-name fxinstrument_id) ")")))


;; Sub track text popup menues
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and *current-track-num*
                     (= Button *right-button*)
                     (cond ((and (<ra> :swingtext-visible *current-track-num*)
                                 (inside-box (<ra> :get-box swingtext *current-track-num*) X Y))
                            (popup-menu (swingtext-popup-elements))
                            #t)
                           ((and (<ra> :centtext-visible *current-track-num*)
                                 (inside-box (<ra> :get-box centtext *current-track-num*) X Y))
                            (popup-menu (centtext-popup-elements))
                            #t)
                           ((and (<ra> :chancetext-visible *current-track-num*)
                                 (inside-box (<ra> :get-box chancetext *current-track-num*) X Y))
                            (popup-menu (chancetext-popup-elements))
                            #t)
                           ((and (<ra> :veltext-visible *current-track-num*)
                                 (inside-box (<ra> :get-box velocitytext *current-track-num*) X Y))
                            (popup-menu (velocitytext-popup-elements))
                            #t)
                           ((and (<ra> :fxtext-visible *current-track-num*)
                                 (inside-box (<ra> :get-box fxtext *current-track-num*) X Y))
                            (popup-menu (fxtext-popup-elements))
                            #t)
                           (else
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
                (inside-box-forgiving (<ra> :get-box track *current-track-num*) X Y)
                (lazy
                  (define-lazy velocity-info (get-velocity-info X Y *current-track-num*))
                  (define-lazy fxnode-info (get-fxnode-info X Y *current-track-num*))
                  
                  (define-lazy velocity-dist (get-shortest-velocity-distance X Y))
                  (define-lazy fx-dist (get-closest-fx X Y))
                  
                  (define-lazy is-in-fx-area (inside-box (<ra> :get-box track-fx *current-track-num*) X Y))
                  
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

                  ;;(c-display "curr" *current-track-num* *current-track-num-all-tracks*)
                  
                  (cond ((and (<ra> :swingtext-visible *current-track-num*)
                              (inside-box (<ra> :get-box swingtext *current-track-num*) X Y))
                         (set-editor-statusbar (<-> "Swing text for track #" *current-track-num*)))
                        ((and (<ra> :centtext-visible *current-track-num*)
                              (inside-box (<ra> :get-box centtext *current-track-num*) X Y))
                         (set-editor-statusbar (<-> "Cent text for track #" *current-track-num*)))
                        ((and (<ra> :chancetext-visible *current-track-num*)
                              (inside-box (<ra> :get-box chancetext *current-track-num*) X Y))
                         (set-editor-statusbar (<-> "Chance text for track #" *current-track-num*)))
                        ((and (<ra> :veltext-visible *current-track-num*)
                              (inside-box (<ra> :get-box velocitytext *current-track-num*) X Y))
                         (set-editor-statusbar (<-> "Velocity text for track #" *current-track-num*)))
                        ((and (<ra> :fxtext-visible *current-track-num*)
                              (inside-box (<ra> :get-box fxtext *current-track-num*) X Y))
                         (define instrument-id (<ra> :get-instrument-for-track  *current-track-num*))
                         (when (>= instrument-id 0)
                           (define effect-num (<ra> :get-fxtext-effect-num-from-x X *current-track-num*))
                           (when (>= effect-num 0)
                             (define effect-name (<ra> :get-instrument-effect-name effect-num instrument-id))
                             (set-editor-statusbar (<-> "FX text \"" effect-name "\", track #" *current-track-num*)))))
                        (velocity-info
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
                         (set-mouse-pointer ra:set-horizontal-split-mouse-pointer (<gui> :get-editor-gui)))
                        
                        ((and is-in-fx-area velocity-dist-is-shortest)
                         (set-mouse-note *current-note-num* *current-track-num*))
                        
                        ((and is-in-fx-area fx-dist-is-shortest)
                         (set! *current-fx/distance* fx-dist)
                         (set-editor-statusbar (get-full-fx-name (fx-dist :fx) *current-track-num*)) ;; TODO: Also write fx value at mouse position.
                         (set-mouse-fx (fx-dist :fx) *current-track-num*)
                         )
                      
                        (else
                         #f)))))
         (if (or (not result)
                 (not resize-mouse-pointer-is-set))
             (if (and (> Y (<ra> :get-block-header-y2))
                      (< Y (<ra> :get-reltempo-slider-y1))
                      *current-track-num*
                      (or (not (<ra> :pianoroll-visible *current-track-num*))
                          (not (inside-box (<ra> :get-box track-pianoroll *current-track-num*) X Y))))
                 (begin
                   ;;(c-display "normal3")
                   (<ra> :set-normal-mouse-pointer (<gui> :get-editor-gui)))))
         result))



;; move tracker cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and ;(= Button *middle-button*)
                 (inside-box (<ra> :get-box editor) X Y)
                 *current-track-num*
                 (<ra> :select-track *current-track-num*)
                 #t))))


(define (show-global-swing-track-popup-menu)
  (c-display "global swing track popup menu")
  (popup-menu "Hide swing track" ra:show-hide-swing-track
              (list "Swing help" (lambda ()
                                   (<ra> :show-swing-help-window)))))

(define (show-bars-and-beats-or-line-numbers-popup-menu)
  (define gui (<gui> :popup))
  (let ((layout (<gui> :vertical-layout
                       (<gui> :radiobutton "Show bars and beats" (not (<ra> :linenumbers-visible))
                              (lambda (is-checked)
                                (when (and is-checked (<ra> :linenumbers-visible))
                                  (<ra> :set-linenumbers-visible #f)
                                  (<gui> :close gui)
                                  (c-display "show barsandobaetsa"))))
                       (<gui> :radiobutton "Show line numbers" (<ra> :linenumbers-visible)
                              (lambda (is-checked)
                                (c-display "show line numbers")
                                (when (and is-checked (not (<ra> :linenumbers-visible)))
                                  (<ra> :set-linenumbers-visible #t)
                                  (<gui> :close gui)))))))
    (<gui> :add gui layout)
    (<gui> :show gui)
    ))
                                              

;; show/hide time tracks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     *current-track-num-all-tracks*
                     (>= Y (<ra> :get-block-header-y2))
                     (< Y (<ra> :get-reltempo-slider-y1))
                     (cond ((= *current-track-num-all-tracks* (<ra> :get-rel-tempo-track-num))
                            (c-display "reltempo")
                            (popup-menu "hide tempo multiplier track" ra:show-hide-reltempo-track))
                           ((= *current-track-num-all-tracks* (<ra> :get-tempo-track-num))
                            (c-display "tempo")
                            (popup-menu "hide BPM track" ra:show-hide-bpm-track))
                           ((= *current-track-num-all-tracks* (<ra> :get-lpb-track-num))
                            (c-display "lpb")
                            (popup-menu "hide LPB track" ra:show-hide-lpb-track))
                           ((= *current-track-num-all-tracks* (<ra> :get-signature-track-num))
                            (c-display "signature")
                            (popup-menu "hide time signature track" ra:show-hide-signature-track))
                           ((= *current-track-num-all-tracks* (<ra> :get-swing-track-num))
                            (show-global-swing-track-popup-menu))
                           ((= *current-track-num-all-tracks* (<ra> :get-linenum-track-num))
                            (show-bars-and-beats-or-line-numbers-popup-menu))
                           (else
                            (c-display "nothing")))
                     #t))))



;; seqtrack / seqblock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (for-each-seqblock func)
  (let loop ((seqblocknum 0)
             (seqtracknum 0))
    (cond ((= seqtracknum (<ra> :get-num-seqtracks))
           #t)
          ((= seqblocknum (<ra> :get-num-seqblocks seqtracknum))
           (loop 0 (1+ seqtracknum)))
          (else
           (define ret (func seqtracknum seqblocknum))
           (if (and (pair? ret) (pair? (cdr ret)) (eq? 'stop (car ret)) (null? (cddr ret)))
               (cadr ret)
               (loop (1+ seqblocknum) seqtracknum))))))

(define (for-each-selected-seqblock func)
  (for-each-seqblock (lambda (seqtracknum seqblocknum)
                       (when (<ra> :is-seqblock-selected seqblocknum seqtracknum)
                         ;;(c-display "funcing" seqtracknum seqblocknum)
                         (func seqtracknum seqblocknum)))))
  


;; sequencer tempo automation
;;
(define (get-seqtemponode-box $num)
  (get-common-node-box (<ra> :get-seqtemponode-x $num)
                       (<ra> :get-seqtemponode-y $num)))

(define (get-seqtempo-value Y)
  (define max-tempo (<ra> :get-seqtempo-max-tempo))
  (define y1 (<ra> :get-seqtempo-area-y1))
  (define y2 (<ra> :get-seqtempo-area-y2))
  (define mid (/ (+ y1 y2) 2))
  ;;(c-display Y y1 y2 max-tempo (<= Y mid))
  (if (<= Y mid)
      (scale Y y1 mid max-tempo 1)
      (scale Y mid y2 1 (/ 1 max-tempo))))

(define (get-highest-seqtemp-value)
  (apply max (map (lambda (n)
                    (<ra> :get-seqtempo-value n))
                  (iota (<ra> :get-num-seqtemponodes)))))

(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and (<ra> :seqtempo-visible)
                                             (<ra> :get-box seqtempo-area)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (match (list (find-node-horizontal X Y get-seqtemponode-box (<ra> :get-num-seqtemponodes)))
                                                         (existing-box Num Box) :> (begin
                                                                                     ;;(c-display "EXISTING " Num)
                                                                                     (define Time (scale X
                                                                                                         (<ra> :get-seqtempo-area-x1) (<ra> :get-seqtempo-area-x2)
                                                                                                         (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                                                                                     (set-grid-type #t)
                                                                                     (callback Num Time Y))
                                                         _                      :> #f))
                        :Get-min-value (lambda (_)
                                         (<ra> :get-sequencer-visible-start-time))
                        :Get-max-value (lambda (_)
                                         (<ra> :get-sequencer-visible-end-time))
                        :Get-x (lambda (Num) (<ra> :get-seqtemponode-x Num))
                        :Get-y (lambda (Num) (<ra> :get-seqtemponode-y Num))
                        :Make-undo (lambda (_)              
                                     (<ra> :undo-seqtempo))
                        :Create-new-node (lambda (X Y callback)

                                           (define Time (scale X
                                                               (<ra> :get-seqtempo-area-x1) (<ra> :get-seqtempo-area-x2)
                                                               (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                                           (define PositionTime (if (<ra> :control-pressed)
                                                                    Time
                                                                    (<ra> :get-seq-gridded-time (floor Time) 0 (<ra> :get-seq-tempo-grid-type))))
                                           (define TempoMul (get-seqtempo-value Y))
                                           (define Num (<ra> :add-seqtemponode PositionTime TempoMul *logtype-linear*))
                                           (if (= -1 Num)
                                               #f
                                               (begin
                                                 (set-grid-type #t)
                                                 (callback Num Time))))
                        :Release-node (lambda (Num)
                                        (set-grid-type #f))
                        :Move-node (lambda (Num Time Y)
                                     (define TempoMul (get-seqtempo-value Y))
                                     (define logtype (<ra> :get-seqtempo-logtype Num))
                                     (if (not (<ra> :control-pressed))
                                         (set! Time (<ra> :get-seq-gridded-time (floor Time) 0 (<ra> :get-seq-tempo-grid-type))))
                                     (<ra> :set-seqtemponode Time TempoMul logtype Num)
                                     ;;(c-display "NUM:" Num ", Time:" Time ", TempoMul:" TempoMul)
                                     Num
                                     )
                        :Publicize (lambda (Num) ;; this version works though. They are, or at least, should be, 100% functionally similar.
                                     (set-editor-statusbar (<-> "Tempo: " (two-decimal-string (<ra> :get-seqtempo-value Num))))
                                     (<ra> :set-curr-seqtemponode Num)
                                     #f)
                        
                        :Use-Place #f
                        :Mouse-pointer-func ra:set-normal-mouse-pointer
                        :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                        :Get-pixels-per-value-unit #f
                        )                        


;; delete seqtemponode / popupmenu
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     (<ra> :seqtempo-visible)                     
                     (inside-box (<ra> :get-box seqtempo-area) $x $y)
                     (begin
                       (define Num (match (list (find-node-horizontal $x $y get-seqtemponode-box (<ra> :get-num-seqtemponodes)))
                                          (existing-box Num Box) :> Num
                                          _                      :> #f))
                       (if (<ra> :shift-pressed)
                           (when Num
                             ;;(c-display "  Deleting" Num)
                             (<ra> :undo-seqtempo)
                             (<ra> :delete-seqtemponode Num))
                           (popup-menu (list "Delete"
                                             :enabled (and Num (> Num 0) (< Num (1- (<ra> :get-num-seqtemponodes))))
                                             (lambda ()
                                               (<ra> :undo-seqtempo)
                                               (<ra> :delete-seqtemponode Num)))
                                       (list "Reset (set value to 1.0)"
                                             :enabled Num
                                             (lambda ()
                                               (<ra> :undo-seqtempo)
                                               (<ra> :set-seqtemponode
                                                     (<ra> :get-seqtempo-abstime Num)
                                                     1.0
                                                     (<ra> :get-seqtempo-logtype Num)
                                                     Num)))
                                       (list "Glide to next break point"
                                             :check (and Num (= (<ra> :get-seqtempo-logtype Num)
                                                                *logtype-linear*))
                                             :enabled Num
                                             (lambda (maybe)
                                               (<ra> :undo-seqtempo)
                                               (<ra> :set-seqtemponode
                                                     (<ra> :get-seqtempo-abstime Num)
                                                     (<ra> :get-seqtempo-value Num)
                                                     (if maybe *logtype-linear* *logtype-hold*)
                                                     Num)))
                                       (list "Set maximum tempo"
                                             (lambda ()
                                               (define highest (get-highest-seqtemp-value))
                                               (define now (<ra> :get-seqtempo-max-tempo))
                                               (define new (<ra> :request-float (<-> "Max tempo automation value (now: "
                                                                                     (two-decimal-string now) " ("
                                                                                     (two-decimal-string highest) "-1000000): ")
                                                                                highest
                                                                                1000000))
                                               (if (> new highest)
                                                   (<ra> :set-seqtempo-max-tempo new))))


                                       (get-sequencer-conf-menues)
                                       ))
                       #t)))))

 
;; highlight current seqtemponode
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and (<ra> :seqtempo-visible)
              ;;(or (c-display "---" $x $y (box-to-string (<ra> :get-box seqtempo-area)) (inside-box-forgiving (<ra> :get-box seqtempo-area) $x $y)) #t)
              (inside-box-forgiving (<ra> :get-box seqtempo-area) $x $y)
              (match (list (find-node-horizontal $x $y get-seqtemponode-box (<ra> :get-num-seqtemponodes)))
                     (existing-box Num Box) :> (begin
                                                 ;;(c-display "hepp" Num)
                                                 (set-editor-statusbar (<-> "Tempo: " (two-decimal-string (<ra> :get-seqtempo-value Num))))
                                                 (<ra> :set-curr-seqtemponode Num)
                                                 #t)
                     A                      :> (begin
                                                 ;;(c-display "**Didnt get it:" A)
                                                 (<ra> :set-curr-seqtemponode -1)
                                                 #f)))))



;; sequencer timeline
;;


(define (get-sequencer-x time)
  (scale time
         (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)
         (<ra> :get-seqtimeline-area-x1) (<ra> :get-seqtimeline-area-x2)))

(define (set-statusbar-looppunch-info name getter)
  (set-editor-statusbar (<-> name " : " (two-decimal-string (/ (getter) (<ra> :get-sample-rate))))))

;; highlight loop start / loop end
(define (create-seqlooppunch-highlighter name in-use? get-start get-end)
  (add-mouse-move-handler
   :move (lambda ($button $x $y)
           (and (in-use?)
                (inside-box (<ra> :get-box seqtimeline-area) $x $y)
                (let* ((start-x (get-sequencer-x (get-start)))
                       (end-x (get-sequencer-x (get-end)))
                       (mid (average start-x end-x)))
                  (apply set-statusbar-looppunch-info (if (< $x mid)
                                                          (list (<-> name " start") get-start)
                                                          (list (<-> name " end") get-end)))
                  #t)))))


;; Set loop start and end and set cursor pos
(define (create-seqlooppunch-mouse-handler base-name in-use? get-start get-end set-start! set-end!)
  (define gakkgakk-seqloop-handler-num-moves 0)
  (define gakkgakk-seqloop-handler-start-X #f)
  (define getter #f)
  (define setter! #f)
  (define name #f)
  (add-horizontal-handler :Get-handler-data (lambda (X Y)
                                              (and (in-use?)
                                                   (inside-box (<ra> :get-box seqtimeline-area) X Y)
                                                   (let* ((start-x (get-sequencer-x (get-start)))
                                                          (end-x (get-sequencer-x (get-end)))
                                                          (mid (average start-x end-x)))
                                                     (set-grid-type #t)
                                                     (set! gakkgakk-seqloop-handler-num-moves 0)
                                                     (set! gakkgakk-seqloop-handler-start-X X)
                                                     (if (< X mid)
                                                         (begin
                                                           (set! name (<-> base-name " start"))
                                                           (set! getter get-start)
                                                           (set! setter! set-start!))
                                                         (begin
                                                           (set! name (<-> base-name " end"))
                                                           (set! getter get-end)
                                                           (set! setter! set-end!)))
                                                     (getter))))
                          :Get-x1 (lambda (_)
                                    (<ra> :get-seqtimeline-area-x1))
                          :Get-x2 (lambda (_)
                                    (<ra> :get-seqtimeline-area-x2))
                          :Get-min-value (lambda (_)
                                           (<ra> :get-sequencer-visible-start-time))
                          :Get-max-value (lambda (_)
                                           (<ra> :get-sequencer-visible-end-time))
                          :Get-value (lambda (Value)
                                       Value)
                          :Release (lambda ()
                                     (set-grid-type #f)
                                     (when (= 1 gakkgakk-seqloop-handler-num-moves)
                                       ;;(c-display "Released " gakkgakk-seqloop-handler-start-X)
                                       (define time (scale gakkgakk-seqloop-handler-start-X
                                                           (<ra> :get-seqtimeline-area-x1)
                                                           (<ra> :get-seqtimeline-area-x2)
                                                           (<ra> :get-sequencer-visible-start-time)
                                                           (<ra> :get-sequencer-visible-end-time)))
                                       (if (not (<ra> :control-pressed))
                                           (set! time (<ra> :get-seq-gridded-time (floor time) 0 (<ra> :get-seq-loop-grid-type))))
                                       (<ra> :play-song-from-pos (floor time))))
                          :Make-undo (lambda (_)
                                       50)
                          :Move (lambda (_ Value)
                                  ;;(c-display "Value: " Value)
                                  (inc! gakkgakk-seqloop-handler-num-moves 1)
                                  (set! Value (floor Value))
                                  (if (not (<ra> :control-pressed))
                                      (set! Value (<ra> :get-seq-gridded-time Value 0 (<ra> :get-seq-loop-grid-type))))
                                  (setter! Value))
                          
                          :Publicize (lambda (Value)
                                       (set-statusbar-looppunch-info name getter))
                          
                          :Mouse-pointer-func ra:set-normal-mouse-pointer
                          :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                          ))

(define (create-seqlooppunch-mouse-handlers name in-use? get-start get-end set-start! set-end!)
  (create-seqlooppunch-highlighter   name in-use? get-start get-end)
  (create-seqlooppunch-mouse-handler name in-use? get-start get-end set-start! set-end!))
  
(define (show-timeline-looping?)
  (not (<ra> :is-seqpunching)))

(create-seqlooppunch-mouse-handlers "Loop"  show-timeline-looping? ra:get-seqlooping-start   ra:get-seqlooping-end  ra:set-seqlooping-start   ra:set-seqlooping-end)
(create-seqlooppunch-mouse-handlers "Punch" ra:is-seqpunching      ra:get-seqpunching-start  ra:get-seqpunching-end ra:set-seqpunching-start  ra:set-seqpunching-end)



(define (get-sequencer-conf-menues)
  (list 
        "-------- Timeline"
        (list
         :radio-buttons
         (list "Free"
               :check (and (not (<ra> :is-seqlooping))
                           (not (<ra> :is-seqpunching)))
               (lambda (val)
                 (c-display "new no-looping-or-punch:" val)))
         (list "Looping"
               :check (<ra> :is-seqlooping)
               (lambda (val)
                 (<ra> :set-seqlooping val)))
         (list "Punch in/out (recording)"
               :check (<ra> :is-seqpunching)
               (lambda (val)
                 (<ra> :set-seqpunching val)
                 (c-display "new punch in/out:" val))))
        "------- Configuration" ;;Various"
        (list "Show song tempo automation"
              :check (<ra> :seqtempo-visible)
              (lambda (doit)
                (<ra> :set-seqtempo-visible doit)))
        (list "Preferences"
              (lambda ()
                (<ra> :open-sequencer-preferences-dialog)))))

(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda (Button X Y)                                
                                (if (and (= Button *right-button*)
                                         (inside-box (<ra> :get-box seqtimeline-area) X Y))
                                    (begin
                                      (popup-menu (append
                                                   (list "-------- Time format" ;;Display bars and beats"
                                                         (list
                                                          :radio-buttons
                                                          (list "Bars and beats"
                                                                :check (<ra> :show-bars-in-timeline)
                                                                (lambda (val)
                                                                  (<ra> :set-show-bars-in-timeline val)))
                                                          (list "HH:MM:SS"
                                                                :check (not (<ra> :show-bars-in-timeline))
                                                                (lambda (val)
                                                                  (<ra> :set-show-bars-in-timeline (not val))))))
                                                   (get-sequencer-conf-menues)))
                                      #t)
                                    #f))))


#||
(/ (<ra> :get-seqlooping-start) (<ra> :get-sample-rate))
(/ (<ra> :get-seqlooping-end) (<ra> :get-sample-rate))
(<ra> :set-seqlooping-end 500000)
||#




;;;;;;;;;;; Seqblock copy / paste / cut / delete / move /etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-seqblock seqblock new-start-time)
  (define duration (- (seqblock :end-time)
                      (seqblock :start-time)))
  (copy-hash seqblock
             :start-time new-start-time
             :end-time (+ new-start-time duration)))

(define (move-seqblock2 seqblock delta-time delta-tracknum)
  (define duration (- (seqblock :end-time)
                      (seqblock :start-time)))
  (copy-hash seqblock
             :start-time (+ (seqblock :start-time) delta-time)
             :end-time (+ (seqblock :end-time) delta-time)
             :seqtracknum (+ (seqblock :seqtracknum) delta-tracknum)))

(define2 *current-seqtrack-num* (curry-or not integer?) #f)

(define (get-seqtracknum X Y)
  (define num-seqtracks (<ra> :get-num-seqtracks))
  (let loop ((seqtracknum 0))
    (cond ((= seqtracknum num-seqtracks)
           #f) ;;seqtracknum)
          ((inside-box (ra:get-box2 seqtrack seqtracknum) X Y)
           seqtracknum)
          (else
           (loop (1+ seqtracknum))))))

;; Update current seqtrack num.
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (let ((old *current-seqtrack-num*)
               (new (get-seqtracknum X Y)))
           ;;(c-display "old/new" old new)
           (cond ((and old (not new))
                  (set! *current-seqtrack-num* #f))
                 ((or (and new (not old))
                      (not (morally-equal? new old)))
                  ;;(c-display "set-normal")
                  ;;(<ra> :set-normal-mouse-pointer)
                  (set! *current-seqtrack-num* new))
                 (else
                  #f)))))




(define (num-seqblocks-in-sequencer)
  (apply +
         (map (lambda (seqtracknum)
                (<ra> :get-num-seqblocks seqtracknum))
              (iota (<ra> :get-num-seqtracks)))))

(define (set-grid-type doit)
  (if doit
      (<ra> :set-sequencer-grid-type 1)
      (<ra> :set-sequencer-grid-type 0)))
      
(define (only-select-one-seqblock dasseqblocknum dasseqtracknum)
  (for-each-seqblock (lambda (seqtracknum seqblocknum)
                       (define should-be-selected (and (= seqtracknum dasseqtracknum)
                                                       (= seqblocknum dasseqblocknum)))
                       (<ra> :select-seqblock should-be-selected seqblocknum seqtracknum))))

(define (delete-all-gfx-gfx-seqblocks)
  (for-each (lambda (seqtracknum)
              (while (> (<ra> :get-num-gfx-gfx-seqblocks seqtracknum) 0)
                (<ra> :delete-gfx-gfx-seqblock 0 seqtracknum)))
            (iota (<ra> :get-num-seqtracks))))

(define2 gakkgakk-last-inc-time number? 0)
(define2 gakkgakk-really-last-inc-time number? 0)
(define2 gakkgakk-last-inc-track number? 0)

(define (get-data-for-seqblock-moving seqblock-infos inc-time inc-track)
  (define num-seqtracks (<ra> :get-num-seqtracks))

  (define skew #f) ;; We want the same skew for all blocks. Use skew for the first block, i.e. the uppermost leftmost one.
  
  (map (lambda (seqblock-info)
         (define seqtracknum (seqblock-info :seqtracknum))
         (define seqblocknum (seqblock-info :seqblocknum))
         (define seqblock (<ra> :get-seqblock-state seqblocknum seqtracknum))
           
         (define start-time (seqblock :start-time))
         (define is-stretched (not (= (<ra> :get-seqblock-stretch seqblocknum seqtracknum) 1.0)))
         (define end-time (seqblock :end-time))
         ;;(c-display "is-stretched:" is-stretched ". end-time:" end-time)
         (define new-seqtracknum (+ seqtracknum inc-track))
         
         (if (and (>= new-seqtracknum 0)
                  (< new-seqtracknum num-seqtracks))
             (begin
               (define new-pos (floor (+ inc-time start-time)))
               
               (when (not skew)
                 (define new-pos2 (if (or (= 1 (num-seqblocks-in-sequencer))
                                          (<ra> :control-pressed))
                                      new-pos
                                      (<ra> :get-seq-gridded-time new-pos 0 (<ra> :get-seq-block-grid-type))))
                 (if (< new-pos2 0)
                     (set! new-pos2 0))
                 (set! skew (- new-pos2 new-pos))
                 (set! gakkgakk-really-last-inc-time (- new-pos2 start-time)))
               
               (define new-start-time (max 0 (+ skew new-pos)))

               (copy-hash seqblock
                          :seqtracknum new-seqtracknum
                          :start-time new-start-time
                          :end-time (if is-stretched
                                        (+ end-time (- new-start-time start-time))
                                        -1)))
             #f))
       seqblock-infos))

(define (create-gfx-gfx-seqblocks seqblock-infos inc-time inc-track)
  (if (< (+ gakkgakk-earliest-time inc-time) 0)
      (set! inc-time (- gakkgakk-earliest-time)))

  (set! gakkgakk-last-inc-time inc-time)
  (set! gakkgakk-last-inc-track inc-track)
  (for-each (lambda (seqblock)
              ;;(c-display "seqblock:" (pp seqblock))
              (if seqblock
                  (ra:create-gfx-gfx-seqblock seqblock)))
            (get-data-for-seqblock-moving seqblock-infos inc-time inc-track)))

(define (apply-gfx-gfx-seqblocks seqblock-infos)
  (define inc-time gakkgakk-last-inc-time)
  (define inc-track gakkgakk-last-inc-track)
  
  (define seqblocks (get-data-for-seqblock-moving seqblock-infos inc-time inc-track)) ;; Must do this before deleting the old seqblocks.

  (undo-block
   (lambda ()     
     (FROM_C-delete-all-selected-seqblocks) ;; Hack. This will only work if seqblock-infos contains the current set of selected blocks. (deleting from seqblock-infos is a little bit tricky since the seqblock indexes changes)
     (for-each (lambda (seqblock)
                 (if seqblock
                     (<ra> :create-seqblock-from-state seqblock)))
               ;;(apply-seqblock seqblock)))
               seqblocks))))

(define (get-sequencer-pixels-per-value-unit)
  (/ (- (<ra> :get-sequencer-x2)
        (<ra> :get-sequencer-x1))
     (- (<ra> :get-sequencer-visible-end-time)
        (<ra> :get-sequencer-visible-start-time))))




;; Change seqblock fade in / out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (create-fade-handler is-left)
  (add-node-mouse-handler :Get-area-box (lambda()
                                          (and *current-seqblock-info* ;;(get-current-seqblock-info)
                                               (not *current-seqautomation/distance*)
                                               (let ((box (if is-left
                                                              (<ra> :get-box seqblock-left-fade (*current-seqblock-info* :seqblocknum) (*current-seqblock-info* :seqtracknum))
                                                              (<ra> :get-box seqblock-right-fade (*current-seqblock-info* :seqblocknum) (*current-seqblock-info* :seqtracknum)))))
                                                 ;;(c-display "Gakk-BOX:" (box-to-string box))
                                                 ;;(<ra> :set-curr-seqblock seqblocknum seqtracknum)
                                                 box)))
                          :Get-existing-node-info (lambda (X Y callback)
                                                    ;;(c-display "Y:" Y)
                                                    
                                                    (define seqblock-info *current-seqblock-info*)
                                                    (define seqtracknum (seqblock-info :seqtracknum))
                                                    (define seqblocknum (seqblock-info :seqblocknum))
                                                    (define start-pos (if is-left
                                                                          (<ra> :get-seqblock-fade-in seqblocknum seqtracknum)
                                                                          (<ra> :get-seqblock-fade-out seqblocknum seqtracknum)))
                                                    (push-seqblock-to-top! seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))
                                                    (if is-left                                                        
                                                        (callback seqblock-info start-pos Y)
                                                        (callback seqblock-info (- 1 start-pos) Y)))

                          :Get-min-value (lambda (seqblock-info)
                                           0
                                           )
                          
                          :Get-max-value (lambda (seqblock-info)
                                           1)

                          :Get-x (lambda (info) #f)
                          :Get-y (lambda (info) #f)

                          :Make-undo (lambda (seqblock-info)
                                       (<ra> :undo-seqblock-fades (seqblock-info :seqblocknum) (seqblock-info :seqtracknum)))
                          
                          :Create-new-node (lambda (X seqtracknum callback)
                                             (assert #f)
                                             #f)
                          
                          :Release-node (lambda (seqblock-info)
                                          #f)

                          :Move-node (lambda (seqblock-info Value Y)
                                       (define seqtracknum (seqblock-info :seqtracknum))
                                       (define seqblocknum (seqblock-info :seqblocknum))
                                       
                                       ;;(set! Value (between 0 Value 1))

                                       (set-grid-type #t)

                                       (if is-left
                                           (<ra> :set-seqblock-fade-in Value seqblocknum seqtracknum)
                                           (<ra> :set-seqblock-fade-out (- 1 Value) seqblocknum seqtracknum))
                                       
                                       seqblock-info)

                          :Publicize (lambda (seqblock-info)
                                       (define seqtracknum (seqblock-info :seqtracknum))
                                       (define seqblocknum (seqblock-info :seqblocknum))
                                       (set-fade-status-bar is-left seqblocknum seqtracknum))

                          :Get-pixels-per-value-unit (lambda (seqblock-info)
                                                       (define seqtracknum (seqblock-info :seqtracknum))
                                                       (define seqblocknum (seqblock-info :seqblocknum))
                                                       (define dx (- (<ra> :get-seqblock-x2 seqblocknum seqtracknum)
                                                                     (<ra> :get-seqblock-x1 seqblocknum seqtracknum)))
                                                       dx)

                          :Use-Place #f

                          :Mouse-pointer-func ra:set-horizontal-resize-mouse-pointer
                          
                          :Get-guinum (lambda() (<gui> :get-sequencer-gui))
                          :Forgiving-box #f
                          ))

(create-fade-handler #f)
(create-fade-handler #t)

;; fade in/out popup menu
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     (not (<ra> :shift-pressed))
                     *current-seqblock-info*
                     (let* ((seqtracknum (*current-seqblock-info* :seqtracknum))
                            (seqblocknum (*current-seqblock-info* :seqblocknum))
                            (is-fade-in (inside-box (<ra> :get-box seqblock-left-fade seqblocknum seqtracknum) X Y))
                            (is-fade-out (inside-box (<ra> :get-box seqblock-right-fade seqblocknum seqtracknum) X Y)))
                       (and (or is-fade-in
                                is-fade-out)
                            (begin
                              (popup-menu (map (lambda (shape-name)
                                                 (list shape-name
                                                       :enabled (not (string=? shape-name (<ra> :get-seqblock-fade-shape is-fade-in seqblocknum seqtracknum)))
                                                       :icon (<ra> :get-fade-shape-icon-filename shape-name is-fade-in)
                                                       (lambda ()
                                                         (<ra> :undo-sequencer)
                                                         (<ra> :set-seqblock-fade-shape shape-name is-fade-in seqblocknum seqtracknum))))
                                               (<ra> :get-fade-shapes)))
                              #t)))))))





;; Change seqblock interior values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure seqblocks become too narrow 
(define (get-min-seqblock-end-interior min-num-pixels seqblock-info)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define start-time (<ra> :get-seqblock-interior-start seqblocknum seqtracknum))
  (floor (+ start-time (get-min-seqblock-duration min-num-pixels seqblock-info)))
  )

;; Make sure seqblocks become too narrow 
(define (get-max-seqblock-start-interior min-num-pixels seqblock-info)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define end-time (<ra> :get-seqblock-interior-end seqblocknum seqtracknum))
  (floor (- end-time (get-min-seqblock-duration min-num-pixels seqblock-info)))
  )

(define (get-seqblock-interior-pixels-per-value-unit seqblock-info)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (let ((ret (* (<ra> :get-seqblock-stretch seqblocknum seqtracknum)
                (get-sequencer-pixels-per-value-unit))))
    ;;(c-display "ret:" ret (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
    ret))

#||
;; Commented out. Too complicated to do it this way. Using ra:get-seqblocks-state / ra:create-gfx-seqblocks-from-state / ra:apply-gfx-seqblocks instead.
(define (apply-seqblock-interior-start seqblock-info make-undo)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define interior-start (<ra> :get-seqblock-interior-start-gfx seqblocknum seqtracknum))
  (c-display "interior-start:" interior-start)
  (if make-undo
      (<ra> :undo-sequencer))
  (<ra> :set-seqblock-interior-start interior-start seqblocknum seqtracknum)
  seqblock-info)

;; Commented out. Too complicated to do it this way. Using ra:get-seqblocks-state / ra:create-gfx-seqblocks-from-state / ra:apply-gfx-seqblocks instead.
(define (apply-seqblock-interior-end seqblock-info make-undo)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define interior-end (<ra> :get-seqblock-interior-end-gfx seqblocknum seqtracknum))
  (if make-undo
      (<ra> :undo-sequencer))
  (<ra> :set-seqblock-interior-end interior-end seqblocknum seqtracknum)
  seqblock-info)
||#

(define *current-seqblocks-state* #f) ;; To avoid having to call (<ra> :get-seqblocks-state seqtracknum)) every time the mouse pointer is moved.

(define (get-new-seqblock-interior-start seqblock-info mousex)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define samples-per-pixel (seqblock-info :samples-per-pixel))
  
  (define seqblock (*current-seqblocks-state* seqblocknum))

  (define not-use-grid (or (<ra> :control-pressed)
                           (and (= 0 seqtracknum)
                                (= 0 seqblocknum))))
  (define use-grid (not not-use-grid))

  (define stretch (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
  (define t1 (seqblock :start-time))
  (define i1 (seqblock :interior-start))
  (define i2 (seqblock :interior-end))
  (define s1 (- t1 (* i1 stretch)))
  
  (define new-i1 (between 0
                          (/ (- (if use-grid
                                    (max s1 (<ra> :get-seq-gridded-time (round (+ s1 (* mousex stretch))) seqtracknum (<ra> :get-seq-block-grid-type)))
                                    (+ s1 (* mousex stretch)))
                                s1)
                             stretch)
                          (- i2
                             (max 16 (* *min-seqblock-width* samples-per-pixel)))))
  
  (define Nt1 (max 0 (+ s1 (* new-i1 stretch))))
  (define Ni1 (/ (- Nt1 s1) stretch)) ;; I.e. = new-i1, at least when Nt1>=0
  
  ;;(c-display "mousex:" mousex ", new-start:" Nt1 ", new interior start:" Ni1)
  ;;(c-display "Nt1: " Nt1 ". Ni1:" Ni1 ". stretch:" stretch ". new-i2:" new-i2 ". min-new-i2:" (+ (max 16 (* *min-seqblock-width* samples-per-pixel)) i1))
  
  (list Nt1 Ni1))

(define (get-new-seqblock-interior-end seqblock-info mousex)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define samples-per-pixel (seqblock-info :samples-per-pixel))

  (define seqblock (*current-seqblocks-state* seqblocknum))

  (define not-use-grid (or (<ra> :control-pressed)
                           (and (= 0 seqtracknum)
                                (= 0 seqblocknum))))
  (define use-grid (not not-use-grid))

  (define original-duration (get-original-seqblock-duration seqblocknum seqtracknum))

  (define stretch (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
  (define t1 (seqblock :start-time))
  (define i1 (seqblock :interior-start))
  (define s1 (- t1 (* i1 stretch)))

  (define new-i2 (between (+ (max 16 (* *min-seqblock-width* samples-per-pixel)) i1) ;;(ceiling stretch) i1)
                          (/ (- (if use-grid
                                    (let ((closest (<ra> :get-seq-gridded-time (round (+ s1 (* stretch mousex))) seqtracknum (<ra> :get-seq-block-grid-type))))
                                      ;;(c-display "  |||Closest: " closest)
                                      closest)
                                    (+ s1 (* stretch mousex)))
                                s1)
                             stretch)
                          original-duration))
  
  (define Nt2 (+ s1 (* new-i2 stretch)))
  (define Ni2 new-i2)
  ;;(c-display "Nt2: " Nt2 ". Ni2:" Ni2 ". stretch:" stretch ". new-i2:" new-i2 ". min-new-i2:" (+ (max 16 (* *min-seqblock-width* samples-per-pixel)) i1))
  (list Nt2 Ni2))

(define (get-new-start seqblock-info mousex)
  (if use-grid
      (<ra> :get-seq-gridded-time (round mousex) 0 (<ra> :get-seq-block-grid-type))
      mousex))

(define (get-new-end seqblock-info mousex)
  (if use-grid
      (<ra> :get-seq-gridded-time (round mousex) 0 (<ra> :get-seq-block-grid-type))
      mousex))


(define gakkgakk-has-moved-left-interior #f)

;; left handle
(add-node-mouse-handler :Get-area-box (lambda()
                                        (and *current-seqblock-info*
                                             (not *current-seqautomation/distance*)
                                             (let ((seqblocknum (*current-seqblock-info* :seqblocknum))
                                                   (seqtracknum (*current-seqblock-info* :seqtracknum)))
                                               (and (not (<ra> :seqblock-holds-block seqblocknum seqtracknum))
                                                    (let ((box (<ra> :get-box seqblock-left-interior seqblocknum seqtracknum)))
                                                      ;;(c-display "BOX:" (box-to-string box) (*current-seqblock-info* :seqblocknum) (*current-seqblock-info* :seqtracknum))
                                                      box)))))
                        
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (define seqblock-info *current-seqblock-info*)
                                                  (define seqtracknum (seqblock-info :seqtracknum))
                                                  (define seqblocknum (seqblock-info :seqblocknum))
                                                  ;;(define start-pos (<ra> :get-seqblock-interior-start-gfx seqblocknum seqtracknum))
                                                  ;;(callback seqblock-info start-pos Y))
                                                  ;;(define start-pos (<ra> :get-seqblock-interior-start-gfx seqblocknum seqtracknum))
                                                  (set! *current-seqblocks-state* (<ra> :get-seqblocks-state seqtracknum))
                                                  (set! gakkgakk-has-moved-left-interior #f)
                                                  ;;(c-display "START:" (pp *current-seqblocks-state*))
                                                  (define seqblock (*current-seqblocks-state* seqblocknum))

                                                  (push-seqblock-to-top! seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))

                                                  (callback seqblock-info (seqblock :interior-start) Y))

                        :Get-min-value (lambda (seqblock-info)
                                         0)
                        
                        :Get-max-value (lambda (seqblock-info)
                                         (define seqtracknum (seqblock-info :seqtracknum))
                                         (define seqblocknum (seqblock-info :seqblocknum))
                                         ;;(get-original-seqblock-duration seqblocknum seqtracknum))
                                         ;;(define seqblock (*current-seqblocks-state* seqblocknum))
                                         (define original-duration (get-original-seqblock-duration seqblocknum seqtracknum))
                                         original-duration)
                                        ;(- (seqblock :interior-end)
                                        ;                   (seqblock :interior-start)))

                        
                        :Get-x (lambda (info) #f)
                        :Get-y (lambda (info) #f)

                        :Make-undo (lambda (_)
                                     #f)
                        
                        :Create-new-node (lambda (X seqtracknum callback)
                                           (assert #f)
                                           #f)

                        :Release-node (lambda (seqblock-info)
                                        (when gakkgakk-has-moved-left-interior
                                          (define seqtracknum (seqblock-info :seqtracknum))
                                          ;;(apply-seqblock-interior-start seqblock-info #t))
                                          (<ra> :undo-sequencer)
                                          (<ra> :apply-gfx-seqblocks seqtracknum)))
                        
                        :Move-node (lambda (seqblock-info mousex Y)
                                     (define seqtracknum (seqblock-info :seqtracknum))
                                     (define seqblocknum (seqblock-info :seqblocknum))

                                     (define temp (get-new-seqblock-interior-start seqblock-info mousex))

                                     (define new-start-time (car temp))                                     
                                     (define new-interior-start (cadr temp))

                                     ;;(set-left-interior-status-bar2 new-interior-start)
                        
                                     (define old-seqblock (*current-seqblocks-state* seqblocknum))
                                     (define new-seqblock (copy-hash old-seqblock
                                                                     :start-time (round new-start-time)
                                                                     :interior-start (round new-interior-start)))                                     
                                     (define new-seqblocks-state (copy *current-seqblocks-state*))
                                     (set! (new-seqblocks-state seqblocknum) new-seqblock)
                                     ;;(c-display "new:" (pp new-seqblocks-state))
                                     ;;(c-display "\n\n mouse-x:" mousex "\n\n")
                                     ;;(<ra> :apply-gfx-seqblocks new-seqblocks-state)

                                     (set! gakkgakk-has-moved-left-interior #t)
                                     (<ra> :create-gfx-seqblocks-from-state new-seqblocks-state seqtracknum)
                                     
                                     (<ra> :set-curr-seqblock-under-mouse seqblocknum seqtracknum)
                                     
                                     (set-grid-type #t)
                                     seqblock-info)
                        
                        :Publicize (lambda (seqblock-info)
                                     (define seqtracknum (seqblock-info :seqtracknum))
                                     (define seqblocknum (seqblock-info :seqblocknum))
                                     (set-left-interior-status-bar seqblocknum seqtracknum))
                        
                        :Get-pixels-per-value-unit (lambda (seqblock-info)
                                                     (get-seqblock-interior-pixels-per-value-unit seqblock-info))

                        :Use-Place #f

                        :Mouse-pointer-func ra:set-horizontal-resize-mouse-pointer
                        
                        :Get-guinum (lambda() (<gui> :get-sequencer-gui))

                        :Forgiving-box #f
                        )

#!!
(c-display (pp *current-seqblocks-state*))
!!#


(define gakkgakk-has-moved-right-interior #f)

;; right handle
(add-node-mouse-handler :Get-area-box (lambda()
                                        (and *current-seqblock-info*
                                             (not *current-seqautomation/distance*)
                                             (let ((seqblocknum (*current-seqblock-info* :seqblocknum))
                                                   (seqtracknum (*current-seqblock-info* :seqtracknum)))
                                               (and (not (<ra> :seqblock-holds-block seqblocknum seqtracknum))
                                                    (let ((box (<ra> :get-box seqblock-right-interior seqblocknum seqtracknum)))
                                                      ;;(c-display "BOX2:" (box-to-string box) (*current-seqblock-info* :seqblocknum) (*current-seqblock-info* :seqtracknum))
                                                      box)))))
                        
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (define seqblock-info *current-seqblock-info*)
                                                  (define seqtracknum (seqblock-info :seqtracknum))
                                                  (define seqblocknum (seqblock-info :seqblocknum))
                                                  
                                                  (set! *current-seqblocks-state* (<ra> :get-seqblocks-state seqtracknum))
                                                  (set! gakkgakk-has-moved-right-interior #f)
                                                  ;;(c-display "START:" (pp *current-seqblocks-state*))
                                                  
                                                  (define seqblock (*current-seqblocks-state* seqblocknum))

                                                  (push-seqblock-to-top! seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))

                                                  (callback seqblock-info (seqblock :interior-end) Y))

                        :Get-min-value (lambda (seqblock-info)
                                         (define seqtracknum (seqblock-info :seqtracknum))
                                         (define seqblocknum (seqblock-info :seqblocknum))
                                         (<ra> :get-seqblock-interior-start seqblocknum seqtracknum))
                        
                        :Get-max-value (lambda (seqblock-info)
                                         (define seqtracknum (seqblock-info :seqtracknum))
                                         (define seqblocknum (seqblock-info :seqblocknum))
                                         ;;(get-original-seqblock-duration seqblocknum seqtracknum))
                                         ;;(define seqblock (*current-seqblocks-state* seqblocknum))
                                         (define original-duration (get-original-seqblock-duration seqblocknum seqtracknum))
                                         original-duration)
                                        ;(- (seqblock :interior-end)
                                        ;                   (seqblock :interior-start)))

                        
                        :Get-x (lambda (info) #f)
                        :Get-y (lambda (info) #f)

                        :Make-undo (lambda (_)
                                     #f)
                        
                        :Create-new-node (lambda (X seqtracknum callback)
                                           (assert #f)
                                           #f)

                        :Release-node (lambda (seqblock-info)
                                        (when gakkgakk-has-moved-right-interior
                                          (define seqtracknum (seqblock-info :seqtracknum))
                                          (<ra> :undo-sequencer)
                                          (<ra> :apply-gfx-seqblocks seqtracknum)))
                        
                        :Move-node (lambda (seqblock-info mousex Y)
                                     
                                     (define seqtracknum (seqblock-info :seqtracknum))
                                     (define seqblocknum (seqblock-info :seqblocknum))

                                     ;;(c-display "mousex:" (/ mousex 44100.0) "/" (/ (get-original-seqblock-duration seqblocknum seqtracknum) 44100.0))

                                     (define temp (get-new-seqblock-interior-end seqblock-info mousex))

                                     (define new-end-time (car temp))                                     
                                     (define new-interior-end (cadr temp))

                                     ;;(c-display "new-end-time:" (/ new-end-time 44100.0) new-end-time (round new-end-time) ". new-interior-end:" (/ new-interior-end 44100.0) new-interior-end (round new-interior-end))
                                     ;;(set-right-interior-status-bar2 new-interior-end)
                                     
                                     (define old-seqblock (*current-seqblocks-state* seqblocknum))
                                     (define new-seqblock (copy-hash old-seqblock
                                                                     :end-time (round new-end-time)
                                                                     :interior-end (round new-interior-end)))
                                     (define new-seqblocks-state (copy *current-seqblocks-state*))
                                     (set! (new-seqblocks-state seqblocknum) new-seqblock)
                                     ;;(c-display "new:" (pp new-seqblocks-state))
                                     ;;(<ra> :apply-gfx-seqblocks new-seqblocks-state)

                                     (set! gakkgakk-has-moved-right-interior #t)
                                     (<ra> :create-gfx-seqblocks-from-state new-seqblocks-state seqtracknum)

                                     (<ra> :set-curr-seqblock-under-mouse seqblocknum seqtracknum)
                                     
                                     (set-grid-type #t)
                                     seqblock-info)
                        
                        :Publicize (lambda (seqblock-info)
                                     (define seqtracknum (seqblock-info :seqtracknum))
                                     (define seqblocknum (seqblock-info :seqblocknum))
                                     (set-right-interior-status-bar seqblocknum seqtracknum))
                        
                        :Get-pixels-per-value-unit (lambda (seqblock-info)
                                                     (get-seqblock-interior-pixels-per-value-unit seqblock-info))

                        :Use-Place #f

                        :Mouse-pointer-func ra:set-horizontal-resize-mouse-pointer
                        
                        :Get-guinum (lambda() (<gui> :get-sequencer-gui))

                        :Forgiving-box #f
                        )






;; Change seqblock stretch values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(delafina (seqblocks-overlap-halfly? :seqblocks
                                     :seqblocknum1
                                     :seqblocknum2
                                     :start1 #f
                                     :start2 #f)
  (define seqblock1 (seqblocks seqblocknum1))
  (define seqblock2 (seqblocks seqblocknum2))

  (define length1 (- (seqblock1 :end-time)
                     (seqblock1 :start-time)))
  (define length2 (- (seqblock2 :end-time)
                     (seqblock2 :start-time)))

  (set! start1 (or start1 (seqblock1 :start-time)))
  (set! start2 (or start2 (seqblock2 :start-time)))
  (define end1 (+ start1 length1))
  (define end2 (+ start2 length2))
  
  (and (>= start2 start1)
       (< start2 end1)
       (> end2 end1)))
  
(delafina (seqblocks-overlap? :seqblock1
                              :seqblock2)

  (define start1 (seqblock1 :start-time))
  (define start2 (seqblock2 :start-time))
  (define end1 (seqblock1 :end-time))
  (define end2 (seqblock2 :end-time))

  ;(c-display ":" start1 end1 "---" start2 end2 "\n"
  ;           (and (>= start1 start2)
  ;                (< start1 end2))
  ;           (and (>= start2 start1)
  ;                (< start2 end1))
  ;           
  ;           (and (<= start1 start2)
  ;                (> end1 start2))
  ;           (and (<= start2 start1)
  ;                (> end2 start1)))
  
  (or (and (>= start1 start2)
           (< start1 end2))
      (and (>= start2 start1)
           (< start2 end1))

      (and (<= start1 start2)
           (> end1 start2))
      (and (<= start2 start1)
           (> end2 start1))))
  

(delafina (seqblock-is-overlapping-with-another-block-seqblock :seqblocks
                                                               :seqblocknum
                                                               :new-start
                                                               :new-end #f)
  (set! new-start (or new-start
                      (seqblocks seqblocknum :start-time)))

  (define seqblock1 (copy-hash (seqblocks seqblocknum)
                               :start-time new-start
                               :end-time (or new-end
                                             (let ((length (- (seqblocks seqblocknum :end-time)
                                                              (seqblocks seqblocknum :start-time))))
                                               (+ new-start
                                                  length)))))
  (define num-seqblocks (length seqblocks))
  (let loop ((n 0))
    (if (>= n num-seqblocks)
        #f
        (cond ((= n seqblocknum)
               (loop (1+ n)))

              ((and ((seqblocks n) :blocknum)
                    (seqblocks-overlap? seqblock1 (seqblocks n)))
               #t)
              
              (else
               (loop (1+ n)))))))



;; autofade
;;;;;;;;;;;;;

(define (auto-add-fades-to-seqblocks seqblocks seqblocknum1 seqblocknum2)
  (define seqblock1 (seqblocks seqblocknum1))
  (define seqblock2 (seqblocks seqblocknum2))
  (define pos1 (seqblock2 :start-time))
  (define pos2 (seqblock1 :end-time))
  
  (define fade-out (between 0
                            (scale pos1
                                   (seqblock1 :start-time) (seqblock1 :end-time)
                                   1 0)
                            1))
  (define fade-in (between 0
                           (scale pos2
                                  (seqblock2 :start-time) (seqblock2 :end-time)
                                  0 1)
                           1))
  
  (define new-seqblock1 (copy-hash seqblock1
                                   :fade-out (* 1.0 fade-out)))
  (define new-seqblock2 (copy-hash seqblock2
                                   :fade-in (* 1.0 fade-in)))
  
  (define ret (copy seqblocks))
  (set! (ret seqblocknum1) new-seqblock1)
  (set! (ret seqblocknum2) new-seqblock2)  
  ret
  )

(define (do-add-auto-fades? seqblocks seqblocknum1 seqblocknum2)
  (define seqblock1 (seqblocks seqblocknum1))
  (define seqblock2 (seqblocks seqblocknum2))
  (and (not (seqblock1 :blocknum))
       (not (seqblock2 :blocknum))
       (seqblocks-overlap-halfly? seqblocks seqblocknum1 seqblocknum2)))
        
(define (find-seqblocks-to-autofade seqblocks seqblocknum)
  (define num-seqblocks (length seqblocks))
  (let loop ((n 0))
    (cond ((= n num-seqblocks)
           #f)
          ((= n seqblocknum)
           (loop (1+ n)))
          ((do-add-auto-fades? seqblocks n seqblocknum)
           (list n seqblocknum))
          ((do-add-auto-fades? seqblocks seqblocknum n)
           (list seqblocknum n))
          (else
           (loop (1+ n))))))

(define (maybe-add-autofades seqblocks seqblocknum)
  (define seqblock (seqblocks seqblocknum))
  (define fade-seqblocks (find-seqblocks-to-autofade seqblocks seqblocknum))
  (if (not fade-seqblocks)
      seqblocks
      (let* ((seqblocknum1 (car fade-seqblocks))
             (seqblocknum2 (cadr fade-seqblocks))
             (seqblock1 (seqblocks seqblocknum1))
             (seqblock2 (seqblocks seqblocknum2)))
        (cond ((and (seqblock1 :blocknum)
                    (seqblock2 :blocknum))
               #f)  ;; no overlaps for editor blocks
              ((seqblock1 :blocknum)
               seqblocks)
              ((seqblock2 :blocknum)
               seqblocks)
              (else
               (auto-add-fades-to-seqblocks seqblocks seqblocknum1 seqblocknum2))))))


;; find minimum block length (don't want seqblocks to be so narrow that they disappear)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *min-seqblock-width* (<gui> :get-system-fontheight))

(define (get-min-seqblock-duration2 min-num-pixels)
  (define width (- (<ra> :get-sequencer-x2)
                   (<ra> :get-sequencer-x1)))
  (define start-time (<ra> :get-sequencer-visible-start-time))
  (define end-time (<ra> :get-sequencer-visible-end-time))
  (define duration (- end-time start-time))
  ;;(c-display "m/w/d:" min-num-pixels width duration)
  (scale min-num-pixels
         0 width
         0 duration))

(define (get-min-seqblock-duration min-num-pixels seqblock-info)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define x1 (<ra> :get-seqblock-x1 seqblocknum seqtracknum))
  (define x2 (<ra> :get-seqblock-x2 seqblocknum seqtracknum))
  (define width (- x2 x1))
  (define start-time (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
  (define end-time (<ra> :get-seqblock-end-time seqblocknum seqtracknum))
  (define duration (- end-time start-time))
  (scale min-num-pixels
         0 width
         0 duration))

(define (get-max-seqblock-start-pos min-num-pixels seqtracknum seqblocknum)
  (define end-time (<ra> :get-seqblock-end-time seqblocknum seqtracknum))
  (- end-time (get-min-seqblock-duration min-num-pixels seqblock-info))
  )

(define (get-min-seqblock-end-pos min-num-pixels seqblock-info)
  (define seqtracknum (seqblock-info :seqtracknum))
  (define seqblocknum (seqblock-info :seqblocknum))
  (define start-time (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
  (+ start-time (get-min-seqblock-duration min-num-pixels seqblock-info))
  )



(define-class (<set-seqblock-stretch> :seqtracknum
                                      :seqblocknum
                                      :is-left)
  
  (define is-right (not is-left))

  (define seqblocks (<ra> :get-seqblocks-state seqtracknum))
  (define seqblock (seqblocks seqblocknum))
  ;;(pretty-print seqblock)(newline)
  (set! *current-seqblocks-state* seqblocks)

  (define curr-pos (seqblock :start-time))

  (define is-sample (seqblock :sample))
  (define is-block (seqblock :blocknum))

  (define num-seqblocks (length seqblocks))

  (define seqblock-length (- (seqblock :end-time)
                             (seqblock :start-time)))
                             
  (define has-made-undo #f)
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (<ra> :undo-sequencer)
      (set! has-made-undo #t)))

  (define (set-new-seqblocks! new-seqblocks new-seqblocknum)
    (set! seqblocks new-seqblocks)
    (set! seqblocknum new-seqblocknum)
    (set! seqblock (seqblocks seqblocknum)))
       
  :seqtracknum ()
  seqtracknum

  :seqblocknum ()
  seqblocknum

  :seqblock ()
  seqblock

  :curr-pos ()
  curr-pos

  :min-value ()
  (+ 1
     (seqblock :start-time)
     (get-min-seqblock-duration2 *min-seqblock-width*))

  :max-value ()
  (- (1- (seqblock :end-time))
     (get-min-seqblock-duration2 *min-seqblock-width*))

  :publicize ()
  (begin
    (define stretch (/ (- (if is-right
                              curr-pos
                              (seqblock :end-time))
                          (if is-left
                              curr-pos
                              (seqblock :start-time)))
                       (- (seqblock :interior-end)
                          (seqblock :interior-start))))
    (set-editor-statusbar (<-> "Stretch: " (two-decimal-string stretch))))

  :move (Value Y)
  (begin
    ;;(c-display "VALUE" Value)
    
    (define new-pos-nongridded (floor Value))
    
    (define new-pos (if (or (= 1 (num-seqblocks-in-sequencer))
                            (<ra> :control-pressed))
                        new-pos-nongridded
                        (<ra> :get-seq-gridded-time (floor Value) 0 (<ra> :get-seq-block-grid-type))))
    
    (if is-left
        (set! new-pos (min (this->max-value) new-pos))
        (set! new-pos (max (this->min-value) new-pos)))

    (set! new-pos (max 0
                       (floor new-pos)))

    ;;(c-display "---------new-pos:" new-pos ". Value:" Value "this->max-value:" (this->max-value))

    (set-grid-type #t)
    
    (when (or is-sample
              (not (seqblock-is-overlapping-with-another-block-seqblock seqblocks seqblocknum (if is-left new-pos #f) (if is-right new-pos #f))))
      (define new-seqblock (if is-left
                               (copy-hash seqblock
                                          :start-time new-pos)
                               (copy-hash seqblock
                                          :end-time new-pos)))
      (define new-seqblocks-state (copy seqblocks))
      (set! (new-seqblocks-state seqblocknum) new-seqblock)
      (set! new-seqblocks-state (maybe-add-autofades new-seqblocks-state seqblocknum))
      (when new-seqblocks-state
        ;;(set! new-seqblocks-state (sort! new-seqblocks-state
        ;;                                 (lambda (a b)
        ;;                                   (< (a :start-time)
        ;;                                      (b :start-time)))))
        ;;(assert-seqblocks-state new-seqblocks-state)
        (set! curr-pos new-pos)
        ;;(if (= 0 seqtracknum)
        ;;    (c-display "state:" (pp new-seqblocks-state)))
        (<ra> :create-gfx-seqblocks-from-state new-seqblocks-state seqtracknum)
        (<ra> :set-curr-seqblock-under-mouse seqblocknum seqtracknum)
        )
      )
    
    (assert (= (length seqblocks) num-seqblocks))
    #t)

  :release ()
  (begin
    (maybe-make-undo)
    (<ra> :apply-gfx-seqblocks seqtracknum)))

(define (create-seqblock-stretch-handler is-left)
  (add-node-mouse-handler :Get-area-box (lambda()
                                          (and *current-seqblock-info*
                                               (not *current-seqautomation/distance*)
                                               (let ((box (if is-left
                                                              (<ra> :get-box seqblock-left-stretch (*current-seqblock-info* :seqblocknum) (*current-seqblock-info* :seqtracknum))
                                                              (<ra> :get-box seqblock-right-stretch (*current-seqblock-info* :seqblocknum) (*current-seqblock-info* :seqtracknum)))))
                                                 ;;(c-display "BOX:" (box-to-string box))
                                                 box)))
                          :Get-existing-node-info (lambda (X Y callback)
                                                    (define seqblock-info *current-seqblock-info*)
                                                    (define seqtracknum (seqblock-info :seqtracknum))
                                                    (define seqblocknum (seqblock-info :seqblocknum))
                                                    (define set-stretch (<new> :set-seqblock-stretch seqtracknum seqblocknum is-left))
                                                    (push-seqblock-to-top! seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))
                                                    (callback set-stretch
                                                              (if is-left
                                                                  (<ra> :get-seqblock-start-time seqblocknum seqtracknum)
                                                                  (<ra> :get-seqblock-end-time seqblocknum seqtracknum))
                                                              Y))
                          
                          :Get-min-value (lambda (set-stretch)
                                           (if is-left
                                               0 ;; The move-seqblock-stretch-left class takes care of finding more accurate minimum value.
                                               (set-stretch :min-value)))
                          
                          :Get-max-value (lambda (set-stretch)
                                           (if is-left
                                               (set-stretch :max-value)
                                               #f))
                          
                          :Get-x (lambda (info) #f)
                          :Get-y (lambda (info) #f)
                          
                          :Make-undo (lambda (_)
                                       #f)
                          
                          :Create-new-node (lambda (X seqtracknum callback)
                                             (assert #f)
                                             #f)
                          
                          :Release-node (lambda (set-stretch)
                                          (set-stretch :release))
                          
                          :Move-node (lambda (set-stretch Value Y)
                                       (set-stretch :move Value Y)
                                       set-stretch)
                          
                          
                          :Publicize (lambda (set-stretch)
                                       (set-stretch :publicize))

                          :Get-pixels-per-value-unit (lambda (set-stretch)
                                                       (get-sequencer-pixels-per-value-unit))
                          
                          :Use-Place #f
                          
                          :Mouse-pointer-func ra:set-horizontal-resize-mouse-pointer
                          
                          :Get-guinum (lambda() (<gui> :get-sequencer-gui))
                          
                          :Forgiving-box #f
                          ))

(create-seqblock-stretch-handler #t)
(create-seqblock-stretch-handler #f)


;; seqblock move
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; sequencer block order
;;;;;;;;;;;;;;;;;;;;;;;;

(define (push-seqblock-to-top! seqtracknum id)
  (define old-order (to-list (<ra> :get-seqblock-z-order seqtracknum)))
  (define new-order (cons id (delete-maybe id old-order =)))
  ;;(c-display "id:" id "old:" old-order ". new-order: " new-order)
  (<ra> :set-seqblock-z-order
        new-order
        seqtracknum))


;; swap sequencer blocks
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-auto-swap-forward? seqblocks seqblocknum1 seqblocknum2 newstart1)
  (define seqblock1 (seqblocks seqblocknum1))
  (define seqblock2 (seqblocks seqblocknum2))

  (if (or (not (seqblock1 :blocknum)) ;; FIX: do this check before calling
          (not (seqblock1 :blocknum)))
      #f
      (begin
                          
        (define length1 (- (seqblock1 :end-time)
                           (seqblock1 :start-time)))
        (define length2 (- (seqblock2 :end-time)
                           (seqblock2 :start-time)))

        (define start1 newstart1) ;;(seqblock1 :start-time))
        (define start2 (seqblock2 :start-time))
        
        (define end1 (+ start1 length1))
        (define end2 (+ start2 length2))

        ;(c-display "forward-swap?"
        ;           (> start1 (seqblock1 :start-time))
        ;           ;(seqblocks-overlap? seqblocks seqblocknum1 seqblocknum2 :start1 start1 :start2 start2)
        ;           (>= end1 ;; if the end of block1 points 1/2 into block2.
        ;               (average start2 end2)))

        (and (> start1 (seqblock1 :start-time))
             ;;(> end1 start2)
             ;;(seqblocks-overlap? seqblocks seqblocknum1 seqblocknum2 :start1 start1 :start2 start2)
             (>= end1 ;; if the end of block1 points 1/2 into block2.
                 (average start2 end2))))))

(define (do-auto-swap-backward? seqblocks seqblocknum1 seqblocknum2 newstart2)
  (define seqblock1 (seqblocks seqblocknum1))
  (define seqblock2 (seqblocks seqblocknum2))

  (if (or (not (seqblock1 :blocknum))
          (not (seqblock1 :blocknum)))
      #f
      (begin
                          
        (define length1 (- (seqblock1 :end-time)
                           (seqblock1 :start-time)))
        (define length2 (- (seqblock2 :end-time)
                           (seqblock2 :start-time)))
        
        (define start1 (seqblock1 :start-time))
        (define start2 newstart2)
        
        (define end1 (+ start1 length1))
        (define end2 (+ start2 length2))
        
        ;(c-display seqblocknum1 seqblocknum2 ":"
        ;           (< start2 (seqblock2 :start-time))
        ;           ;(seqblocks-overlap? seqblocks seqblocknum1 seqblocknum2 :start1 start1 :start2 start2)
        ;           (< start2 ;; if the start of block2 points 1/2 into block1
        ;              (average start1 end1)))

        (and (< start2 (seqblock2 :start-time))
             ;;(< start2 end1)
             ;;(seqblocks-overlap? seqblocks seqblocknum1 seqblocknum2 :start1 start1 :start2 start2)
             (< start2 ;; if the start of block2 points 1/2 into block1
                (average start1 end1))))))


(define (find-seqblock-to-forward-autoswap-with seqblocks seqblocknum new-pos)
  (define num-seqblocks (length seqblocks))
  (let loop ((n (1+ seqblocknum)))
    (if (= n num-seqblocks)
        #f
        (let ((seqblock2 (seqblocks n)))
          (cond ((seqblock2 :sample)
                 (loop (1+ n)))
                ((do-auto-swap-forward? seqblocks seqblocknum n new-pos)
                 n)
                (else
                 #f))))))

(define (find-seqblock-to-backward-autoswap-with seqblocks seqblocknum new-pos)
  (let loop ((n (1- seqblocknum)))
    (if (< n 0)
        #f
        (let ((seqblock2 (seqblocks n)))
          (cond ((seqblock2 :sample)
                 (loop (1- n)))
                ((do-auto-swap-backward? seqblocks n seqblocknum new-pos)
                 n)
                (else
                 #f))))))

(define (swap-seqblocks seqblocks seqblocknum1 seqblocknum2 swap-forward)
  (define seqblock1 (seqblocks seqblocknum1))
  (define seqblock2 (seqblocks seqblocknum2))

  (define start1 (seqblock1 :start-time))
  (define start2 (seqblock2 :start-time))
  (define end1 (seqblock1 :end-time))
  (define end2 (seqblock2 :end-time))
  (define length1 (- end1 start1))
  (define length2 (- end2 start2))

  ;;(c-display "\n\n------------" swap-forward start1 start2)

  (define new-start2 (if swap-forward
                         (max 0 (- start2 length1))
                         start1))

  (define new-start1 (+ new-start2 length2))

  (define new-seqblock1 (copy-hash seqblock1
                                   :start-time new-start1
                                   :end-time (+ new-start1 length1)))

  (define new-seqblock2 (copy-hash seqblock2
                                   :start-time new-start2
                                   :end-time (+ new-start2 length2)))

  ;;(c-display "start1:" start1 ". new-start1:" new-start1 ". new-start2:" new-start2 ". length2:" length2)

  (define ret (copy seqblocks))
  (set! (ret seqblocknum2) new-seqblock1) ;; keep sorted
  (set! (ret seqblocknum1) new-seqblock2) ;; keep sorted
  ret
  )

(define (assert-seqblocks-state seqblocks)
  (let loop ((seqblocks (to-list seqblocks))
             (prev-end 0))
    (if (null? seqblocks)
        #t
        (let* ((seqblock (car seqblocks))
               (start (seqblock :start-time))
               (end (seqblock :end-time)))
          (c-display "start:" start)
          (when (> (abs (- start prev-end)) 500)
            (c-display "prev/start/end:" prev-end start end)
            (assert #f))
          (loop (cdr seqblocks)
                end)))))
  

(define (sort-seqblocks-state seqblocks-state)
  (sort seqblocks-state
        (lambda (a b)
          (< (a :start-time)
             (b :start-time)))))


(define-class (<move-single-block-class> :seqtracknum
                                         :seqblocknum
                                         :start-x
                                         :start-y
                                         :has-made-undo #f)

  (define seqblocks (<ra> :get-seqblocks-state seqtracknum))
  (define seqblock (seqblocks seqblocknum))
  ;;(pretty-print seqblock)
  (set! *current-seqblocks-state* seqblocks)

  (push-seqblock-to-top! seqtracknum (seqblock :id))

  (define curr-pos (seqblock :start-time))

  (define is-sample (seqblock :sample))
  (define is-block (seqblock :blocknum))

  (define num-seqblocks (length seqblocks))

  (define seqblock-length (- (seqblock :end-time)
                             (seqblock :start-time)))
                             
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (<ra> :undo-sequencer)
      (set! has-made-undo #t)))

  (define (set-new-seqblocks! new-seqblocks new-seqblocknum)
    (set! seqblocks new-seqblocks)
    (set! seqblocknum new-seqblocknum)
    (set! seqblock (seqblocks seqblocknum)))

  (define (both-seqtracks-are-same-type seqtracknum new-seqtracknum)
    (eq? (<ra> :seqtrack-for-audiofiles seqtracknum)
         (<ra> :seqtrack-for-audiofiles new-seqtracknum)))
         
  (define (change-seqtrack! new-seqtracknum new-pos)
    (maybe-make-undo)
    
    ;; remove from old seqtrack
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (set! seqblocks (list-remove (to-list seqblocks) seqblocknum))
    (try-finally :try (lambda ()
                        (<ra> :create-gfx-seqblocks-from-state seqblocks seqtracknum)
                        (<ra> :apply-gfx-seqblocks seqtracknum))
                 :failure (lambda ()
                            (<ra> :cancel-gfx-seqblocks seqtracknum)))

    
    ;; add to new seqtrack
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (set! seqtracknum new-seqtracknum)
    (set! seqblock (copy-hash seqblock
                              :start-time new-pos
                              :end-time (+ new-pos seqblock-length)))
    ;;(pretty-print seqblock)(newline)
    (set! seqblocks (sort-seqblocks-state (cons seqblock
                                                (to-list (<ra> :get-seqblocks-state seqtracknum)))))
    (set! num-seqblocks (length seqblocks))
    (set! seqblocknum (list-position seqblocks
                                     (lambda (maybe)
                                       (equal? seqblock maybe))))
    
    (try-finally :try (lambda ()
                        (<ra> :create-gfx-seqblocks-from-state seqblocks seqtracknum)
                        (<ra> :set-curr-seqblock-under-mouse seqblocknum seqtracknum)
                        (<ra> :apply-gfx-seqblocks seqtracknum)) ;; To avoid displaying block-seqblocks overlapping. :select-seqblock also fails if it isn't there
                 :failure (lambda ()
                            (<ra> :cancel-gfx-seqblocks seqtracknum)))

    ;; Keep *current-seqblock-info* up to date since it is used to auto-call :cancel-gfx-seqblocks if anything goes wrong.
    (set! *current-seqblock-info* (copy-seqblock-info *current-seqblock-info*
                                                      :seqtracknum seqtracknum
                                                      :seqblocknum seqblocknum))

    (<ra> :set-curr-seqtrack seqtracknum)
    )

  :seqtracknum ()
  seqtracknum

  :seqblocknum ()
  seqblocknum

  :seqblock ()
  seqblock

  :curr-pos ()
  curr-pos

  :move (Value Y)
  (begin
    ;;(<ra> :select-seqblock #t seqblocknum seqtracknum) ;; to avoid flickering
    
    (define new-seqtracknum (or (get-seqtracknum (1+ (<ra> :get-seqtrack-x1 0)) Y)
                                seqtracknum))
    ;;(c-display "  Y" Y new-seqtracknum)
    
    (define new-pos-nongridded (floor Value))
    
    (define new-pos (if (or (= 1 (num-seqblocks-in-sequencer))
                            (<ra> :control-pressed))
                        new-pos-nongridded
                        (<ra> :get-seq-gridded-time (floor Value) 0 (<ra> :get-seq-block-grid-type))))
    
    (set-grid-type #t)
    
    ;; changing track
    (if (and (not (= seqtracknum new-seqtracknum))
             (both-seqtracks-are-same-type seqtracknum new-seqtracknum))
        (change-seqtrack! new-seqtracknum new-pos))
    
    ;;(pretty-print seqblocks)
    
    ;; maybe swap forward
    (if is-block
        (let loop ((has-changed #f))
          (define seqblocknum2 (find-seqblock-to-forward-autoswap-with seqblocks seqblocknum new-pos-nongridded))
          (cond (seqblocknum2
                 ;;(c-display "BEFORE swap:::")
                 ;;(pretty-print seqblocks)(newline)
                 (set-new-seqblocks! (swap-seqblocks seqblocks seqblocknum seqblocknum2 #t) seqblocknum2)
                 ;;(c-display "   swapped forward")
                 (loop #t))
                (has-changed
                 (set! curr-pos (seqblock :start-time))
                 (<ra> :create-gfx-seqblocks-from-state seqblocks seqtracknum)
                 (<ra> :set-curr-seqblock-under-mouse seqblocknum seqtracknum)
                 ))))
    
    ;; maybe swap backward
    (if is-block
        (let loop ((has-changed #f))
          (define seqblocknum1 (find-seqblock-to-backward-autoswap-with seqblocks seqblocknum new-pos-nongridded))
          (cond (seqblocknum1
                 (set-new-seqblocks! (swap-seqblocks seqblocks seqblocknum1 seqblocknum #f) seqblocknum1)
                 ;;(c-display "   swapped forward")
                 (loop #t))
                (has-changed
                 (set! curr-pos (seqblock :start-time))
                 (<ra> :create-gfx-seqblocks-from-state seqblocks seqtracknum)
                 (<ra> :set-curr-seqblock-under-mouse seqblocknum seqtracknum)
                 ))))

    ;;(c-display "overlapping?" (seqblock-is-overlapping-with-another-block-seqblock seqblocks seqblocknum new-pos))

    (when (or is-sample
              (not (seqblock-is-overlapping-with-another-block-seqblock seqblocks seqblocknum new-pos)))
      (define diff (- new-pos
                      (seqblock :start-time)))
      (define new-seqblock (copy-hash seqblock
                                      :start-time new-pos
                                      :end-time (+ (seqblock :end-time) diff)))
      (define new-seqblocks-state (copy seqblocks))
      (set! (new-seqblocks-state seqblocknum) new-seqblock)
      (set! new-seqblocks-state (maybe-add-autofades new-seqblocks-state seqblocknum))
      (when new-seqblocks-state
        ;;(set! new-seqblocks-state (sort! new-seqblocks-state
        ;;                                 (lambda (a b)
        ;;                                   (< (a :start-time)
        ;;                                      (b :start-time)))))
        ;;(assert-seqblocks-state new-seqblocks-state)
        (set! curr-pos new-pos)
        (<ra> :create-gfx-seqblocks-from-state new-seqblocks-state seqtracknum)
        (<ra> :set-curr-seqblock-under-mouse seqblocknum seqtracknum)
        ))
    
    (assert (= (length seqblocks) num-seqblocks))
    #t)

  :release ()
  (begin
    (maybe-make-undo)
    (<ra> :apply-gfx-seqblocks seqtracknum)))


;; Move single seqblock
(add-node-mouse-handler :Get-area-box (lambda()
                                        (<ra> :get-box sequencer))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (let ((seqtracknum *current-seqtrack-num*))
                                                    ;;(c-display "seqtracknum" seqtracknum)
                                                    (and (not *current-seqautomation/distance*)
                                                         seqtracknum
                                                         (begin
                                                           (<ra> :set-curr-seqtrack seqtracknum)
                                                           (let ((seqblock-info *current-seqblock-info*))
                                                             ;;(c-display "get-existing " seqblock-info X Y seqtracknum)
                                                             (and seqblock-info
                                                                  (let* ((seqtracknum (and seqblock-info (seqblock-info :seqtracknum)))
                                                                         (seqblocknum (and seqblock-info (seqblock-info :seqblocknum)))
                                                                         (is-selected (<ra> :is-seqblock-selected seqblocknum seqtracknum)))
                                                                    
                                                                    (cond ((<= (<ra> :get-num-selected-seqblocks) 1)
                                                                           
                                                                           (if (and (not (<ra> :is-playing-song))
                                                                                    (<ra> :seqblock-holds-block seqblocknum seqtracknum))
                                                                               (<ra> :select-block (<ra> :get-seqblock-blocknum seqblocknum seqtracknum)))
                                                                           (cond ((and (not is-selected)
                                                                                       (not (<ra> :control-pressed)))
                                                                                  ;;(only-select-one-seqblock seqblocknum seqtracknum)
                                                                                  )
                                                                                 (else
                                                                                  (<ra> :select-seqblock #t seqblocknum seqtracknum)
                                                                                  )
                                                                                 )
                                                                           
                                                                           (set-grid-type #t)                                                                           

                                                                           (define has-made-undo #f)
                                                                           
                                                                           ;; Make a copy if holding shift.
                                                                           (when (<ra> :shift-pressed)
                                                                             (<ra> :undo-sequencer)
                                                                             (set! has-made-undo #t)
                                                                             (set! seqblocknum (<ra> :create-seqblock-from-state (<ra> :get-seqblock-state seqblocknum seqtracknum)))
                                                                             (set! *current-seqblock-info* (make-seqblock-info2 seqtracknum seqblocknum)))
                                                                      
                                                                           (define move-single-block (<new> :move-single-block-class seqtracknum seqblocknum X Y has-made-undo))

                                                                           (callback move-single-block (<ra> :get-seqblock-start-time seqblocknum seqtracknum)
                                                                                     Y))
                                                                          (else
                                                                           #f)))))))))
                        :Get-min-value (lambda (move-single)
                                         0)
                        :Get-max-value (lambda (move-single)
                                         (define seqtracknum (move-single :seqtracknum))
                                         (define seqblocknum (move-single :seqblocknum))
                                         (+ 10000000000 ((move-single :seqblock) :end-time)))
                        :Get-x (lambda (info) #f) ;;(/ (+ (<ra> :get-seqblock-x1 (info :seqblocknum)
                                                  ;;        (info :seqtracknum))
                                                  ;;  (<ra> :get-seqblock-x2 (info :seqblocknum)
                                                  ;;        (info :seqtracknum)))
                                                  ;;2))
                        :Get-y (lambda (info) #f) ;;(/ (+ (<ra> :get-seqblock-y1 (info :seqblocknum)
                                                  ;;        (info :seqtracknum))
                                                  ;;  (<ra> :get-seqblock-y2 (info :seqblocknum)
                                                  ;;        (info :seqtracknum)))
                                                  ;;2))
                        :Make-undo (lambda (_)
                                     #f)
                        :Create-new-node (lambda (X seqtracknum callback)
                                           #f)
                        :Publicize (lambda (move-single)
                                     (set-editor-statusbar (two-decimal-string (/ (move-single :curr-pos)
                                                                                      (<ra> :get-sample-rate)))))
                        
                        :Release-node (lambda (move-single)                                        
                                        (move-single :release))
                        
                        :Move-node (lambda (move-single Value Y)
                                     (move-single :move Value Y)
                                     move-single)

                        :Use-Place #f

                        :Mouse-pointer-func ra:set-closed-hand-mouse-pointer
                        :Get-guinum (lambda() (<gui> :get-sequencer-gui))
                        ;;:Mouse-pointer-func ra:set-blank-mouse-pointer
                        
                        :Get-pixels-per-value-unit (lambda (move-single)
                                                     (get-sequencer-pixels-per-value-unit))

                        :Forgiving-box #f
                        :Check-shift #f
                        )


(define2 gakkgakk-last-value (curry-or not number? list?) #f) ;; TODO: Fix.
(define2 gakkgakk-start-pos (curry-or number? list?) 0)
(define2 gakkgakk-was-selected boolean? #f)

(define2 gakkgakk-startseqtracknum integer? 0)
(define2 gakkgakk-startseqblocknum integer? 0)
(define2 gakkgakk-earliest-time number? 0)

;; Move multiple seqblocks
(add-node-mouse-handler :Get-area-box (lambda()
                                        (<ra> :get-box sequencer))
                        :Get-existing-node-info (lambda (X Y callback)                                                  
                                                  (let ((seqtracknum *current-seqtrack-num*))
                                                    (and (not *current-seqautomation/distance*)
                                                         seqtracknum
                                                         (begin
                                                           (<ra> :set-curr-seqtrack seqtracknum)
                                                           (let ((seqblock-info *current-seqblock-info*))
                                                             ;;(c-display "get-existing " seqblock-info X Y)
                                                             (and seqblock-info
                                                                  (let* ((seqtracknum (and seqblock-info (seqblock-info :seqtracknum)))
                                                                         (seqblocknum (and seqblock-info (seqblock-info :seqblocknum)))
                                                                         (is-selected (<ra> :is-seqblock-selected seqblocknum seqtracknum)))
                                                                    
                                                                    (if (and (not (<ra> :is-playing-song))
                                                                             (<ra> :seqblock-holds-block seqblocknum seqtracknum))
                                                                        (<ra> :select-block (<ra> :get-seqblock-blocknum seqblocknum seqtracknum)))
                                                                    
                                                                    (cond ((and (not is-selected)
                                                                                (not (<ra> :control-pressed)))
                                                                           (only-select-one-seqblock seqblocknum seqtracknum))
                                                                          (else
                                                                           (<ra> :select-seqblock #t seqblocknum seqtracknum)))
                                                                    
                                                                    
                                                                    (cond ((> (<ra> :get-num-selected-seqblocks) 1)
                                                                           (set-grid-type #t)
                                                                           
                                                                           (set! gakkgakk-start-pos 0);;(<ra> :get-seqblock-start-time seqblocknum seqtracknum))
                                                                           (set! gakkgakk-was-selected is-selected)
                                                                           (set! gakkgakk-startseqtracknum seqtracknum)
                                                                           (set! gakkgakk-startseqblocknum seqblocknum)

                                                                           (define seqblock-infos (get-selected-seqblock-infos))

                                                                           (set! gakkgakk-earliest-time (apply min (map (lambda (seqblock-info)
                                                                                                                          (<ra> :get-seqblock-start-time
                                                                                                                                (seqblock-info :seqblocknum)
                                                                                                                                (seqblock-info :seqtracknum)))
                                                                                                                        seqblock-infos)))

                                                                           (create-gfx-gfx-seqblocks seqblock-infos 0 0)

                                                                           (set! gakkgakk-start-pos (list 0 Y))

                                                                           (callback seqblock-infos 0 Y))

                                                                          (else
                                                                           #f)))))))))
                        
                        :Get-min-value (lambda (seqblock-infos)
                                         -100000000000)
                                         ;;(define seqtracknum (seqblock-info :seqtracknum))
                                         ;;(define seqblocknum (seqblock-info :seqblocknum))
                                         ;;(if (or #t (= 0 seqblocknum))
                                         ;;    0
                                         ;;    (<ra> :get-seqblock-end-time (1- seqblocknum) seqtracknum)))
                        ;;:Get-max-value (lambda (seqblock-infos)
                        ;;                 100000000000)
                                         ;;(define seqtracknum (seqblock-info :seqtracknum))
                                         ;;(define seqblocknum (seqblock-info :seqblocknum))
                                         ;;(define num-seqblocks (<ra> :get-num-seqblocks (seqblock-info :seqtracknum)))
                                         ;;(if (or #t (= (1- num-seqblocks) seqblocknum))
                                         ;;    (+ 10000000000 (<ra> :get-seqblock-end-time (seqblock-info :seqblocknum) seqtracknum))
                                         ;;    (<ra> :get-seqblock-start-time (1+ seqblocknum) seqtracknum)))
                        :Get-x (lambda (info) #f) ;;(/ (+ (<ra> :get-seqblock-x1 (info :seqblocknum)
                                                  ;;        (info :seqtracknum))
                                                  ;;  (<ra> :get-seqblock-x2 (info :seqblocknum)
                                                  ;;        (info :seqtracknum)))
                                                  ;;2))
                        :Get-y (lambda (info) #f) ;;(/ (+ (<ra> :get-seqblock-y1 (info :seqblocknum)
                                                  ;;        (info :seqtracknum))
                                                  ;;  (<ra> :get-seqblock-y2 (info :seqblocknum)
                                                  ;;        (info :seqtracknum)))
                                                  ;;2))
                        :Make-undo (lambda (_)
                                     #f)
                        :Create-new-node (lambda (X seqtracknum callback)
                                           #f)

                        :Publicize (lambda (seqblock-info)
                                     (set-editor-statusbar (two-decimal-string (/ gakkgakk-really-last-inc-time
                                                                                      (<ra> :get-sample-rate)))))

                        
                        :Release-node (lambda (seqblock-infos)
                                        (define has-moved (and gakkgakk-last-value (not (morally-equal? gakkgakk-start-pos gakkgakk-last-value))))
                                        (delete-all-gfx-gfx-seqblocks)
                                        ;;(c-display "has-moved:" has-moved gakkgakk-start-pos gakkgakk-last-value gakkgakk-was-selected)
                                        (if has-moved
                                            (apply-gfx-gfx-seqblocks seqblock-infos))

                                        (if (and (not has-moved)
                                                 (<ra> :control-pressed))
                                            (<ra> :select-seqblock (not gakkgakk-was-selected) gakkgakk-startseqblocknum gakkgakk-startseqtracknum))
                                        
                                        (set-grid-type #f)

                                        seqblock-infos)

                        :Move-node (lambda (seqblock-infos Value Y)
                                     (set! gakkgakk-last-value (list Value Y))

                                     (define new-seqtracknum (or (get-seqtracknum (1+ (<ra> :get-seqtrack-x1 0)) Y)
                                                                 (if (<= Y (<ra> :get-seqtrack-y1 0))
                                                                     0
                                                                     (<ra> :get-num-seqtracks))))
                                     (define inctrack (- new-seqtracknum gakkgakk-startseqtracknum))
                                     ;;(c-display new-seqtracknum gakkgakk-startseqtracknum inctrack)
                                     (delete-all-gfx-gfx-seqblocks)
                                     (create-gfx-gfx-seqblocks seqblock-infos (floor Value) inctrack)
                                     seqblock-infos)
                                          
                        :Use-Place #f

                        :Mouse-pointer-func ra:set-closed-hand-mouse-pointer
                        :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                        ;;:Mouse-pointer-func ra:set-blank-mouse-pointer
                        
                        :Get-pixels-per-value-unit (lambda (seqblock-infos)
                                                     (/ (- (<ra> :get-sequencer-x2)
                                                           (<ra> :get-sequencer-x1))
                                                        (- (<ra> :get-sequencer-visible-end-time)
                                                           (<ra> :get-sequencer-visible-start-time))))

                        :Forgiving-box #f
                        )

;; selection rectangle
(add-mouse-cycle
 (let* ((*selection-rectangle-start-x* #f)
        (*selection-rectangle-start-y* #f))
   
   (define (set-rect! $x $y)
     (define min-x (min $x *selection-rectangle-start-x*))
     (define min-y (min $y *selection-rectangle-start-y*))
     (define max-x (max $x *selection-rectangle-start-x*))
     (define max-y (max $y *selection-rectangle-start-y*))
     ;;(c-display min-x min-y max-x max-y)
     (<ra> :set-sequencer-selection-rectangle min-x min-y max-x max-y))

   (make-mouse-cycle
    :press-func (lambda ($button $x $y)
                  ;;(c-display "in-sequencer: " (inside-box (<ra> :get-box sequencer) $x $y) (< $y (<ra> :get-seqnav-y1)))
                  (and (= $button *left-button*)
                       (inside-box (<ra> :get-box sequencer) $x $y)
                       (< $y (<ra> :get-seqnav-y1))
                       (begin
                         (set! *selection-rectangle-start-x* $x)
                         (set! *selection-rectangle-start-y* $y)
                         #t)))
    
    :drag-func  (lambda ($button $x $y)
                  (set-rect! $x $y))
    
    :release-func (lambda ($button $x $y)
                    (set-rect! $x $y)
                    (<ra> :unset-sequencer-selection-rectangle)))))





;; sequencer track automation / sequencer block volume
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct seqautomation/distance
  :seqtrack
  :automation-num #f ;;
  :seqblock #f       ;; If #t, this is a seqblock envelope
  :distance)


(define *seqnode-min-distance* (* 1 (<ra> :get-half-of-node-width)))

(define2 *current-seqautomation/distance* (curry-or not hash-table?) #f)

(define (get-current-seqautomationnum)
  (and *current-seqautomation/distance*
       (*current-seqautomation/distance* :automation-num)))
(define (get-current-seqautomation-distance)
  (and *current-seqautomation/distance*
       (*current-seqautomation/distance* :distance)))
  
  
(define (min-seqautomation/distance A B)
  (cond ((not A)
         B)
        ((not B)
         A)
        ((<= (A :distance) (B :distance))
         A)
        (else
         B)))

(define (get-closest-seqautomation-1 Nodenum Total-Nodes Automation-Num X Y X1 Y1 X2 Y2)
  (define this (and (>= X (- X1 *seqnode-min-distance*))
                    (<= X (+ X2 *seqnode-min-distance*))
                    (make-seqautomation/distance :seqtrack *current-seqtrack-num*
                                                 :automation-num Automation-Num
                                                 :distance (let ((dist (get-distance-vertical X Y X1 Y1 X2 Y2
                                                                                              (<ra> :get-seq-automation-logtype (- Nodenum 2) Automation-Num *current-seqtrack-num*))))
                                                             ;;(c-display " Dist seqaut:" dist)
                                                             dist))))

  (define next (and (< Nodenum Total-Nodes)
                    (get-closest-seqautomation-1 (1+ Nodenum)
                                                 Total-Nodes
                                                 Automation-Num
                                                 X Y
                                                 X2 Y2
                                                 (<ra> :get-seq-automation-node-x Nodenum Automation-Num *current-seqtrack-num*)
                                                 (<ra> :get-seq-automation-node-y Nodenum Automation-Num *current-seqtrack-num*))))
  (min-seqautomation/distance this
                              next))

(define-match get-closest-seqautomation-0
  Automation-Num Automation-Num        _ _ :> #f
  Automation-Num Total-Automation-Nums X Y :> (min-seqautomation/distance (get-closest-seqautomation-1 2
                                                                                                       (<ra> :get-num-seq-automation-nodes Automation-Num *current-seqtrack-num*)
                                                                                                       Automation-Num
                                                                                                       X Y
                                                                                                       (<ra> :get-seq-automation-node-x 0 Automation-Num *current-seqtrack-num*)
                                                                                                       (<ra> :get-seq-automation-node-y 0 Automation-Num *current-seqtrack-num*)
                                                                                                       (<ra> :get-seq-automation-node-x 1 Automation-Num *current-seqtrack-num*)
                                                                                                       (<ra> :get-seq-automation-node-y 1 Automation-Num *current-seqtrack-num*))
                                                                          (get-closest-seqautomation-0 (1+ Automation-Num)
                                                                                                       Total-Automation-Nums
                                                                                                       X
                                                                                                       Y)))


(define *0* 0)

(define (get-closest-seqblock-automation automationnum x y)
  (define width/2 (<ra> :get-half-of-node-width))

  (define seqtracknum (*current-seqblock-info* :seqtracknum))
  (define seqblocknum (*current-seqblock-info* :seqblocknum))
  (define total-nodes (<ra> :get-num-seqblock-automation-nodes automationnum seqblocknum seqtracknum))

  (let loop ((n 1)
             (x1 (<ra> :get-seqblock-automation-node-x 0 automationnum seqblocknum seqtracknum))
             (x2 (<ra> :get-seqblock-automation-node-x 1 automationnum seqblocknum seqtracknum)))
    ;;(c-display "N:" n total-nodes x1 x2)
    (define (next)
      (if (< n (1- total-nodes))
          (loop (1+ n)
                x2
                (<ra> :get-seqblock-automation-node-x (1+ n) automationnum seqblocknum seqtracknum))
          #f))
    (cond ((or (= n (1- total-nodes))
               (and (>= x (- x1 width/2))
                    (<= x (+ x2 width/2))))
           (let* ((y1 (<ra> :get-seqblock-automation-node-y (1- n) automationnum seqblocknum seqtracknum))
                  (y2 (<ra> :get-seqblock-automation-node-y n automationnum seqblocknum seqtracknum))
                  (dist (get-distance-vertical x y x1 y1 x2 y2 (<ra> :get-seqblock-automation-logtype (1- n) automationnum seqblocknum seqtracknum))))
             (if (<= dist *seqnode-min-distance*)
                 (min-seqautomation/distance (make-seqautomation/distance :seqtrack *current-seqtrack-num*
                                                                          :automation-num #f
                                                                          :distance (begin
                                                                                      ;;(c-display " DIST volume envelope:" dist)
                                                                                      dist)
                                                                          :seqblock seqblocknum)
                                             (next))
                 (next))))
          ;((> x (+ x2) width/2)
          ; #f)
          (else
           (next)))))

(define (get-closest-seqautomation X Y)
  (and *current-seqtrack-num*
       (min-seqautomation/distance (get-closest-seqautomation-0 0 (<ra> :get-num-seq-automations *current-seqtrack-num*) X Y)
                                   (and *current-seqblock-info*
                                        (<ra> :get-seqblock-automation-enabled *0* (*current-seqblock-info* :seqblocknum) (*current-seqblock-info* :seqtracknum))
                                        (get-closest-seqblock-automation *0* X Y)))))


#||
(get-closest-seqautomation (<ra> :get-mouse-pointer-x) (<ra> :get-mouse-pointer-y))
||#

;; Highlight current seq automation / seqblock envelope
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (let ((curr-dist (get-closest-seqautomation X Y)))
           (set! *current-seqautomation/distance* (and curr-dist
                                                       (< (curr-dist :distance) *seqnode-min-distance*)
                                                       curr-dist))
           (cond ((not *current-seqautomation/distance*)
                  (<ra> :cancel-curr-seq-automation)
                  (<ra> :cancel-curr-seqblock-automation))

                 ((*current-seqautomation/distance* :automation-num)
                  (<ra> :cancel-curr-seqblock-automation)
                  (set-seqblock-selected-box 0 -1 -1)
                  (let* ((automationnum (*current-seqautomation/distance* :automation-num))
                         (seqtracknum (*current-seqautomation/distance* :seqtrack))
                         (instrument-id (<ra> :get-seq-automation-instrument-id automationnum seqtracknum))
                         (instrument-name (<ra> :get-instrument-name instrument-id))
                         (effect-num (<ra> :get-seq-automation-effect-num automationnum seqtracknum))
                         (effect-name (<ra> :get-instrument-effect-name effect-num instrument-id)))
                    (<ra> :set-normal-mouse-pointer (<gui> :get-sequencer-gui))
                    (set-editor-statusbar (<-> instrument-name "/" effect-name))
                    (<ra> :set-curr-seq-automation (*current-seqautomation/distance* :automation-num)
                          (*current-seqautomation/distance* :seqtrack))))

                 ((*current-seqautomation/distance* :seqblock)
                  (<ra> :cancel-curr-seq-automation)
                  (set-seqblock-selected-box 0 -1 -1)
                  (<ra> :set-normal-mouse-pointer (<gui> :get-sequencer-gui))
                  (set-editor-statusbar "Volume envelope")
                  (<ra> :set-curr-seqblock-automation *0* (*current-seqautomation/distance* :seqblock) (*current-seqautomation/distance* :seqtrack)))

                 (else
                  (assert #f))))))




;; sequencer track automation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-seqnode-statusbar-text Num)
  (let* ((automationnum (*current-seqautomation/distance* :automation-num))
         (seqtracknum (*current-seqautomation/distance* :seqtrack))
         (instrument-id (<ra> :get-seq-automation-instrument-id automationnum seqtracknum))
         ;;(instrument-name (<ra> :get-instrument-name instrument-id))
         (effect-num (<ra> :get-seq-automation-effect-num automationnum seqtracknum))
         (effect-name (<ra> :get-instrument-effect-name effect-num instrument-id)))
    (set-editor-statusbar (<-> effect-name ": "
                                   (two-decimal-string (<ra> :get-seq-automation-value Num automationnum seqtracknum))))))
  


;; move and create sequencer automation
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-seqautomation/distance*
                                             (*current-seqautomation/distance* :automation-num)
                                             (<ra> :get-box sequencer)))

                        :Get-existing-node-info (lambda (X Y callback)
                                                  (let ((automationnum (*current-seqautomation/distance* :automation-num))
                                                        (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                                    (define (get-nodebox $num)
                                                      (get-common-node-box (<ra> :get-seq-automation-node-x $num automationnum seqtracknum)
                                                                           (<ra> :get-seq-automation-node-y $num automationnum seqtracknum)))
                                                    (match (list (find-node-horizontal X Y get-nodebox (<ra> :get-num-seq-automation-nodes automationnum seqtracknum)))
                                                           (existing-box Num Box) :> (begin
                                                                                       (define Time (scale X
                                                                                                           (<ra> :get-seqtrack-x1 seqtracknum) (<ra> :get-seqtrack-x2 seqtracknum)
                                                                                                           (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                                                                                       (set-grid-type #t)
                                                                                       (callback Num Time Y))
                                                           _                      :> #f)))
                        :Get-min-value (lambda (_)
                                         (<ra> :get-sequencer-visible-start-time))
                        :Get-max-value (lambda (_)
                                         (<ra> :get-sequencer-visible-end-time))
                        :Get-x (lambda (Num)
                                 (let ((automationnum (*current-seqautomation/distance* :automation-num))
                                       (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                   (<ra> :get-seq-automation-node-x Num automationnum seqtracknum)))
                        :Get-y (lambda (Num)
                                 (let ((automationnum (*current-seqautomation/distance* :automation-num))
                                       (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                   (<ra> :get-seq-automation-node-y Num automationnum seqtracknum)))
                        :Make-undo (lambda (_)
                                     (<ra> :undo-sequencer-automations))
                        :Create-new-node (lambda (X Y callback)
                                           (let ((automationnum (*current-seqautomation/distance* :automation-num))
                                                 (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                             (define Time (scale X
                                                                 (<ra> :get-seqtrack-x1 seqtracknum) (<ra> :get-seqtrack-x2 seqtracknum)
                                                                 (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                                             (define PositionTime (if (<ra> :control-pressed)
                                                                      Time
                                                                      (<ra> :get-seq-gridded-time (floor Time) 0 (<ra> :get-seq-automation-grid-type))))
                                             (define Value (scale Y (<ra> :get-seqtrack-y1 seqtracknum) (<ra> :get-seqtrack-y2 seqtracknum) 1 0))
                                             (define Num (<ra> :add-seq-automation-node (floor PositionTime) Value *logtype-linear* automationnum seqtracknum))
                                             (if (= -1 Num)
                                               #f
                                               (begin
                                                 (set-grid-type #t)
                                                 (callback Num Time)))))
                        :Release-node (lambda (Num)
                                        (set-grid-type #f))
                        :Move-node (lambda (Num Time Y)
                                     (let ((automationnum (*current-seqautomation/distance* :automation-num))
                                           (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                       (define Value (scale Y (<ra> :get-seqtrack-y1 seqtracknum) (<ra> :get-seqtrack-y2 seqtracknum) 1 0))
                                       (define logtype (<ra> :get-seq-automation-logtype Num automationnum seqtracknum))
                                       (set! Time (floor Time))
                                       (if (not (<ra> :control-pressed))
                                           (set! Time (<ra> :get-seq-gridded-time Time 0 (<ra> :get-seq-automation-grid-type))))
                                       (<ra> :set-seq-automation-node Time Value logtype Num automationnum seqtracknum)
                                       ;;(c-display "NUM:" Num ", Time:" (/ Time 48000.0) ", Value:" Value)
                                       Num))
                        :Publicize (lambda (Num)
                                     (set-seqnode-statusbar-text Num)
                                     ;;(<ra> :set-curr-seqtemponode Num)
                                     #f)
                        
                        :Use-Place #f
                        :Mouse-pointer-func ra:set-normal-mouse-pointer
                        :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                        :Get-pixels-per-value-unit #f
                        )         


;; delete seqautomation / popupmenu
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     *current-seqautomation/distance*
                     (*current-seqautomation/distance* :automation-num)
                     (let ((automationnum (*current-seqautomation/distance* :automation-num))
                           (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                       (define (get-nodebox $num)
                         (get-common-node-box (<ra> :get-seq-automation-node-x $num automationnum seqtracknum)
                                              (<ra> :get-seq-automation-node-y $num automationnum seqtracknum)))
                       (define Num (match (list (find-node-horizontal $x $y get-nodebox (<ra> :get-num-seq-automation-nodes automationnum seqtracknum)))
                                          (existing-box Num Box) :> Num
                                          A                      :> #f))
                       (if (<ra> :shift-pressed)
                           (if Num
                               (<ra> :delete-seq-automation-node Num automationnum seqtracknum))
                           (popup-menu (list "Delete"
                                             :enabled Num
                                             (lambda ()
                                               (<ra> :delete-seq-automation-node Num automationnum seqtracknum)))
                                       ;;(list "Reset (set value to 0.5)"
                                       ;;     :enabled Num
                                       ;;      (lambda ()
                                       ;;        (<ra> :undo-seqtempo)
                                       ;;        (<ra> :set-seqtemponode
                                       ;;              (<ra> :get-seqtempo-abstime Num)
                                       ;;              1.0
                                       ;;              (<ra> :get-seqtempo-logtype Num)
                                       ;;              Num)))
                                       (list "Glide to next break point"
                                             :check (and Num (= (<ra> :get-seq-automation-logtype Num automationnum seqtracknum)
                                                                *logtype-linear*))
                                             :enabled (and Num
                                                           (< Num (- (<ra> :get-num-seq-automation-nodes automationnum seqtracknum) 1)))
                                             (lambda (maybe)
                                               (<ra> :undo-sequencer-automations)
                                               (<ra> :set-seq-automation-node
                                                     (<ra> :get-seq-automation-time Num automationnum seqtracknum)
                                                     (<ra> :get-seq-automation-value Num automationnum seqtracknum)
                                                     (if maybe *logtype-linear* *logtype-hold*)
                                                     Num
                                                     automationnum
                                                     seqtracknum)))
                                       ))
                       #t)))))

;; highlight current seq automation node
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and *current-seqautomation/distance*
              (*current-seqautomation/distance* :automation-num)
              (let ((automationnum (*current-seqautomation/distance* :automation-num))
                    (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                (define (get-nodebox $num)
                  (get-common-node-box (<ra> :get-seq-automation-node-x $num automationnum seqtracknum)
                                       (<ra> :get-seq-automation-node-y $num automationnum seqtracknum)))
                (match (list (find-node-horizontal $x $y get-nodebox (<ra> :get-num-seq-automation-nodes automationnum seqtracknum)))
                       (existing-box Num Box) :> (begin
                                                   (set-seqnode-statusbar-text Num)
                                                   (<ra> :set-curr-seq-automation-node Num automationnum seqtracknum)
                                                   #t)
                       A                      :> (begin
                                                   ;;(c-display "**Didnt get it:" A)
                                                   (<ra> :cancel-curr-seq-automation-node automationnum seqtracknum)
                                                   #f))))))


;; seqblock volume automation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-seqblock-automation-node-statusbar-text Num)
  (let* ((seqblocknum (*current-seqautomation/distance* :seqblock))
         (seqtracknum (*current-seqautomation/distance* :seqtrack)))
    (set-editor-statusbar (<-> "Volume: "
                                   (let ((db (<ra> :get-seqblock-automation-value Num *0* seqblocknum seqtracknum)))
                                     (<-> ;(<ra> :get-seqblock-automation-value Num seqblocknum seqtracknum)
                                          ;" : "
                                          (db-to-text db #t)))))))


;; move and create seqblock volume envelope
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-seqautomation/distance*
                                             (*current-seqautomation/distance* :seqblock)
                                             (<ra> :get-box sequencer)))

                        :Get-existing-node-info (lambda (X Y callback)
                                                  (let ((seqblocknum (*current-seqautomation/distance* :seqblock))
                                                        (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                                    (define (get-nodebox $num)
                                                      (get-common-node-box (<ra> :get-seqblock-automation-node-x $num *0* seqblocknum seqtracknum)
                                                                           (<ra> :get-seqblock-automation-node-y $num *0* seqblocknum seqtracknum)))
                                                    (match (list (find-node-horizontal X Y get-nodebox (<ra> :get-num-seqblock-automation-nodes *0* seqblocknum seqtracknum)))
                                                           (existing-box Num Box) :> (begin
                                                                                       (define Time (scale X
                                                                                                           (<ra> :get-seqtrack-x1 seqtracknum) (<ra> :get-seqtrack-x2 seqtracknum)
                                                                                                           (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                                                                                       (set-grid-type #t)
                                                                                       (push-seqblock-to-top! seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))
                                                                                       (callback Num Time Y))
                                                           _                      :> #f)))
                        :Get-min-value (lambda (_)
                                         (<ra> :get-sequencer-visible-start-time))
                        :Get-max-value (lambda (_)
                                         (<ra> :get-sequencer-visible-end-time))
                        
                        :Get-x (lambda (Num)
                                 (let ((seqblocknum (*current-seqautomation/distance* :seqblock))
                                       (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                   (<ra> :get-seqblock-automation-node-x Num *0* seqblocknum seqtracknum)))
                        :Get-y (lambda (Num)
                                 (let ((seqblocknum (*current-seqautomation/distance* :seqblock))
                                       (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                   (<ra> :get-seqblock-automation-node-y Num *0* seqblocknum seqtracknum)))
                        :Make-undo (lambda (_)
                                     (<ra> :undo-sequencer-envelopes))
                        :Create-new-node (lambda (X Y callback)
                                           (let ((seqblocknum (*current-seqautomation/distance* :seqblock))
                                                 (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                             (define Time (scale X
                                                                 (<ra> :get-seqtrack-x1 seqtracknum) (<ra> :get-seqtrack-x2 seqtracknum)
                                                                 (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                                             (define PositionTime (if (<ra> :control-pressed)
                                                                      Time
                                                                      (<ra> :get-seq-gridded-time (floor Time) 0 (<ra> :get-seq-automation-grid-type))))
                                             (define min-value (<ra> :get-seqblock-automation-min-value *0* seqblocknum seqtracknum))
                                             (define max-value (<ra> :get-seqblock-automation-max-value *0* seqblocknum seqtracknum))
                                             (define db (scale Y (<ra> :get-seqblock-header-y2 seqblocknum seqtracknum) (<ra> :get-seqtrack-y2 seqtracknum) max-value min-value))
                                             (c-display "db1" db ". Y:" Y)
                                             (define Num (<ra> :add-seqblock-automation-node (floor PositionTime) db *logtype-linear* *0* seqblocknum seqtracknum))
                                             (if (= -1 Num)
                                               #f
                                               (begin
                                                 (set-grid-type #t)
                                                 (push-seqblock-to-top! seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))
                                                 (callback Num Time)))))
                        :Release-node (lambda (Num)
                                        (set-grid-type #f))
                        :Move-node (lambda (Num Time Y)
                                     (let ((seqblocknum (*current-seqautomation/distance* :seqblock))
                                           (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                                       (define min-value (<ra> :get-seqblock-automation-min-value *0* seqblocknum seqtracknum))
                                       (define max-value (<ra> :get-seqblock-automation-max-value *0* seqblocknum seqtracknum))
                                       (define db (between min-value
                                                           (scale Y (<ra> :get-seqblock-header-y2 seqblocknum seqtracknum) (<ra> :get-seqtrack-y2 seqtracknum) max-value min-value)
                                                           max-value))
                                       (define logtype (<ra> :get-seqblock-automation-logtype Num *0* seqblocknum seqtracknum))
                                       (set! Time (floor Time))
                                       (if (not (<ra> :control-pressed))
                                           (set! Time (<ra> :get-seq-gridded-time Time 0 (<ra> :get-seq-automation-grid-type))))
                                       (c-display "db" db ". Y:" Y)
                                       (<ra> :set-seqblock-automation-node Time db logtype Num *0* seqblocknum seqtracknum)
                                       ;;(c-display "NUM:" Num ", Time:" (/ Time 48000.0) ", Value:" Value)
                                       Num))
                        :Publicize (lambda (Num)
                                     (set-seqblock-automation-node-statusbar-text Num)
                                     ;;(<ra> :set-curr-seqtemponode Num)
                                     #f)
                        
                        :Use-Place #f
                        :Mouse-pointer-func ra:set-normal-mouse-pointer
                        :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                        :Get-pixels-per-value-unit #f
                        )         

;; delete block volume envelope node / popupmenu
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     *current-seqautomation/distance*
                     (*current-seqautomation/distance* :seqblock)
                     (let ((seqblocknum (*current-seqautomation/distance* :seqblock))
                           (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                       (define (get-nodebox $num)
                         (get-common-node-box (<ra> :get-seqblock-automation-node-x $num *0* seqblocknum seqtracknum)
                                              (<ra> :get-seqblock-automation-node-y $num *0* seqblocknum seqtracknum)))
                       (define Num (match (list (find-node-horizontal $x $y get-nodebox (<ra> :get-num-seqblock-automation-nodes *0* seqblocknum seqtracknum)))
                                          (existing-box Num Box) :> Num
                                          A                      :> #f))
                       (if (<ra> :shift-pressed)
                           (if Num
                               (<ra> :delete-seqblock-automation-node Num *0* seqblocknum seqtracknum))
                           (popup-menu (list "Delete"
                                             :enabled Num
                                             (lambda ()
                                               (<ra> :delete-seqblock-automation-node Num *0* seqblocknum seqtracknum)))
                                       (list "Reset (set value to 0.0 dB)"
                                            :enabled Num
                                             (lambda ()
                                               (<ra> :undo-sequencer-envelopes)
                                               (<ra> :set-seqblock-automation-node
                                                     (<ra> :get-seqblock-automation-time Num *0* seqblocknum seqtracknum)
                                                     0.0
                                                     (<ra> :get-seqblock-automation-logtype Num *0* seqblocknum seqtracknum)
                                                     Num
                                                     *0* 
                                                     seqblocknum
                                                     seqtracknum)))
                                       (list "Glide to next break point"
                                             :check (and Num
                                                         (= (<ra> :get-seqblock-automation-logtype Num *0* seqblocknum seqtracknum)
                                                            *logtype-linear*))
                                             :enabled Num
                                             (lambda (maybe)
                                               (<ra> :undo-sequencer-envelopes)
                                               (<ra> :set-seqblock-automation-node
                                                     (<ra> :get-seqblock-automation-time Num *0* seqblocknum seqtracknum)
                                                     (<ra> :get-seqblock-automation-value Num *0* seqblocknum seqtracknum)
                                                     (if maybe *logtype-linear* *logtype-hold*)
                                                     Num
                                                     *0* 
                                                     seqblocknum
                                                     seqtracknum)))
                                       ))
                       #t)))))

;; highlight current seq block volume envelope node
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and *current-seqautomation/distance*
              (*current-seqautomation/distance* :seqblock)
              (let ((seqblocknum (*current-seqautomation/distance* :seqblock))
                    (seqtracknum (*current-seqautomation/distance* :seqtrack)))
                (define (get-nodebox $num)
                  (get-common-node-box (<ra> :get-seqblock-automation-node-x $num *0* seqblocknum seqtracknum)
                                       (<ra> :get-seqblock-automation-node-y $num *0* seqblocknum seqtracknum)))
                (match (list (find-node-horizontal $x $y get-nodebox (<ra> :get-num-seqblock-automation-nodes *0* seqblocknum seqtracknum)))
                       (existing-box Num Box) :> (begin
                                                   (set-seqblock-automation-node-statusbar-text Num)
                                                   (<ra> :set-curr-seqblock-automation-node Num *0* seqblocknum seqtracknum)
                                                   #t)
                       A                      :> (begin
                                                   ;;(c-display "**Didnt get it:" A)
                                                   (<ra> :cancel-curr-seqblock-automation-node *0* seqblocknum seqtracknum)
                                                   #f))))))



;; seqblock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; delete seqblock
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     (<ra> :shift-pressed)
                     (let ((seqblock-info *current-seqblock-info*))
                       ;;(c-display "get-existing " seqblock-info X Y)
                       (and seqblock-info
                            (begin
                              (set! *current-seqblock-info* #f)
                              (<ra> :delete-seqblock (seqblock-info :seqblocknum) (seqblock-info :seqtracknum))
                              (set! *current-seqblock-info* #f)
                              #t)))))))

;; delete seqtrack
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     (<ra> :shift-pressed)
                     (> (<ra> :get-num-seqtracks) 1)
                     (let ((seqtracknum *current-seqtrack-num*))
                       (and seqtracknum
                            (begin
                              (<ra> :delete-seqtrack seqtracknum)
                              #t)))))))

(define (get-sequencer-pos-from-x X)
  (round (scale X
                (<ra> :get-sequencer-x1) (<ra> :get-sequencer-x2)
                (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time))))


(define2 *seqblock-clipboard* list? '())

(define (seqblock-selected?)
  (call/cc (lambda (return)
             (for-each-seqblock (lambda (seqtracknum seqblocknum)
                                  (if (<ra> :is-seqblock-selected seqblocknum seqtracknum)
                                      (return #t))))
             (return #f))))

(define (FROM_C-copy-all-selected-seqblocks)
  (define minseqtrack #f)
  (define mintime #f)
  (set! *seqblock-clipboard* '())

  (define (add-seqblock! seqtracknum seqblocknum)
    (define seqblock (<ra> :get-seqblock-state seqblocknum seqtracknum))
    (push-back! *seqblock-clipboard* seqblock)                
    (if (not minseqtrack)
        (set! minseqtrack seqtracknum)
        (set! minseqtrack (min seqtracknum minseqtrack)))
    (let ((start-time (seqblock :start-time)))
      (if (not mintime)
          (set! mintime start-time)
          (set! mintime (min start-time mintime)))))
    
  ;; Find all selected seqblocks
  (for-each-seqblock (lambda (seqtracknum seqblocknum)
                       (when (<ra> :is-seqblock-selected seqblocknum seqtracknum)
                         (add-seqblock! seqtracknum seqblocknum))))

  (when (null? *seqblock-clipboard*)
    (define x (<ra> :get-mouse-pointer-x))
    (define y (<ra> :get-mouse-pointer-y))
    (for-each-seqblock (lambda (seqtracknum seqblocknum)
                         (if (inside-box (<ra> :get-box seqblock seqblocknum seqtracknum) x y)
                             (add-seqblock! seqtracknum seqblocknum)))))

  ;; Scale time
  (set! *seqblock-clipboard*
        (map (lambda (seqblock)
               (move-seqblock2 seqblock
                               (- mintime)
                               (- minseqtrack)))
             *seqblock-clipboard*)))

(define (FROM_C-paste-sequencer-blocks seqtracknum time)
  (<ra> :undo-sequencer)
  (for-each (lambda (seqblock)
              (define seqtracknum2 (+ seqtracknum (seqblock :seqtracknum)))
              (if (< seqtracknum2 (<ra> :get-num-seqtracks))
                  (<ra> :create-seqblock-from-state (move-seqblock2 seqblock time seqtracknum))))
            *seqblock-clipboard*))

(define (FROM_C-delete-all-selected-seqblocks)
  (define deleted-something #f)

  (set! *current-seqblock-info* #f)

  (undo-block
   (lambda ()
     (let loop ((seqblocknum 0)
                (seqtracknum 0))
       (cond ((= seqtracknum (<ra> :get-num-seqtracks))
              #t)
             ((= seqblocknum (<ra> :get-num-seqblocks seqtracknum))
              (loop 0 (1+ seqtracknum)))
             ((<ra> :is-seqblock-selected seqblocknum seqtracknum)
              (<ra> :delete-seqblock seqblocknum seqtracknum)
              (set! *current-seqblock-info* #f)
              (set! deleted-something #t)
              (loop seqblocknum seqtracknum))
             (else
              (loop (1+ seqblocknum) seqtracknum))))
     
     (when (not deleted-something)
       (define x (<ra> :get-mouse-pointer-x))
       (define y (<ra> :get-mouse-pointer-y))
       (let loop ((seqblocknum 0)
                  (seqtracknum 0))
         (cond ((= seqtracknum (<ra> :get-num-seqtracks))
                #t)
               ((= seqblocknum (<ra> :get-num-seqblocks seqtracknum))
                (loop 0 (1+ seqtracknum)))
               ((inside-box (<ra> :get-box seqblock seqblocknum seqtracknum) x y)
                (<ra> :delete-seqblock seqblocknum seqtracknum)
                (set! *current-seqblock-info* #f)
                (loop seqblocknum seqtracknum))
               (else
                (loop (1+ seqblocknum) seqtracknum))))))))

(define (FROM_C-cut-all-selected-seqblocks)
  (FROM_C-copy-all-selected-seqblocks)
  (FROM_C-delete-all-selected-seqblocks))


(define (create-sequencer-automation seqtracknum X Y)
  (define (instrument-popup-menu instrument-id)
    (popup-menu (map (lambda (effectnum)
                       (list (<-> effectnum ". " (<ra> :get-instrument-effect-name effectnum instrument-id))
                             (lambda ()
                               (define Time1 (scale X
                                                    (<ra> :get-seqtrack-x1 seqtracknum) (<ra> :get-seqtrack-x2 seqtracknum)
                                                    (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                               (define Time2 (scale (+ X (* 5 *seqnode-min-distance*))
                                                    (<ra> :get-seqtrack-x1 seqtracknum) (<ra> :get-seqtrack-x2 seqtracknum)
                                                    (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
                               (define Value (scale Y (<ra> :get-seqtrack-y1 seqtracknum) (<ra> :get-seqtrack-y2 seqtracknum) 1 0))
                               ;;(c-display effectnum)
                               (<ra> :add-seq-automation
                                     (floor Time1) Value
                                     (floor Time2) Value
                                     effectnum
                                     instrument-id
                                     seqtracknum))))
                     (iota (<ra> :get-num-instrument-effects instrument-id)))))

  (define seqtrack-instrument-id (and (<ra> :seqtrack-for-audiofiles seqtracknum)
                                      (<ra> :get-seqtrack-instrument seqtracknum)))

  (define all-instruments (get-all-audio-instruments))

  (popup-menu
   (if (and seqtrack-instrument-id
            (>= seqtrack-instrument-id 0))
       (list (<ra> :get-instrument-name seqtrack-instrument-id)
             (lambda ()
               (instrument-popup-menu seqtrack-instrument-id))
             "---------------------")
       '())
   (map (lambda (num instrument-id)
          (list (<-> num ". " (<ra> :get-instrument-name instrument-id))
                (lambda ()
                  (instrument-popup-menu instrument-id))))
        (iota (length all-instruments))
        all-instruments)))

(define *curr-seqblock-track-on-off-window* #f)
(define *curr-seqblock-track-on-off-gui* #f)

(define (show-seqblock-track-on-off-configuration seqtracknum seqblocknum blocknum)
  (define starting #t)
  (define gui (<gui> :vertical-layout))

  (for-each (lambda (tracknum)
              (<gui> :add gui
                     (<gui> :checkbox
                            (<-> tracknum)
                            (<ra> :is-seqblock-track-enabled tracknum seqblocknum seqtracknum)
                            #t
                            (lambda (enabled)
                              (when (not starting)
                                (c-display "clicked" tracknum enabled)
                                (<ra> :set-seqblock-track-enabled enabled tracknum seqblocknum seqtracknum)
                                )))))
            (iota (<ra> :get-num-tracks blocknum)))
  
  (define window (or *curr-seqblock-track-on-off-window*
                     (let ((window (<gui> :horizontal-layout)))
                       (<gui> :add-deleted-callback window (lambda x
                                                             (c-display " Deleted")
                                                             ;;(<gui> :close window)
                                                             (set! *curr-seqblock-track-on-off-window* #f)
                                                             (set! *curr-seqblock-track-on-off-gui* #f)))
                       window)))

  (define close-button (<gui> :button "Close" (lambda ()
                                                (<gui> :close window))))

  (<gui> :add gui close-button)
  
  (if *curr-seqblock-track-on-off-gui*
      (begin
        (<gui> :replace window *curr-seqblock-track-on-off-gui* gui)
        (<gui> :close *curr-seqblock-track-on-off-gui*))
      (<gui> :add window gui))

  (set! *curr-seqblock-track-on-off-gui* gui)

  (if *curr-seqblock-track-on-off-window*
      (<gui> :raise window)
      (begin      
        (<gui> :set-takes-keyboard-focus window #f)
        (<gui> :set-parent window -1)
        (<gui> :show window)
        (set! *curr-seqblock-track-on-off-window* window)))

  (set! starting #f)
  window
  )

#!!
(show-seqblock-track-on-off-configuration 0 0 0)
!!#

(define (FROM_C-update-seqblock-track-on-off-configuration seqtracknum seqblocknum)
  (if (and *curr-seqblock-track-on-off-window*
           (>= seqtracknum 0)
           (>= seqblocknum 0)
           (<ra> :seqblock-holds-block seqblocknum seqtracknum))
      (show-seqblock-track-on-off-configuration seqtracknum seqblocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))))
  

(define (get-original-seqblock-duration seqblocknum seqtracknum)
  (if (<ra> :seqblock-holds-block seqblocknum seqtracknum)
      (<ra> :get-block-length (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
      (* (<ra> :get-seqblock-resample-ratio seqblocknum seqtracknum)
         (<ra> :get-sample-length (<ra> :get-seqblock-sample seqblocknum seqtracknum)))))
      
(define (get-nonstretched-seqblock-duration seqblocknum seqtracknum)
  (- (<ra> :get-seqblock-interior-end seqblocknum seqtracknum)
     (<ra> :get-seqblock-interior-start seqblocknum seqtracknum)))
      
;; seqblock stretch handle menu
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (c-display "WARNING: Fix Reset stretch")
                (and (= Button *right-button*)
                     (not (<ra> :shift-pressed))
                     *current-seqblock-info*
                     (let* ((seqtracknum (*current-seqblock-info* :seqtracknum))
                            (seqblocknum (*current-seqblock-info* :seqblocknum))
                            (is-left (inside-box (<ra> :get-box seqblock-left-stretch seqblocknum seqtracknum) X Y))
                            (is-right (not is-left)))
                       (and (or is-left
                                (inside-box (<ra> :get-box seqblock-right-stretch seqblocknum seqtracknum) X Y))
                            (begin
                              (popup-menu (list "Reset stretch"
                                                :enabled (not (= 1.0 (<ra> :get-seqblock-stretch seqblocknum seqtracknum)))
                                                (lambda ()
                                                  (c-display "stretch:" (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
                                                  (define duration (get-nonstretched-seqblock-duration seqblocknum seqtracknum))
                                                  (define seqblocks (<ra> :get-seqblocks-state seqtracknum))
                                                  (define seqblock (seqblocks seqblocknum))
                                                  (define new-seqblock (if is-right
                                                                           (copy-hash seqblock
                                                                                      :end-time (+ (seqblock :start-time) duration))
                                                                           (copy-hash seqblock
                                                                                      :start-time (- (seqblock :end-time) duration))))
                                                  (if (and is-left
                                                           (< (new-seqblock :start-time) 0))
                                                      (set! new-seqblock (copy-hash new-seqblock
                                                                                    :start-time 0
                                                                                    :end-time (+ (new-seqblock :end-time) (- (new-seqblock :start-time))))))
                                                  (define new-seqblocks-state (copy seqblocks))
                                                  (set! (new-seqblocks-state seqblocknum) new-seqblock)
                                                  (set! new-seqblocks-state (maybe-add-autofades new-seqblocks-state seqblocknum))
                                                  (when new-seqblocks-state
                                                    (<ra> :create-gfx-seqblocks-from-state new-seqblocks-state seqtracknum)
                                                    (<ra> :apply-gfx-seqblocks seqtracknum))))
                                          (list "Granulation parameters"
                                                :enabled (not (= 1.0 (<ra> :get-seqblock-stretch seqblocknum seqtracknum)))
                                                (lambda ()
                                                  (create-audio-seqblock-gui seqblocknum seqtracknum))))
                              #t)))))))

(define (split-sample-seqblock pos seqtracknum seqblocknum)
  (call-with-exit
   (lambda (return)
     (define seqblocks-state (to-list (<ra> :get-seqblocks-state seqtracknum)))
     (define seqblock (seqblocks-state seqblocknum))
     
     (define stretch (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
     (define t1 (seqblock :start-time))
     (define i1 (seqblock :interior-start))
     (define i2 (seqblock :interior-end))
     (define s1 (- t1 (* i1 stretch)))
     
     (define interior-split (to-integer (/ (- pos s1) stretch)))
     
     (if (<= interior-split i1)
         (return #f))
     (if (>= interior-split i2)
         (return #f))

     (define seqblock1 (copy-hash seqblock
                                  :end-time pos
                                  :interior-end interior-split))
     
     (define seqblock2 (copy-hash seqblock
                                  :id -1
                                  :start-time pos
                                  :interior-start interior-split))
     
     (define new-seqblocks-state (append (if (= 0 seqblocknum)
                                             '()
                                             (take seqblocks-state seqblocknum))
                                         (list seqblock1 seqblock2)
                                         (if (= (1- (length seqblocks-state)) seqblocknum)
                                             '()
                                             (sublist seqblocks-state (1+ seqblocknum) (length seqblocks-state)))))
     
     (try-finally :try (lambda ()                   
                         (<ra> :create-gfx-seqblocks-from-state new-seqblocks-state seqtracknum)
                         (<ra> :undo-sequencer)
                         (<ra> :apply-gfx-seqblocks seqtracknum))
                  :failure (lambda ()
                             (<ra> :cancel-gfx-seqblocks seqtracknum)))
     #t)))

(define (FROM_C-split-sample-seqblock-under-mouse use-grid)
  (let ((seqtracknum *current-seqtrack-num*))
    (and seqtracknum
         (let ((seqblock-info *current-seqblock-info*))
           (and seqblock-info          
                (let ((seqblocknum (seqblock-info :seqblocknum)))
                  (and (not (<ra> :seqblock-holds-block seqblocknum seqtracknum))
                       (let* ((X (<ra> :get-mouse-pointer-x))
                              (seqpos (get-sequencer-pos-from-x X))
                              (pos (if use-grid
                                       (<ra> :get-seq-gridded-time seqpos seqtracknum (<ra> :get-seq-block-grid-type))
                                       seqpos)))
                         (split-sample-seqblock pos seqtracknum seqblocknum)
                         #t))))))))
  
;; seqblock menu
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda (Button X Y)
                (and (= Button *right-button*)
                     (not (<ra> :shift-pressed))
                     (let ((seqtracknum *current-seqtrack-num*))
                       (and seqtracknum
                            (let ((seqblock-infos (get-selected-seqblock-infos))
                                  (seqblock-info *current-seqblock-info*))
                              (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
                              (define for-blocks (not for-audiofiles))
                              (define seqblocknum (and seqblock-info
                                                       (seqblock-info :seqblocknum)))
                              (define blocknum (and seqblock-info
                                                    (<ra> :seqblock-holds-block seqblocknum seqtracknum)
                                                    (<ra> :get-seqblock-blocknum seqblocknum seqtracknum)))
                              (if seqblock-info
                                  (if (not (<ra> :is-seqblock-selected seqblocknum seqtracknum))
                                      (only-select-one-seqblock seqblocknum seqtracknum)
                                      (<ra> :select-seqblock #t seqblocknum seqtracknum)))

                              (popup-menu (list
                                           "Insert existing block"
                                           :enabled for-blocks
                                           (lambda ()
                                             (let ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type))))
                                               (if (= 1 (<ra> :get-num-blocks))
                                                   (<ra> :create-seqblock seqtracknum 0 pos)                                          
                                                   (apply popup-menu
                                                          (map (lambda (blocknum)
                                                                 (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
                                                                       (lambda ()
                                                                         (<ra> :create-seqblock seqtracknum blocknum pos))))
                                                               (iota (<ra> :get-num-blocks))))))))
                                          
                                          ;;   Sub menues version. It looks better, but it is less convenient.
                                          ;;"Insert existing block" (map (lambda (blocknum)
                                          ;;                               (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
                                          ;;                                     (lambda ()
                                          ;;                                       (let ((pos (get-sequencer-pos-from-x X)))
                                          ;;                                         (<ra> :add-block-to-seqtrack seqtracknum blocknum pos))
                                          ;;                                       (<ra> :select-block blocknum))))
                                          ;;                             (iota (<ra> :get-num-blocks)))
                                          
                                          (list                                          
                                           "Insert current block"
                                           :enabled for-blocks
                                           (lambda ()
                                             (let* ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type)))
                                                    (blocknum (<ra> :current-block)))
                                               (<ra> :create-seqblock seqtracknum blocknum pos))))
                                          
                                          (list                                           
                                           "Insert new block"
                                           :enabled for-blocks
                                           (lambda ()
                                             (let* ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type)))
                                                    (blocknum (<ra> :append-block)))
                                               (<ra> :create-seqblock seqtracknum blocknum pos))))
                                          
                                          (list
                                           "Insert new block from disk (BETA)"
                                           :enabled for-blocks
                                           (lambda ()
                                             (let* ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type)))
                                                    (num-blocks (<ra> :get-num-blocks)))
                                               (<ra> :load-block "")
                                               (if (not (= num-blocks (<ra> :get-num-blocks)))
                                                   (<ra> :create-seqblock seqtracknum num-blocks pos)))))
                                          "--------------------"
                                          (if (<ra> :release-mode)
                                              '()
                                              (list                                               
                                               "Insert my soundfile"
                                               :enabled for-audiofiles
                                               (lambda ()
                                                 (let* ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type))))
                                                   ;;(<ra> :create-sample-seqblock seqtracknum (<ra> :to-base64 "/home/kjetil/demosong_24000.wav") pos))))
                                                   (<ra> :create-sample-seqblock seqtracknum (<ra> :to-base64 "/home/kjetil/karin_24000.wav") pos))))
                                              ;;(<ra> :create-sample-seqblock seqtracknum (<ra> :to-base64 "/home/kjetil/tannenbaum.ogg") pos)))
                                              )
                                          ;;
                                          (list
                                           "Insert new audio file"
                                           :enabled for-audiofiles
                                           (lambda ()
                                             (let* ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type))))
                                               (create-file-requester "Choose audio file" "" "audio files" (<ra> :get-audiofile-postfixes) #t #f -1
                                                                      (lambda (filename)
                                                                        (<ra> :create-sample-seqblock seqtracknum filename pos))))))
                                          (list
                                           "Split audio file (S)"
                                           :enabled (and seqblocknum
                                                         (not blocknum))
                                           (lambda ()
                                             (let* ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type))))
                                               (split-sample-seqblock pos seqtracknum seqblocknum))))

                                          (let ((get-old-gain (lambda ()
                                                                (db-to-text (if (and seqblocknum
                                                                                     (not blocknum))
                                                                                (<ra> :gain-to-db (<ra> :get-seqblock-gain (<ra> :get-seqblock-id seqblocknum seqtracknum)))
                                                                                0.0)
                                                                            #t))))
                                            (list
                                             (<-> "Set gain (now: " (get-old-gain) ")")
                                             :enabled (and seqblocknum
                                                           (not blocknum)) ;; FIX: Need to ensure new envelope values aren't continually sent to the instruments before enabling gain for editor seqblocks.
                                             (lambda ()
                                               (define new (<ra> :request-float (<-> "New gain (now: " (get-old-gain) ")")
                                                                 -1000
                                                                 1000))
                                               (<ra> :set-seqblock-gain (<ra> :db-to-gain new) (<ra> :get-seqblock-id seqblocknum seqtracknum)))))

                                          (let* ((is-enabled (and seqblocknum
                                                                  (not blocknum)))
                                                 (get-normalized-gain (lambda ()
                                                                        (if is-enabled
                                                                            (get-normalized-seqblock-gain (<ra> :get-seqblock-id seqblocknum seqtracknum))
                                                                            1.0))))
                                            (list
                                             (<-> "Set normalized gain (" (db-to-text (<ra> :gain-to-db (get-normalized-gain)) #t) ")")
                                             :enabled is-enabled
                                             (lambda ()
                                               (<ra> :set-seqblock-gain (get-normalized-gain) (<ra> :get-seqblock-id seqblocknum seqtracknum)))))

                                          (list "Preferences"
                                                :enabled (and seqblocknum (not blocknum))
                                                (lambda ()
                                                  (create-audio-seqblock-gui seqblocknum seqtracknum)))
                              
                                          "--------------------"
                                          
                                          (list (if (> (<ra> :get-num-selected-seqblocks) 1)
                                                    "Copy sequencer blocks"
                                                    "Copy sequencer block")
                                                :enabled (seqblock-selected?)
                                                ra:copy-selected-seqblocks)
                                          
                                          (list (if (> (<ra> :get-num-selected-seqblocks) 1)
                                                    "Cut sequencer blocks"
                                                    "Cut sequencer block")
                                                :enabled (seqblock-selected?)
                                                ra:cut-selected-seqblocks)
                                          
                                          (list (if (> (<ra> :get-num-selected-seqblocks) 1)
                                                    "Delete sequencer blocks"
                                                    "Delete sequencer block")
                                                :enabled (seqblock-selected?)
                                                ra:delete-selected-seqblocks)
                                          
                                          (list (if (> (<ra> :get-num-selected-seqblocks) 1)
                                                    "Paste sequencer blocks"
                                                    "Paste sequencer block")
                                                :enabled (not (empty? *seqblock-clipboard*))
                                                (lambda ()
                                                  (let ((pos (<ra> :get-seq-gridded-time (get-sequencer-pos-from-x X) seqtracknum (<ra> :get-seq-block-grid-type))))
                                                    (<ra> :paste-seqblocks seqtracknum pos))))
                                          
                                          "--------------------"
                                          
                                          ;;(list "Replace with current block"
                                          ;;      :enabled seqblock-info
                                          ;;      (lambda ()
                                          ;;        (undo-block
                                          ;;         (lambda ()
                                          ;;           (define pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
                                          ;;           (<ra> :delete-seqblock seqblocknum seqtracknum)                 
                                          ;;           (<ra> :add-block-to-seqtrack seqtracknum (<ra> :current-block) pos)))))
                                          
                                          (list (if (pair? seqblock-infos) "Replace blocks with existing block" "Replace with existing block")
                                                :enabled (and for-blocks
                                                              (or seqblock-info (pair? seqblock-infos)))
                                                (lambda ()
                                                  (apply popup-menu
                                                         (map (lambda (blocknum)
                                                                (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
                                                                      (lambda ()
                                                                        (undo-block
                                                                         (lambda ()
                                                                           (for-each (lambda (seqblock-info)
                                                                                       (let* ((seqblocknum (seqblock-info :seqblocknum))
                                                                                              (seqtracknum (seqblock-info :seqtracknum))
                                                                                              (pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                                                                                         (set! *current-seqblock-info* #f)
                                                                                         (<ra> :delete-seqblock seqblocknum seqtracknum)
                                                                                         (<ra> :create-seqblock seqtracknum blocknum pos)))
                                                                                     (if (null? seqblock-infos)
                                                                                         (list seqblock-info)
                                                                                         seqblock-infos))))
                                                                        (<ra> :select-block blocknum))))                                                         
                                                              (iota (<ra> :get-num-blocks))))))
                                                
                                          ;;   Sub menues version. It looks better, but it is less convenient.
                                          ;;(list "Replace with existing block"
                                          ;;      :enabled seqblock-info
                                          ;;      (if seqblock-info
                                          ;;          (let ((pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                                          ;;            (map (lambda (blocknum)
                                          ;;                   (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
                                          ;;                         (lambda ()
                                          ;;                           (undo-block
                                          ;;                            (lambda ()
                                          ;;                              (<ra> :delete-seqblock seqblocknum seqtracknum)
                                          ;;                              (<ra> :add-block-to-seqtrack seqtracknum blocknum pos)))
                                          ;;                           (<ra> :select-block blocknum))))                                                         
                                          ;;                 (iota (<ra> :get-num-blocks))))
                                          ;;          (lambda ()
                                          ;;            #f)))

                                          ;; Doesn't make sense since we select block under mouse when right-clicking on it.
                                          ;;(list "Replace with current block"
                                          ;;      :enabled seqblock-info
                                          ;;      (lambda ()
                                          ;;        (let ((pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
                                          ;;              (blocknum (<ra> :current-block)))
                                          ;;          (undo-block
                                          ;;           (lambda ()
                                          ;;             (<ra> :delete-seqblock seqblocknum seqtracknum)
                                          ;;             (<ra> :add-block-to-seqtrack seqtracknum blocknum pos)))
                                          ;;          (<ra> :select-block blocknum))))
                                          
                                          (list (if (pair? seqblock-infos) "Replace blocks with new block" "Replace with new block")
                                                :enabled (and for-blocks
                                                              (or (pair? seqblock-infos)
                                                                  seqblock-info))
                                                (lambda ()
                                                  (let ((blocknum (<ra> :append-block)))
                                                    (undo-block
                                                     (lambda ()
                                                       (for-each (lambda (seqblock-info)
                                                                   (let* ((seqblocknum (seqblock-info :seqblocknum))
                                                                          (seqtracknum (seqblock-info :seqtracknum))
                                                                          (pos (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                                                                     (set! *current-seqblock-info* #f)
                                                                     (<ra> :delete-seqblock seqblocknum seqtracknum)
                                                                     (<ra> :create-seqblock seqtracknum blocknum pos)))
                                                                 (if (null? seqblock-infos)
                                                                     (list seqblock-info)
                                                                     seqblock-infos))))
                                                    (<ra> :select-block blocknum))))
                                          
                                          ;;"-----------------"
                                          "------------------"

                                          (list (if (pair? seqblock-infos) "Clone blocks" "Clone block")
                                                :enabled (and blocknum
                                                              (or (pair? seqblock-infos) seqblock-info)
                                                              (not (<ra> :is-playing-song)))
                                                (lambda ()
                                                  (<ra> :select-block blocknum)
                                                  (<ra> :copy-block)
                                                  (undo-block
                                                   (lambda ()
                                                     (for-each (lambda (seqblock-info)
                                                                 (define new-blocknum (<ra> :append-block))
                                                                 (<ra> :select-block new-blocknum)
                                                                 (<ra> :paste-block))
                                                               (if (null? seqblock-infos)
                                                                   (list seqblock-info)
                                                                   seqblock-infos))))))
                                          
                                          (list "Configure block"
                                                :enabled (and blocknum seqblock-info (not (<ra> :is-playing-song)))
                                                (lambda ()
                                                  (<ra> :select-block blocknum)
                                                  (<ra> :config-block)))

                                          (list
                                           "Rename"
                                           (lambda ()
                                             (let* ((old-name (<ra> :get-seqblock-name seqblocknum seqtracknum))
                                                    (new-name (<ra> :request-string "New name:" #t old-name)))
                                               (when (and (not (string=? new-name ""))
                                                          (not (string=? new-name old-name)))
                                                 (<ra> :set-seqblock-name new-name seqblocknum seqtracknum)))))

                                          (list "Configure color"
                                                :enabled seqblock-info
                                                (lambda ()
                                                  (if blocknum
                                                      (<ra> :color-dialog (<ra> :get-block-color blocknum) -1
                                                            (lambda (color)
                                                              (<ra> :set-block-color color blocknum)))
                                                  (let ((filename (<ra> :get-seqblock-sample seqblocknum seqtracknum)))
                                                    (<ra> :color-dialog (<ra> :get-audiofile-color filename) -1
                                                          (lambda (color)
                                                            (<ra> :set-audiofile-color color filename)))))))
                                          
                                          (list "Generate new color"
                                                :enabled seqblock-info
                                                (lambda ()
                                                  (let ((color (<ra> :generate-new-color 0.9)))
                                                    (if blocknum
                                                        (<ra> :set-block-color color blocknum)
                                                        (let ((filename (<ra> :get-seqblock-sample seqblocknum seqtracknum)))
                                                          (<ra> :set-audiofile-color color filename))))))
                                          
                                          (list "Enable envelope"
                                                :check (and seqblocknum (<ra> :get-seqblock-automation-enabled *0* seqblocknum seqtracknum))
                                                :enabled seqblocknum
                                                (lambda (enable)
                                                  (<ra> :set-seqblock-automation-enabled enable *0* seqblocknum seqtracknum)))

                                          (list "Enable/disable editor tracks"
                                                :enabled (and blocknum seqblocknum)
                                                (lambda ()
                                                  (show-seqblock-track-on-off-configuration seqtracknum seqblocknum blocknum)))

                                          ;;(list "Reset stretch"
                                          ;;      :enabled (and seqblocknum
                                          ;;                    (not (= 1.0 (<ra> :get-seqblock-stretch seqblocknum seqtracknum))))
                                          ;;      (lambda ()
                                          ;;        (c-display "stretch:" (<ra> :get-seqblock-stretch seqblocknum seqtracknum))
                                          ;;        (define start-time (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
                                          ;;        (define blocklength (<ra> :get-block-length blocknum))
                                          ;;        (<ra> :position-seqblock start-time (+ start-time blocklength) seqblocknum seqtracknum)
                                          ;;        (c-display "hepp")))
                                                
                                          ;;
                                          ;;(list "Remove pause"
                                          ;;      :enabled #f
                                          ;;      (lambda ()
                                          ;;        #f))

                                          "-----------------"

                                          "New automation" (lambda ()
                                                             (create-sequencer-automation seqtracknum X Y))

                                          ;;"-----------------"
                                          ;;
                                          ;;"Insert sequencer track" (lambda ()
                                          ;;                           (<ra> :insert-seqtrack seqtracknum))
                                          ;;(list "Delete sequencer track"
                                          ;;      :enabled (> (<ra> :get-num-seqtracks) 1)
                                          ;;      (lambda ()
                                          ;;        (set! *current-seqblock-info* #f)
                                          ;;        (<ra> :delete-seqtrack seqtracknum)))
                                          ;;"Append sequencer track" (lambda ()
                                          ;;                           (<ra> :append-seqtrack))
                                          (get-sequencer-conf-menues)
                                          ))))))))
                                                                                 


;; right size handle in navigator
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (define box (<ra> :get-box seqnav-right-size-handle))
                                            ;;(c-display "  RIGHT box" box
                                            ;;           (inside-box box X Y)
                                            ;;           (<ra> :get-seqnav-right-size-handle-x2))
                                            (and (inside-box box X Y)
                                                 (<ra> :get-seqnav-right-size-handle-x2)))
                        :Get-x1 (lambda (_)
                                  (1- (<ra> :get-seqnav-left-size-handle-x1)))
                        :Get-x2 (lambda (_)
                                  (<ra> :get-seqnav-x2))
                        :Get-min-value (lambda (_)
                                         (- (<ra> :get-seqnav-right-size-handle-x2) (<ra> :get-seqnav-right-size-handle-x1)))
                        :Get-max-value (lambda (_)
                                         (<ra> :get-seqnav-x2))
                        ;;(<ra> :get-seqnav-x2))
                        ;;:Get-x (lambda (_)                                 
                        ;;         (/ (+ (<ra> :get-seqnav-right-size-handle-x1)
                        ;;               (<ra> :get-seqnav-right-size-handle-x2))
                        ;;            2))
                        :Get-value (lambda (Value)
                                     Value)
                        :Make-undo (lambda (_)
                                     50)
                        :Move (lambda (_ Value)
                                (define song-length (<ra> :get-sequencer-song-length-in-frames))
                                (define new-end-time (floor (scale Value
                                                                   (<ra> :get-seqnav-x1) (<ra> :get-seqnav-x2);; (<ra> :get-seqnav-right-size-handle-x1)
                                                                   0 song-length)))
                                ;;(c-display "       Move" Value (/ new-start-time 48000.0) "x1:" (<ra> :get-seqnav-x1) "x2:" (<ra> :get-seqnav-x2) "end:" (/ (<ra> :get-sequencer-visible-end-time) 48000.0))
                                (define start-time (<ra> :get-sequencer-visible-start-time))
                                ;;(c-display "new-end-time:" (/ new-end-time 48000.0) Value)
                                (<ra> :set-sequencer-visible-end-time (min song-length (max (1+ start-time) new-end-time))))
                        :Publicize (lambda (_)
                                     (set-editor-statusbar (<-> (two-decimal-string (/ (<ra> :get-sequencer-visible-start-time) (<ra> :get-sample-rate)))
                                                                    " -> "
                                                                    (two-decimal-string (/ (<ra> :get-sequencer-visible-end-time) (<ra> :get-sample-rate))))))
                        :Mouse-pointer-func ra:set-horizontal-resize-mouse-pointer
                        :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                        )

;; left size handle in navigator
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (define box (<ra> :get-box seqnav-left-size-handle))
                                            ;;(c-display "  LEFT box" box
                                            ;;           (inside-box box X Y)
                                            ;;           (<ra> :get-seqnav-left-size-handle-x1))
                                            (and (inside-box box X Y)
                                                 (<ra> :get-seqnav-left-size-handle-x1)))
                        :Get-x1 (lambda (_)
                                  (<ra> :get-seqnav-x1))
                        :Get-x2 (lambda (_)
                                  (1- (<ra> :get-seqnav-right-size-handle-x2)))
                        :Get-min-value (lambda (_)
                                         (<ra> :get-seqnav-x1))
                        :Get-max-value (lambda (_)
                                         (1- (<ra> :get-seqnav-right-size-handle-x2)))
                        ;;(<ra> :get-seqnav-x2))
                        ;;:Get-x (lambda (_)                                 
                        ;;         (/ (+ (<ra> :get-seqnav-left-size-handle-x1)
                        ;;               (<ra> :get-seqnav-left-size-handle-x2))
                        ;;            2))
                        :Get-value (lambda (Value)
                                     Value)
                        :Make-undo (lambda (_)
                                     50)
                        :Move (lambda (_ Value)
                                (define song-length (<ra> :get-sequencer-song-length-in-frames))
                                (define new-start-time (floor (scale Value
                                                                     (<ra> :get-seqnav-x1) (<ra> :get-seqnav-x2);; (<ra> :get-seqnav-right-size-handle-x1)
                                                                     0 song-length)))
                                ;;(c-display "       Move" Value (/ new-start-time 48000.0) "x1:" (<ra> :get-seqnav-x1) "x2:" (<ra> :get-seqnav-x2) "end:" (/ (<ra> :get-sequencer-visible-end-time) 48000.0))
                                (define end-time (<ra> :get-sequencer-visible-end-time))
                                (<ra> :set-sequencer-visible-start-time (max 0 (min (1- end-time) new-start-time))))
                        :Publicize (lambda (_)
                                     (set-editor-statusbar (<-> (two-decimal-string (/ (<ra> :get-sequencer-visible-start-time) (<ra> :get-sample-rate)))
                                                                    " -> "
                                                                    (two-decimal-string (/ (<ra> :get-sequencer-visible-end-time) (<ra> :get-sample-rate))))))
                        :Mouse-pointer-func ra:set-horizontal-resize-mouse-pointer
                        :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                        )

;; seqtrack select
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-mouse-cycle
; (make-mouse-cycle
;  :press-func (lambda (Button X Y)
;                (let ((seqtracknum (get-seqtracknum X Y)))
;                  (if seqtracknum
;                      (begin
;                        (<ra> :set-curr-seqtrack seqtracknum)))
;                  #f))))


(define (get-seqnav-width)
  (define space-left (- (<ra> :get-seqnav-left-size-handle-x1)
                        (<ra> :get-seqnav-x1)))
  (define space-right (- (<ra> :get-seqnav-x2)
                         (<ra> :get-seqnav-right-size-handle-x2)))
  (+ space-left space-right))

(define (get-seqnav-move-box)
  (make-box2 (<ra> :get-seqnav-left-size-handle-x2) (<ra> :get-seqnav-left-size-handle-y1)
             (<ra> :get-seqnav-right-size-handle-x1) (<ra> :get-seqnav-right-size-handle-y2)))


;; move navigator left/right
(add-horizontal-handler :Get-handler-data (lambda (X Y)
                                            (if (> (get-seqnav-width) 0)
                                                (let ((box (get-seqnav-move-box)))
                                                  (and (inside-box box X Y)
                                                       (<ra> :get-seqnav-left-size-handle-x1)))
                                                #f))
                        :Get-x1 (lambda (_)
                                  0)
                        :Get-x2 (lambda (_)
                                  (get-seqnav-width))
                        :Get-min-value (lambda (_)
                                         (<ra> :get-seqnav-x1))
                        :Get-max-value (lambda (_)
                                         (+ (<ra> :get-seqnav-x1) (get-seqnav-width)))
                        ;;:Get-x (lambda (_)
                        ;;         (+ org-x (- (<ra> :get-seqnav-left-size-handle-x1)
                        ;;                     org-seqnav-left)))
;                                 (/ (+ (<ra> :get-seqnav-left-size-handle-x1)
;                                       (<ra> :get-seqnav-right-size-handle-x2))
;                                    2))
                        :Get-value (lambda (Value)
                                     Value)
                        :Make-undo (lambda (_)
                                     50)
                        :Move (lambda (_ Value)
                                (define old-start-time (<ra> :get-sequencer-visible-start-time))
                                (define song-length (<ra> :get-sequencer-song-length-in-frames))
                                (define new-start-time (floor (scale Value
                                                                     (<ra> :get-seqnav-x1) (<ra> :get-seqnav-x2);; (<ra> :get-seqnav-right-size-handle-x1)
                                                                     0 song-length)))
                                ;;(c-display "       Move" Value (/ new-start-time 48000.0) "x1:" (<ra> :get-seqnav-x1) "x2:" (<ra> :get-seqnav-x2) "end:" (/ (<ra> :get-sequencer-visible-end-time) 48000.0))
                                (define end-time (<ra> :get-sequencer-visible-end-time))
                                (define new-start-time2 (max 0 (min (1- end-time) new-start-time)))
                                
                                (define diff (- new-start-time2 old-start-time))
                                (define new-end-time (+ end-time diff))
                                (define new-end-time2 (min song-length (max (1+ new-start-time2) new-end-time)))

                                (<ra> :set-sequencer-visible-start-time new-start-time2)
                                (<ra> :set-sequencer-visible-end-time new-end-time2))
                                
                        :Publicize (lambda (_)
                                     (set-editor-statusbar (<-> (two-decimal-string (/ (<ra> :get-sequencer-visible-start-time) (<ra> :get-sample-rate)))
                                                                    " -> "
                                                                    (two-decimal-string (/ (<ra> :get-sequencer-visible-end-time) (<ra> :get-sample-rate))))))
                        
                        :Mouse-pointer-func ra:set-closed-hand-mouse-pointer
                        :Get-guinum (lambda () (<gui> :get-sequencer-gui))
                        )




#||
(load "lint.scm")
(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #t)
(define *report-multiply-defined-top-level-functions* #f) ; same name defined at top level in more than one file
(define *report-shadowed-variables* #t)
(define *report-minor-stuff* #t)                          ; let*, docstring checks, (= 1.5 x), numerical and boolean simplification
(lint "/home/kjetil/radium/bin/scheme/mouse/mouse.scm")

(c-display (<ra> :add-temponode 2.1 -5.0))

(box-to-string (find-temponode 210 1210))

(<ra> :set-temponode 1 65/2 8.0)
(<ra> :set-temponode 1 0.01 8.0)
(<ra> :set-temponode 3 100.01 8.0)

(<ra> :control-pressed)

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

(<ra> :move-mouse-pointer 50 50)

(<ra> :append-seqtrack)
(<ra> :set-curr-seqtrack 0)
(<ra> :set-curr-seqtrack 1)
(<ra> :set-curr-seqtrack 2)

(box-to-string (ra:get-box2 seqtrack 0))
(box-to-string (ra:get-box2 seqtrack 1))

(let ((time1 5.0)
      (time2 8.0)
      (val1 0.8)
      (val2 20.1))
  (scale 1.0 val1 val2 time1 time2))

(let ((time1 5.0)
      (time2 8.0)
      (val1 2.1)
      (val2 0.8))
  (scale 1.0 val1 val2 time1 time2))


||#


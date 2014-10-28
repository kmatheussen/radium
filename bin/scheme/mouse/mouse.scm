(provide 'mouse/mouse.scm)

(define *left-button* 1)
(define *right-button* 5)

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

(delafina (add-delta-mouse-handler :press :move-and-release)
  (define start-x #f)
  (define start-y #f)
  (define value #f)
  (define (call-move-and-release $button $x $y)
    (define dx (if (ra:ctrl-pressed)
                   (/ (- $x start-x)
                      10.0)
                   (- $x start-x)))
    (define dy (if (ra:ctrl-pressed)
                   (/ (- $y start-y)
                      10.0)
                   (- $y start-y)))
    (set! start-x $x)
    (set! start-y $y)
    (set! value (move-and-release $button
                                  dx
                                  dy
                                  value)))
  (add-mouse-cycle (make-mouse-cycle
                    :press-func (lambda ($button $x $y)
                                  (set! value (press $button $x $y))
                                  (if value
                                      (begin
                                        (set! start-x $x)
                                        (set! start-y $y)
                                        #t)
                                      #f))
                    :drag-func  call-move-and-release
                    :release-func call-move-and-release)))
  


;; Functions called from radium
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (radium-mouse-press $button $x $y)
  (if (not *current-mouse-cycle*)
      (set! *current-mouse-cycle* (get-mouse-cycle $button $x $y)))
  current-mouse-cycle)
  
(define (radium-mouse-press $button $x $y)
  ;;(c-display "mouse press" $button $x $y)
  (if (not *current-mouse-cycle*)
      (set! *current-mouse-cycle* (get-mouse-cycle $button $x $y)))
  *current-mouse-cycle*)

(define (radium-mouse-move $button $x $y)
  ;;(c-display "mouse move" $button $x $y (ra:ctrl-pressed) (ra:shift-pressed))
  (if *current-mouse-cycle*
      (begin 
        ((*current-mouse-cycle* :drag-func) $button $x $y)
        #t)
      (begin
        (run-mouse-move-handlers $button $x $y)
        #f)))

(define (radium-mouse-release $button $x $y)
  ;;(c-display "mouse release" $button $x $y)
  (if *current-mouse-cycle*
      (begin
        ((*current-mouse-cycle* :release-func) $button $x $y)
        (set! *current-mouse-cycle* #f)
        (ra:cancel-current-node)
        #t)
      #f))


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

(add-mouse-move-handler
 :move (lambda (Button X Y)
         (set! *current-track-num* (get-track-num X Y))
         (c-display "curr: " *current-track-num*)
         (cond (*current-track-num*
                (ra:set-mouse-track *current-track-num*))
               ((inside-box (ra:get-box temponode-area) X Y)
                (ra:set-mouse-track-to-reltempo))
               (else
                (ra:set-no-mouse-track)))
         #f))

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
                                           (get-subtrack-from-x $x *current-track-num*)))
         ;;(c-display "current-subtrack-num" *current-subtrack-num*)
         #f))

;; reltempo
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; status bar
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (if (inside-box (ra:get-box reltempo-slider) $x $y)
             (ra:show-reltempo-in-statusbar))))



;; slider
(add-delta-mouse-handler
 :press (lambda ($button $x $y)
          ;;(c-display "inside? " (inside-box (ra:get-box reltempo-slider) $x $y) $x $y "box:" (box-to-string (ra:get-box reltempo-slider)))
          (and (= $button *left-button*)
               (inside-box (ra:get-box reltempo-slider) $x $y)
               (begin
                 (ra:undo-reltempo)
                 (ra:get-reltempo))))

 :move-and-release (lambda ($button $dx $dy $org-reltempo)
                     (define box          (ra:get-box reltempo-slider))
                     (define max-reltempo (ra:get-max-reltempo))
                     (define min-reltempo (ra:get-min-reltempo))
                     (define new-value    (+ $org-reltempo
                                             (scale $dx
                                                    0 (box :width)
                                                    min-reltempo max-reltempo)))
                     (ra:set-reltempo new-value)
                     new-value)
 )

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
  :node)

(define-struct node-mouse-cycle
  :press
  :move-and-release
  )

(define *move-existing-node-mouse-cycles* '())
(define *create-new-node-mouse-cycles* '())

(define-match get-cycle-and-node
  ______ _ _ ()             :> #f
  Button X Y (Cycle . Rest) :> (let ((Node ((Cycle :press) Button X Y)))
                                 (if Node
                                     (make-move-node-handler :move (Cycle :move-and-release) :node Node)
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
                     (define Old-node (Cycle-and-node :node))
                     (define New-node (Move Button Dx Dy Old-node))
                     (make-move-node-handler :move Move :node New-node))
 )



(delafina (add-node-mouse-handler :Get-area-box
                                  :Get-existing-node-info
                                  :Get-min-value
                                  :Get-max-value
                                  :Make-undo
                                  :Create-new-node
                                  :Move-node
                                  :Get-pixels-per-value-unit #f
                                  )
  
  (define-struct node
    :node-info
    :value
    :y)

  (define (press-and-move-existing-node Button X Y)
    (define area-box (Get-area-box))
    (and (= Button *left-button*)
         area-box
         (inside-box-forgiving area-box X Y)
         (Get-existing-node-info X
                                 Y
                                 (lambda (Node-info Value Node-y)
                                   (Make-undo)
                                   (make-node :node-info Node-info
                                              :value Value
                                              :y Node-y
                                              )))))

  (define (press-and-create-new-node Button X Y)
    (define area-box (Get-area-box))
    (and (= Button *left-button*)
         area-box
         (inside-box area-box X Y)
         (begin
           (define min (Get-min-value))
           (define max (Get-max-value))
           ;;(c-display "min/max" min max)
           (define value (scale X
                                (area-box :x1) (area-box :x2)
                                min max))
           (Create-new-node value
                            (ra:get-place-from-y Y)
                            (lambda (Node-info Value)
                              (make-node :node-info Node-info
                                         :value Value
                                         :y Y))))))

  (define (move-and-release Button Dx Dy Node)
    ;;(c-display "temponode box" (box-to-string ($temponode :box)))
    (define min (Get-min-value))
    (define max (Get-max-value))
    (define area-box (Get-area-box))
    (define node-area-width (area-box :width))
    (define pixels-per-value-unit (if Get-pixels-per-value-unit
                                      (Get-pixels-per-value-unit)
                                      (/ node-area-width
                                         (- max min))))
    (define new-value (+ (Node :value)
                         (/ Dx
                            pixels-per-value-unit)))
    ;;(c-display "num" ($node :num) ($get-num-nodes-func) "value" $dx ($node :value) (node-area :x1) (node-area :x2) ($get-node-value-func ($node :num)))
    (define new-y (+ (Node :y)
                     Dy))                                                 
    ;;(c-display "dx:" $dx "value:" (* 1.0 ($temponode :value)) 0 ((ra:get-box temponode-area) :width) "min/max:" min max "new-value: " new-value)
    (Move-node (Node :node-info) new-value (ra:get-place-from-y new-y))
    (make-node :node-info (Node :node-info)
               :value new-value
               :y new-y))

  (define move-existing-node-mouse-cycle (make-node-mouse-cycle :press press-and-move-existing-node
                                                                :move-and-release move-and-release))
  
  (define create-new-node-mouse-cycle (make-node-mouse-cycle :press press-and-create-new-node
                                                             :move-and-release move-and-release))

  (push-back! *move-existing-node-mouse-cycles* move-existing-node-mouse-cycle)
  (push-back! *create-new-node-mouse-cycles* create-new-node-mouse-cycle)

  )

      

;; temponodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (if (inside-box (ra:get-box temponode-area) $x $y)
             (c-display "inside" $x $y))))
||#


(define (get-temponode-box $num)
  (get-common-node-box (ra:get-temponode-x $num)
                       (ra:get-temponode-y $num)))


(add-node-mouse-handler :Get-area-box (lambda () (ra:get-box temponode-area))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (match (list (find-node X Y get-temponode-box (ra:get-num-temponodes)))
                                                         (existing-box Num Box) :> (callback Num (ra:get-temponode-value Num) (Box :y))
                                                         _                      :> #f))
                        :Get-min-value (lambda () (- (1- (ra:get-temponode-max))))
                        :Get-max-value (lambda () (1- (ra:get-temponode-max)))
                        :Make-undo ra:undo-temponodes
                        :Create-new-node (lambda (Value Place callback)
                                           (define Num (ra:create-temponode Value Place))
                                           (if (= -1 Num)
                                               #f
                                               (callback Num (ra:get-temponode-value Num))))
                        :Move-node ra:set-temponode
                        :Get-pixels-per-value-unit #f
                        )                        

;; delete temponode
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
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
                                                 (ra:set-current-tempo-node Num)
                                                 #t)
                     _                      :> (begin
                                                 (ra:cancel-current-node)
                                                 #f)))))



;; current note
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
(delafina (get-note-num :$x
                        :$y
                        :$num 0
                        :$num-notes (and *current-track-num* (ra-get-num-notes *current-track-num*)))
  
  (define note-y (delay (ra:get-note-y *num* *current-track-num*)))
  
  (cond ((not *current-track-num*)
         #f)
        ((= $num $num-notes)
         #f)
        ((< $y (force note-y))
         #f)
        (else
         (get-note-num $x $y (1+ $num) $num-notes))))
        

(define *current-note-num* #f)

(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (set! *current-note-num* (get-note-num $x $y))
         (c-display "curr track/note:" *current-track-num* *current-note-num*)
         #f))

||#


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

(add-delta-mouse-handler
 :press (lambda ($button $x $y)
          (c-display $x $y)
          #f))

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

;; add and move
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (ra:get-box track-notes *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (match (list (find-node X Y get-pitch-box (ra:get-num-pitches *current-track-num*)))
                                                         (existing-box Num Box) :> (callback Num (ra:get-pitch-value Num *current-track-num*) (Box :y))
                                                         _                      :> #f))
                        :Get-min-value get-min-pitch-in-current-track
                        :Get-max-value get-max-pitch-in-current-track
                        :Make-undo (lambda () (ra:undo-notes *current-track-num*))
                        :Create-new-node (lambda (Value Place callback)
                                           (define Num (ra:create-pitch Value Place *current-track-num*))
                                           (if (= -1 Num)
                                               #f
                                               (callback Num (ra:get-pitch-value Num *current-track-num*))))
                        :Move-node (lambda (Num Value Place) (ra:set-pitch Num Value Place *current-track-num*))
                        :Get-pixels-per-value-unit (lambda ()
                                                     5.0)
                        )


;; delete pitch
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
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
                                                 ;;(c-display "--" Num "highlight")
                                                 (ra:set-current-pitch Num *current-track-num*)
                                                 #t)
                     _                      :> (begin
                                                 (ra:cancel-current-node)
                                                 #f)))))




;; velocities
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct velocity-info
  :tracknum
  :notenum
  :velocitynum
  :value
  :y  
  )

(define (highest-rated-velocity-info . Args)
  (match (list Args)
         ()          :> #f
         (#f . Rest) :> (apply highest-rated-velocity-info Rest)
         (A  . ____) :> A))
#||
(highest-rated-velocity-info)
(highest-rated-velocity-info 2)
(highest-rated-velocity-info #f 3)
(highest-rated-velocity-info 4 #f)
||#

(define-match get-velocity-2
  X Y Tracknum Notenum Velocitynum Velocitynum      :> #f
  X Y Tracknum Notenum Velocitynum Total-Velocities :> (begin                                                                     
                                                         (define box (get-common-node-box (ra:get-velocity-x Velocitynum Notenum Tracknum)
                                                                                          (ra:get-velocity-y Velocitynum Notenum Tracknum)))
                                                         (if (> (box :y1) Y)
                                                             #f
                                                             (highest-rated-velocity-info (get-velocity-2 X Y Tracknum Notenum (1+ Velocitynum) Total-Velocities)
                                                                                          (and (inside-box box X Y)
                                                                                               (make-velocity-info :velocitynum Velocitynum
                                                                                                                   :notenum Notenum
                                                                                                                   :tracknum Tracknum
                                                                                                                   :value (ra:get-velocity-value Velocitynum Notenum Tracknum)
                                                                                                                   :y (box :y)
                                                                                                                   ))))))

(define-match get-velocity-1
  X Y Tracknum Notenum Notenum     :> #f
  X Y Tracknum Notenum Total-Notes :> (highest-rated-velocity-info (get-velocity-1 X Y Tracknum (1+ Notenum) Total-Notes)
                                                                   (get-velocity-2 X Y Tracknum Notenum 0 (ra:get-num-velocities Notenum Tracknum))))
                                   
  
(define-match get-velocity-0
  X Y -1       :> #f
  X Y Tracknum :> #f :where (>= Tracknum (ra:get-num-tracks))
  X Y Tracknum :> (get-velocity-1 X Y Tracknum 0 (ra:get-num-notes Tracknum)))
  
(define-match get-velocity-info
  X Y #f       :> (get-velocity-info X Y 0)
  X Y Tracknum :> (highest-rated-velocity-info (get-velocity-0 X Y (1- Tracknum)) ;; node in the prev track can overlap into the current track
                                               (get-velocity-0 X Y Tracknum)
                                               (get-velocity-0 X Y (1+ Tracknum)))) ;; node in the next track can overlap into the current track


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
                                       (get-note-num $x $y)))
         ;;(c-display "current-note-num" *current-note-num* ", subtrack: " *current-subtrack-num* ", track: " *current-track-num*)
         (if *current-note-num*
             (ra:set-mouse-note *current-note-num* *current-track-num*)
             (ra:set-no-mouse-note))
         #f))

(define (get-velocity-box $num)
  (get-common-node-box (ra:get-velocity-x $num *current-note-num* *current-track-num*)
                       (ra:get-velocity-y $num *current-note-num* *current-track-num*)))

(define (get-current-num-velocities)
  (ra:get-num-velocities *current-note-num* *current-track-num*))

;; add and move velocity
(add-node-mouse-handler :Get-area-box (lambda ()
                                        (and *current-track-num*
                                             (ra:get-box track *current-track-num*)))
                        :Get-existing-node-info (lambda (X Y callback)
                                                  (define velocity-info (get-velocity-info X Y *current-track-num*))
                                                  (if velocity-info
                                                      (ra:set-mouse-note (velocity-info :notenum) (velocity-info :tracknum)))
                                                  (and velocity-info
                                                       (callback velocity-info (velocity-info :value) (velocity-info :y))))
                        :Get-min-value (lambda () 0.0)
                        :Get-max-value (lambda () 1.0)
                        :Make-undo (lambda () (ra:undo-notes *current-track-num*))
                        :Create-new-node (lambda (Value Place callback)
                                           (and #t *current-note-num*
                                                (begin
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
                        :Move-node (lambda (velocity-info Value Place)
                                     (ra:set-velocity (velocity-info :velocitynum) Value Place (velocity-info :notenum) (velocity-info :tracknum)))
                        )

;; delete velocity
(add-mouse-cycle
 (make-mouse-cycle
  :press-func (lambda ($button $x $y)
                (and (= $button *right-button*)
                     *current-note-num*
                     *current-track-num* 
                     (inside-box-forgiving (ra:get-box track-fx *current-track-num*) $x $y)
                     (match (list (find-node $x $y get-velocity-box (ra:get-num-velocities *current-note-num* *current-track-num*)))
                            (existing-box Num Box) :> (begin
                                                        (ra:undo-notes *current-track-num*)
                                                        (ra:delete-velocity Num *current-note-num* *current-track-num*)
                                                        #t)
                            _                      :> #f)))))


;; show current velocity
(add-mouse-move-handler
 :move (lambda (Button X Y)
         (and *current-track-num*
              (inside-box-forgiving (ra:get-box track *current-track-num*) X Y)
              (begin
                (define velocity-info (get-velocity-info X Y *current-track-num*))
                (if velocity-info
                    (begin
                      (ra:set-mouse-note (velocity-info :notenum) (velocity-info :tracknum))
                      (ra:set-current-velocity-node (velocity-info :velocitynum) (velocity-info :notenum) (velocity-info :tracknum))
                      #t)
                    (begin
                      (ra:cancel-current-node)
                      #f))))))

#||
(ra:get-num-velocities 0 0)

(ra:get-velocitynode-y 0 0)
(ra:get-velocitynode-y 2 0)
(ra:get-velocity-value 7 1)



||#


;; track borders
;;;;;;;;;;;;;;;;;;;;;;;;;;;

                         
#||

(add-delta-mouse-handler
 :press (lambda ($button $x $y)
          (and (= $button *left-button*)               
               (inside-box (ra:get-box reltempo-slider) $x $y)
               (begin
                 (ra:undo-reltempo)
                 (ra:get-reltempo))))

 :move-and-release (lambda ($button $dx $dy $org-reltempo)
                     (define box          (ra:get-box reltempo-slider))
                     (define min-reltempo (ra:get-min-reltempo))
                     (define max-reltempo (ra:get-max-reltempo))
                     (define new-value    (+ $org-reltempo
                                             (scale $dx
                                                    0 (box :width)
                                                    min-reltempo max-reltempo)))
                     (ra:set-reltempo new-value)
                     new-value)
 )
||#




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

||#


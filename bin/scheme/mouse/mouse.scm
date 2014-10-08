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


(define (get-mouse-cycle button x y)
  #f)


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

(define (get-mouse-cycle $button $x $y)
  (find-if (lambda (cycle)
             ((cycle :press-func) $button $x $y))
           *mouse-cycles*))
  
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


(delafina (get-track-num :$x
                         :$y
                         :$num 0
                         :$num-tracks (ra:get-num-tracks))
  (define box (delay (ra:get-box track $num)))
  ;;(c-display (box-to-string (force box)))
  
  (cond ((= $num $num-tracks)
         #f)
        ((< $x ((force box) :x1))
         #f)
        ((inside-box (force box) $x $y)
         $num)
        (else
         (get-track-num $x $y (1+ $num) $num-tracks))))         

#||
(get-track-num 650 50)
||#

(define *current-track-num* #f)

(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (set! *current-track-num* (get-track-num $x $y))
         ;;(c-display "curr: " *current-track-num*)
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
                     (define min-reltempo (ra:get-min-reltempo))
                     (define max-reltempo (ra:get-max-reltempo))
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
  (define width/2 (/ (ra:get-temponode-width) 1.5)) ;; if changing 1.5 here, also change 1.5 in draw_skewed_box in Render.cpp
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
         (find-node $x $y $get-node-box $num-nodes (1+ $num)))))


(delafina (add-node-mouse-handler :$get-area-box-func
                                  :$get-node-box
                                  :$get-num-nodes-func
                                  :$get-node-value-func
                                  :$get-max-value-func
                                  :$get-min-value-func
                                  :$make-undo-func
                                  :$create-node-func
                                  :$move-node-func
                                  :$get-pixels-per-value-unit #f
                                  )

  (define-struct node
    :num
    :box
    :value
    :y)

  (add-delta-mouse-handler
   :press (lambda ($button $x $y)
            ;;(c-display "inside? " (inside-box (ra:get-box reltempo-slider) $x $y) $x $y "box:" (box-to-string (ra:get-box reltempo-slider)))
            (and (= $button *left-button*)
                 (inside-box ($get-area-box-func) $x $y)
                 (match (list (find-node $x $y $get-node-box ($get-num-nodes-func)))
                        before :> #f
                        after  :> #f
                        (existing-box Num Box) :> (begin
                                                    ($make-undo-func)
                                                    (make-node :num Num
                                                               :box Box
                                                               :value ($get-node-value-func Num)
                                                               :y (Box :y)))
                        (new-box _)            :> (begin
                                                    (define max ($get-max-value-func))
                                                    (define min ($get-min-value-func))
                                                    (define node-area ($get-area-box-func))
                                                    (define value (scale $x
                                                                         (node-area :x1) (node-area :x2)
                                                                         min max))
                                                    (define new-num ($create-node-func value (ra:get-place-from-y $y)))
                                                    (if new-num
                                                        (let ((new-box ($get-node-box new-num)))
                                                          (make-node :num new-num
                                                                     :box new-box
                                                                     :value ($get-node-value-func new-num)
                                                                     :y $y))
                                                        #f)))))
   
   :move-and-release (lambda ($button $dx $dy $node)
                       ;;(c-display "temponode box" (box-to-string ($temponode :box)))
                       (define max ($get-max-value-func))
                       (define min ($get-min-value-func))
                       (define node-area ($get-area-box-func))
                       (define node-area-width (node-area :width))
                       (define pixels-per-value-unit (if $get-pixels-per-value-unit
                                                         ($get-pixels-per-value-unit)
                                                         (/ node-area-width
                                                            (- max min))))
                       (define new-value (+ ($node :value)
                                            (/ $dx
                                               pixels-per-value-unit)))
                       (c-display "value" $dx ($node :value) (node-area :x1) (node-area :x2) ($get-node-value-func ($node :num)))
                       (define new-y (+ ($node :y)
                                        $dy))                                                 
                       ;;(c-display "dx:" $dx "value:" (* 1.0 ($temponode :value)) 0 ((ra:get-box temponode-area) :width) "min/max:" min max "new-value: " new-value)
                       ($move-node-func ($node :num) new-value (ra:get-place-from-y new-y))
                       (make-node :num ($node :num)
                                  :box ($node :box)
                                  :value new-value
                                  :y new-y))
   )
  )
      

;; temponodearea
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

;; add and move temponode
(add-node-mouse-handler :$get-area-box-func (lambda () (ra:get-box temponode-area))
                        :$get-node-box get-temponode-box
                        :$get-num-nodes-func ra:get-num-temponodes
                        :$get-node-value-func ra:get-temponode-value
                        :$get-max-value-func (lambda () (1- (ra:get-temponode-max)))
                        :$get-min-value-func (lambda () (- (1- (ra:get-temponode-max))))
                        :$make-undo-func ra:undo-temponodes
                        :$create-node-func (lambda (value pos)
                                             (ra:undo-temponodes)
                                             (ra:create-temponode value pos))
                        :$move-node-func ra:set-temponode)
                        

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


;; show current temponode
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and (inside-box (ra:get-box temponode-area) $x $y)
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
  (c-display "get-pitch-box" $num)
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

;; add and move
(add-node-mouse-handler :$get-area-box-func (lambda ()
                                              (if *current-track-num*
                                                  (ra:get-box track *current-track-num*)))
                        :$get-node-box get-pitch-box
                        :$get-num-nodes-func (lambda () (ra:get-num-pitches *current-track-num*))
                        :$get-node-value-func (lambda ($num) (ra:get-pitch-value $num *current-track-num*))
                        :$get-max-value-func (lambda () 40)
                        :$get-min-value-func (lambda () 60)
                        :$make-undo-func (todofunc "make-pitch-undo")
                        :$create-node-func (todofunc "create-pitch" #f)
                        :$move-node-func (lambda ($num $value $place) (ra:set-pitch $num $value $place *current-track-num*))
                        :$get-pixels-per-value-unit (lambda ()
                                                      5.0))


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


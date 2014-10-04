(provide 'mouse/mouse.scm)

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
  :drag-func (lambda x #f)
  :release-func (lambda x #f)
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
          (and (or #t (= $button *left-button*))
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

(define (get-node-box $x $y $num $node-width)
  (define width/2 (/ $node-width 1.5)) ;; if changing 1.5 here, also change 1.5 in draw_skewed_box in Render.cpp
  (define x1 (- $x width/2))
  (define y1 (- $y width/2))
  (define x2 (+ $x width/2))
  (define y2 (+ $y width/2))
  (make-box2 x1 y1 x2 y2))
  

(delafina (find-node :$x
                     :$y
                     :$get-node-x-func
                     :$get-node-y-func
                     :$num-nodes
                     :$num 0
                     :$node-width (ra:get-temponode-width))

  (define box (get-node-box ($get-node-x-func $num) ($get-node-y-func $num) $num $node-width))
  
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
         (find-node $x $y $get-node-x-func $get-node-y-func $num-nodes (1+ $num) $node-width))))

(delafina (add-node-mouse-handler :$get-area-box-func
                                  :$get-node-x-func
                                  :$get-node-y-func
                                  :$get-num-nodes-func
                                  :$get-node-value-func
                                  :$get-max-value-func
                                  :$get-min-value-func
                                  :$make-undo-func
                                  :$create-node-func
                                  :$move-node-func
                                  )

  (define-struct node
    :num
    :box
    :value
    :y)

  (add-delta-mouse-handler
   :press (lambda ($button $x $y)
            ;;(c-display "inside? " (inside-box (ra:get-box reltempo-slider) $x $y) $x $y "box:" (box-to-string (ra:get-box reltempo-slider)))
            (and (or #t (= $button *left-button*))
                 (inside-box ($get-area-box-func) $x $y)
                 (match (list (find-node $x $y $get-node-x-func $get-node-y-func ($get-num-nodes-func)))
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
                                                    ($make-undo-func)
                                                    (define new-num ($create-node-func value (ra:get-place-from-y $y)))
                                                    (define new-box (get-node-box ($get-node-x-func new-num)
                                                                                  ($get-node-y-func new-num)
                                                                                  new-num
                                                                                  (ra:get-temponode-width)))
                                                    (make-node :num new-num
                                                               :box new-box
                                                               :value ($get-node-value-func new-num)
                                                               :y $y)))))
   
   :move-and-release (lambda ($button $dx $dy $node)
                       ;;(c-display "temponode box" (box-to-string ($temponode :box)))
                       (define max ($get-max-value-func))
                       (define min ($get-min-value-func))
                       (define node-area ($get-area-box-func))
                       (define node-area-width (node-area :width))
                       (define new-value (+ ($node :value)
                                            (scale $dx
                                                   (- (/ node-area-width 2)) (/ node-area-width 2)
                                                   min max)))
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





(add-node-mouse-handler :$get-area-box-func (lambda () (ra:get-box temponode-area))
                        :$get-node-x-func ra:get-temponode-x
                        :$get-node-y-func ra:get-temponode-y
                        :$get-num-nodes-func ra:get-num-temponodes
                        :$get-node-value-func ra:get-temponode-value
                        :$get-max-value-func (lambda () (1- (ra:get-temponode-max)))
                        :$get-min-value-func (lambda () (- (1- (ra:get-temponode-max))))
                        :$make-undo-func ra:undo-temponodes
                        :$create-node-func ra:create-temponode
                        :$move-node-func ra:set-temponode)
                        

(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (and (inside-box (ra:get-box temponode-area) $x $y)
              (match (list (find-node $x $y ra:get-temponode-x ra:get-temponode-y (ra:get-num-temponodes)))
                     (existing-box Num Box) :> (begin
                                                 (ra:set-current-tempo-node Num)
                                                 #t)
                     _                      :>  (begin
                                                  (ra:cancel-current-node)
                                                  #f)))))
 

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

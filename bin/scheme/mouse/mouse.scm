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
    (define new-x (if (ra:ctrl-pressed)
                      (/ (- $x start-x)
                         10.0)
                      (- $x start-x)))
    (define new-y (if (ra:ctrl-pressed)
                      (/ (- $y start-y)
                         10.0)
                      (- $y start-y)))
    (set! start-x $x)
    (set! start-y $y)
    (set! value (move-and-release $button
                                  new-x
                                  new-y
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

#|
(ra:set-reltempo 0.2)
|#


;; temponodearea
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-temponode-box $n $width)
  (define x (ra:get-temponode-x $n))
  (define y (ra:get-temponode-y $n))
  (define width/2 (+ 3 (/ $width 2)))
  (define x1 (- x width/2))
  (define y1 (- y width/2))
  (define x2 (+ x width/2))
  (define y2 (+ y width/2))
  (make-box2 x1 y1 x2 y2))

#||
(add-mouse-move-handler
 :move (lambda ($button $x $y)
         (if (inside-box (ra:get-box temponode-area) $x $y)
             (c-display "inside" $x $y))))
||#


(define-struct temponode
  :num
  :box
  :value
  :y)



      
(delafina (find-temponode :$x
                          :$y
                          :$num 0
                          :$num-temponodes (ra:get-num-temponodes)
                          :$temponode-width (ra:get-temponode-width))

  (define box (get-temponode-box $num $temponode-width))
  
  (cond ((inside-box box $x $y)
         (make-temponode :num $num
                         :box box
                         :value (ra:get-temponode-value $num)
                         :y (average (box :y1) (box :y2))))
        ((and (= 0 $num)
              (< $y (box :y1)))
         (make-temponode :num $num
                         :box box
                         :value (ra:get-temponode-value $num)
                         :y $y))
        ((> (box :y1) $y)
         (define max (1- (ra:get-temponode-max)))
         (define min (- max))
         (define temponode-area (ra:get-box temponode-area))
         (define value (scale $x
                              (temponode-area :x1) (temponode-area :x2)
                              min max))
         (define new-num (ra:create-temponode value (ra:get-place-from-y $y)))
         (define new-box (get-temponode-box new-num $temponode-width))
         (make-temponode :num new-num
                         :box new-box
                         :value (ra:get-temponode-value new-num)
                         :y $y))         
        ((= $num (1- $num-temponodes))
         (make-temponode :num $num
                         :box box
                         :value (ra:get-temponode-value $num)
                         :y $y))
        (else
         (find-temponode $x $y (1+ $num) $num-temponodes $temponode-width))))
      
  

  

(add-delta-mouse-handler
 :press (lambda ($button $x $y)
          ;;(c-display "inside? " (inside-box (ra:get-box reltempo-slider) $x $y) $x $y "box:" (box-to-string (ra:get-box reltempo-slider)))
          (and (or #t (= $button *left-button*))
               (inside-box (ra:get-box temponode-area) $x $y)
               (find-temponode $x $y)))
 
 :move-and-release (lambda ($button $dx $dy $temponode)
                     ;;(c-display "temponode box" (box-to-string ($temponode :box)))
                     (define max (1- (ra:get-temponode-max)))
                     (define min (- max))
                     (define temponode-area-width ((ra:get-box temponode-area) :width))
                     (define new-value (+ ($temponode :value)
                                          (scale $dx
                                                 (- (/ temponode-area-width 2)) (/ temponode-area-width 2)
                                                 min max)))
                     (define new-y (+ ($temponode :y)
                                      $dy))                                                 
                     ;;(c-display "dx:" $dx "value:" (* 1.0 ($temponode :value)) 0 ((ra:get-box temponode-area) :width) "min/max:" min max "new-value: " new-value)
                     (ra:set-temponode ($temponode :num) new-value (ra:get-place-from-y new-y))
                     (make-temponode :num ($temponode :num)
                                     :box ($temponode :box)
                                     :value new-value
                                     :y new-y))
 )

#||
(c-display (ra:create-temponode 2.1 -5.0))

(box-to-string (get-temponode-box 0 (ra:get-temponode-width)))
(box-to-string (get-temponode-box 1 (ra:get-temponode-width)))

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

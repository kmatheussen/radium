;;
;; A very lightweight and super simple widget manager.
;;
;; This code is originally taken from hurtigmixer (https://github.com/kmatheussen/hurtigmixer/blob/master/src/area.scm),
;; but slightly simplified and modified. Blitting has, for instance, been removed since we don't
;; need it. It might be put back later if needed.

;; All x and y values are according to the underlying Qt widget, and not according to the area.
;; (makes everything much simpler, and straight forward)

;; All x and y values can be floating points (QWidget's x and y values must be integers).

;; Subclasses of def-area must define x1, y1, x2, y2, and gui. All of these would usually be provided as class parameters.

;; Subclasses can implement the following virtual methods: ismoved, key-pressed, paint, post-paint, etc.
;; Areas only holding sub areas will often not implement any of these, and for those areas, we can simply
;; use the ready-made (and ultimately simple) <area> class instead (see below).

;; How to implement a custom method in an area subclass:
;;  (def-area-subclass (<area-with-custom-method> :gui :x1 :y1 :x2 :y2)  
;;    :custom-method ()
;;    (c-display "this text was printed from a custom method!"))
;; i.e. just like you normally would; methods go at the end of the class.
;;

(provide 'area.scm)

#!!
(gc #t)
(set! (*s7* 'gc-stats) #f)
!!#

(define (myfloor a)
  (floor a))


(define-expansion (define-override def . body)
  (let* ((funcname (car def))
         (org (<_> "super:" funcname)))
    `(let ((,org ,funcname))
       (set! ,funcname (lambda ,(cdr def)
                         ,@body)))))


(define-expansion (def-area-subclass def . body)

  `(define-class ,def

     ;; To avoid overlapping paint updates, we convert all coordinates to integers.
     ;; (Qt uses integers in the widget update system, so if we use floats here, widgets on all sides of the area will be repainted unnecessarily.)
     (set! x1 (myfloor x1))
     (set! y1 (myfloor y1))
     (set! x2 (myfloor x2))
     (set! y2 (myfloor y2))
     
     (define width (- x2 x1))
     (define height (- y2 y1))

     (define (paint?)
       #t)

     (define is-alive #t)

     ;; Position
     (define (get-position callback)
       (callback x1 y1 x2 y2 width height))

     (define (inside? x* y*)
       (and (>= x* x1)
            (< x* x2)
            (>= y* y1)
            (< y* y2)))

     (define (overlaps? x1* y1* x2* y2*)
       (and (> x2* x1)
	    (< x1* x2)
	    (> y2* y1)
	    (< y1* y2)))

     (define ismoved #f)

     (define (move-internal! dx dy)
       (inc! x1 dx)
       (inc! y1 dy)
       (inc! x2 dx)
       (inc! y2 dy)
       (for-each (lambda (sub-area)
		   (sub-area :move-internal! dx dy))
		 sub-areas)
       (if ismoved
	   (ismoved)))
     
     (define (move! dx dy)
       (let ((old-x1 x1)
             (old-x2 x2)
             (old-y1 y1)
             (old-y2 y2))
         ;;(update-me!)
         (move-internal! dx dy)
         (update (min old-x1 x1)
                 (min old-y1 y1)
                 (max old-x2 x2)
                 (max old-y2 y2))))

     (define (update x1* y1* x2* y2*)
       (let ((x1 (max x1 x1*))
             (y1 (max y1 y1*))
             (x2 (min x2 x2*))
             (y2 (min y2 y2*)))
         ;;(c-display "     UPDATE" x1 y1 x2 y2)
         (if (and (> x2 x1)
                  (> y2 y1))
             (<gui> :update gui x1 y1 x2 y2)
             (c-display "Warning, illegal parameters for update: " x1 y1 x2 y2))))

     (define (update-me!)
       ;;(c-display "     UPDATE-ME!" x1 y1 x2 y2)
       (<gui> :update gui x1 y1 x2 y2))
      
     (define (set-position! x* y*)
       (let ((dx (- x* x1))
	     (dy (- y* y1)))
	 (move! dx dy)))     

     (define (set-position2! x1* y1* x2* y2*)
       (define new-width (- x2* x1*))
       (define new-height (- y2* y1*))
       (let ((old-x1 x1)
             (old-x2 x2)
             (old-y1 y1)
             (old-y2 y2))
         (set! x1 x1*)
         (set! y1 y1*)
         (set! x2 x2*)
         (set! y2 y2*)))

     (define effect-monitors '())

     (define (add-area-effect-monitor! instrument-id effect-name monitor-stored monitor-automation callback)
       (push-back! effect-monitors (<ra> :add-effect-monitor effect-name instrument-id monitor-stored monitor-automation
                                         (lambda (radium-normalized automation)
                                           (if (<gui> :is-open gui)
                                               (callback radium-normalized automation)
                                               (c-display "---" ',(car def) "add-area-effect-monitor!: Warning! gui" gui " has been closed"))))))

     (define (remove-sub-areas!)
       (for-each (lambda (effect-monitor)
                   (<ra> :remove-effect-monitor effect-monitor))
                 effect-monitors)
       (set! effect-monitors '())

       (for-each (lambda (sub-area)
                   (sub-area :i-am-removed!))
                 sub-areas)
       (set! sub-areas '())
       (set! top-area #f))

     (define (i-am-removed!)
       (remove-sub-areas!)
       (set! is-alive #f))

     (define (reset! x1* y1* x2* y2*)
       (set-position2! x1* y1* x2* y2*)
       (remove-sub-areas!)
       (set! parent-area #f)
       (update-me!))

     (define parent-area #f)

     ;; Sub areas
     (define sub-areas '())
     (define top-area #f)

     (define (add-sub-area-plain! sub-area)
       (push-back! sub-areas sub-area)
       (set! top-area sub-area)
       (sub-area :set-parent-area! this))

     (define (add-sub-area! sub-area x y)
       ;;(c-display " THIS10:" this x y ". sub-area:" sub-area)
       (sub-area :set-position! x y)

       (add-sub-area-plain! sub-area)
       ;;(c-display " THIS:" this)

       (sub-area :get-position
                 (lambda (x* y* x2* y2* with* height*)
                   (if (inside? x* y*)
                       (update x* y* x2* y2*))))
       ;;(c-display "sub-area added to" ',(car def) ". New sub-area length:" (length sub-areas))
       )

     (define (add-sub-area-above! sub-area-below sub-area)
       (sub-area-below :get-position
                       (lambda (x1 y1 x2 y2 width height)
                         (add-sub-area! sub-area x2 y1))))
     
     (define (add-sub-area-below! above-sub-area sub-area)
       (sub-area-above :get-position
                       (lambda (x1 y1 x2 y2 width height)
                         (add-sub-area! sub-area x1 y2))))
     
     (define (remove-sub-area! sub-area)
       (sub-area :i-am-removed!)
       (set! sub-areas (delete sub-area sub-areas eq?))
       (set! top-area
	     (if (null? sub-areas)
		 #f
		 (last sub-areas)))
       )

     (define (lift-sub-area! sub-area)
       (when (not (eq? sub-area top-area))
	 (set! sub-areas (append (delete sub-area sub-areas eq?)
                                 (list sub-area)))
	 (set! top-area sub-area)
         (sub-area :get-position
                   (lambda (x* y* x2* y2* with* height*)
                     (update x* y* x2* y2*)))))

     
     (define (lift-me!)
       (if parent-area
           (parent-area :lift-sub-area! this)))
     
     ;; Keyboard listener
     (define key-pressed #f)
     (define (key-pressed-internal key-event)
       (call-with-exit
	(lambda (return)
	  (if key-pressed
	      (let ((ret (key-pressed key-event)))
		(if ret
		    (return #t)))
	      (for-each (lambda (sub-area)
			  (if (sub-area :key-pressed-internal key-event)
			      (return #t)))
			sub-areas))
	  #f)))

     (define (key-released-internal key-event)
       ;;(c-display "released something")
       #t
       )

     
     ;; Mouse cycles
     ;;;;;;;;;;;;;;;;;;;;;;;;
     
     (define curr-nonpress-mouse-cycle #f)
     (define curr-mouse-cycle #f)


     (define mouse-cycles '())
     (define nonpress-mouse-cycles '())
       
     (delafina (add-nonpress-mouse-cycle! :enter-func (lambda x #t)
                                          :move-func (lambda x #f)
                                          :leave-func (lambda x #f)) ;; Leave area, or button was pressed.
       (push-back! nonpress-mouse-cycles
                   (list enter-func move-func leave-func inside?))
       )

     (define (start-nonpress-mouse-cycle! new-cycle)
       (assert (not curr-nonpress-mouse-cycle))
       (set! curr-nonpress-mouse-cycle new-cycle))
     
     (define (end-nonpress-mouse-cycle!)
       (assert curr-nonpress-mouse-cycle)
       (let ((nonpress-mouse-cycle curr-nonpress-mouse-cycle))
         (set! curr-nonpress-mouse-cycle #f)
         ((caddr nonpress-mouse-cycle))))
  
     (delafina (add-mouse-cycle! :press-func (lambda x #t)
                                 :drag-func (lambda x #f)
                                 :release-func (lambda x #f))
       (push-back! mouse-cycles
                   (list press-func drag-func release-func))
       )

     (delafina (add-delta-mouse-cycle! :press-func (lambda x #t)
                                       :drag-func (lambda x #f)
                                       :release-func (lambda x #f))
       (define prev-x #f)
       (define prev-y #f)
       (define inc-x 0)
       (define inc-y 0)
       (define (call-drag-func button x* y*)
         (define dx (cond ((only-y-direction)
                           0)
                          ((<ra> :control-pressed)
                           (/ (- x* prev-x)
                              10))
                          (else
                           (- x* prev-x))))
         (define dy (cond ((only-x-direction)
                           0)
                          ((<ra> :control-pressed)
                           (/ (- y* prev-y)
                              10))
                          (else
                           (- y* prev-y))))
         (when (or (not (= 0 dx))
                   (not (= 0 dy)))
           (inc! inc-x dx)
           (inc! inc-y dy)
           ;;(c-display "dx:" dx ". inc-x:" inc-x)
           (drag-func button x* y* inc-x inc-y))
         (set! prev-x x*)
         (set! prev-y y*))

       (push-back! mouse-cycles
                   (list (lambda (button x* y*)
                           (set! prev-x x*)
                           (set! prev-y y*)
                           (set! inc-x 0)
                           (set! inc-y 0)
                           (press-func button x* y*))
                         call-drag-func
                         (lambda (button x* y*)
                           (call-drag-func button x* y*)
                           (release-func button x* y*)))))
                         

     (define (get-nonpress-mouse-cycle x* y*)
       (and (paint?)
            (inside? x* y*)
	    (or (call-with-exit (lambda (return)
                                  (for-each (lambda (sub-area)
                                              (and-let* ((res (sub-area :get-nonpress-mouse-cycle x* y*)))
                                                        (return res)))
                                            (reverse sub-areas))
                                  #f))
		(call-with-exit (lambda (return)                                  
                                  (for-each (lambda (mouse-cycle)
                                              (and-let* ((res ((car mouse-cycle) x* y*)))
                                                        (return mouse-cycle)))
                                            nonpress-mouse-cycles)
                                  #f)))))
       
     

     (define (get-mouse-cycle button x* y*)
       (and (paint?)
            (inside? x* y*)
	    (or (call-with-exit (lambda (return)
                                  (for-each (lambda (sub-area)
                                              (and-let* ((res (sub-area :get-mouse-cycle button x* y*)))
                                                        (return res)))
                                            (reverse sub-areas))
                                  #f))
		(call-with-exit (lambda (return)
                                  (for-each (lambda (mouse-cycle)
                                              (and-let* ((res ((car mouse-cycle) button x* y*)))
                                                        (return mouse-cycle)))
                                            mouse-cycles)
                                  #f)))))

     (define (mouse-press-internal button x* y*)
       (c-display "_____________________________________mouse-press" curr-mouse-cycle)
       (if curr-nonpress-mouse-cycle
           (end-nonpress-mouse-cycle!))
       ;;(assert (not curr-mouse-cycle)) Unfortunately, we can't trust Qt to send release events.
       (set! curr-mouse-cycle (get-mouse-cycle button x* y*)))
     (define (mouse-move-internal button x* y*)
       (cond (curr-mouse-cycle
              ((cadr curr-mouse-cycle) button x* y*))
             (curr-nonpress-mouse-cycle
              (if ((cadddr curr-nonpress-mouse-cycle) x* y*)
                  ((cadr curr-nonpress-mouse-cycle) x* y*) ;; still inside
                  (end-nonpress-mouse-cycle!))) ;; not inside any more
             (else
              (start-nonpress-mouse-cycle! (get-nonpress-mouse-cycle x* y*)))))
     (define (mouse-release-internal button x* y*)
       ;;(c-display "mouse-release enter" curr-mouse-cycle)
       (let ((mouse-cycle curr-mouse-cycle))
         (set! curr-mouse-cycle #f)
         (if mouse-cycle
             ((caddr mouse-cycle) button x* y*)))
       ;;(c-display "mouse-release leave" curr-mouse-cycle)
       )

     (define (mouse-callback-internal button state x y)
       (cond (*current-mouse-cycle*
              #f) ;; i.e. mouse.scm is handling mouse now.
             ((= state *is-pressing*)
              (mouse-press-internal button x y))
             ((= state *is-moving*)
              (mouse-move-internal button x y))
             ((= state *is-releasing*)
              (mouse-release-internal button x y))
             ((= state *is-leaving*)
              (if curr-nonpress-mouse-cycle
                  (end-nonpress-mouse-cycle!)))))


     
     ;; Status bar
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
     (define statusbar-text-id -1)
     
     (define (set-statusbar-text! text)
       (set! statusbar-text-id (<ra> :set-statusbar-text text)))

     (define (remove-statusbar-text)
       (<ra> :remove-statusbar-text statusbar-text-id))
     
     (define (add-statusbar-text-handler string-or-func)
       (add-nonpress-mouse-cycle!
        :enter-func (lambda (x* y)
                      (set-statusbar-text! (if (string? string-or-func)
                                               string-or-func
                                               (string-or-func)))
                      #t)
        :leave-func remove-statusbar-text))
       
     ;; Painting
     ;;;;;;;;;;;;;;;
       
     (define (paint)  ;; Called before painting the current area's sub-areas
       #f)
     (define (post-paint) ;; Called after painting the current area's sub-areas
       #f)
     
     (define (paint-internal px1 py1 px2 py2) ;; px1, py1, etc. is the clip area of the parent area.

       ;;(c-display "\n\npaint-internal called" ',(car def) "(" x1 y1 x2 y2 "). p: (" px1 py1 px2 py2 ")")
                  
       
       '(c-display (paint?)
                   (<gui> :area-needs-painting gui x1 y1 x2 y2));;overlaps? x1* y1* x2* y2*))

       (when (and (paint?)
                  (<gui> :area-needs-painting gui x1 y1 x2 y2));;overlaps? x1* y1* x2* y2*))

         ;;(c-display "paint-internal hepp" ',(car def) paint "sub-areas" sub-areas)
         (let ((cx1 (max x1 px1))
               (cy1 (max y1 py1))
               (cx2 (min x2 px2))
               (cy2 (min y2 py2)))

           (when (and (> cx2 cx1)
                      (> cy2 cy1))

             (<gui> :set-clip-rect gui cx1 cy1 cx2 cy2)
             (paint)
             
             (for-each (lambda (sub-area)
                         (sub-area :paint-internal cx1 cy1 cx2 cy2))
                       sub-areas)
             
             (<gui> :set-clip-rect gui cx1 cy1 cx2 cy2)
             (post-paint)))
           
         (if (not parent-area)
             (<gui> :cancel-clip-rect gui))))
     
     ;;(define class-name ',(car def))

     (let () ;; Put body into new scope to avoid accidentally overriding an internal method. (use define-override instead of define to purposefully override)
       #t ;; Added to silence "let has no body" error messages.
       ,@body)
     
     :get-position x (apply get-position x)
     :repaint-me! x (apply repaint-me! x)
     :set-position! x (apply set-position! x)
     :move! x (apply move! x)
     :move-internal! x (apply move-internal! x)
     :set-parent-area! (new-parent-area) (begin
                                           (assert new-parent-area)
                                           (set! parent-area new-parent-area))
     :add-sub-area-plain! (sub-area) (add-sub-area-plain! sub-area)
     :add-sub-area! x (apply add-sub-area! x)
     :add-sub-area-above! x (apply add-sub-area-above! x)
     :add-sub-area-below! x (apply add-sub-area-below! x)
     :remove-sub-area-below! x (apply remove-sub-area! x)
     :lift-sub-area! x (apply lift-sub-area! x)
     :lift-me! x (apply lift-me! x)
     :key-pressed-internal! x (apply key-pressed-internal! x)
     :key-released-internal! x (apply key-released-internal! x)
     :add-mouse-cycle! x (apply add-mouse-cycle! x)
     :get-mouse-cycle x (apply get-mouse-cycle x)
     :add-nonpress-mouse-cycle! x (apply add-nonpress-mouse-cycle! x)
     :get-nonpress-mouse-cycle x (apply get-nonpress-mouse-cycle x)
     :overlaps? x (apply overlaps? x)
     :paint-internal x (apply paint-internal x)
     :mouse-callback-internal x (apply mouse-callback-internal x)
     :has-mouse () (or curr-mouse-cycle curr-nonpress-mouse-cycle)
     :reset! x (apply reset! x)
     :i-am-removed! x (apply i-am-removed! x)
     ))
 


(def-area-subclass (<area> :gui :x1 :y1 :x2 :y2)  
  )

#!!
(def-area-subclass (<testarea> :gui :x1 :y1 :x2 :y2)  
  (define X 0)
  (define Y 0)

  (define-override (paint)
    ;;(c-display "x1:" gui x1 y1 x2 y2 (<ra> :generate-new-color))
    (<gui> :filled-box gui (<ra> :generate-new-color 1) x1 y1 x2 y2)
    (<gui> :draw-text gui "green" "hello" X Y x2 y2)
    (<gui> :draw-line gui "white" X Y x2 y2 2.3))
  
  (add-mouse-cycle! (lambda (button x* y*)
                      (set! X x*)
                      (set! Y y*)
                      (update X Y x2 y2)
                      (c-display "press button/x/y" x* y*))
                    (lambda (button x* y*)
                      (set! X x*)
                      (set! Y y*)
                      (update X Y x2 y2)
                      (c-display "move button/x/y" x* y*))
                    (lambda (button x* y*)
                      (set! X x*)
                      (set! Y y*)
                      (update X Y x2 y2)
                      (c-display "release button/x/y" x* y*)))
  )
!!#


(define *use-testgui* #f)

(if (and (defined? '*testgui*)
         *testgui*
         (<gui> :is-open *testgui*))
    (<gui> :close *testgui*))

(define *testgui* (and *use-testgui*
                     (<gui> :widget 500 500)))


;; Save some cycles by not painting background color if only vertical audio meters are updated (meters are repainted continously)
(when *use-testgui*
  (<gui> :dont-autofill-background *testgui*)
  (<gui> :set-background-color *testgui* (<gui> :get-background-color *testgui*)))


#||
(define testarea (<new> :testarea *testgui* 100 200 1100 1200))

(testarea :get-position c-display)

(<gui> :show *testgui*)

(<gui> :add-paint-callback *testgui*
       (lambda (width height)
         (testarea :paint-internal 0 0 width height)))

(<gui> :add-mouse-callback *testgui*
       (lambda (button state x y)
         (c-display "asd" x y)
         (testarea :mouse-callback-internal button state x y)
         (if (testarea :has-mouse)
             #t
             #f)))
||#


(def-area-subclass (<horizontal-instrument-slider> :gui :x1 :y1 :x2 :y2
                                                   :instrument-id
                                                   :effect-name "System Volume"
                                                   :use-two-rows #f
                                                   :get-color)

  (define has-made-undo #f)
  
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (set! has-made-undo #t)
      (<ra> :undo-instrument-effect instrument-id effect-name)))

  (define (get-radium-normalized)
    (<ra> :get-stored-instrument-effect instrument-id effect-name))

  (define (get-db-value radium-normalized)
    (radium-normalized-to-db radium-normalized))

  (define (set-db-value db)    
    ;;(c-display "      -------set-db-val:" (db-to-radium-normalized db))
    (<ra> :set-instrument-effect instrument-id effect-name (db-to-radium-normalized db)))
  
  (define (get-scaled-value radium-normalized)
    (db-to-slider (get-db-value radium-normalized)))

  (define (get-volume-slider-value-text value)
    (db-to-text (slider-to-db value) #t))
  
  (define (get-volume-slider-text radium-normalized)
    (let ((volume-text (get-volume-slider-value-text (get-scaled-value radium-normalized))))
      (if use-two-rows
          (<-> "  " volume-text)
          (let ((instrument-name (<ra> :get-instrument-name instrument-id)))
            (<-> instrument-name ": " volume-text)))))
    
  (define last-painted-radium-normalized -10000)

  (define automation-value #f)
  (define automation-color (<ra> :get-instrument-effect-color instrument-id effect-name))
  (define (get-automation-data kont)
    (if automation-value
        (kont automation-value automation-color)))

  (add-area-effect-monitor! instrument-id effect-name #t #t
                            (lambda (radium-normalized automation)
                              (when radium-normalized
                                (when (> (abs (- radium-normalized last-painted-radium-normalized))
                                         0.0001)
                                  (update-me!)))
                              (when automation
                                (set! automation-value (if (< automation 0)
                                                           #f
                                                           (radium-normalized-to-slider automation)))
                                (update-me!))))
  
  (define-override (paint)
    (define b 1)
    (define radium-normalized (get-radium-normalized))
    (set! last-painted-radium-normalized radium-normalized)

    (paint-horizontal-instrument-slider gui
                                        instrument-id
                                        (get-scaled-value radium-normalized)
                                        (get-volume-slider-text radium-normalized)
                                        #t
                                        #f
                                        get-automation-data
                                        (+ b x1)
                                        (+ b x1) (+ b y1) (- x2 b) (- y2 b)
                                        (get-color)
                                        ))
  
  (define start-mouse-value #f)
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (and (= button *left-button*)
          (begin
            (define radium-normalized (get-radium-normalized))
            (set! start-mouse-value (get-scaled-value radium-normalized));;(scale x* x1 x2 0 1));;(get-db-value));;(<ra> :get-stored-instrument-effect instrument-id effect-name))
            ;;(c-display "press button/x/y" x* y*)
            (set-statusbar-text! (get-statusbar-text))
            #t)))
   (lambda (button x* y* dx dy)
     (maybe-make-undo)
     (define slider-value (between 0 (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                   1))
     (set-db-value (slider-to-db slider-value))
     (set-statusbar-text! (get-statusbar-text))
     (update-me!)
     )
   (lambda (button x* y*)
     (c-display "release button/x/y" x* y*)))

  (define (get-statusbar-text)
    (get-volume-slider-text (get-radium-normalized)))
  
  (add-statusbar-text-handler get-statusbar-text)
                                

  '(define (mouse-callback button state x y)
    (if (and (>= x x1)
             (< x x2)
             (>= y y1)
             (< y y2))
        (let ((status-text (get-volume-slider-text (get-radium-normalized))))
          ;;(c-display "hepp " status-text)
          (<ra> :set-statusbar-text status-text)
          )))

  )


(def-area-subclass (<checkbox> :gui :x1 :y1 :x2 :y2
                               :is-selected
                               :paint-func
                               :value-changed-callback)

  (define-override (paint)
    (paint-func is-selected))

  (add-mouse-cycle! (lambda (button x* y*)
                      (and (= button *left-button*)                        
                           (begin
                             (set! is-selected (not is-selected))
                             (value-changed-callback is-selected)
                             (update-me!)
                             #t)))))
                      

(def-area-subclass (<button> :gui :x1 :y1 :x2 :y2
                             :paint-func #f
                             :text ""
                             :background-color #f
                             :statusbar-text #f
                             :callback #f
                             :callback-release #f)

  (define is-pressing #f)

  (define gui-background-color (<gui> :get-background-color gui))
  (define fontheight (get-fontheight))
  (define b (max 1 (myfloor (/ fontheight 2.5)))) ;; border
  
  (define r 3) ;;rounding
  (define r/2 2)
  
  (define (mypaint)
    (let ((background-color (if background-color
                                background-color
                                (<gui> :mix-colors "#010101" gui-background-color 0.5))))
      (if (not is-pressing)
          (<gui> :filled-box gui background-color (+ x1 0) (+ y1 0) (- x2 0) (- y2 0) r r))
          
      (if (not (string=? "" text))
          (<gui> :draw-text
                 gui
                 *text-color*
                 text
                 (+ x1 3) (+ y1 2) (- x2 3) (- y2 2)
                 #t ; wrap lines
                 #f ; align left
                 #f ; align top
                 0  ; rotate
                 #f ; cut text to fit
                 #t ; scale font size
                 ))
      (if is-pressing
          (<gui> :draw-box gui background-color (+ x1 r/2) (+ y1 r/2) (- x2 r/2) (- y2 r/2) b r r))))
    
  (define-override (paint)
    (if paint-func
        paint-func
        (mypaint)))

  (if statusbar-text
      (add-statusbar-text-handler statusbar-text))
  
  (add-mouse-cycle! (lambda (button x* y*)
                      (and (= button *left-button*)                        
                           (begin
                             (set! is-pressing #t)
                             (if callback
                                 (callback))
                             (update-me!)
                             #t)))
                    (lambda (button x* y*)
                      #t)
                    (lambda (button x* y*)
                      (set! is-pressing #f)
                      (if callback-release
                          (callback-release))
                      (update-me!))))

                                  
(delafina (horizontally-layout-areas :x1 :y1 :x2 :y2
                                     :args
                                     :x1-border 0
                                     :y1-border 0
                                     :x2-border 0
                                     :y2-border 0
                                     :spacing 0
                                     :callback)
  (define half-spacing (/ spacing 2))
  (define num-areas (length args))
  (let loop ((args args)
             (n 0))
    (when (not (null? args))
      (let ((arg (car args))
            (x1* (+ (scale n 0 num-areas (+ x1 x1-border) (- x2 x2-border))
                    (if (> n 0)
                        half-spacing
                        0)))
            (x2* (- (scale (1+ n) 0 num-areas (+ x1 x1-border) (- x2 x2-border))
                    (if (< n (- num-areas 1))
                        half-spacing
                        0))))
        (apply callback (append (if (list? arg) arg (list arg))
                                (list x1* (+ y1 y1-border)
                                      x2* (- y2 y2-border)))))
      (loop (cdr args)
            (1+ n)))))

(delafina (vertically-layout-areas :x1 :y1 :x2 :y2
                                   :args
                                   :x1-border 0
                                   :y1-border 0
                                   :x2-border 0
                                   :y2-border 0
                                   :spacing 0
                                   :callback)
  (define half-spacing (/ spacing 2))
  (define num-areas (length args))
  (let loop ((args args)
             (n 0))
    (when (not (null? args))
      (let ((arg (car args))
            (y1* (+ (scale n 0 num-areas (+ y1 y1-border) (- y2 y2-border))
                    (if (> n 0)
                        half-spacing
                        0)))                 
            (y2* (- (scale (1+ n) 0 num-areas (+ y1 y1-border) (- y2 y2-border))
                    (if (< n (- num-areas 1))
                        half-spacing
                        0))))
        (apply sub-area-creation-callback (append (if (list? arg) arg (list arg))
                                                  (list (+ x1 x1-border) y1* (- x2 x2-border) x2*))))
      (loop (cdr args)
            (1+ n)))))



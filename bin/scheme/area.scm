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

;; Subclasses can implement the following virtual methods: has-been-moved-callback, about-to-be-removed-callback, key-pressed, paint, post-paint, etc.
;; Areas only holding sub areas will often not implement any of these, and for those areas, we can simply
;; use the ready-made (and ultimately simple) <area> class instead (see below).

;; How to implement a custom method in an area subclass:
;;  (def-area-subclass (<area-with-custom-method> :gui :x1 :y1 :x2 :y2)  
;;    :custom-method ()
;;    (c-display "this text was printed from a custom method!"))
;; i.e. just like you normally would; methods go at the end of the class.
;; EDIT: That doesn't work anymore. The body of the subclasses was later put into their own scope to avoid accidentally
;; overriding symbols in the superclass. Instead you need to use :add-method! manually, after an instance has been created.
;; That's very inconvenient though, so maybe this should be improved.


(provide 'area.scm)

(my-require 'mouse-primitives.scm)
(my-require 'gui.scm)


#!!
(gc #t)
(set! (*s7* 'gc-stats) #f)
!!#

(define (myfloor a)
  ;;a
  (floor a)
  )

(define-expansion (define-override def . body)
  (let* ((funcname (car def))
         (org (<_> "super:" funcname)))
    `(let ((,org ,funcname))
       (set! ,funcname (lambda ,(cdr def)
                         ,@body)))))


(define *area-id-counter* 0)

(c-define-expansion (*def-area-subclass* def . body)

  (define body-methods '())
  (let ((temp (split-list body keyword?)))
    (set! body (car temp))
    (set! body-methods (cadr temp)))
                     
  `(define-class ,def

     ;; To avoid overlapping paint updates, we convert all coordinates to integers.
     ;; (Qt uses integers in the widget update system, so if we use floats here, widgets on all sides of the area will be repainted unnecessarily.)
     (set! x1 (myfloor x1))
     (set! y1 (myfloor y1))
     (set! x2 (myfloor x2))
     (set! y2 (myfloor y2))
     
     (define width (- x2 x1))
     (define height (- y2 y1))

     (define font #f)
     
     (define (paint?)
       #t)

     (define id (inc! *area-id-counter* 1))

     (define is-alive #t)

     ;; Position
     
     (define (get-position callback)
       (callback x1 y1 x2 y2 width height))
     
     (define i-x1 #f)
     (define i-y1 #f)
     (define i-x2 #f)
     (define i-y2 #f)
     
     ;; optimization to avoid inside? to call parent-area::inside?
     (define (set-i-variables!)
       (when (not i-x1)
         (if parent-area
             (parent-area :get-i-position
                          (lambda (px1 py1 px2 py2)
                            (set! i-x1 (max x1 px1))
                            (set! i-y1 (max y1 py1))
                            (set! i-x2 (min x2 px2))
                            (set! i-y2 (min y2 py2))))
             (begin
               (set! i-x1 x1)
               (set! i-y1 y1)
               (set! i-x2 x2)
               (set! i-y2 y2)))))
       
     (define (get-i-position callback)
       (set-i-variables!)
       (callback i-x1 i-y1 i-x2 i-y2))
     
     ;; We return false if x* and y* aren't inside the parent either.
     (define (inside? x* y*)
       (set-i-variables!)
       (and (>= x* i-x1)
            (< x* i-x2)
            (>= y* i-y1)
            (< y* i-y2)))

     (define (overlaps? x1* y1* x2* y2*)
       (and (> x2* x1)
	    (< x1* x2)
	    (> y2* y1)
	    (< y1* y2)))

     (define-optional-func has-been-moved ()) ;; Called after being moved.

     (define (move-internal! dx dy)
       (set! i-x1 #f)
       (inc! x1 dx)
       (inc! y1 dy)
       (inc! x2 dx)
       (inc! y2 dy)
       (for-each (lambda (sub-area)
		   (sub-area :move-internal! dx dy))
		 sub-areas)
       (if has-been-moved
	   (has-been-moved)))
     
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
             (begin
               (<gui> :update gui x1 y1 x2 y2)
               ;;#f
               )
             (c-display "Warning, illegal parameters for update: " x1 y1 x2 y2))))

     (define (update-me!)
       ;;(c-display "     UPDATE-ME!" x1 y1 x2 y2)
       (<gui> :update gui x1 y1 x2 y2)
       ;;#f
       )
      
     (define (set-position! x* y*)
       (let ((dx (- x* x1))
	     (dy (- y* y1)))
	 (move! dx dy)))     

     (define (set-position-and-size! x1* y1* x2* y2*)
       (set! width (- x2* x1*))
       (set! height (- y2* y1*))
       (set! x2 (+ x1 width))
       (set! y2 (+ y1 height))
       (set-position! x1* y1*))

     (define (resize! width height)
       (set-position-and-size! x1 y1 (+ x1 width) (+ y1 height)))

     (define effect-monitors '())

     (define (add-area-effect-monitor! instrument-id effect-name monitor-stored monitor-automation callback)
       (define effect-monitor #f)
       (set! effect-monitor (<ra> :add-effect-monitor effect-name instrument-id monitor-stored monitor-automation
                                  (lambda (radium-normalized automation)
                                    (if (<gui> :is-open gui)
                                        (callback radium-normalized automation)
                                        (begin                                                 
                                          (c-display (<-> "Warning! In " ',(car def) "::add-area-effect-monitor!: Warning! gui #" gui " has been closed. (removing the effect monitor)"))
                                          (<ra> :remove-effect-monitor effect-monitor #t))))))
       (push-back! effect-monitors effect-monitor))

     (define (remove-sub-areas!)
       (for-each (lambda (effect-monitor)
                   (c-display "Note: In" ',(car def) ", the effect monitor" effect-monitor "was automatically removed")
                   (<ra> :remove-effect-monitor effect-monitor #f))
                 effect-monitors)
       (set! effect-monitors '())

       (for-each (lambda (sub-area)
                   (sub-area :about-to-be-removed-internal!))
                 sub-areas)
       (set! sub-areas '())
       (set! top-area #f))

     (define-optional-func about-to-be-removed-callback ())

     (define (about-to-be-removed-internal!)
       (if about-to-be-removed-callback
           (about-to-be-removed-callback))
       (remove-sub-areas!)
       (set! is-alive #f))

     (define (reset! x1* y1* x2* y2*)
       (set-position-and-size! x1* y1* x2* y2*)
       (remove-sub-areas!)
       (set! parent-area #f)
       (update-me!))

     (define-optional-func parent-area (key . rest))

     ;; Sub areas
     (define sub-areas '())
     (define top-area #f)

     (define (add-sub-area-plain! sub-area)
       (push-back! sub-areas sub-area)
       (set! top-area sub-area)
       (sub-area :set-parent-area! this)
       (sub-area :update-me!))

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
     
     (define (add-sub-area-below! sub-area-above sub-area)
       (sub-area-above :get-position
                       (lambda (x1 y1 x2 y2 width height)
                         (add-sub-area! sub-area x1 y2))))
     
     (define (remove-sub-area! sub-area)
       (sub-area :about-to-be-removed-internal!)
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

     ;; State
     (define (get-state)
       #f)

     (define (apply-state! a-hash-table)
       #t)

     ;; Keyboard listener
     ;;;;;;;;;;;;;;;;;;;;;;;;
     (define-optional-func key-pressed (key-event))
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

     
     ;; Mouse wheel
     ;;;;;;;;;;;;;;;;;;;;;;;;
     (define-optional-func mouse-wheel-moved (is-up x y))
     (define (mouse-wheel-moved-internal! is-up x* y*)
       (and (inside? x* y*)
            (call-with-exit
             (lambda (return)
               (if mouse-wheel-moved
                   (let ((ret (mouse-wheel-moved is-up x* y*)))
                     (if ret
                         (return #t)))
                   (for-each (lambda (sub-area)
                               (if (sub-area :mouse-wheel-moved-internal! is-up x* y*)
                                   (return #t)))
                             sub-areas))
               #f))))
     
     ;; Mouse cycles
     ;;;;;;;;;;;;;;;;;;;;;;;;

     (define-optional-hash-table curr-nonpress-mouse-cycle)
     (define-optional-hash-table curr-mouse-cycle)

     (define mouse-cycles '())
     (define nonpress-mouse-cycles '())
       
     (delafina (add-nonpress-mouse-cycle! :enter-func (lambda x #t)
                                          :move-func (lambda x #f)
                                          :leave-func (lambda x #f)) ;; Leave area, or button was pressed.
       (push-back! nonpress-mouse-cycles
                   (make-mouse-cycle enter-func
                                     move-func
                                     leave-func
                                     inside?))
       )

     (define (start-nonpress-mouse-cycle! new-cycle)
       (assert (not curr-nonpress-mouse-cycle))
       (set! curr-nonpress-mouse-cycle new-cycle))
     
     (define (end-nonpress-mouse-cycle! button-was-pressed)
       (assert curr-nonpress-mouse-cycle)
       (let ((nonpress-mouse-cycle curr-nonpress-mouse-cycle))
         (set! curr-nonpress-mouse-cycle #f)
         ;;(c-display "end-nonpress-mouse-cycle called" (nonpress-mouse-cycle :release-func))
         ((nonpress-mouse-cycle :release-func) button-was-pressed)))
  
     (delafina (add-mouse-cycle! :press-func (lambda x #t)
                                 :drag-func (lambda x #f)
                                 :release-func (lambda x #f))
       (push-back! mouse-cycles
                   (make-mouse-cycle press-func drag-func release-func))
       )

     (delafina (add-delta-mouse-cycle! :press-func (lambda x #t)
                                       :drag-func (lambda x #f)
                                       :release-func (lambda x #f))
       (define prev-x #f)
       (define prev-y #f)
       (define inc-x 0)
       (define inc-y 0)
       (define (call-delta-func func button x* y* force-call)
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
         (when (or force-call
                   (not (= 0 dx))
                   (not (= 0 dy)))
           (inc! inc-x dx)
           (inc! inc-y dy)
           (func button x* y* inc-x inc-y)
           )
         (set! prev-x x*)
         (set! prev-y y*))

       (push-back! mouse-cycles
                   (make-mouse-cycle (lambda (button x* y*)
                                       (set! prev-x x*)
                                       (set! prev-y y*)
                                       (set! inc-x 0)
                                       (set! inc-y 0)
                                       (press-func button x* y*))
                                     (lambda (button x* y*)
                                       (call-delta-func drag-func button x* y* #f))
                                     (lambda (button x* y*)
                                       (call-delta-func drag-func button x* y* #f)
                                       (call-delta-func release-func button x* y* #t)))))

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
                                              (and-let* ((res (mouse-cycle :press-func x* y*)))
                                                        (return mouse-cycle)))
                                            nonpress-mouse-cycles)
                                  #f)))))
       
     (define do-nothing-mouse-cycle (make-mouse-cycle (lambda (button x* y*)
                                                        (assert #f))
                                                      (lambda (button x* y*)
                                                        #f)
                                                      (lambda (button x* y*)
                                                        #f)))

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
                                              (let ((use-it (mouse-cycle :press-func button x* y*)))
                                                (cond ((eq? 'eat-mouse-cycle use-it)
                                                       (return do-nothing-mouse-cycle))
                                                      ((eq? #t use-it)
                                                       (return mouse-cycle))
                                                      (else
                                                       (assert (eq? #f use-it))))))
                                            mouse-cycles)
                                  #f)))))

     (define (mouse-press-internal button x* y*)
       ;;(c-display "_____________________________________mouse-press" curr-mouse-cycle)
       (if curr-nonpress-mouse-cycle
           (end-nonpress-mouse-cycle! #t))
       (if (not (<ra> :release-mode))
           (assert (not curr-mouse-cycle))) ;; Unfortunately, we can't trust Qt to send release events. (fixed now)
       (set! curr-mouse-cycle (get-mouse-cycle button x* y*))
       ;;(c-display "====-------Setting curr-mouse-cycle to:" curr-mouse-cycle)
       ;;(<ra> :show-warning "gakk")

       curr-mouse-cycle)
     
     (define (mouse-move-internal button x* y*)
       ;;(c-display "..mouse-move-internal for" class-name ". y:" y* ". has-curr:" (to-boolean curr-mouse-cycle)  ". has_nonpress:" (to-boolean curr-nonpress-mouse-cycle))
       (let ((ret (cond (curr-mouse-cycle
              (curr-mouse-cycle :drag-func button x* y*))
             (curr-nonpress-mouse-cycle
              ;;(c-display "inside?" class-name y1 y2 (curr-nonpress-mouse-cycle :inside? x* y*))
              (if (curr-nonpress-mouse-cycle :inside? x* y*)
                  (curr-nonpress-mouse-cycle :drag-func x* y*) ;; still inside
                  (end-nonpress-mouse-cycle! #f))) ;; not inside any more
             (else
              ;;(c-display "hepp")
              (start-nonpress-mouse-cycle! (get-nonpress-mouse-cycle x* y*)))))

             )
         ;;(c-display "ret:" ret)
         ret))
     (define (mouse-release-internal button x* y*)
       ;;(if curr-mouse-cycle
       ;;    (c-display "..mouse-release-internal for" class-name ". y:" y*))
       ;;(c-display "mouse-release enter" curr-mouse-cycle)
       (let ((mouse-cycle curr-mouse-cycle))
         (set! curr-mouse-cycle (<optional-hash-table>))
         ;;(c-display "===------ Unsetting curr-mouse-cycle");
         (if mouse-cycle
             (mouse-cycle :release-func button x* y*)))
       ;;(c-display "mouse-release leave" curr-mouse-cycle)
       )

     (define (mouse-callback-internal button state x y)

       ;;(c-display "   mouse-callback-internal" "has:" (if curr-mouse-cycle #t #f) ". button/state:" button state
       ;;           (if (= state *is-releasing*) "releasing" (if (= state *is-leaving*) "leaving" (if (= state *is-pressing*) "pressing" "unknown"))))
       
       ;; make sure release is always called when releasing, no matter other states.
       (when (or (= state *is-releasing*)
                 (= state *is-leaving*))
         ;;(if curr-mouse-cycle
         ;;    (c-display "     MOUSE-CALLBACK-INTERNAL called for" class-name ". y:" y ". type:" (if (= state *is-releasing*) "RELEASE" "LEAVE")))
         (mouse-release-internal button x y))
       
       (cond (*current-mouse-cycle*
              #f) ;; i.e. mouse.scm is handling mouse now.
             ((= state *is-leaving*)
              (if curr-nonpress-mouse-cycle
                  (end-nonpress-mouse-cycle! #f)
                  #f))
             ((= state *is-moving*)
              (mouse-move-internal button x y))
             ((not (inside? x y))
              #f)
             ((= state *is-pressing*)
              (mouse-press-internal button x y))
             ((= state *is-releasing*)
              #f)
             (else
              (assert (= state *is-entering*))
              #f)))

     (define (has-mouse)
       ;;(c-display "has-mouse:" class-name curr-mouse-cycle curr-nonpress-mouse-cycle)
       (get-bool (or curr-mouse-cycle
                     curr-nonpress-mouse-cycle)))
     
     ;; Status bar
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
     (define statusbar-text-id -1)
     
     (define (set-statusbar-text! text)
       (set! statusbar-text-id (<ra> :set-statusbar-text text)))

     (define (remove-statusbar-text)
       (<gui> :tool-tip "")
       (<ra> :remove-statusbar-text statusbar-text-id))
     
     (define (add-statusbar-text-handler string-or-func)
       (add-nonpress-mouse-cycle!
        :enter-func (lambda (x* y)
                      (define string-or-pair (if (procedure? string-or-func)
                                                 (string-or-func)
                                                 string-or-func))
                      (define text (if (pair? string-or-pair)
                                       (cadr string-or-pair)
                                       string-or-pair))
                      (define also-show-tooltip (if (pair? string-or-pair)
                                                    (car string-or-pair)
                                                    #f))
                      
                      (if also-show-tooltip
                          (<gui> :tool-tip text))
                      (set-statusbar-text! text)
                      #t)
        :leave-func (lambda (button-was-pressed)
                      remove-statusbar-text)))
       
     ;; Painting
     ;;;;;;;;;;;;;;;
       
     (define (paint)  ;; Called before painting the current area's sub-areas
       #f)
     (define (post-paint) ;; Called after painting the current area's sub-areas
       #f)
     
     (define (paint-internal px1 py1 px2 py2) ;; px1, py1, etc. is the clip area of the parent area.

       ;;(c-display "\n\npaint-internal called" ',(car def) "(" x1 y1 x2 y2 "). p: (" px1 py1 px2 py2 ")")

       (when (and (paint?)
                  (<gui> :area-needs-painting gui x1 y1 x2 y2));;overlaps? x1* y1* x2* y2*))

         ;;(c-display "paint-internal hepp" ',(car def) paint "sub-areas" sub-areas)
         (let ((cx1 (max x1 px1))
               (cy1 (max y1 py1))
               (cx2 (min x2 px2))
               (cy2 (min y2 py2)))

           (when (and (> cx2 cx1)
                      (> cy2 cy1))

             (<gui> :do-clipped gui cx1 cy1 cx2 cy2
                    (lambda ()
                      (define (paintit)
                        (paint)                
                        (for-each (lambda (sub-area)
                                    (sub-area :paint-internal cx1 cy1 cx2 cy2))
                                  sub-areas))
                      
                      (if font
                          (<gui> :do-font gui font paintit)
                          (paintit))
             
                      (post-paint)))))))
     
     (define class-name ',(car def))

     (let () ;; Put body into new scope to avoid accidentally overriding an internal method. (use define-override instead of define to purposefully override)
       #t ;; Added to silence "let has no body" error messages.
       ,@body)

     ,@body-methods

     :get-width () width
     :get-height () height
     :get-position x (apply get-position x)
     :get-i-position x (apply get-i-position x)
     :inside? x (apply inside? x)
     :update-me! x (apply update-me! x)
     :set-font! dasfont (set! font dasfont)
     :set-position! x (apply set-position! x)
     :set-position-and-size! x (apply set-position-and-size! x)
     :resize! x (apply resize! x)
     :move! x (apply move! x)
     :move-internal! x (apply move-internal! x)
     :set-parent-area! (new-parent-area) (begin
                                           (assert new-parent-area)
                                           (set! parent-area new-parent-area))
     :get-parent-area x parent-area
     :add-sub-area-plain! (sub-area) (add-sub-area-plain! sub-area)
     :add-sub-area! x (apply add-sub-area! x)
     :add-sub-area-above! x (apply add-sub-area-above! x)
     :add-sub-area-below! x (apply add-sub-area-below! x)
     :remove-sub-area! x (apply remove-sub-area! x)
     :remove-sub-areas! x (apply remove-sub-areas! x)
     :lift-sub-area! x (apply lift-sub-area! x)
     :lift-me! x (apply lift-me! x)
     :get-sub-areas () sub-areas
     :get-state  x (apply get-state x)
     :apply-state! x (apply apply-state! x)

     :key-pressed-internal! x (apply key-pressed-internal x)
     :key-released-internal! x (apply key-released-internal x)
     :mouse-wheel-moved-internal! x (apply mouse-wheel-moved-internal! x)
     :add-mouse-cycle! x (apply add-mouse-cycle! x)
     :get-mouse-cycle x (apply get-mouse-cycle x)
     :add-nonpress-mouse-cycle! x (apply add-nonpress-mouse-cycle! x)
     :get-nonpress-mouse-cycle x (apply get-nonpress-mouse-cycle x)
     :overlaps? x (apply overlaps? x)
     ;;:paint x (apply paint x)
     :paint-internal x (apply paint-internal x)
     :mouse-callback-internal x (apply mouse-callback-internal x)
     :has-mouse () (has-mouse)
     :is-alive () is-alive
     :reset! x (apply reset! x)
     :about-to-be-removed-internal! x (apply about-to-be-removed-internal! x)
     :add-statusbar-text-handler x (apply add-statusbar-text-handler x)
     :override-method! (funcname func) (let* ((org (<_> "super:" funcname)))
                                         (eval `(let ((,org ,funcname))
                                                  ;;(c-display "FUNCNAME:" ',funcname ". old:" ,funcname)
                                                  (set! ,funcname ,func)
                                                  )))
     :class-name () class-name
     ))
 


(def-area-subclass (<area> :gui :x1 :y1 :x2 :y2)  
  )


;; Warning: Does not check if the states are compatible.
(def-area-subclass (<keep-states-area> :gui :x1 :y1 :x2 :y2)    
  
  (define-override (get-state)
    (hash-table :sub-states (map (lambda (area)
                                    (area :get-state))
                                  sub-areas)))
  
  (define-override (apply-state! state)
    (define sub-states (state :sub-states))
    (if (= (length sub-states)
           (length sub-areas))
        (for-each (lambda (state sub-area)
                    (sub-area :apply-state! state))
                  sub-states
                  sub-areas)))
  )

(def-area-subclass (<use-first-subarea-state-as-state-area> :gui :x1 :y1 :x2 :y2)
  (define-override (get-state)
    ((car sub-areas) :get-state))
  (define-override (apply-state! state)
    ((car sub-areas) :apply-state! state)))


(delafina (make-qtarea :width 100 :height 100 :sub-area-creation-callback #f :enable-mouse-callbacks #t)
  (define gui (<gui> :widget width height))  
  (define x1 0)
  (define y1 0)
  (define x2 width)
  (define y2 height)
  (def-area-subclass (<qtarea>)
    (<gui> :add-paint-callback gui
           (lambda (width height)
             (paint-internal 0 0 width height)))

    (when enable-mouse-callbacks
      (<gui> :add-mouse-callback gui
             (lambda (button state x y)
               (mouse-callback-internal button state x y)
               ;;(c-display "has-mouse:" (and (defined? 'has-mouse) (has-mouse)))
               ;;50))
               (has-mouse)))
      (<gui> :add-mouse-wheel-callback gui mouse-wheel-moved-internal!))

    (define-optional-func the-sub-area (key . rest))
    
    (define (recreate width* height*)
      (resize! width* height*)
      (define state (and the-sub-area
                         (the-sub-area :get-state)))
      (remove-sub-areas!)
      (set! the-sub-area (sub-area-creation-callback gui width height state))
      (if state
          (the-sub-area :apply-state! state))
      (add-sub-area-plain! the-sub-area))

    (when sub-area-creation-callback
      (<gui> :add-resize-callback gui recreate)
      (recreate width height))

    (add-method! :recreate (lambda ()
                             (recreate width height)))
    )
  
  (define area (<new> :qtarea))
  
  (area :add-method! :get-gui (lambda ()
                                gui))

  area)



(define *use-testgui* #f)
(<declare-variable> *testgui*)


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


(pretty-print (macroexpand (def-area-subclass (<area-with-custom-method> :gui :x1 :y1 :x2 :y2)
                             (this :add-method 'custom-method (lambda ()
                                                                (c-display "this text was printed from a custom method!"))))
                           )
              )



(define testarea2 (<new> :area-with-custom-method *testarea* 0 0 100 100))))

(pretty-print (macroexpand (<new> :area-with-custom-method *testarea* 0 0 100 100)))
(testarea2 :add-method! :ai (lambda () (c-display "hello")))

(testarea2 :custom-method)
(testarea2 :ai)

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


!!#

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

                     
(def-area-subclass (<text-area> :gui :x1 :y1 :x2 :y2
                                :text ;; can also be function
                                :background-color #f ;; If #f, background will not be painted. can also be function
                                :text-color *text-color* ;; can also be function
                                :wrap-lines #f
                                :align-top #f
                                :align-right #f
                                :align-left #f
                                :paint-border #t
                                :border-rounding 2
                                :scale-font-size #t
                                :cut-text-to-fit #f
                                :only-show-left-part-if-text-dont-fit #t ;; only make sense to set #f if both scale-font-size and cut-text-to-fit is #f.
                                :text-is-base64 #f
                                )

  (define (get-text)
    (if (procedure? text)
        (text)
        text))

  (define (get-text-color)
    (if (procedure? text-color)
        (text)
        text-color))

  (define (get-background-color)
    (and background-color
         (maybe-thunk-value background-color)))

  (define (paint-text-area gui x1 y1 x2 y2)
    (let ((background-color (get-background-color)))
      (if background-color
          (<gui> :filled-box gui background-color x1 y1 x2 y2 border-rounding border-rounding #f)))

    (define text (maybe-thunk-value text))

    (define text-width (<gui> :text-width text gui))

    (let ()
      (define x1 (+ (cond (align-right
                           (- x2 text-width)
                           )
                          (align-left
                           (+ 2 x1))
                          (else
                           x1))
                    1))
      (define x2 (- x2 1))
      
      (when (and (not scale-font-size)
                 (not cut-text-to-fit)
                 (not only-show-left-part-if-text-dont-fit))
        (when (> text-width (- x2 x1))
          (set! x1 (+ x1 (- (- x2 x1) text-width)))))
      
      (<gui> :draw-text gui (maybe-thunk-value text-color) text
             x1
             y1
             x2
             y2
             wrap-lines
             align-top
             (or align-left
                 align-right)
             0 ;; rotate
             cut-text-to-fit
             scale-font-size
             text-is-base64
             ))

    (when paint-border
      (define background-color (<gui> :get-background-color gui))
      (<gui> :do-clipped gui x1 y1 x2 y2
             (lambda ()
               ;;(<gui> :draw-box gui background-color (+ 0 x1) (+ 0 y1) (- x2 0) (- y2 0) 2 0 0)
               ;;(<gui> :draw-box gui *mixer-strip-border-color* x1 y1 x2 y2 1.5 border-rounding border-rounding)
               (<gui> :draw-box gui "#222222" x1 y1 x2 y2 1.2 border-rounding border-rounding)
               ))
      )
    )

  (add-method! :paint-text-area paint-text-area)

  (define-override (paint)
    (paint-text-area gui x1 y1 x2 y2))

  )


(def-area-subclass (<line-input> :gui :x1 :y1 :x2 :y2
                                 :prompt ""
                                 :text ""
                                 :background-color "color9"
                                 :get-wide-string #f
                                 :callback)
  (add-sub-area-plain! (<new> :text-area gui x1 y1 x2 y2
                              :text (lambda () 
                                      (if get-wide-string
                                          (<ra> :from-base64 text)
                                          text))
                              :background-color background-color
                              :align-left #t
                              :scale-font-size #f
                              :only-show-left-part-if-text-dont-fit #f
                              ))
  
  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (and (= button *left-button*)
                                       (<ra> :schedule 0
                                             (lambda ()
                                               (let ((new-name (if get-wide-string
                                                                   (<ra> :request-w-string prompt #t text)
                                                                   (<ra> :request-string prompt #t text))))
                                                 (c-display "GAKKKGAKK_________ NEWNAME" (<-> "-" new-name "-"))
                                                 (when (not (string=? new-name text))
                                                   (set! new-name (callback new-name))
                                                   (if new-name
                                                       (set! text new-name))
                                                   (update-me!))
                                                 #f)))
                                       #t))))

(define (get-default-button-color gui)
  (define gui-background-color (<gui> :get-background-color gui))
  (<gui> :mix-colors "#010101" gui-background-color 0.5))

(def-area-subclass (<checkbox> :gui :x1 :y1 :x2 :y2
                               :is-selected-func
                               :value-changed-callback
                               :paint-func #f
                               :text "" ;; Only used if paint-func is #f
                               :text-color *text-color* ;; Only used if paint-func is #f
                               :selected-color #f ;; only used if paint-func is #f. If #f, use get-default-button-color
                               :right-mouse-clicked-callback #f
                               :border-width 0.25
                               :box-rounding #f
                               )

  (if (not selected-color)
      (set! selected-color (get-default-button-color gui)))

  (define-override (paint)
    (if paint-func
        (paint-func (is-selected-func))
        (begin
          (<gui> :filled-box gui (<gui> :get-background-color gui) x1 y1 x2 y2 3 3)
          (draw-checkbox gui
                         (if (procedure? text) (text) text)
                         (is-selected-func)
                         x1 y1 x2 y2
                         selected-color
                         :text-color text-color
                         :paint-implicit-border #f
                         :implicit-border-width border-width
                         :box-rounding box-rounding
                         )
          (<gui> :draw-box gui "black" x1 y1 x2 y2 1.1 3 3))))

  (add-mouse-cycle! (lambda (button x* y*)
                      (cond ((and right-mouse-clicked-callback
                                  (= button *right-button*)
                                  ;;(not (<ra> :shift-pressed))
                                  )
                             (right-mouse-clicked-callback)
                             #t)
                            ((= button *left-button*)                        
                             (value-changed-callback (not (is-selected-func)))
                             (update-me!)
                             #t)
                            (else
                             #t)))))
                          
                            
(def-area-subclass (<radiobuttons> :gui :x1 :y1 :x2 :y2
                                   :num-buttons
                                   :curr-button-num
                                   :value-changed-callback
                                   :layout-horizontally #t
                                   :paint-func #f
                                   :text-func (lambda (num) "") ;; Only used if paint-func is #f
                                   :text-color *text-color* ;; Only used if paint-func is #f
                                   :selected-color #f ;; only used if paint-func is #f. If #f, use get-default-button-color
                                   :right-mouse-clicked-callback #f
                                   :border-width 0.25
                                   :box-rounding #f
                                   )

  (define layout-func (if layout-horizontally
                          horizontally-layout-areas
                          vertically-layout-areas))
  
  (layout-func x1 y1 x2 y2
               (iota num-buttons)
               :spacing 2
               :callback
               (lambda (num x1 y1 x2 y2)
                 (add-sub-area-plain!
                  (<new> :checkbox gui x1 y1 x2 y2
                         (lambda ()
                           (= num curr-button-num))
                         (lambda (is-on)
                           (if is-on
                               (set! curr-button-num num))
                           (for-each (lambda (num)
                                       (value-changed-callback num (= curr-button-num num)))
                                     (iota num-buttons))
                           (update-me!)
                           )
                         :paint-func (and paint-func
                                          (lambda ()
                                            (paint-func num)))
                         :text (if text-func
                                   (text-func num)
                                   "o")
                         :text-color text-color
                         :selected-color #f
                         :right-mouse-clicked-callback (and right-mouse-clicked-callback
                                                            (lambda ()
                                                              (right-mouse-clicked-callback num)))
                         :border-width border-width
                         :box-rounding box-rounding))))

  )

(def-area-subclass (<button> :gui :x1 :y1 :x2 :y2
                             :paint-func #f
                             :text ""
                             :background-color #f
                             :statusbar-text #f
                             :callback #f
                             :callback-release #f
                             :right-mouse-clicked-callback #f)
  

  (define is-pressing #f)

  (define fontheight (get-fontheight))
  (define b (max 1 (myfloor (/ fontheight 2.5)))) ;; border
  
  (define r 3) ;;rounding
  (define r/2 2)

  (if (not background-color)
      (set! background-color (get-default-button-color gui)))
  
  (define (mypaint)
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
        (<gui> :draw-box gui background-color (+ x1 r/2) (+ y1 r/2) (- x2 r/2) (- y2 r/2) b r r))

    (<gui> :draw-box gui "black" x1 y1 x2 y2 1.1 r r))
    
  (define-override (paint)
    (if paint-func
        paint-func
        (mypaint)))

  (if statusbar-text
      (add-statusbar-text-handler statusbar-text))
  
  (add-mouse-cycle! (lambda (button x* y*)
                      (cond ((and right-mouse-clicked-callback
                                  (= button *right-button*)
                                  ;;(not (<ra> :shift-pressed))
                                  )
                             (right-mouse-clicked-callback)
                             'eat-mouse-cycle)
                            ((= button *left-button*)
                             (set! is-pressing #t)
                             (if callback
                                 (callback))
                             (update-me!)
                             #t)
                            (else
                             #f)))
                    (lambda (button x* y*)
                      #t)
                    (lambda (button x* y*)
                      (set! is-pressing #f)
                      (cond ((and callback-release
                                  (= button *left-button*))
                             (callback-release)))
                      (update-me!))))


(def-area-subclass (<scrollbar> :gui :x1 :y1 :x2 :y2
                                :callback
                                :slider-pos
                                :slider-length ;; between 0 and 1. E.g. for 0.5; slider length = scrollbar length * 0.5. Can also be a function returning the slider length.
                                :vertical
                                :background-color #f
                                :paint-border #t
                                :border-color "black"
                                :border-rounding 0
                                :slider-color "#400010"
                                :slider-pressed-color #f
                                :border-width #f
                                :mouse-press-callback #f
                                :mouse-release-callback #f
                                )

  (if (not slider-pressed-color)
      (set! slider-pressed-color (<gui> :mix-colors "#fefefe" slider-color 0.1)))

  (define b (if border-width
                border-width
                (/ (get-fontheight) 5.0)))

  (define (get-slider-length)
    (if (procedure? slider-length)
        (slider-length)
        slider-length))

  (define (set-legal-slider-pos! new-pos call-callback?)
    (set! slider-pos (between 0
                              new-pos
                              (- 1.0 (get-slider-length))))
    (if call-callback?
        (report!))
    (update-me!))

  (add-method! :get-slider-pos (lambda () slider-pos))

  (add-method! :set-slider-pos! set-legal-slider-pos!)
                 
  (define is-moving #f)
  
  (define-override (paint)
    (paint-scrollbar gui
                     slider-pos
                     (+ slider-pos (get-slider-length))
                     vertical
                     x1 y1 x2 y2
                     background-color
                     (if is-moving
                         slider-pressed-color
                         slider-color)
                     slider-color
                     b
                     border-rounding))

  (define (report!)
    (callback slider-pos
	      (+ slider-pos (get-slider-length))))

  (define start-mouse-pos 0)
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (and (= button *left-button*)
          (begin
            (if mouse-press-callback
                (mouse-press-callback))
            (set-mouse-pointer ra:set-closed-hand-mouse-pointer gui)
            ;;(c-display "start:" slider-pos)
            (set! start-mouse-pos slider-pos)
            (set! is-moving #t)
            (update-me!)
            #t)))
   (lambda (button x* y* dx dy)
     (set-legal-slider-pos! (+ start-mouse-pos
                               (scale (if vertical dy dx)
                                      0 (if vertical
                                            (- height (* b 2))
                                            (- width (* b 2)))
                                      0 1))
                            #t))
   
   (lambda (button x* y* dx dy)
     (set-mouse-pointer ra:set-open-hand-mouse-pointer gui)
     (set! is-moving #f)
     (update-me!)
     (if mouse-release-callback
         (mouse-release-callback))
     ;;(c-display "release button/x/y" x* y*)
     #f
     ))

  (add-nonpress-mouse-cycle!
   :enter-func (lambda (x* y)
                 ;;(c-display "ENTER DRAGGER" class-name)
                 (set-mouse-pointer ra:set-open-hand-mouse-pointer gui)
                 #t)
   :leave-func (lambda (button-was-pressed)
                 ;;(c-display "LEAVE DRAGGER")
                 (set-mouse-pointer ra:set-normal-mouse-pointer gui)
                 #f))

  )

#!!
(begin
  (define testarea (make-qtarea))
  (define scrollbar (<new> :scrollbar (testarea :get-gui)
                           10 10 100 100
                           (lambda x (c-display "callback:" x))
                           0
                           0.2
                           #f
                           :background-color "white"
                           ))
  (testarea :add-sub-area-plain! scrollbar)
  (<gui> :show (testarea :get-gui)))
!!#

#!!
(def-area-subclass (<scroll-area> :gui :x1 :y1 :x2 :y2
                                  :child-area
                                  :dx 0
                                  :dy 0)

  (define vertical-scrollbar ...)
  (define horizontal-scrollbar ...)

  (define (update-areas!)
    (remove-sub-areas!)
    (add-sub-area! child-area (+ x1 dx) (+ y1 dy))
    (child-area :get-position
                (lambda (x1* y1* x2* y2* width* height*)
                  (if (> width* width)
                      (add-vertical-scrollbar))
                  (if (> height* height)
                      (add-horizontal-scrollbar))
                  ...))

    (update-me!)
    )

  (update-areas!)

  )
!!#

     
(def-area-subclass (<vertical-list-area> :gui :x1 :y1 :x2 :y2
                                         :areas
                                         :scrollbar-color "#400010"
                                         :background-color #f
                                         :expand-area-widths #t
                                         :load-areas-from-state #f
                                         )

  (define (get-areas)
    (if (list? areas)
        areas
        (areas)))

  (add-method! :get-areas get-areas)
  
  (define total-area-height (apply + (map (lambda (area)
                                            (area :get-position (lambda (x1 y1 x2 y2 width height)
                                                                  height)))
                                          (get-areas))))
  
  (define slider-length (if (= 0 total-area-height)
                            1
                            (min 1 (/ height total-area-height))))
  
  (define all-fits (>= slider-length 1))
  
  (define scrollbar-width (if all-fits
                              0
                              (between 1
                                       (/ width 10)
                                       (min (average (<gui> :text-width "Xx")
                                                     (<gui> :text-width "x"))
                                            (/ width 2)))))
  
  (define scrollbar-x1 (- x2 scrollbar-width))

  (define (scrollbar-callback pos1 pos2)
    (define pos1 (scale pos1
                        0 1
                        0 total-area-height))
    (position-areas! (+ y1 (- pos1))))

  (define scrollbar (<new> :scrollbar
                           gui
                           scrollbar-x1 y1
                           x2 y2
                           scrollbar-callback
                           :slider-pos 0
                           :slider-length slider-length
                           :vertical #t
                           :background-color background-color))
  
  (define-override (get-state)
    (define areas (get-areas))
    (hash-table :areas areas
                :y1 y1
                :start-y1 (if (null? areas)
                              0
                              ((car areas) :get-position
                               (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                                 a-y1)))
                :slider-pos (scrollbar :get-slider-pos)))
  
  (define-override (apply-state! state)
    (if load-areas-from-state
        (set! areas (state :areas)))
    
    (when (or load-areas-from-state
              (not all-fits))
      (define dy (+ (state :start-y1) (- y1 (state :y1))))
      ;;(c-display "     apply-state!. Position dy:" dy)
      (scrollbar :set-slider-pos! (state :slider-pos) #t)))
;;      (position-areas! dy)))

  (define (position-areas! start-y1)
    (remove-sub-areas!)
    (add-sub-area-plain! scrollbar)
    (let loop ((areas (get-areas))
               (area-y1 start-y1))
      (when (not (null? areas))
        (define area (car areas))
        (define area-y2 (+ area-y1 (area :get-height)))
        (if expand-area-widths
            (area :set-position-and-size! x1 area-y1 (- scrollbar-x1 1) area-y2)
            (area :set-position! x1 area-y1))
        (if (and (>= area-y2 y1)
                 (< area-y1 y2))
            (add-sub-area-plain! area))
        (loop (cdr areas)
              area-y2))))

  (position-areas! y1)

  (define (get-total-areas-height)
    (apply + (map (lambda (area)
                    (area :get-position
                          (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                            a-height)))
                  (get-areas))))
  
  (define (scroll-area-to-top area)
    (scrollbar :set-slider-pos!
               (scale (area :get-position
                            (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                              a-y1))
                      ((car (get-areas)) :get-position
                       (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                         a-y1))
                      ((last (get-areas)) :get-position
                       (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                         a-y2))
                      ;;(get-total-areas-height)
                      0 1)
               #t))

  (add-method! :ensure-area-is-visible
               (lambda (area)
                 (define (a-y1)
                   (area :get-position
                         (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                           a-y1)))
                 (define (a-y2)
                   (area :get-position
                         (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                           a-y2)))
                 (define (test-y1)
                   (< (a-y1) y1))
                 (define (test-y2)
                   (> (a-y2) y2))
                 
                 (define (run-loop test direction)
                   (let loop ((last (scrollbar :get-slider-pos))
                              (n 0))
                     (when (test)
                       (scroll! direction)
                       (let ((now (scrollbar :get-slider-pos)))
                         (if (and (not (= last now))
                                  (< n 10000))
                             (loop now (+ n 1)))))))                 
                 (cond ((test-y1)
                        (run-loop test-y1 #t))
                       ((test-y2)
                        (run-loop test-y2 #f)))))

  (define (scroll! is-up)
    (define areas (get-areas))
    (define is-down (not is-up))
    (define first-y1 (and (not (null? areas))
                          ((car areas) :get-position
                           (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                             a-y1))))
    
    (define last-y2 (and (not (null? areas))
                         ((last areas) :get-position
                          (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                            a-y2))))

    ;;(c-display "is-up:" is-up x* y* first-y1 last-y2)
    
    (call-with-exit
     (lambda (exit)
       (let loop ((areas areas)
                  (n 0))
         (when (not (null? areas))
           (define area (car areas))
           (area :get-position
                 (lambda (a-x1 a-y1 a-x2 a-y2 a-width a-height)
                   (define (doit dy)
                     ;;(set! dy (* dy 4))
                     (define new-first-y1 (+ first-y1 dy))
                     (define new-last-y2 (+ last-y2 dy))
                     (position-areas! new-first-y1)
                     ;;(c-display "scrolling" n new-first-y1 new-last-y2)
                     (scrollbar :set-slider-pos!
                                (scale y1
                                       new-first-y1
                                       new-last-y2
                                       0 1)
                                #f)
                     (exit))
                   (if is-up
                       (when (>= a-y2 (- y1 0.0001)) ;; subtracting 0.0001 to eliminate rounding error
                         ;;(c-display "  " n " how-much1:" (- y1 a-y1))
                         (doit (- y1 a-y1)))
                       (when (< y1 (- a-y2 0.0001)) ;; subtracting 0.0001 to eliminate rounding error
                         ;;(c-display "  " n " how-much2:" (- a-y2 y1))
                         (define dy (- last-y2 y2))                                                
                         (when (> (+ dy 0.0001) 0)
                           (set! dy (min dy (- a-y2 y1)))
                           (doit (- dy)))))))
           (loop (cdr areas)
                 (1+ n)
                 ))))))
    
  (define-override (mouse-wheel-moved-internal! is-up x* y*)
    (scroll! is-up)
    #t)
  )

#!!
(when (defined? 'horizontally-layout-areas)
  (define (recreate gui width height state)
    (define area
      (<new> :vertical-list-area gui 0 0 width height
           (map (lambda (i)
                  (define blocknum i)
                  (define color ;;(<ra> :get-block-color blocknum))
                    (<gui> :mix-colors
                           (<ra> :get-block-color blocknum)
                           "white" ;;(<gui> :get-background-color -1)
                           0.95))
                  (define line
                    (<new> :text-area gui
                           10 0 100 (* 1.2 (get-fontheight))
                           (lambda ()
                             (if (< i (<ra> :get-num-blocks))
                                 (<-> i ": " (<ra> :get-block-name i))
                                 ""))
                           :text-color "sequencer_text_color"
                           :background-color (lambda ()
                                               (if (= (<ra> :current-block) i)
                                                   (<gui> :mix-colors color "green" 0.1)
                                                   color))
                           :align-left #t))
                  (line :add-mouse-cycle!
                        (lambda (button x* y*)
                          ;;(c-display "hepp" i)
                          (line :get-position
                                (lambda (x1 y1 x2 y2 width height)
                                  (<gui> :create-block-drag-icon gui (floor width) (floor height) (floor (- x* x1)) (floor (- y* y1)) i
                                         (lambda (gui width height)
                                           ;;(c-display "-------w2: " width height)
                                           (line :paint-text-area gui 0 0 width height)
                                           ;;(<gui> :draw-line gui "black" 5 3 10 5 20)
                                           ))))
                          #t)
                        (lambda (button x* y*)
                          #t)
                        (lambda (button x* y*)
                          #t))
                  line)
                (iota (<ra> :get-num-blocks)))))
    (if state
        (area :apply-state! state))
    area)

  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback recreate))
  (<gui> :show (testarea :get-gui))
  )
!!#
#!!
(when (defined? 'horizontally-layout-areas)
  (define (recreate gui width height state)
    (define audiofiles (to-list (<ra> :get-audio-files)))
    (<new> :vertical-list-area gui 0 0 width height
           (map (lambda (i audiofile)
                  (define color (<gui> :mix-colors
                                       (<ra> :get-audiofile-color audiofile)
                                       "white"
                                       0.65))
                  (define line
                    (<new> :text-area gui
                           10 0 100 (* 1.2 (get-fontheight))
                           (lambda ()
                             (<-> i ": " (<ra> :get-path-string audiofile)))
                           :text-color "sequencer_text_color"
                           :background-color color
                           :align-left #t))
                  (line :add-mouse-cycle!
                        (lambda (button x* y*)
                          ;;(c-display "hepp" i)
                          (line :get-position
                                (lambda (x1 y1 x2 y2 width height)
                                  (<gui> :create-file-drag-icon gui (floor width) (floor height) (floor (- x* x1)) (floor (- y* y1)) audiofile
                                         (lambda (gui width height)
                                           ;;(c-display "-------w2: " width height)
                                           (line :paint-text-area gui 0 0 width height)
                                           ;;(<gui> :draw-line gui "black" 5 3 10 5 20)
                                           ))))
                          #t)
                        (lambda (button x* y*)
                          #t)
                        (lambda (button x* y*)
                          #t))
                  line)
                (iota (length audiofiles))
                audiofiles)))

  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback recreate))
  (<gui> :show (testarea :get-gui))
  )
!!#


#!!
(begin
  ;(def-area-subclass (<text-area> :gui :x1 :y1 :x2 :y2 :text)
  ;  (define-override (paint)
  ;    (<gui> :draw-text gui *text-color* text (+ x1 (get-fontheight)) y1 x2 y2
  ;           #f ;wrap-lines 
  ;           #f ;align-top 
  ;           #t ;align-left
  ;           )
  ;    (<gui> :draw-box gui "black" x1 y1 x2 y2 1.5 2 2)))
      
  (define testarea (make-qtarea :width 150 :height 450))
  (define list-area (<new> :vertical-list-area (testarea :get-gui) 10 20 150 400
                           (map (lambda (i)
                                  (<new> :text-area (testarea :get-gui)
                                         10 0 100 (* 1.2 (get-fontheight))
                                         (<-> i ": hello")
                                         :align-left #t))
                                (iota 20))
                           ))
  (testarea :add-sub-area-plain! list-area)
  (<gui> :show (testarea :get-gui))
  )
(show-async-message :text "hello")

(<ra> :get-path "/tmpwef")

(<ra> :iterate-directory (<ra> :get-path "/home/kjetil") #t
      (lambda (is-final file-info)
        (if (and (not is-final)
                 (file-info :is-audiofile))
            (c-display "file-info:" file-info))
        #t))
(<ra> :iterate-directory "L3RtcA==" #f c-display)
!!#

(def-area-subclass (<seqblock-table-entry-area> :gui :x1 :y1 :x2 :y2
                                                :is-current
                                                :entry-num
                                                :file-info #f
                                                :blocknum #f
                                                :allow-dragging #f
                                                :background-color #f
                                                :callback #f)
  
  (assert (or file-info blocknum))

  (define (is-current?)
    (if (procedure? is-current)
        (is-current)
        is-current))
  
  (define (get-text-color)
    (cond ((is-current?)
           *text-color*)
          (blocknum
           "black")
          ((file-info :is-dir)
           *text-color*)
          (background-color
           "black")
          (else
           "soundfile"
           )))
  
  (define ch-color "black")
  (define size-color (if background-color
                         "black"
                         *text-color*)) ;;"#081040")

  (define is-dir (and file-info (file-info :is-dir)))
  (define is-soundfile (and file-info (file-info :is-audiofile)))

  (define name-text (if blocknum
                        (<ra> :to-base64 (get-block-table-entry-text blocknum))
                        (let ((filename (file-info :filename)))
                          (if (and is-dir
                                   ;;(not (string=? "." filename))
                                   ;;(not (string=? ".." filename))
                                   )
                              (<ra> :append-base64-strings filename (<ra> :to-base64 "/"))
                              filename))))
    
  (define ch-text (cond (blocknum
                         (<-> (<ra> :get-num-tracks blocknum) "tr"))
                        (is-soundfile 
                         (<-> (file-info :num-ch) "ch"))
                        (else                         
                         "")))

  (define size-text (cond ((or blocknum is-soundfile)
                           (let ((s (if blocknum
                                        (/ (/ (<ra> :get-block-length blocknum)
                                              (<ra> :get-sample-rate))
                                           (<ra> :get-reltempo blocknum))
                                        (/ (file-info :num-frames)
                                           (file-info :samplerate)))))
                             (if (< s 60)
                                 (<-> (if (< s 10)
                                          " "
                                          "")
                                      (two-decimal-string s)
                                      "s")
                                 (let* ((minutes (floor (/ s 60)))
                                        (seconds (floor (- s (* minutes 60)))))
                                   (<-> (if (< minutes 10)
                                            (<-> " " minutes)
                                            minutes)
                                        ":"
                                        (if (< seconds 10)
                                            (<-> "0" seconds)
                                            seconds)
                                        "m")))))
                          (is-dir
                           "")
                          (else
                           (<-> (one-decimal-string (/ (file-info :size)
                                                       (* 1024 1024)))
                                "MB"))))

  (add-method! :set-current! (lambda (doit)
                               (set! is-current doit)
                               (update-me!)))

  (add-mouse-cycle! (lambda (button x* y*)
                      (define gotit #f)
                      (if callback
                          (set! gotit (callback button x* y* entry-num)))
                      (if gotit
                          #t
                          (and (= button *left-button*)
                               (begin
                                 ;;(set! dragging-entry (make-dragging-entry))
                                 ;;(<gui> :show dragging-entry)
                                 ;;(move-dragging-entry!)
                                 ;;(c-display "w: " width height)
                                 ;;(c-display "file-info:" file-info)
                                 (cond (blocknum
                                        (<gui> :create-block-drag-icon gui (floor width) (floor height) (floor (- x* x1)) (floor (- y* y1)) blocknum
                                               (lambda (gui width height)
                                                 ;;(c-display "-------w2: " width height)
                                                 ;;(<gui> :filled-box gui "#00000000" 0 0 width height 0 0 #f) ;; fill with transparent background
                                                 (paint2 gui -1 0 width height)
                                                 ;;(line :paint-text-area gui 0 0 width height)
                                                 ;;(<gui> :draw-line gui "black" 5 3 10 5 20)
                                                 )))
                                       ((and (not (file-info :is-dir))
                                             allow-dragging)
                                        (<gui> :create-file-drag-icon gui (floor width) (floor height) (floor (- x* x1)) (floor (- y* y1)) (file-info :path)
                                               (lambda (gui width height)
                                                 (c-display "-------w2: " width height)
                                                 (paint2 gui -1 0 width height)
                                                 ))))
                                 #t))))
                    (lambda (button x* y*)
                      ;;(move-dragging-entry!)
                      #t)
                    (lambda (button x* y*)
                      ;;(<gui> :close dragging-entry)
                      ;;(<gui> :hide dragging-entry)
                      ;;(set! dragging-entry #f)
                      ;;(drop-callback (<ra> :get-mouse-pointer-x) (<ra> :get-mouse-pointer-y))
                      #t))

  (define (paint2 gui x1 y1 x2 y2)
    (define default-size-x1 (- x2 (<gui> :text-width "2.99m" gui)))
    (define ch-x1 (- default-size-x1 (<gui> :text-width "2ch" gui)))
    (define name-x2 (- ch-x1 (<gui> :text-width " " gui)))
    (define ch-x2 (- default-size-x1 (<gui> :text-width " " gui)))

    (define size-x1 (max default-size-x1 (- x2 (<gui> :text-width size-text gui))))

    (define entry-background-color (if background-color
                                       background-color
                                       "color9"))
    
    ;;(if is-current
    ;;    (set! entry-background-color (<gui> :mix-colors entry-background-color "green" 0.1)))

    (<gui> :filled-box gui entry-background-color (+ 1 x1) y1 x2 y2 4 4 (if background-color #t #f))

    ;; name
    (<gui> :draw-text gui (get-text-color) name-text
           (+ 4 x1) y1
           (if is-dir x2 name-x2) y2
           #f ;; wrap lines
           #f ;; align-top
           #t ;; align-left
           0 ;; rotate
           #t ;; cut-text-to-fit
           #f ;; scale-font-size
           #t ;; text is base64
           )

    (cond ((or blocknum
               is-soundfile)
           ;; ch
           (<gui> :draw-text gui ch-color ch-text
                  ch-x1 y1
                  ch-x2 y2
                  #f ;; wrap lines
                  #f ;; align-top
                  #t ;; align-left
                  0 ;; rotate
                  #f ;; cut-text-to-fit
                  #t ;; scale-font-size
                  )
           
           ;; duration
           (<gui> :draw-text gui size-color size-text
                  size-x1 y1
                  x2 y2
                  #f ;; wrap lines
                  #f ;; align-top
                  #t ;; align-left
                  0 ;; rotate
                  #f ;; cut-text-to-fit
                  #t ;; scale-font-size
                  ))

          ((not is-dir)
           ;; size
           (<gui> :draw-text gui size-color size-text
                  ch-x1 y1
                  x2 y2
                  #f ;; wrap lines
                  #f ;; align-top
                  #f ;; align-left
                  0 ;; rotate
                  #t ;; cut-text-to-fit
                  #f ;; scale-font-size
                  )))

    (when (is-current?)

      ;; Why was this line here?
      ;;(<gui> :set-clip-rect gui (+ x1 1) y1 x2 y2)
      
      (<gui> :draw-box gui (<gui> :mix-colors "#010101" "green" 0.5) (+ 1 x1) (+ y1 0) (- x2 0) (- y2 0) (/ (get-fontheight) 2.5) 4 4)
      ;;(<gui> :set-clip-rect gui cx1 cy1 cx2 cy2)
      )

    (<gui> :draw-box gui "black" (+ 1 x1) y1 x2 y2 0.5 4 4)
    )
  
  (define-override (paint)
    (paint2 gui x1 y1 x2 y2))
  )

;; TODO: Right-click options/double click: "Insert/append new audio seqtrack for this audio file".  
(def-area-subclass (<file-browser> :gui :x1 :y1 :x2 :y2
                                   :path
                                   :id-text
                                   :only-audio-files #f
                                   :state #f)

  (define num-settings-buttons 9)

  (define curr-settings-num (string->number (<ra> :get-settings (<-> "filebrowser_" id-text "_curr-settings-num") "0")))
  (define curr-entry-num 0)

  (set! path (<ra> :get-settings-w (<-> "filebrowser_" id-text "_" curr-settings-num) path))
  
  (define entries '())
  (define file-browser-entries '())

  (define-optional-func vertical-list-area (key . rest))

  (define states (make-vector num-settings-buttons #f))

  (set! font (<ra> :get-sample-browser-font #f))

  (define (store-curr-entry-state!)
    (set! (states curr-settings-num)
          (hash-table :path path
                       :entries entries
                       :entries-is-complete entries-is-complete
                       :vertical-list-area-state (and vertical-list-area
                                                      (vertical-list-area :get-state))
                       )))
    
  (define-override (get-state)
    (store-curr-entry-state!)
    (hash-table :curr-settings-num curr-settings-num
                 :states states))

  (define (apply-state2! state)
    (set! path (state :path))
    ;;(c-display "          Apply state. Complete state:" (state :entries-is-complete))
    (if (state :entries-is-complete)
        (begin
          (set! entries (state :entries))
          (set! entries-is-complete #t)
          (inc! update-num 1)
          (update-areas!)
          (let ((vla-state (state :vertical-list-area-state)))
            (if (and vla-state vertical-list-area)
                (vertical-list-area :apply-state! vla-state))))
        (update-directory!)))
    
  (define-override (apply-state! state)
    ;;(c-display "apply-state:" state)
    (set! curr-settings-num (state :curr-settings-num))
    (set! states (state :states))
    (apply-state2! (states curr-settings-num)))

  (delafina (set-new-path! :new-path
                           :store-setting #t)
    (define old-path path)
    (let loop ((new-path new-path)
               (n 0))
      (set! path new-path)
      (set! curr-entry-num 0)
      (set! entries '())
      (set! file-browser-entries '())
      (c-display "         Calling 2")
      (if (not (update-directory!))
          (if (< n 5)
              (if (= n 0)
                  (loop old-path 1)
                  (loop (<ra> :get-home-path) 1)))
          (<ra> :put-settings-w (<-> "filebrowser_" id-text "_" curr-settings-num) path))))
    
  (define (set-new-curr-entry! new-curr-entry-num)
    (when (not (null? entries))
      (c-display "selected:" new-curr-entry-num ".old:" curr-entry-num)
      (c-display "entry:" (file-browser-entries curr-entry-num))
      (define file-info (entries new-curr-entry-num))
      
      (if (file-info :is-dir)
          (set-new-path! (file-info :path))
          (begin
            (file-browser-entries curr-entry-num :set-current! #f)
            (set! curr-entry-num new-curr-entry-num)
            (file-browser-entries curr-entry-num :set-current! #t)))
      ))
  
  (define (update-areas!)
    ;;(c-display "\n\n\n---------------------- num entries:" (length entries) "-----------------------\n\n\n")
    (remove-sub-areas!)

    (define border 2)

    (define pathline-y1 (+ y1 (get-fontheight) border))

    (add-sub-area-plain! (<new> :radiobuttons gui x1 y1 x2 (- pathline-y1 border)
                                num-settings-buttons
                                curr-settings-num
                                (lambda (num is-on)
                                  (c-display "numison:" num is-on)
                                  (when is-on
                                    (store-curr-entry-state!)
                                    (set! curr-settings-num num)
                                    (<ra> :put-settings (<-> "filebrowser_" id-text "_curr-settings-num") (<-> num))
                                    (let ((state (states curr-settings-num)))
                                      (if state
                                          (apply-state2! state)
                                          (set-new-path! (<ra> :get-settings-w (<-> "filebrowser_" id-text "_" num) path)
                                                         :store-setting #f))))
                                  #t)
                                #t
                                :text-func (lambda (num)
                                             (<-> num))))
    
    (define button-width (* 2 (<gui> :text-width "R")))
    (define reload-x1 (+ x1 button-width border))
    (define line-input-x1 (+ reload-x1 button-width border))

    (define browser-y1 (+ pathline-y1 (get-fontheight) border))

    (add-sub-area-plain! (<new> :button gui x1 pathline-y1 (- reload-x1 border) (- browser-y1 border)
                                :text "⇧"
                                :callback-release
                                (lambda ()
                                  (set-new-path! (<ra> :get-parent-path path)))))

    (add-sub-area-plain! (<new> :button gui reload-x1 pathline-y1 (- line-input-x1 border) (- browser-y1 border)
                                :text "↻"
                                :callback-release
                                (lambda ()
                                  (set-new-path! path))))

    (define line-input (<new> :line-input gui line-input-x1 pathline-y1 x2 (- browser-y1 border)
                              :prompt ""
                              :text path
                              :get-wide-string #t
                              :callback
                              (lambda (new-name)
                                (if (not (string=? new-name ""))
                                    (begin
                                      (c-display "new-name: -" new-name "-" (<ra> :from-base64 new-name) "-")
                                      (set-new-path! new-name)
                                      new-name)
                                    path))))
                                
    (add-sub-area-plain! line-input)

    (set! file-browser-entries
          (map (lambda (entry entry-num)
                 (<new> :seqblock-table-entry-area gui 
                        0 0 10 (* 1.2 (get-fontheight))
                        (= entry-num curr-entry-num)
                        entry-num
                        :file-info entry
                        :allow-dragging #t
                        :background-color #f
                        :callback 
                        (lambda (button x y entry-num)
                          (if (= button *right-button*)
                              #t
                              (begin
                                (set-new-curr-entry! entry-num)
                                #f)))
                        ))
               entries
               (iota (length entries))
               ))

    (set! vertical-list-area (<new> :vertical-list-area gui x1 browser-y1 x2 y2 file-browser-entries))
    (add-sub-area-plain! vertical-list-area)
    )
  
  (update-areas!) ;; necessary in case there are no entries. (fix: update-directory! probably doesn't update if there is no entries in dir)

  (define entries-is-complete #f)
  (define update-num 0) ;; Used to be able to cancel previous ra:iterate-directory iterations.
  
  (define (update-directory!)
    (inc! update-num 1)
    ;;(c-display "Updating path: -" (<ra> :get-path-string path) "-. id:" id ". update-num:" update-num)
    (let ((last-time (time))
          (temp '())
          (curr-update-num update-num))
      (<ra> :iterate-directory path #t
            (lambda (is-finished file-info)
              ;;(c-display "file-info:" file-info)
              (cond ((not is-alive)
                     ;;(c-display "   Abort: not alive. curr:" curr-update-num ". update-num:" update-num ". id:" id)
                     #f)
                    ((not (= curr-update-num update-num))
                     ;;(c-display "   Abort: Not update. curr:" curr-update-num ". update-num:" update-num ". id:" id)
                     #f)  ;; There has been a later call to ra:iterate-directory.
                    (else
                     (set! entries-is-complete #f)
                     (if (and (not is-finished)
                              (or (not only-audio-files)
                                  (file-info :is-dir)
                                  (file-info :is-audiofile)))
                         (set! temp (cons file-info temp)))
                     ;;(c-display "timeetc." time last-time (> (time) (+ last-time 50)))
                     (when (or is-finished
                               (and (not (null? temp))
                                    (> (time) (+ last-time 50))))
                       (set! entries (sort (append entries temp)
                                           (lambda (a b)
                                             (define is-dir-a (a :is-dir))
                                             (define is-dir-b (b :is-dir))
                                             (if (eq? is-dir-a is-dir-b)
                                                 (string<? (<ra> :from-base64 (a :filename))
                                                           (<ra> :from-base64 (b :filename)))
                                                 is-dir-a))))
                       (if is-finished
                           (set! entries-is-complete #t))
                       (set! temp '())
                       (update-areas!)
                       (set! last-time (time))
                       )
                     #t))))))

  (if state
      (apply-state! state)
      (update-directory!)
      )
  
  )

#!!
(when (defined? 'horizontally-layout-areas)
  (define (recreate gui width height state)
    (define list-area (<new> :file-browser gui 10 20 (- width 10) (- height 20)
                             :path (<ra> :get-path "/home/kjetil/") ;;radium/bin/sounds")
                             :id-text "test"
                             :only-audio-files #t
                             :state state
                             ))
    list-area)
  ;;(testarea :add-sub-area-plain! list-area)

  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback recreate))
  (<gui> :show (testarea :get-gui))
  )
!!#

(def-area-subclass (<tabs> :gui :x1 :y1 :x2 :y2
                           :is-horizontal
                           :curr-tab-num
                           :tab-names
                           :state
                           :get-tab-area-func)

  (define num-tabs (length tab-names))
  (define tab-bar-height (* (<ra> :get-tab-bar-height) (get-fontheight)))

  (define tab-bar-x2 (if is-horizontal
                       x2
                       (+ x1 tab-bar-height)))
  (define tab-bar-y2 (if is-horizontal
                         (+ y1 tab-bar-height)
                         y2))

  (define sub-x1 (if is-horizontal
                     x1
                     (+ x1 tab-bar-height)))
  (define sub-y1 (if is-horizontal
                     (+ y1 tab-bar-height)
                     y1))

  (define-optional-func tab-area (key . rest))

  (define tab-states (make-vector num-tabs #f))

  (define-override (get-state)
    (set! (tab-states curr-tab-num) (tab-area :get-state))
    (hash-table :curr-tab-num curr-tab-num
                 :tab-states tab-states))

  (define-override (apply-state! state)
    (set! curr-tab-num (state :curr-tab-num))
    (set! tab-states (state :tab-states))
    (recreate-areas!))

  (define-override (paint)
    (paint-tab-bar gui x1 y1 tab-bar-x2 tab-bar-y2
                   is-horizontal
                   tab-names
                   curr-tab-num
                   ;;:background-color (<gui> :mix-colors (<gui> :get-background-color gui) "red" 0.95)
                   ))

  (define (recreate-areas!)
    (remove-sub-areas!)
    (set! tab-area (get-tab-area-func curr-tab-num
                                      sub-x1 sub-y1
                                      x2 y2
                                      (tab-states curr-tab-num)
                                      ))
    (add-sub-area-plain! tab-area)
    (update-me!))

  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (and (= button *left-button*)
                                       (call-with-exit
                                        (lambda (return)
                                          (for-each (lambda (tab-num)
                                                      (get-tab-coords is-horizontal tab-num num-tabs x1 y1 tab-bar-x2 tab-bar-y2
                                                                      (lambda (x1 y1 x2 y2)
                                                                        ;;(c-display "x*/y*:" x* y* ". x1/y1/x2/y2:" x1 y1 x2 y2)
                                                                        (when (and (>= x* x1)
                                                                                   (< x* x2)
                                                                                   (>= y* y1)
                                                                                   (< y* y2))
                                                                          (when (not (= tab-num curr-tab-num))
                                                                            (set! (tab-states curr-tab-num) (tab-area :get-state))
                                                                            (set! curr-tab-num tab-num)
                                                                            (recreate-areas!))
                                                                          (return #t)))))
                                                    (iota num-tabs))
                                          #f)))))
  
  ;;(c-display "STATE:" state)
  (if state
      (apply-state! state)
      (recreate-areas!))
  )

#!!
(when (defined? 'horizontally-layout-areas)
  (define (recreate gui width height state)
    (define list-area (<new> :tabs gui 10 20 (- width 10) (- height 20)
                             :is-horizontal #f
                             :curr-tab-num 0
                             :tab-names '("Hide" "Blocks" "Audio files" "File Browser")
                             :get-tab-area-func
                             (lambda (tab-num x1 y1 x2 y2 state)
                               (if (< tab-num 2)
                                   (<new> :button gui x1 y1 x2 y2
                                          :text (<-> "ai:" tab-num))
                                   (<new> :file-browser gui x1 y1 x2 y2
                                          :path (<ra> :get-path "/home/kjetil/") ;;radium/bin/sounds")
                                          :id-text "test"
                                          :only-audio-files #t
                                          :state state
                                          )))
                             ))
    list-area)
  ;;(testarea :add-sub-area-plain! list-area)

  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback recreate))
  (<gui> :show (testarea :get-gui))
  )
!!#

#!!
(when (defined? 'horizontally-layout-areas)
  (define (recreate gui width height state)
    (define list-area (<new> :tabs gui 10 20 (- width 10) (- height 20)
                             :is-horizontal #f
                             :curr-tab-num 0
                             :tab-names '("tab1" "tab2" "browser")
                             :get-tab-area-func
                             (lambda (tab-num x1 y1 x2 y2)
                               (if (< tab-num 2)
                                   (<new> :button gui x1 y1 x2 y2
                                          :text (<-> "ai:" tab-num))
                                   (<new> :file-browser gui x1 y1 x2 y2
                                          :path (<ra> :get-path "/home/kjetil/") ;;radium/bin/sounds")
                                          :id-text "test"
                                          :only-audio-files #t
                                          )))
                             ))
    list-area)
  ;;(testarea :add-sub-area-plain! list-area)

  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback recreate))
  (<gui> :show (testarea :get-gui))
  )
  
!!#


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
        (apply callback (append (if (list? arg) arg (list arg))
                                (list (+ x1 x1-border) y1*
                                      (- x2 x2-border) y2*))))
      (loop (cdr args)
            (1+ n)))))



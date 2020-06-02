(provide 'gui.scm)

(my-require 'mouse-primitives.scm)


(define-constant *min-db* (<ra> :get-min-db))
(define-constant *max-db* (<ra> :get-max-db))
(define-constant *max-mixer-db* 6)

(define (get-fontheight)
  (+ 4 (<gui> :get-system-fontheight))) ;; If changing this one, change the "font_height" variable in Seqtracks_widget::get_heights, and paintVamps in api_gui.cpp, too.

(define-constant *text-color* "#cccccc")


(define-constant *mixer-strip-border-color* "#bb222222")
(define-constant *mixer-strip-background-color* "#222222")
(define *current-mixer-strip-border-color* "mixerstrips_selected_object_color_num")

(define *last-statusbar-id* -1)

(define (set-editor-statusbar text)
  (set! *last-statusbar-id* (<ra> :set-statusbar-text text)))

(define (set-statusbar-value val)
  (set-editor-statusbar (<-> val)))

(define (set-velocity-statusbar-text value)
  (set-editor-statusbar (<-> "Velocity: " (one-decimal-percentage-string value) "%")))



(define (rectangle-intersects-with-parallelogram a_x1 a_y1 a_x2 a_y2
                                                 b_x1 b_y1 b_x2 b_y2 b_width)
  (define a_triangle1 (list a_x1 a_y1
                            a_x2 a_y1
                            a_x1 a_y2))
  (define a_triangle2 (list a_x2 a_y1
                            a_x2 a_y2
                            a_x1 a_y2))
  (define b_triangle1 (list b_x1 b_y1
                            (+ b_x1 b_width) b_y1
                            b_x2 b_y2))
  (define b_triangle2 (list (+ b_x1 b_width) b_y1
                            (+ b_x2 b_width) b_y2
                            b_x2 b_y2))
  ;;(c-display "rect1:" a_x1 a_y1 " - " a_x2 a_y2)
  ;;(c-display "rect2:" b_x1 b_y1 " - " (* 1.0 b_x2) b_y2 b_width)
  ;;(c-display "tri1:" (map (lambda (a) (* 1.0 a)) a_triangle1))
  ;;(c-display "tri2:" (map (lambda (a) (* 1.0 a)) b_triangle1))
  (define ret (or (apply ra:triangles-intersects (append a_triangle1 b_triangle1))
                  (apply ra:triangles-intersects (append a_triangle1 b_triangle2))
                  (apply ra:triangles-intersects (append a_triangle2 b_triangle1))
                  (apply ra:triangles-intersects (append a_triangle2 b_triangle2))))
  ;;(c-display "ret" ret
  ;;           (apply ra:triangles-intersects (append a_triangle1 b_triangle1))
  ;;           (apply ra:triangles-intersects (append a_triangle1 b_triangle2))
  ;;           (apply ra:triangles-intersects (append a_triangle2 b_triangle1))
  ;;           (apply ra:triangles-intersects (append a_triangle2 b_triangle2))
  ;;           "\n\n")
  ret)


#!!
(rectangle-intersects-with-parallelogram 0 0 4 4
                                         4 4 10 10 1)

(rectangle-intersects-with-parallelogram 4.01 4 10 10
                                         0 0 4 4 1)
(apply ra:triangles-intersects
       (append (list 0 0
                     0 10
                     5 5)
               (list 0 0
                     0 10
                     5 5)))
!!#


(define (set-fixed-size gui width height)
  (<gui> :set-min-height gui height)
  (<gui> :set-max-height gui height)
  (<gui> :set-min-width gui width)
  (<gui> :set-max-width gui width)
  (<gui> :set-size gui width height);
  (<gui> :set-size-policy gui #f #f))
  
(define (set-fixed-height gui height)
  (<gui> :set-min-height gui height)
  (<gui> :set-max-height gui height)
  (<gui> :set-size gui (<gui> :width gui) height)
  (<gui> :set-size-policy gui #t #f))


(define (set-fixed-width gui width)
  (<gui> :set-min-width gui width)
  (<gui> :set-max-width gui width)
  (<gui> :set-size gui width (<gui> :height gui))
  (<gui> :set-size-policy gui #f #t))


(define (mid-vertical-layout . guis)
  (define layout (<gui> :vertical-layout))
  (<gui> :add-layout-space layout 1 1 #f #t)
  (for-each (lambda (gui)
              (<gui> :add layout gui))
            guis)
  (<gui> :add-layout-space layout 1 1 #f #t)
  layout)
  
(define (mid-horizontal-layout . guis)
  (define layout (<gui> :horizontal-layout))
  (<gui> :add-layout-space layout 1 1 #t #f)
  (for-each (lambda (gui)
              (<gui> :add layout gui))
            guis)
  (<gui> :add-layout-space layout 1 1 #t #f)
  layout)

(define (gui-create-layout create-layout-func layout-args . guis)
  (define layout (apply create-layout-func layout-args))
  (for-each (lambda (gui)
              ;;(c-display "Adding" gui "to layout" layout)
              (<ra> :gui_add layout gui))
            (flatten guis))
  layout)
  
(define (my-gui_group title args)
  (define group (<ra> :gui_group title))
  (for-each (lambda (gui)
              (<ra> :gui_add group gui))
            args)
  group)


(define (mybutton text background-color pressed-color text-color callback)
  (define button (<gui> :button text callback))
  (<gui> :add-callback button
         (lambda (width height)
           (define is-down (<gui> :get-value button))
           (<gui> :filled-box
                  button
                  background-color
                  0 0 width height)
           (if is-down
               (<gui> :filled-box
                      button
                      pressed-color
                      2 1 (- width 2) (- height 1)))
           (<gui> :my-draw-text
                  button
                  text-color
                  text
                  3 2 (- width 3) (- height 2)
                  #f
                  #f
                  #f
                  0)))
  button)

#!!
(let ((button (mybutton "hello" "green" "blue" "red" (lambda () (c-display "helo")))))
  (<gui> :show button))
!!#

(define (my-gui_tablelayout . args)
  (if (= 1 (length args))
      (apply ra:gui_table-layout (car args))
      (let* ((rows args)
             (max-num-columns (apply max (map length rows)))
             (table (<ra> :gui_table-layout max-num-columns)))
        (for-each (lambda (row)
                    (for-each (lambda (gui)
                                (<ra> :gui_add table gui))
                              row)
                    (for-each (lambda (n)
                                (<ra> :gui_add table (<gui> :empty)))
                              (iota (- max-num-columns (length row)))))
                  rows)
        table)))

(define (split-text-at-first-best-space text width callback)
  (define (return-result before after)
    (callback (string-strip before) (string-strip after)))
  (if (<= (<gui> :text-width text)
          width)
      (apply return-result (list text ""))
      (let loop ((before-chars '())
                 (after-chars (string->list text))
                 (result (list "" text)))
        (define char (cl-car after-chars))
        (define next-before-chars (append before-chars 
                                          (list char)))
        (cond ((null? after-chars)
               (apply return-result result))
              ((or (not (char=? #\space char))
                   (string=? (list->string (take after-chars 3))
                             " dB"))
               (loop next-before-chars
                     (cdr after-chars)
                     result))
              (else
               (let ((before (list->string before-chars)))
                 ;;(c-display "bef:" before (<gui> :text-width before) width)
                 (if (<= (<gui> :text-width before)
                         width)
                     (loop next-before-chars
                           (cdr after-chars)
                           (list before (list->string (cdr after-chars))))
                     (loop next-before-chars
                           (cdr after-chars)
                           result))))))))

#!!
(split-text-at-first-best-space "sdf erio gaerg" 2000 list)
=>
("" "sdf erio gaerg")

(split-text-at-first-best-space "sdf erio gaerg" 20 list)
=>
("sdf" "erio gaerg")
!!#

(define (fit-text-by-adding-dots width text)
  ;;(c-display "text:" text)
  (if (= 0 (string-length text))
      ".."
      (let* ((dot-text (<-> text ".."))
             (dot-text-width (<gui> :text-width dot-text)))
        (if (<= dot-text-width width)
            dot-text
            (fit-text-by-adding-dots width (string-drop-right text 1))))))

#!!
(fit-text-by-adding-dots 50 "hello1234")
!##

;; Returns a version of 'text' with added line shifts so that it fits width and heigh.
;; If that is not possible, it will stop adding line shifts before it doesn't fit vertically anymore.
(define (fit-text text width height call-me-if-not-all-text-fitted!)
  (let loop ((text text)
             (num-lines (max 1 (floor (/ height (get-fontheight))))))
    (define text-width (<gui> :text-width text))
    (cond ((or (string=? "" text)
               (<= text-width width))
           text)
          ((= num-lines 1)
           (call-me-if-not-all-text-fitted!)
           (fit-text-by-adding-dots width text))
          (else
           (split-text-at-first-best-space 
            text width
            (lambda (before after)
              ;;(assert (not (string=? "" before)))
              (if (string=? after "")
                  before
                  (<-> (if (string=? before "")
                           ""
                           (<-> before "\n")) ;;".n."
                       (loop after
                             (- num-lines 1))))))))))

#!!
(fit-text "ab abab    9 d9d9d  " 30 (* (get-fontheight) 3))
=>
"ababab\n9d9d9d"
!!#

(delafina (my-gui_draw-text :gui :color :text :x1 :y1 :x2 :y2
                            :wrap-lines #t
                            :align-top #f
                            :align-left #f
                            :rotate 0
                            :cut-text-to-fit #t
                            :scale-font-size #t)

  (define vertical-text (= 0 (modulo (+ 90 rotate) 180)))
  (define horizontal-text (= 0 (modulo rotate 180)))

  (define all-text-fitted #t)
  
  (define paint-text
    (if (and wrap-lines
             (or vertical-text horizontal-text)
             )
        (fit-text text  ;; Replacement code. Wrapping lines in gui_draw-text has been disabled since it didn't work very well.
                  (if horizontal-text
                      (- x2 x1)
                      (- y2 y1))
                  (if horizontal-text
                      (- y2 y1)
                      (- x2 x1))
                  (lambda ()
                    (set! all-text-fitted #f)))
        text))
  
  (define ret
    (<gui> :draw-text gui color
           paint-text
           x1 y1 x2 y2
           #f
           align-top ;; align-top
           align-left ;; align-left
           rotate ;; rotate
           cut-text-to-fit ;; cut-text-to-fit
           scale-font-size
           ))

  (cond ((not all-text-fitted)
         #f)
        ((not ret)
         #f)
        (else
         #t))      
  )

(define *last-tooltip-and-statusbar-text* "")
(define (set-tooltip-and-statusbar text)
  (when (not (string=? text *last-tooltip-and-statusbar-text*))
    (set! *last-tooltip-and-statusbar-text* text)
    (<ra> :set-statusbar-text text)
    (<gui> :tool-tip text)))

#||
(define (<gui-helper> command . args)
  ;;(c-display "****" command args)
  (cond ((eq? command :group)
         (gui-create-layout ra:gui_group (list (car args)) (cdr args)))
        
        ((eq? command :vertical-layout)
         (gui-create-layout ra:gui_vertical-layout '() args))
        
        ((eq? command :horizontal-layout)
         (gui-create-layout ra:gui_horizontal-layout '() args))
        
        ((eq? command :flow-layout)
         (gui-create-layout ra:gui_flow-layout '() args))

        ((eq? command :empty)
         (<ra> :gui_vertical-layout))
        
        ((eq? command :table-layout)
         (my-gui_tablelayout args))

        ((eq? command :my-draw-text)
         (my-gui_draw-text args))

        ((eq? command :add-callback)
         (<ra> :gui_add-callback (car args) (cadr args)))
        
        ((eq? command :add-mouse-callback)
         (<ra> :gui_add-mouse-callback (car args) (cadr args)))
        
        ((eq? command :add-double-click-callback)
         (<ra> :gui_add-double-click-callback (car args) (cadr args)))
        
        ((eq? command :add-close-callback)
         (<ra> :gui_add-close-callback (car args) (cadr args)))
        
        ((eq? command :add-resize-callback)
         (<ra> :gui_add-resize-callback (car args) (cadr args)))
        
        ((eq? command :add-paint-callback)
         (<ra> :gui_add-paint-callback (car args) (cadr args)))
        
        ((eq? command :add-deleted-callback)
         (<ra> :gui_add-deleted-callback (car args) (cadr args)))
        
        ((eq? command :add-audio-meter-peak-callback)
         (<ra> :gui_add-audio-meter-peak-callback (car args) (cadr args)))
        
        ((eq? command :requester-operations)
         (ra:gui_requester-operations (car args) (cadr args)))
        
        (else
         (let* ((func (eval (<_> 'ra:gui_ (keyword->symbol command))))
                (last-arg (and (not (null? args)) (last args)))
                (gui (if (and last-arg (procedure? last-arg))
                         (apply func (butlast args))
                         (apply func args))))
           (when (and last-arg (procedure? last-arg))
             (<ra> :gui_add-callback gui last-arg))
           gui))))
||#

#||
        ((eq? command :add-callback)
         `(<ra> :gui_add-callback ,(car args) ,(cadr args)))
        
        ((eq? command :add-mouse-callback)
         `(<ra> :gui_add-mouse-callback ,(car args) ,(cadr args)))
        
        ((eq? command :add-double-click-callback)
         `(<ra> :gui_add-double-click-callback ,(car args) ,(cadr args)))
        
        ((eq? command :add-close-callback)
         `(<ra> :gui_add-close-callback ,(car args) ,(cadr args)))
        
        ((eq? command :add-resize-callback)
         `(<ra> :gui_add-resize-callback ,(car args) ,(cadr args)))
        
        ((eq? command :add-paint-callback)
         `(<ra> :gui_add-paint-callback ,(car args) ,(cadr args)))
        
        ((eq? command :add-deleted-callback)
         `(<ra> :gui_add-deleted-callback ,(car args) ,(cadr args)))
        
        ((eq? command :add-audio-meter-peak-callback)
         `(<ra> :gui_add-audio-meter-peak-callback ,(car args) ,(cadr args)))
||#


(c-define-expansion (*<gui>* command . args)
  (define (get-funcname) (<_> 'ra:gui_ (keyword->symbol command)))

  (cond ((eq? command :group)
         `(gui-create-layout ra:gui_group (list ,(car args)) ,@(cdr args)))
        
        ((eq? command :vertical-layout)
         `(gui-create-layout ra:gui_vertical-layout '() ,@args))
        
        ((eq? command :horizontal-layout)
         `(gui-create-layout ra:gui_horizontal-layout '() ,@args))

        ((eq? command :flow-layout)
         `(gui-create-layout ra:gui_flow-layout '() ,@args))

        ((eq? command :scroll-area)
         `(gui-create-layout ra:gui_scroll-area (list ,(car args) ,(cadr args)) ,@(cddr args)))

        ((eq? command :empty)
         `(<ra> :gui_vertical-layout))
        
        ((eq? command :table-layout)
         `(my-gui_tablelayout ,@args))

        ((eq? command :my-draw-text)
         `(my-gui_draw-text ,@args))

        ((eq? command :add-callback)
         `(<ra> :gui_add-callback ,(car args) ,(cadr args)))
        
        ((eq? command :requester-operations)
         `(ra:gui_requester-operations ,(car args) ,(cadr args)))
        
        ((eq? command :do-alpha)
         `(ra:gui_do-alpha ,@args))
        
        ((eq? command :do-font)
         `(ra:gui_do-font ,@args))
        
        ((eq? command :do-clipped)
         `(ra:gui_do-clipped ,@args))
        
        ((eq? command :create-block-drag-icon)
         `(ra:gui_create-block-drag-icon ,@args))
        
        ((eq? command :create-file-drag-icon)
         `(ra:gui_create-file-drag-icon ,@args))
        
        ((let ((stringcommand (symbol->string (keyword->symbol command))))
           (and (string-starts-with? stringcommand "add-")
                (string-ends-with? stringcommand "-callback")))
         `(,(get-funcname) ,@args))
        
        (else
         (define funcname (get-funcname))
         (define gui (gensym "gui"))
         (define last-arg (gensym "last-arg"))
         
         (define (last-arg-is-lambda)
           (let ((last (last args)))
             (and (pair? args)
                  (symbol? (car args))
                  (eq? 'lambda (car args)))))

         (cond ((null? args)
                `(,funcname))
               ((last-arg-is-lambda)
                `(let ((,gui (,funcname ,@(butlast args))))
                   (<ra> :gui_add-callback ,gui ,(last args))
                   ,gui))
               (else
                `(let ((,last-arg ,(last args)))
                   (if (procedure? ,last-arg)
                       (let ((,gui (,funcname ,@(butlast args))))
                         (<ra> :gui_add-callback ,gui ,last-arg)
                         ,gui)                        
                       (,funcname ,@(butlast args) ,last-arg))))))))


(delafina (<gui-number-input> :text text
                              :input-type 'float ;; float or int
                              :direction 'horizontal ;; horizontal or vertical
                              :min 0
                              :curr 0
                              :max 1
                              :num-decimals 2
                              :step-interval 0.1
                              :callback
                              )
          (define can-modify #f)
          
          (define layout (if (eq? direction 'horizontal)
                             (<gui> :horizontal-layout)
                             (<gui> :vertical-layout)))
          (define slider-callback (lambda (val)
                                    (when can-modify
                                      (set! can-modify #f)
                                      (<gui> :set-value text-input val)
                                      (set! can-modify #t)
                                      (callback val)
                                      )))
          (define slider (if (eq? direction 'horizontal)
                             (if (eq? input-type 'int)
                                 (<gui> :horizontal-int-slider text min curr max slider-callback)
                                 (<gui> :horizontal-slider text min curr max slider-callback))
                             (if (eq? input-type 'int)
                                 (<gui> :vertical-int-slider text min curr max slider-callback)
                                 (<gui> :vertical-slider text min curr max slider-callback))))
          (define text-input-callback (lambda (val)
                                        (when can-modify
                                          (set! can-modify #f)
                                          (<gui> :set-value slider val)
                                          (set! can-modify #t)
                                          (callback val)
                                          )))
          (define text-input (if (eq? input-type 'int)
                                 (<gui> :int-text min curr max text-input-callback)
                                 (<gui> :float-text min curr max num-decimals step-interval text-input-callback)))
          (<gui> :add layout slider)
          (<gui> :add layout text-input)

          (set! can-modify #t)
          
          (callback curr)
          
          layout)

;; returns actual background color
(delafina (paint-pan-slider :gui :x1 :y1 :x2 :y2
                            :value ;; -90 -> 90
                            :is-on
                            :background-color
                            :automation-slider-value #f
                            :automation-color #f
                            )
  (define background (if is-on
                         (<gui> :mix-colors background-color "black" 0.39)
                         (<gui> :mix-colors background-color "white" 0.95)))
  (<gui> :filled-box gui background x1 y1 x2 y2 5 5 #f)
  (define col1 (<gui> :mix-colors "white" background 0.4))
  (define col2 (<gui> :mix-colors "#010101" background 0.5))

  (define width (- x2 x1))
  
  (define inner-width/2 (scale 1 0 18 0 (get-fontheight)))
  (define outer-width/2 (* inner-width/2 2))
  
  (define middle (scale value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))
  
  (<gui> :filled-box gui col1 (+ x1 (- middle inner-width/2))               (+ y1 2) (+ x1 middle inner-width/2)               (- y2 3) -1 -1 #f)
  (<gui> :filled-box gui col2 (+ x1 (- middle inner-width/2 outer-width/2)) (+ y1 2) (+ x1 (- middle inner-width/2))           (- y2 3) -1 -1 #f)
  (<gui> :filled-box gui col2 (+ x1 (+ middle inner-width/2))               (+ y1 2) (+ x1 middle inner-width/2 outer-width/2) (- y2 3) -1 -1 #f)
  ;;(<gui> :draw-text gui "white" (<-> value "o") 0 0 width height #t)

  (when (and automation-slider-value
             (> automation-slider-value -100))
    (define middle (scale automation-slider-value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))
    (<gui> :draw-line gui automation-color (+ x1 middle) (+ y1 2) (+ x1 middle) (- y2 3) 2.0))
  
  (<gui> :draw-box gui "#404040" x1 y1 x2 y2 2)

  background
  )
      

;; returns true if all text was drawn
(delafina (paint-horizontal-slider :widget
                                   :value ;; between 0 and 1
                                   :text
                                   :x1 2
                                   :y1 2
                                   :x2 (- (<gui> :width widget) 2)
                                   :y2 (- (<gui> :height widget) 2)
                                   :color "gray"
                                   :is-enabled #t
                                   :is-current #f
                                   :get-automation-data #f
                                   :text-x1 (+ x1 2)
                                   :rounding 2.6
                                   :color2 "black"
                                   :text-color #f
                                   :border-color "gray"
                                   :border-width 0.8
                                   :cut-text-to-fit #t
                                   :wrap-lines #t
                                   )

  (if (not border-color)
      (set! border-color "gray"))
  
  (define pos (scale value 0 1 x1 x2))
  ;;(<gui> :filled-box widget (<gui> :get-background-color widget) x1 y1 x2 y2)
  (if color2
      (<gui> :filled-box widget color2 (1+ x1) (1+ y1) (1- x2) (1- y2) rounding rounding #f))
  (<gui> :filled-box widget color x1 y1 pos y2 rounding rounding #f)
  
  ;;(if (equal? (<ra> :get-current-instrument) instrument-id)
  ;;    (<gui> :filled-box widget "#aa111144" 1 1 (1- width) (1- height) 5 5))
  
  (define w 1.2)
  (define w2 (* 2 w))
  (define w3 (* 1.2 w))
  
  (if get-automation-data
      (get-automation-data
       (lambda (value color)
         (let* ((w (if is-current w3 1))
                (x (between 0 (scale value 0 1 (+ x1 w) (- x2 w)) x2)))
           (<gui> :draw-line
                  widget color
                  x (+ y1 w)
                  x (- y2 w)
                  2.0)))))
  
  
  ;;(if show-tooltip
  ;;    (set-tooltip-and-statusbar text))

  (if (not text-color)
      (set! text-color (if (not is-enabled)
                           (<gui> :mix-colors *text-color* "#ff000000" 0.5)
                           *text-color*)))
  
  (define ret (<gui> :my-draw-text widget text-color text
                     (floor (+ (/ (get-fontheight) 4) text-x1)) y1 (- x2 4) y2
                     wrap-lines ;; wrap-lines
                     #f ;; align top
                     #t
                     :cut-text-to-fit cut-text-to-fit
                     )) ;; align left

  ;; border
  (if (> border-width 0)
      (<gui> :do-clipped widget x1 y1 x2 y2
             (lambda ()
               (if is-current
                   (<gui> :draw-box widget border-color (+ x1 w) (+ y1 w) (- x2 w) (- y2 w) w3 rounding rounding) ;; "#aa111144"
                   (<gui> :draw-box widget border-color x1 y1 x2 y2 border-width rounding rounding)))))

  ret
  )
  

(delafina (paint-scrollbar :gui
                           :slider-pos1 ;; between 0 and slider-pos2
                           :slider-pos2 ;; between slider-pos1 and 1
                           :vertical
                           :x1 :y1 :x2 :y2
                           :background-color
                           :color
                           :border-color #f
                           :border 0.5
                           :border-rounding 0
                           :paint-border #t)
  (define b (min 2 (round border)))
  (define slider-length (- slider-pos2 slider-pos1))

  (if (not border-color)
      (set! border-color color))
  
  (if background-color
      (<gui> :filled-box gui background-color x1 y1 x2 y2 border-rounding border-rounding))
    
  (define sx1 (+ b x1))
  (define sy1 (+ b y1))
  (define sx2 (- x2 b))
  (define sy2 (- y2 b))
  (define sheight (- sy2 sy1))
  (define swidth (- sx2 sx1))
    
  (if vertical
      (begin
        (set! sy1 (scale slider-pos1 0 1 sy1 sy2))
        (set! sy2 (+ sy1 (scale slider-length 0 1 0 sheight))))
      (begin
        (set! sx1 (scale slider-pos1 0 1 sx1 sx2))
        (set! sx2 (+ sx1 (scale slider-length 0 1 0 swidth)))))
  
  (<gui> :filled-box gui
         color
         sx1 sy1 sx2 sy2 border-rounding border-rounding)
  
  (if paint-border
      (<gui> :do-clipped gui x1 y1 x2 y2
             (lambda ()
               (<gui> :draw-box gui border-color x1 y1 x2 y2 b (* 2 border-rounding) (* 2 border-rounding)))))
  
  #t)


(delafina (draw-button :gui :text :is-selected
                       :x1 :y1 :x2 :y2
                       :selected-color "check_box_selected_v2"
                       :unselected-color "#404040"
                       :background-color "button_v2" ;;(<gui> :get-background-color gui) ;; if #f, background will not be painted.
                       :is-hovering #f
                       :is-enabled #t
                       :prepend-checked-marker #f
                       :prepend-space-if-prepending-checked-marker #f
                       :vertical-text #f
                       :text-color "buttons_text" ;;*text-color* ;;"black"
                       :y-border 0
                       :x-border 0
                       :gradient-background #t
                       :paint-implicit-border #f ;; used by the mute buttons (when implicitly muted by someone else solo-ing)
                       :implicit-border-width 2
                       :box-rounding #f) ;; if #f, rounding will be set automatically based on checkbox size.

  (if (not box-rounding)
      (set! box-rounding (if (and #f (> (string-length text) 1))
                             5
                             3)))

  (when background-color
    (if (not is-selected)
        (cond ((string=? text "Record")
               (set! background-color (<gui> :mix-colors background-color "red" 0.9)))
              ((string=? text "Waiting for note...")
               (set! background-color (<gui> :mix-colors background-color "red" 0.6)))
              ((string=? text "Recording")
               (set! background-color (<gui> :mix-colors background-color "red" 0.1))))
        (cond ((string=? text "Mute")
               (set! selected-color (<gui> :mix-colors background-color "#44ff44" 0.55)))
              ((string=? text "Solo")
               (set! selected-color (<gui> :mix-colors background-color "yellow" 0.75)))
              ((string=? text "Bypass")
               (set! selected-color (<gui> :mix-colors background-color "zoomline_text1" 0.6))))))
  
  
  (define (paintit)
    (if (or is-selected
            background-color)
        (<gui> :filled-box
               gui
               (let ((color (if is-selected
                                selected-color
                                background-color)))
                 (if is-hovering
                     (<gui> :make-color-lighter color 1.25)
                     color))
               (+ x-border x1) (+ y-border y1) (- x2 x-border) (- y2 y-border)
               box-rounding box-rounding
               gradient-background))

    ;;(set! text "Gakk")

    (define text-len (<ra> :get-string-length text))

    (if (or (> text-len 0)
            (and is-selected
                 prepend-checked-marker))
        (<gui> :my-draw-text
               gui
               (if is-enabled
                   text-color ;;(if is-hovering "black" text-color)
                   (<gui> :set-alpha-for-color text-color 0.35))
               (cond ((= text-len 0)
                      "✔")
                     ((not prepend-checked-marker)
                      text)
                     (is-selected
                      (<-> "✔ " text))
                     (prepend-space-if-prepending-checked-marker
                      (<-> "     " text))
                     (else
                      text))
               ;;(+ x1 2) (+ y1 2) (- x2 2) (- y2 2)
               (+ x1 0) (+ y1 0) (- x2 0) (- y2 -1)
               #f
               #f
               #f
               (if vertical-text
                   90
                   0)
               #f
               #t
               )))

  (if (and #f is-hovering)
      (<gui> :do-alpha gui 0.5 paintit)
      (paintit))

  

  (when paint-implicit-border

    (define box-border (if paint-implicit-border
                           implicit-border-width
                           0))
    (<gui> :do-alpha gui (if paint-implicit-border
                             1.0
                             0.2)
           (lambda ()
             (<gui> :draw-box
                    gui
                    (if paint-implicit-border
                        selected-color
                        unselected-color
                        )
                    (+ x1 x-border box-border) (+ y1 y-border box-border) (- x2 (+ x-border box-border)) (- y2 (+ y-border box-border))
                    (if paint-implicit-border
                        2.0
                        1.0)
                    box-rounding box-rounding))))
  ;(<gui> :draw-box gui "black" x1 y1 x2 y2 1.1 3 3)
  ;(<gui> :draw-box gui "black" x1 y1 x2 y2 1.1 box-rounding box-rounding)
  
  '(let ((b 0.5))
     (<gui> :draw-box gui "#60000000" (+ x1 b) (+ y1 b) (- x2 b) (- y2 b) 1.2 box-rounding box-rounding))
  '(if (not gradient-background)
      (let ((b 0.5))
        (<gui> :draw-box gui "#222222" (+ x1 b) (+ y1 b) (- x2 b) (- y2 b) 1.2 box-rounding box-rounding)))
  '(let ((b 0.5))
    (<gui> :draw-box gui "black" (+ x1 b) (+ y1 b) (- x2 b) (- y2 b) 1.2 box-rounding box-rounding))
  '(let ((b 0.5))
    (<gui> :draw-box gui "#222222" (+ x1 b) (+ y1 b) (- x2 b) (- y2 b) 1.2 box-rounding box-rounding))

  )

(define (ra:gui_do-font gui font func)
  (define old-font (<gui> :get-font gui))
  (if (string=? font old-font)
      (func)
      (begin
        (<gui> :set-font gui font)
        (try-finally :try func
                     :finally (lambda ()
                                (<gui> :set-font gui old-font))))))

(define (ra:gui_do-alpha gui alpha func)
  (<gui> :set-paint-opacity gui alpha)
  (try-finally :try func
               :finally (lambda ()
                          (<gui> :set-paint-opacity gui 1))))

(define *curr-clip-rect* #f)

(define (ra:gui_do-clipped gui x1 y1 x2 y2 func)
  (define last-clip-rect *curr-clip-rect*)

  (when last-clip-rect
    (set! x1 (max (last-clip-rect 0) x1))
    (set! y1 (max (last-clip-rect 1) y1))
    (set! x2 (min (last-clip-rect 2) x2))
    (set! y2 (min (last-clip-rect 3) y2)))
  
  (set! *curr-clip-rect* (vector x1 y1 x2 y2))
    
  (try-finally :try (lambda ()
                      (<gui> :set-clip-rect gui x1 y1 x2 y2)
                      (func))
               :finally (lambda ()
                          (eat-errors :try (lambda ()
                                             (if last-clip-rect
                                                 (<gui> :set-clip-rect gui (last-clip-rect 0) (last-clip-rect 1) (last-clip-rect 2) (last-clip-rect 3))
                                                 (<gui> :cancel-clip-rect gui))))
                          (set! *curr-clip-rect* last-clip-rect))))
                           
  
(define (ra:gui_requester-operations text block)
  (c-display "OPEN REQ")
  (<ra> :open-requester text)
  (try-finally :try block
               :finally (lambda ()
                          (let ((safe-to-close (<ra> :safe-to-call-close-requester)))
                            (c-display "CLOSING REQ. Safe: " safe-to-close)
                            (if safe-to-close
                                (<ra> :close-requester))))))

(define (disable-gui-updates-block gui block)
  (if (not (<gui> :is-open gui))
      (let ((message (<-> "Error! disable-gui-updates-block: GUI " gui " is closed.")))
        (c-display "\n\n\n ==============   " message " ===============\n\n\n")
        (<ra> :show-message message)
        (c-display (safe-history-ow!))
        (assert #f))
      (begin
        (<gui> :disable-updates gui)
        (try-finally :try block
                     :finally (lambda ()
                                (<gui> :enable-updates gui)
                                )))))

(delafina (reopen-gui-at-curr-pos :gui
                                  :parentgui -1
                                  :parent-centre-gui #f)
  (disable-gui-updates-block
   gui
   (lambda ()       
     (let ((changed-parent (<gui> :set-parent gui parentgui)))
       (c-display "                  CHANGED-PARENT " changed-parent)
       (when (not (<gui> :is-visible gui))
         (<gui> :show gui)
         (<gui> :move-to-parent-centre gui))
       (if changed-parent
           (begin
             (if #f ;; definitely not do this. If we try to set two siblings modal at the same time, we lose mouse and keyboard access to the whole program after closing the second sibling. This HAS to be a Qt bug... Tried under Qt 5.9.0 in Linux (using FVWM). Could be Ticket #1 or #3 in QTBUG-27206. TODO: Try again after converting to MDI, it might be related to window manager.
                 (begin
                   (let ((mymodality (<gui> :is-modal gui))
                         (parentmodality (<gui> :is-modal parentgui)))
                     (c-display "  my-modality: " mymodality ", parentmodality:" parentmodality)
                     (if (not (eq? mymodality parentmodality))
                         (begin
                           (c-display "   Setting modal: " parentmodality)
                           (<gui> :set-modal gui parentmodality))))))))
       (<gui> :raise gui)))))

;;(curr-window (<gui> :get-parent-window -2)))
;           (if (not (= parent-window curr-window))
 ;              (<gui> :move-to-centre-of gui curr-window)))))))
           


(delafina (show-async-message :parentgui -2
                              :text ""
                              :buttons '("OK")
                              :is-modal #t
                              :callback #f)
  (if (pair? text)
      (set! text (<ra> :from-base64 (car text))))
  (define can-be-closed (null? buttons))
  (define buttonlayout (<gui> :horizontal-layout))
  (<gui> :add-layout-space buttonlayout 0 0 #t #f)
  
  (define textlayout (<gui> :horizontal-layout))
  (<gui> :add-layout-space textlayout 0 0 #t #f)
  (<gui> :add textlayout (<gui> :text text))
  (<gui> :add-layout-space textlayout 0 0 #t #f)
  
  (define gui (<gui> :vertical-layout textlayout buttonlayout))
  (<gui> :set-layout-spacing gui 8 8 8 8 8 )
  (for-each (lambda (button-text)
              (define button (<gui> :button button-text))
              (<gui> :add-callback button (lambda ()
                                            (set! can-be-closed #t)
                                            (<gui> :close gui)
                                            (if callback 
                                                (callback button-text))))
              (<gui> :add buttonlayout button))
            buttons)
  
  (if is-modal
      (<gui> :set-modal gui #t))
  
  (<gui> :add-close-callback gui (lambda (radium-runs-custom-exec)
                                   can-be-closed))

  (<gui> :set-parent gui parentgui)
  (<gui> :show gui)
  gui)

  
#!!
(show-async-message)
(show-async-message :buttons '())
(show-async-message :text "hello Gakk gakk\nHmm.2")
(show-async-message "hello2" :callback c-display)
(show-async-message "hello1" (list "BBBb1" "AAAb2") #f c-display)
(show-message "gakk")
!!#

(when (and (defined? '*message-gui*)
           (number? *message-gui*)
           (<gui> :is-open *message-gui*))
  (<gui> :set-static-toplevel-widget *message-gui* #f)
  (<gui> :close *message-gui*))


(define *message-gui* #f)
(define *message-gui-text-edit* #f)

;; Note! This function is called from the error handler.
(define (show-message-gui message)
  (<ra> :schedule 0 ;; In case we are called from a paint callback (or other qt event handlers) or error handler. Not only isn't the message displayed if we call directly, we could also end up in an infinite loop since this function is called from various error handlers.
        (lambda ()
          (when (or (not *message-gui*)
                    (not (<gui> :is-open *message-gui*)))
            (define buttonlayout (<gui> :horizontal-layout))
            (<gui> :set-layout-spacing buttonlayout 2 0 2 0 2)
            
            (<gui> :add-layout-space buttonlayout 0 0 #t #f)
            
            (define hide-button (<gui> :button "Hide"))
            (<gui> :add-callback hide-button (lambda ()                                       
                                               (<gui> :close *message-gui*)))
            (<gui> :add buttonlayout hide-button)

            (set! *message-gui-text-edit* (<gui> :text-edit "" #t))
            (define gui (<gui> :vertical-layout *message-gui-text-edit* buttonlayout))
            (<gui> :set-window-title gui "Message Window")
            (<gui> :set-layout-spacing gui 2 2 2 2 2)
            
            (<gui> :set-size gui
                   (floor (<gui> :text-width "Could not find..... Plugin file. asdf  wefawe3451345 13451345 oiwaefoajefoijaowepijaeporgijpoaghjto#$#$% 2q3e4tERTQERT paerjgoijaerpoiporegi"))
                   (floor (<gui> :text-width "Could not find..... Plugin file. asdf  wefawe3451345 13451345")))

            (<gui> :add-close-callback gui
                   (lambda (radium-runs-custom-exec)
                     ;;(set! *message-gui* #f)
                     (c-display "              GAKK GAKK GAKK")
                     (<gui> :hide *message-gui*)
                     #f)) ;; close it.
                   
            (set! *message-gui* gui))
          
          (reopen-gui-at-curr-pos :gui *message-gui*
                                  :parentgui -2)
          ;;
          ;; In the call above, -1 is the main window. We could use -2 (current window), but then the message gui automatically becomes modal if current window is modal, and we risk locking the program if the message window pops up again immediately after closing it. In addition, it's annoying having to click "hide" while doing something in a modal window. With that said, the alternative is not working so well either (although it"s much better this way since we avoid locking up the program). If the message window pops up while a modal window is active, the "hide" button doesn't work at all while the modal window is open (which is quite annoying since it seems like the program has locked up), plus that we get graphical flickering because Qt forcefully lowers the message gui at non-obvious times (we call "raise" on the message gui whenever it shows a new message), and then it is put on top of the currrent modal window. I don't know how to solve this problem. It doesn't seem like Qt has support for "modal group"s of several widgets, which would have been THE solution to this problem. (simply setting two sibling widgets modal locks up the whole program, see comment above in the 'reopen-gui-at-curr-pos' function.)
          ;;
          ;; Update: Realized (by accident) that it was just the call to "(<gui> :move-to-parent-centre gui)" every time that could lock up the computer.
          ;; By using -2 as parentgui, and only calling :move-to-parent-centre when the message gui is invisible, Qt actually does the right thing:
          ;;   Letting both the current modal window, plus the message window, react to mouse and keyboard. Need to test this on Windows and OSX too though.
          ;;
          
          (<gui> :append-value *message-gui-text-edit* message)
                 
          #f)))

;;#||
;; For debugging. The message gui can open at any time, and
;; this function opens the message window every 3 seconds.
;; Evaluate "(set! *is-running-debug-pulse* #f)" to stop it.
(define *is-running-debug-pulse* #f)
(define (maybe-start-debug-pulse)
  (when (not *is-running-debug-pulse*)
    (set! *is-running-debug-pulse* #t)
    (<ra> :schedule 1000
          (lambda ()
            (if *is-running-debug-pulse*
                (begin
                  (add-message-window-message "pulse ")
                  3000)
                #f)))))
#!!
(maybe-start-debug-pulse)
(set! *is-running-debug-pulse* #f)
!!#
;;||#

;; Called from ra:add-message
;; Note! This function is called from the error handler.
(define (add-message-window-message message)
  ;;(maybe-start-debug-pulse)
  (define html-message (string-append "<h4>" (<ra> :get-date-string) " " (<ra> :get-time-string) ":</h4>"
                                      "<blockquote>"
                                      (format #f "~A" message)
                                      "</blockquote>"))
  (show-message-gui (string-append "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd\">"
                                   "<html><head>" 
                                   "</head><body>"                                                 
                                   html-message
                                   "<br>"                 
                                   "</body></html>\n"
                                   )))

#!!
(add-message-window-message "hello")
(<ra> :add-message "hello2")
!!#

(define (safe-add-message-window-txt txt)
  (c-display "    SAFE_ADD_MESSAGE_WINDOW_TXT 1")
  (catch #t
         (lambda ()
           (c-display "    SAFE_ADD_MESSAGE_WINDOW_TXT 2")
           (add-message-window-message txt)) ;;(<ra> :get-html-from-text txt)))
         (lambda args
           (c-display "    SAFE_ADD_MESSAGE_WINDOW_TXT 3")
           ;; Don't want to call safe-ow! here since we might have been called from safe-ow!.
           (define txt (catch #t
                              ow!
                              (lambda args
                                (c-display "    SAFE_ADD_MESSAGE_WINDOW_TXT 3")
                                (get-as-displayable-string-as-possible (list "safe-add-message-window-message failed very hard: " args)))))
           (c-display "    SAFE_ADD_MESSAGE_WINDOW_TXT 4")
           (display txt))))

#||
(<ra> :add-message "hello1345weert446        werttqwertqert qqerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrt                           qerrrrrrrrrrrrrrrrrrrrrrrrt\nasdfasdf")
(<ra> :show-message "hello1345weert446        werttqwertqert qqerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrt                           qerrrrrrrrrrrrrrrrrrrrrrrrt\nasdfasdf")
||#

#!!
(let ((gui (<gui> :horizontal-layout)))  (<gui> :show gui)  (<gui> :move-to-parent-centre gui)  )

(+ a 9)

!!#


(define-constant *help-windows* (make-hash-table 10 string=?))

(define (FROM-C-show-help-window filename)
  (define web (or (*help-windows* filename)
                  (let ((web (<gui> :web filename)))
                    (set! (*help-windows* filename) web)
                    (<gui> :add-close-callback web (lambda x
                                                     (<gui> :hide web)
                                                     #f))
                    web)))
  (reopen-gui-at-curr-pos web))

#!!
(FROM-C-show-help-window "help/home.html")
!!#


(define (get-tab-coords is-horizontal i num-tabs x1 y1 x2 y2 kont)
  ;;(c-display "x1,x2:" x1 x2 ". y1/y2:" y1 y2 ". " is-horizontal)
  (if is-horizontal
      (kont (scale i 0 num-tabs x1 x2)
            y1
            (scale (1+ i) 0 num-tabs x1 x2)
            y2)
      (kont x1
            (scale i 0 num-tabs y1 y2)
            x2
            (scale (1+ i) 0 num-tabs y1 y2))))


(delafina (paint-tab-bar :gui :x1 :y1 :x2 :y2
                         :is-horizontal
                         :tab-names
                         :current-tab-num
                         :background-color #f)

    ;; vertical: The height we get from the paint callback is wrong
    ;; When resizing, the paint callback is callled with the old height,
    ;; and then, a few ms later, we get the correct height, causing jumpiness.
    ;; (this is caused by resizing the tab-bar from the tabs resize callback
    ;; to work around non-working QTabBar::setExpanded function.)
    ;;
    ;; horizontal: Same thing, but for width.

  (if (not background-color)      
      (set! background-color (<gui> :mix-colors (<gui> :get-background-color gui) "tab_unselected" 0.65)))

  ;;(<gui> :get-background-color gui)))

  (define curr-tab-background (<gui> :mix-colors "tab_selected" background-color 0.47))

  (define num-tabs (length tab-names))

  ;;(c-display "background-color/etc:" gui background-color x1 y1 x2 y2)
  (<gui> :filled-box gui background-color x1 y1 x2 y2)

  (for-each (lambda (tab-name i)
              (get-tab-coords is-horizontal i num-tabs x1 y1 x2 y2
                              (lambda (x1 y1 x2 y2)
                                ;;(c-display i "y1/y2:" (floor y1) (floor y2) "x1/x2" (floor x1) (floor x2))
                                (if (= i current-tab-num)
                                    (<gui> :filled-box gui curr-tab-background x1 y1 x2 y2 5 5))
                                ;;(<gui> :draw-box gui "#202020" x1 y1 x2 y2 1.0 2 2)
                                (<gui> :my-draw-text gui *text-color* tab-name x1 y1 x2 y2 #t #f #f (if is-horizontal 0 270))
                                (<gui> :draw-box gui "black" x1 y1 x2 y2 0.5)
                                )))
            tab-names
            (iota num-tabs))

  ;;(<gui> :draw-box gui "black" x1 y1 x2 y2 0.5)
  )
  

;; ra:gui_tabs with simpler gfx. (no borders, etc.)
;;
(delafina (my-tabs :horizontal
                   :width 5
                   :height 5)
  (define tabs (<gui> :tabs (if horizontal 0 2)))

  ;;(<gui> :set-style-sheet tabs "QTabWidget::pane { border: 0; }")
  
  (define tab-bar (<gui> :get-tab-bar tabs))

  (define background-color (<gui> :get-background-color tabs))

  (define (get-index-from-x-y x y)
    (define num-tabs (<gui> :num-tabs tabs))
    (between 0
             (if horizontal
                 (floor (scale x 0 (<gui> :width tab-bar) 0 num-tabs))
                 (floor (scale y 0 (<gui> :height tab-bar) 0 num-tabs)))
             (1- num-tabs)))

  (define (my-paint-tab-bar gui)
    (define width (if horizontal
                      (<gui> :width tabs)
                      (<gui> :width tab-bar)))
    (define height (if horizontal
                       (<gui> :height tab-bar)
                       (<gui> :height tabs)))
    (paint-tab-bar gui 0 0 width height horizontal
                   (map (lambda (tabnum) (<gui> :tab-name tabs tabnum))
                        (iota (<gui> :num-tabs tabs)))
                   (<gui> :current-tab tabs)))
;;                   background-color))

  (<gui> :add-paint-callback tab-bar
         (lambda (width height)           
           (my-paint-tab-bar tab-bar)))

  (<gui> :add-mouse-callback tab-bar
         (lambda (button state x y)
           (if (and (= state *is-pressing*)
                    (= button *left-button*))
               (begin
                 (<gui> :set-current-tab tabs (get-index-from-x-y x y))
                 #t)
               #f)))

  ;; Prevent Qt from painting background. We don't want border.
  (<gui> :add-paint-callback tabs
         (lambda (width height)
           ;;(c-display "paint" width height)

           (define tab-bar-width (<gui> :width tab-bar))
           (define tab-bar-height (<gui> :height tab-bar))
           
           (define tabs-background-color (<gui> :mix-colors "high_background" "low_background" 0.8))

           ;;(<gui> :filled-box tabs tabs-background-color 0 0 width height)
           
           ;; To avoid flicker when tab-bar height is less than tabs height.
           (if horizontal
               (begin
                 (<gui> :filled-box tabs tabs-background-color
                        0 tab-bar-height
                        width height)
                 (when (< tab-bar-width
                          width)
                   (my-paint-tab-bar tabs)))
               (begin
                 (<gui> :filled-box tabs tabs-background-color
                        tab-bar-width 0
                        width height)
                 (when (< tab-bar-height
                          height)
                   (my-paint-tab-bar tabs))))))

  (define (resize-tabs tabs horizontal width height)
    (define tab-bar (<gui> :get-tab-bar tabs))
    (if horizontal
        (set-fixed-width tab-bar width)
        (set-fixed-height tab-bar height)))
  
  (define (resize-callback width height)
    ;;(c-display "resize-callback" width height)
    (resize-tabs tabs horizontal width height)
    )
  
  (<gui> :add-resize-callback tabs resize-callback)
               
  (<gui> :set-size tabs width height)
  
  tabs
  )


#!!

(let ((tabs (my-tabs #f)))
  (<gui> :show tabs)
  (<gui> :set-size tabs 200 100)
  (<gui> :add-tab tabs "Quantitize 1" (create-quantitize-gui-for-tab))
  (<gui> :add-tab tabs "Sequencer" (create-transpose-notem))
  (<gui> :add-tab tabs "Instrument" (create-transpose-notem))
  (<gui> :add-tab tabs "Edit" (create-quantitize-gui-for-tab))
  )
!!#



;; Proper rubberband. (QRubberBand doesn't work)
;; The function returns a function that must be called to update position.
;; Note that the lines are drawn at integer position. floating points are ignored.
;; It also draws in between the rectangle, not on the rectangle, which :draw-box does.
;;
;; Warning: Strange thing(s) seems to happen if parent has a layout.
;; (extra gap at the bottom with size of layout()->spacing()*n, where n seems to be around 4.)
;;
(define (gui-rubberband parent w color is-enabled-func)
  (define top (<gui> :widget w w))
  (define right (<gui> :widget w w))
  (define bottom (<gui> :widget w w))
  (define left (<gui> :widget w w))

  (for-each (lambda (part)
              (<gui> :add parent part)
              (<gui> :show part)
              
              (<gui> :add-callback part
                     part
                     (lambda (width height)
                       (if (is-enabled-func)
                           (<gui> :filled-box part color 0 0 width height)))))
            (list top right bottom left))

  (lambda (x1 y1 x2 y2)
    (set! x1 (floor x1))
    (set! y1 (floor y1))
    (set! x2 (floor x2))
    (set! y2 (floor y2))
    
    (define width (- x2 x1))
    (define height (- y2 y1))
    
    (<gui> :set-pos top x1 y1)
    (<gui> :set-size top width w)
    
    (<gui> :set-pos right (- x2 w) y1)
    (<gui> :set-size right w height)

    (<gui> :set-pos bottom x1 (- y2 w))
    (<gui> :set-size bottom width w)

    (<gui> :set-pos left x1 y1)
    (<gui> :set-size left w height)))



;; Font requesters
;;;;;;;;;;;;;;;;;;;;;;

(define (create-font-requester org-font set-font-func)
  (let ((fontreq (<gui> :font-requester "")))
    
    (<gui> :add-callback fontreq
           (lambda (fontstring-or-buttons)
             (c-display "fontstring:" fontstring-or-buttons ". gui:" fontreq)
             (if (boolean? fontstring-or-buttons)
                 (if (not fontstring-or-buttons)
                     (set-font-func org-font))
                 (set-font-func fontstring-or-buttons))))

    (<gui> :set-parent fontreq -2)
    
    (<gui> :show fontreq)
    fontreq))

(define (create-change-system-font-requester)
  (create-font-requester (<ra> :get-system-font)
                         ra:set-system-font))

(define (create-change-editor-font-requester)
  (create-font-requester (<ra> :get-editor-font)
                         ra:set-editor-font))


#!!
(create-change-system-font-requester)
(create-change-editor-font-requester)
!!#



;; File requester
;;;;;;;;;;;;;;;;;

;; Includes some qt problem workarounds.
(define (create-file-requester header-text dir filetype-name postfixes for-loading is-modal parent callback)
  (let ((gui (<gui> :file-requester header-text dir filetype-name postfixes for-loading
                    (lambda (filename)
                      (<gui> :update parent) ;; (not sure this makes any difference)
                      (<ra> :schedule 50 ;; Give some time to update graphics after closing the file requester (not always enough)
                            (lambda ()
                              (callback filename)
                              #f))))))
    (if is-modal
        (<gui> :set-modal gui #t))
    
    (<gui> :set-parent gui parent)
    (<gui> :show gui)))


;; Table
;;;;;;;;;;;;;;;;;;;;;;

(define-struct table-row
  :header-name
  :initial-width #f ;; Can contain a string or a number.
  :stretch #f)
  
(delafina (create-table-gui :table-rows
                            :selected-row-callback #f
                            :hide-callback #f
                            :accept-key-callback?-callback #f
                            :curr-selected-row-changed-callback #f)

  (c-display "table-rows:" table-rows)

  (define table #f)

  (define doit #f)
  (set! table (<gui> :table (map (lambda (conf)
                                   (conf :header-name))
                                 table-rows)
                     (lambda ()
                       (if (and doit curr-selected-row-changed-callback)
                           (curr-selected-row-changed-callback table
                                                               (<gui> :curr-table-row table)
                                                               (<gui> :get-value table)))
                       (c-display "TABLECALLBACK:"))))
  (set! doit #t)

  (for-each (lambda (header-num conf)
              (let ((width (cond ((string? (conf :initial-width))
                                  (floor (* 1.5 (<gui> :text-width (conf :initial-width)))))
                                 ((number? (conf :initial-width))
                                  (floor (conf :initial-width)))
                                 (else
                                  #f))))
                (if width
                    (<gui> :stretch-table table header-num (conf :stretch) width))))
            (iota (length table-rows))
            table-rows)

  (define (maybe-call-selected-row-callback)
    (if selected-row-callback
        (selected-row-callback table
                               (<gui> :curr-table-row table)
                               (<gui> :get-value table))))

  (<gui> :add-key-callback table
         (lambda (presstype key)
           ;;(c-display "GOT KEY" presstype key (string=? key "\n"))
           (cond ((and accept-key-callback?-callback
                       (not (accept-key-callback?-callback table presstype key)))
                  ;;(c-display "not accepted")
                  #f)
                 ;;((= 1 presstype) ;; Qt eats a lot of key down events, so we can't ignore key up. TODO: Let :add-key-callback sniff native keyboard events from Qt_Main.cpp instead.
                 ;; #f)
                 ((string=? key "HOME")
                  (<gui> :set-value table 0)
                  #t)
                 ((string=? key "END")
                  (<gui> :set-value table (1- (<gui> :get-num-table-rows table)))
                  #t)
                 ((string=? key "\n")
                  (maybe-call-selected-row-callback)
                  #t)
                 ((string=? key "ESC")
                  (if hide-callback
                      (hide-callback table))
                  #t)
                 (else
                  #f))))

  (<gui> :add-double-click-callback table
         (lambda (x y)
           (maybe-call-selected-row-callback)
           #f))

  table)

#!!
(define table (create-table-gui (list (make-table-row "number" #f #f)
                                      (make-table-row "row1" "asdfasdfasdf" #t)
                                      (make-table-row "row2" 300))
                                :selected-row-callback (lambda (table row-num row-content)
                                                         (c-display "row num" row-num "selected. Content:" row-content))
                                :hide-callback (lambda (table)
                                                 (<gui> :close table))))
(<gui> :add-table-rows table 0 20)
(<gui> :show table)

!!#


;; Menu entries (note: must call GFX_clear_menu_cache() first to update already displayed menu entries.)

(define *last-pressed-menu-entry-widget-mouse-button* 0)

(define (draw-keybinding gui x1 y1 x2 y2 keybinding)
  (define nonhover-background-color (<gui> :mix-colors "low_background" "#ffffff" 0.97))
  
  (define is-unassigned (string=? keybinding "unassigned"))

  (<gui> :my-draw-text gui
         (let ((color (if is-unassigned
                          (<gui> :mix-colors
                                 nonhover-background-color 
                                 (<gui> :mix-colors "menu_text" "menu_keybinding_text" 0.2)
                                 0.85)
                          "menu_keybinding_text")))
           (if (<gui> :is-enabled gui)
               color
               (<gui> :mix-colors color nonhover-background-color 0.5)))
         keybinding
         x1 y1 x2 y2
         #f ;; wrap lines
         #f ;; align top
         #t ;; align left
         0 ;; rotate
         ))


  
(define (FROM_C-create-menu-entry-widget entry-id name shortcut shortcut-width is-checkbox is-checked is-radiobutton is-first is-last)
  (if is-radiobutton
      (assert is-checkbox))
  
  (define fontheight (get-fontheight))
  
  (define b (max 1 (round (/ fontheight 20))))

  (define height (+ fontheight (* 2 b)))

  (define before-and-after-width (* 1.1 fontheight)) ;;(<gui> :get-system-fontheight))) ;;(<gui> :text-width "---"))
  (define name-width (<gui> :text-width name))
  
  ;;(define shortcut-width (max (ceiling (* 1.5 (<gui> :text-width "Right Ctrl + P")))
  ;;                           (<gui> :text-width shortcut)))
  
  (define between-width (* 1.5 (<gui> :text-width " - ")))
  
  (define width (round (+ b
                          before-and-after-width
                          (max (+ name-width
                                  between-width
                                  shortcut-width)
                               0) ;;(* (/ 300 19.0) fontheight))
                          before-and-after-width
                          b)))

  (define widget (<gui> :widget width height))

  (<gui> :set-min-width widget width)
  (<gui> :set-min-height widget height)

  (<gui> :dont-autofill-background widget)
  
  (when is-checkbox
    (define checkbox (if is-radiobutton
                         (<gui> :radiobutton "" is-checked)
                         (<gui> :checkbox "" is-checked)))
    
    (<gui> :set-name checkbox "checkbox") ;; The checkbox is picked up in "MyQAction".
    
    (define checkbox-x1 (+ b
                           (max 0
                                (round (/ (- before-and-after-width
                                             (<gui> :width checkbox))
                                          2)))))
    (define checkbox-y1 (max 0
                             (round (/ (- height
                                          (<gui> :height checkbox))
                                       2))))
    
    ;;(c-display "x1/y1:" checkbox-x1 checkbox-y1 (<gui> :width checkbox) before-and-after-width)
    (<gui> :add widget checkbox checkbox-x1 checkbox-y1))
    
  (define nonhover-background-color (<gui> :mix-colors "low_background" "#ffffff" 0.97))
  (define hover-background-color "high_background") ;;(<gui> :mix-colors (<gui> :get-background-color widget) "#010101" 0.5))
  (define border-color (<gui> :mix-colors hover-background-color "#010101" 0.5))
  
  (<gui> :add-paint-callback widget
         (lambda (width height)
           ;;(c-display "  paint menu entry. Disabled: " name (<gui> :is-enabled widget) (<gui> :get-background-color widget) ". gui:" widget ". :last-hovered" (<ra> :get-last-hovered-popup-menu-entry))
           
           (let ((background-color (if (and (= widget (<ra> :get-last-hovered-popup-menu-entry))
                                            (<gui> :is-enabled widget))
                                       hover-background-color
                                       nonhover-background-color)))
             (<gui> :filled-box widget background-color 0 0 width height -1 -1 #f))
           
           (if is-first
               (<gui> :draw-line widget
                      border-color
                      0 (/ b 2.0) width (/ b 2.0)
                      b))

           (if is-last
               (<gui> :draw-line widget
                      border-color
                      0 (- height (/ b 2.0)) width (- height (/ b 2.0))
                      b))
               
           (<gui> :draw-line widget
                  border-color
                  (/ b 2.0) 0 (/ b 2.0) height
                  b)
           (<gui> :draw-line widget
                  border-color
                  (- width (/ b 2.0)) 0 (- width (/ b 2.0)) height
                  b)
           
           (<gui> :my-draw-text widget
                  (if (<gui> :is-enabled widget)
                      "menu_text"
                      (<gui> :mix-colors "menu_text" nonhover-background-color 0.5))
                  name
                  (+ b before-and-after-width) b (- width b) (- height b)
                  #f ;; wrap lines
                  #f ;; align top
                  #t ;; align left
                  0 ;; rotate
                  )

           (when (not (string=? "" shortcut))
             (define shortcut-x1 (- width (+ shortcut-width before-and-after-width b)))
             (draw-keybinding widget shortcut-x1 b (- width b) (- height b) shortcut)
             )))

  (<gui> :add-mouse-callback widget
         (lambda (button state x y)
           (if (> button 0)
               (set! *last-pressed-menu-entry-widget-mouse-button* button))
           (when (and (= state *is-entering*)
                      );(<gui> :is-enabled widget))
             ;;(c-display "Entering" name)
             (<ra> :hover-popup-menu-entry entry-id))
           #f))
  
  widget)

#!!!
(popup-menu "Hello!"
            (lambda ()
              (c-display "clicked")))
(popup-menu "Hello!"
            (lambda ()
              (c-display "clicked"))
            "hello2"
            (lambda ()
              (c-display "clicked2")))
!#


(when (not (<ra> :release-mode))
  (let ((html ""))
    (***assert*** html
                  (<ra> :get-html-from-text (<ra> :get-text-from-html html))))
  (let ((html "<br>"))
    (***assert*** html
                  (<ra> :get-html-from-text (<ra> :get-text-from-html html))))
  (let ((html "<br><br>"))
    (***assert*** html
                  (<ra> :get-html-from-text (<ra> :get-text-from-html html))))
  (let ((html "<br>hello<br><br>hello2"))
    (***assert*** html
                  (<ra> :get-html-from-text (<ra> :get-text-from-html html))))
  (let ((text ""))
    (***assert*** text
                  (<ra> :get-text-from-html (<ra> :get-html-from-text text))))
  (let ((text "\n"))
    (***assert*** text
                  (<ra> :get-text-from-html (<ra> :get-html-from-text text))))
  (let ((text "\n\n"))
    (***assert*** text
                  (<ra> :get-text-from-html (<ra> :get-html-from-text text))))
  (let ((text "\nhello"))
    (***assert*** text
                  (<ra> :get-text-from-html (<ra> :get-html-from-text text))))
  (let ((text "\nhello\n"))
    (***assert*** text
                  (<ra> :get-text-from-html (<ra> :get-html-from-text text))))
  
  )

#!!
(<ra> :get-html-from-text "\n ")
!!#




(define* (has-range (blocknum -1))
  (or (<ra> :has-range blocknum)
      (<ra> :has-selected-notes -1 blocknum)))

#!!
(has-range)
!!#

(define (show-missing-range-message)
  (show-async-message :text
                      (<-> "No range in block, and no notes in the pianroll selected."
                           "<br>"
                           "<UL>"
                           "<LI>To select range:<OL>"
                           "<br>"
                           "  <LI>Move cursor to range should start and press Left Meta + B."
                           "  <LI>Move cursor to range should end and press Left Meta + B, again."
                           "  </OL>"
                           "<br>"
                           "<LI>To select pianoroll notes:<UL>"
                           "  <LI>Ctrl-click notes (in the pianoroll)."
                           "  <LI>Ctrl-drag to create selection-rectangle (in the pianoroll)."
                           "  </UL>"
                           "</UL>"
                           )))


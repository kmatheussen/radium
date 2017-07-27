(provide 'gui.scm)

(define-constant *min-db* (<ra> :get-min-db))
(define-constant *max-db* (<ra> :get-max-db))
(define-constant *max-mixer-db* 6)


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
  (<gui> :add-layout-space layout 10 10 #f #t)
  (for-each (lambda (gui)
              (<gui> :add layout gui))
            guis)
  (<gui> :add-layout-space layout 10 10 #f #t)
  layout)
  
(define (mid-horizontal-layout . guis)
  (define layout (<gui> :horizontal-layout))
  (<gui> :add-layout-space layout 10 10 #t #f)
  (for-each (lambda (gui)
              (<gui> :add layout gui))
            guis)
  (<gui> :add-layout-space layout 10 10 #t #f)
  layout)

(define (gui-create-layout create-layout-func layout-args . guis)
  (define layout (apply create-layout-func layout-args))
  (for-each (lambda (gui)
              (c-display "Adding" gui "to layout" layout)
              (<ra> :gui_add layout gui))
            (flatten guis))
  layout)
  
(define (my-gui_group title args)
  (define group (<ra> :gui_group title))
  (for-each (lambda (gui)
              (<ra> :gui_add group gui))
            args)
  group)

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


(define-expansion (<gui> command . args)
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

        ((eq? command :add-callback)
         `(<ra> :gui_add-callback ,(car args) ,(cadr args)))
        
        ((eq? command :requester-operations)
         `(ra:gui_requester-operations ,(car args) ,(cadr args)))
        
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


(define (ra:gui_requester-operations text block)
  (c-display "OPEN REQ")
  (<ra> :open-requester text)
  (try-finally :try block
               :finally (lambda ()
                          (c-display "CLOSE REQ")
                          (<ra> :close-requester))))

(define (disable-gui-updates-block gui block)
  (<gui> :disable-updates gui)
  (try-finally :try block
               :finally (lambda ()
                          (<gui> :enable-updates gui))))

(delafina (reopen-gui-at-curr-pos :gui
                                  :parentgui -1
                                  :parent-centre-gui #f)
  (disable-gui-updates-block
   gui
   (lambda ()       
     (let ((parent-window (if (< parentgui 0)
                              (<gui> :get-parent-window parentgui)
                              parentgui))
           (changed-parent (<gui> :set-parent gui parentgui)))
       (c-display "                  CHANGED-PARENT " changed-parent)
       (if (not (<gui> :is-visible gui))
           (<gui> :show gui))
       (if changed-parent
           (<gui> :move-to-parent-centre gui)
           (<gui> :raise gui))))))
;;(curr-window (<gui> :get-parent-window -2)))
;           (if (not (= parent-window curr-window))
 ;              (<gui> :move-to-centre-of gui curr-window)))))))
           


(delafina (show-async-message :parentgui -2
                              :text ""
                              :buttons '("OK")
                              :is-modal #t
                              :callback #f)
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

(define (show-message-gui message)
  (<ra> :schedule 0 ;; In case we are called from a paint callback. Not only isn't the message displayed if we call directly, we also end up in an infinite loop since this function is called from various error handlers.
        (lambda ()
          (when (or (not *message-gui*)
                    (not (<gui> :is-open *message-gui*)))
            (define buttonlayout (<gui> :horizontal-layout))
            (<gui> :set-layout-spacing buttonlayout 2 0 2 0 2)
            
            (<gui> :add-layout-space buttonlayout 0 0 #t #f)
            
            (define hide-button (<gui> :button "Hide"))
            (<gui> :add-callback hide-button (lambda ()                                       
                                               (<gui> :hide *message-gui*)))
            (<gui> :add buttonlayout hide-button)

            (set! *message-gui-text-edit* (<gui> :text-edit "" #t))
            (define gui2 (<gui> :vertical-layout *message-gui-text-edit* buttonlayout))
            (<gui> :set-layout-spacing gui2 2 2 2 2 2)
            
            (<gui> :set-size gui2
                   (floor (<gui> :text-width "Could not find..... Plugin file. asdf  wefawe3451345 13451345 oiwaefoajefoijaowepijaeporgijpoaghjto#$#$% 2q3e4tERTQERT paerjgoijaerpoiporegi"))
                   (floor (<gui> :text-width "Could not find..... Plugin file. asdf  wefawe3451345 13451345")))

            ;; definitely not.
            ;;(<gui> :set-static-toplevel-widget gui2 #t)
            
            ;; Just hide window when closing it.
            (<gui> :add-close-callback gui2
                   (lambda (radium-runs-custom-exec)
                     ;;(<gui> :set-parent *message-gui* -3)
                     (c-display "              GAKK GAKK GAKK")
                     (<gui> :hide *message-gui*)
                     #f))
            
            (set! *message-gui* gui2))
          
          ;;(c-display gui2)
          
          (reopen-gui-at-curr-pos *message-gui*)

          (<gui> :set-value *message-gui-text-edit* message)
                 
          #f)))

#||
;; for debugging. The message gui can open at any time.
(define *has-started-it* #f)
(define (maybe-start-it)
  (when (not *has-started-it*)
    (set! *has-started-it* #t)
    (<ra> :schedule 1000
          (lambda ()
            (add-message-window-message "pulse")
            3000))))
||#

(define *g-complete-message* #f)
(define (add-message-window-message message)
  ;;(maybe-start-it)
  (set! *g-complete-message* (<-> (if (not (string? *g-complete-message*))
                                      ""
                                      (<-> *g-complete-message* "<p><br>\n"))
                                  "<h4>" (<ra> :get-date-string) " " (<ra> :get-time-string) ":</h4>"
                                  "<blockquote>" message "</blockquote>"))
  (show-message-gui (<-> "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd\">"
                         "<html><head>" 
                         "</head><body>"                                                 
                         *g-complete-message*                                                 
                         "<br>"                                                 
                         "</body></html>\n")))

#!!
(add-message-window-message "hello")
(<ra> :add-message "hello2")
!!#

(define (safe-add-message-window-txt txt)
  (catch #t
         (lambda ()
           (add-message-window-message (<ra> :get-html-from-text txt)))
         (lambda args
           ;; Don't want to call safe-ow! here since we might have been called from safe-ow!.
           (define txt (catch #t
                              ow!
                              (lambda args
                                (get-as-displayable-string-as-possible (list "safe-add-message-window-message failed very hard: " args)))))           
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
  
;; ra:gui_tabs with simpler gfx. (no borders, etc.)
;;
(delafina (my-tabs :horizontal
                   :width 5
                   :height 5)
  (define tabs (<gui> :tabs (if horizontal 0 2)))

  ;;(<gui> :set-style-sheet tabs "QTabWidget::pane { border: 0; }")
  
  (define tab-bar (<gui> :get-tab-bar tabs))

  (define background-color (<gui> :get-background-color tabs))
  (define curr-tab-background (<gui> :mix-colors "green" background-color 0.47))

  
  (define (get-index-from-x-y x y)
    (define num-tabs (<gui> :num-tabs tabs))
    (between 0
             (if horizontal
                 (floor (scale x 0 (<gui> :width tab-bar) 0 num-tabs))
                 (floor (scale y 0 (<gui> :height tab-bar) 0 num-tabs)))
             (1- num-tabs)))

  (define (get-tab-coords i num-tabs width height kont)
    (if horizontal
        (kont (scale i 0 num-tabs 0 width)
              0
              (scale (1+ i) 0 num-tabs 0 width)
              height)
        (kont 0
              (scale i 0 num-tabs 0 height)
              width
              (scale (1+ i) 0 num-tabs 0 height))))
              
  (<gui> :add-paint-callback tab-bar
         (lambda (width height)
           (define num-tabs (<gui> :num-tabs tabs))
           (<gui> :filled-box tab-bar background-color 0 0 width height)
           (for-each (lambda (i)
                       (get-tab-coords i num-tabs width height
                                       (lambda (x1 y1 x2 y2)
                                         ;;(c-display i (floor y1) (floor y2) "x1/x2" (floor x1) (floor x2) width)
                                         (if (= i (<gui> :current-tab tabs))
                                             (<gui> :filled-box tab-bar curr-tab-background x1 y1 x2 y2))
                                         ;;(<gui> :draw-box tab-bar "#202020" x1 y1 x2 y2 1.0 2 2)
                                         (<gui> :draw-text tab-bar *text-color* (<gui> :tab-name tabs i) x1 y1 x2 y2 #t #f #f (if horizontal 0 270)))))
                     (iota num-tabs))))
  
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
           (define background-color (<gui> :mix-colors "color11" "color9" 0.8))
           (<gui> :filled-box tabs background-color 0 0 width height)
           #t))

  (define (resize-tabs tabs horizontal width height)
    (define tab-bar (<gui> :get-tab-bar tabs))
    (if horizontal
        (set-fixed-width tab-bar width)
        (set-fixed-height tab-bar height)))
  
  (define (resize-callback width height)
    (resize-tabs tabs horizontal width height))
  
  (<gui> :add-resize-callback tabs resize-callback)
               
  (<gui> :set-size tabs width height)
  
  tabs
  )


#!!

(let ((tabs (my-tabs #f)))
  (<gui> :show tabs)
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
              
              (add-safe-paint-callback
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

    
                                              

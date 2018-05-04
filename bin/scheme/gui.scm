(provide 'gui.scm)

(define-constant *min-db* (<ra> :get-min-db))
(define-constant *max-db* (<ra> :get-max-db))
(define-constant *max-mixer-db* 6)
(define-constant *max-volume-envelope-db* (<ra> :get-seqblock-envelope-max-db))



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
  (add-safe-paint-callback button
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
                             (<gui> :draw-text
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

(define (set-tooltip-and-statusbar text)
  (<ra> :set-statusbar-text text)
  (<gui> :tool-tip text))

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
                          (let ((safe-to-close (<ra> :safe-to-call-close-requester)))
                            (c-display "CLOSING REQ. Safe: " safe-to-close)
                            (if safe-to-close
                                (<ra> :close-requester))))))

(define (disable-gui-updates-block gui block)
  (<gui> :disable-updates gui)
  (try-finally :try block
               :finally (lambda ()
                          (<gui> :enable-updates gui)
                          )))

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
          (when (not *message-gui*)
            (define buttonlayout (<gui> :horizontal-layout))
            (<gui> :set-layout-spacing buttonlayout 2 0 2 0 2)
            
            (<gui> :add-layout-space buttonlayout 0 0 #t #f)
            
            (define hide-button (<gui> :button "Hide"))
            (<gui> :add-callback hide-button (lambda ()                                       
                                               (<gui> :close *message-gui*)))
            (<gui> :add buttonlayout hide-button)

            (set! *message-gui-text-edit* (<gui> :text-edit "" #t))
            (define gui (<gui> :vertical-layout *message-gui-text-edit* buttonlayout))
            (<gui> :set-window-title *pluginmanager-gui* "Message Window")
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
(define (add-message-window-message message)
  ;;(maybe-start-debug-pulse)
  (define html-message (<-> "<h4>" (<ra> :get-date-string) " " (<ra> :get-time-string) ":</h4>"
                            "<blockquote>" message "</blockquote>"))
  (show-message-gui (<-> "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd\">"
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

              





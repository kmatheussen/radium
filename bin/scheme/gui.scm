(provide 'gui.scm)

(define *min-db* (<ra> :get-min-db))
(define *max-db* (<ra> :get-max-db))
(define *max-mixer-db* 6)

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


(define (disable-gui-updates-block gui block)
  (<gui> :disable-updates gui)
  (let ((ret (catch #t
                    block
                    (lambda args ;; Catch exceptions to ensure (<ra> :enable-updates gui) will be called
                      (display "args")(display args)(newline)
                      (apply format #t (cadr args))
                      (display (ow!))))))
    (<gui> :enable-updates gui)
    ret))

(define (reopen-gui-at-curr-pos gui)
  (disable-gui-updates-block
   gui
   (lambda ()
     (let ((changed-parent (<gui> :set-parent gui -2)))
       (c-display "                  CHANGED-PARENT " changed-parent)
       (if (not (<gui> :is-visible gui))
           (<gui> :show gui))
       (if changed-parent
           (<gui> :move-to-parent-centre gui))))))


(delafina (ra:show-async-message :parentgui -2
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
(<ra> :show-async-message)
(<ra> :show-async-message :buttons '())
(<ra> :show-async-message "hello2")
(<ra> :show-async-message "hello2" :callback c-display)
(<ra> :show-async-message "hello1" (list "BBBb1" "AAAb2") #f c-display)
(<ra> :show-message "gakk")
!!#

(when (and (defined? '*message-gui*)
           (number? *message-gui*)
           (<gui> :is-open *message-gui*))
  (<gui> :set-static-toplevel-widget *message-gui* #f)
  (<gui> :close *message-gui*))


(define *message-gui* #f)
(define *message-gui-text-edit* (<gui> :text-edit "" #t))

(define (show-message-gui)
  (when (not *message-gui*)
    (define buttonlayout (<gui> :horizontal-layout))
    (<gui> :set-layout-spacing buttonlayout 2 0 2 0 2)

    (<gui> :add-layout-space buttonlayout 0 0 #t #f)
    
    (define hide-button (<gui> :button "Hide"))
    (<gui> :add-callback hide-button (lambda ()                                       
                                       (<gui> :hide *message-gui*)))
    (<gui> :add buttonlayout hide-button)

    (define gui2 (<gui> :vertical-layout *message-gui-text-edit* buttonlayout))
    (<gui> :set-layout-spacing gui2 2 2 2 2 2)
    
    (<gui> :set-size gui2
           (floor (<gui> :text-width "Could not find..... Plugin file. asdf  wefawe3451345 13451345 oiwaefoajefoijaowepijaeporgijpoaghjto#$#$% 2q3e4tERTQERT paerjgoijaerpoiporegi"))
           (floor (<gui> :text-width "Could not find..... Plugin file. asdf  wefawe3451345 13451345")))
    
    (<gui> :set-static-toplevel-widget gui2 #t)
    
    ;; Just hide window when closing it.
    (<gui> :add-close-callback gui2
           (lambda (radium-runs-custom-exec)
             ;;(<gui> :set-parent *message-gui* -3)
             (c-display "              GAKK GAKK GAKK")
             (<gui> :hide *message-gui*)
             #f))

    (set! *message-gui* gui2))

  ;;(c-display gui2)

  (reopen-gui-at-curr-pos *message-gui*))

  

;;(<gui> :get-parent-window *message-gui*)
#!!
(add-message-window-message "aiai")

(define (disable-gui-updates-block gui block)
  (let ((ret (catch #t
                    (lambda ()
                      gui)
                    (lambda args
                      (display (ow!))))))
    ret))

(define (show-message-gui)
  (when #f
    (define gui2 50)
    #t)
  (disable-gui-updates-block
   gui2
   (lambda ()
     50)))


(define (show-message-gui)
  (when #f
    (define gui2 50)
    #t)
  gui2)

(eval '(show-message-gui))

(show-message-gui)


(<ra> :add-message "aiai")

(<ra> :add-message "hello1345weert446        werttqwertqert qqerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrt                           qerrrrrrrrrrrrrrrrrrrrrrrrt\nasdfasdf")
(show-message-gui)
(<gui> :hide *message-gui*)
(<gui> :show *message-gui*)

!!#

(define *g-complete-message* #f)
(define (add-message-window-message message)
  (set! *g-complete-message* (<-> (if (not (string? *g-complete-message*))
                                      ""
                                      (<-> *g-complete-message* "<p><br>\n"))
                                  "<h4>" (<ra> :get-date-string) " " (<ra> :get-time-string) ":</h4>"
                                  "<blockquote>" message "</blockquote>"))
  (define old-message (<gui> :get-value *message-gui-text-edit*))
  (<gui> :set-value *message-gui-text-edit* (<-> "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd\">"
                                                 "<html><head>" 
                                                 "</head><body>"                                                 
                                                 *g-complete-message*                                                 
                                                 "<br>"                                                 
                                                 "</body></html>\n"))
  (show-message-gui))

#||
(<ra> :add-message "hello1345weert446        werttqwertqert qqerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrt                           qerrrrrrrrrrrrrrrrrrrrrrrrt\nasdfasdf")
(<ra> :show-message "hello1345weert446        werttqwertqert qqerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrt                           qerrrrrrrrrrrrrrrrrrrrrrrrt\nasdfasdf")
||#

#!!
(let ((gui (<gui> :horizontal-layout)))  (<gui> :show gui)  (<gui> :move-to-parent-centre gui)  )

!!#


(define *help-windows* (make-hash-table 10 string=?))

(define (FROM-C-show-help-window filename)
  (define web (or (*help-windows* filename)
                  (let ((web (<gui> :web filename)))
                    (set! (*help-windows* filename) web)
                    (<gui> :add-close-callback web (lambda x
                                                     (<gui> :hide web)
                                                     #f))
                    web)))
  (reopen-gui-at-curr-pos web))



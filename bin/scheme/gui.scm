(provide 'gui.scm)


(define (gui-create-layout create-layout-func layout-args guis)
  (define layout (apply create-layout-func layout-args))
  (for-each (lambda (gui)
              (c-display "layout/gui:" layout gui)
              (<ra> :gui_add layout gui))
            guis)
  layout)
  
(define (my-gui_group title args)
  (define group (<ra> :gui_group title))
  (for-each (lambda (gui)
              (<ra> :gui_add group gui))
            args)
  group)

(define (my-gui_tablelayout args)
  (if (= 1 (length args))
      (<ra> :gui_table-layout (car args))
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

(define (<gui> command . args)
  (c-display "****" command args)
  (cond ((eq? command :group)
         (gui-create-layout ra:gui_group (list (car args)) (cdr args)))
        
        ((eq? command :vertical-layout)
         (gui-create-layout ra:gui_vertical-layout '() args))
        
        ((eq? command :horizontal-layout)
         (gui-create-layout ra:gui_horizontal-layout '() args))
        
        ((eq? command :empty)
         (<ra> :gui_vertical-layout))
        
        ((eq? command :table-layout)
         (my-gui_tablelayout args))

        ((eq? command :add-callback)
         (<ra> :gui_add-callback (car args) (cadr args)))
        
        (else
         (let* ((func (eval (<_> 'ra:gui_ (keyword->symbol command))))
                (last-arg (last args))
                (gui (if (procedure? last-arg)
                         (apply func (butlast args))
                         (apply func args))))
           (when (procedure? last-arg)
             (<ra> :gui_add-callback gui last-arg))
           gui))))
               
                           
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
                                    (c-display "slider: " val)
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
                                 

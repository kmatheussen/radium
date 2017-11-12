(provide 'various.scm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editor: Zoom exponentially
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (line-zoom-out-exponentially)
  (define curr (<ra> :get-line-zoom-block))
  ;;(c-display "z1" curr)
  (cond ((< curr 0 )
         (<ra> :line-zoom-block (- (floor (/ curr 2)))))
        (else
         (if (< curr 40)
             (<ra> :line-zoom-block curr)))))

(define (line-zoom-in-exponentially)
  (define curr (<ra> :get-line-zoom-block))
  ;;(c-display "z2" curr)
  (cond ((= 1 curr)
         (<ra> :line-zoom-block -1))
        ((< curr 0 )
         (if (> curr -32)
             (<ra> :line-zoom-block curr)))
        (else
         (<ra> :line-zoom-block (- (floor (/ curr 2)))))))

#!!
(line-zoom-out-exponentially)
(line-zoom-in-exponentially)
!!#

#||
(define (FROM_C-apply-block-track-onoff-to-seqblock)
  (define blocknum (<ra> :current-block))
  (define seqtracknum (<ra> :get-curr-seqtrack))
  (define is-playing-song (<ra> :is-playing-song))
  )
||#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modulator GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define (FROM_C-create-modulator-gui instrument-id)

  (define gui (<gui> :ui "modulatorgui.ui"))
  (<gui> :set-window-title gui (<-> "GUI for " (<ra> :get-instrument-name instrument-id)))

  (let ((width (floor (* 3 (<gui> :text-width "Instrument name, and an effect name, Enabled, Delete")))))
    (<gui> :set-size gui width (floor (/ width 3))))

  (define table (create-table-gui (list (make-table-row "Instrument" "Long name of an instrument" #t)
                                        (make-table-row "Effect" "Long name of an effect" #t)
                                        (make-table-row "Enabled" #f #f)
                                        (make-table-row "Delete" #f #f))
                                  :selected-row-callback (lambda (table row-num row-content)
                                                           (c-display "row num" row-num "selected. Content:" row-content))
                                  :hide-callback (lambda (table)
                                                   (<gui> :close table))))
  
  (let ((table-parent (<gui> :child gui "tableParent")))
    ;;(<gui> :set-layout-spacing table-parent 2 0 2 0 2)
   ;; (<gui> :set-layout-spacing gui 2 2 2 2 2)
    (<gui> :add table-parent table))

  (define doit #f)
  
  (define (create-enabled target-id effect-name)
    (<gui> :checkbox ""
           (<ra> :get-modulator-enabled target-id effect-name)
           (lambda (enabled)
             (if (and doit
                      (<ra> :instrument-is-open-and-audio instrument-id)
                      (<ra> :has-modulator target-id effect-name))
                 (<ra> :set-modulator-enabled target-id effect-name enabled)))))

  (define (create-delete target-id effect-name)
    (<gui> :button "Delete"
           (lambda ()
             (when (and (<ra> :instrument-is-open-and-audio instrument-id)
                        (<ra> :has-modulator target-id effect-name))
               (<ra> :remove-modulator target-id effect-name)
               (update-rows!)))))
  
  (define (create-row! n target-id effect-num effect-name)
    (<gui> :add-table-string-cell table (<ra> :get-instrument-name target-id) 0 n)

    (<gui> :add-table-string-cell table effect-name 1 n)
    
    (define onoff (create-enabled target-id effect-name))
    (<gui> :add-table-gui-cell table onoff 2 n)

    (define delete (create-delete target-id effect-name))
    (<gui> :add-table-gui-cell table delete 3 n)
    )

  (define curr-targets #f)
  
  (define (update-rows!)
    (define targets (<ra> :get-modulator-targets instrument-id))
    (when (not (morally-equal? targets curr-targets))
      (set! curr-targets targets)
      
      ;;(c-display "NEW TARGETS:" (pp targets))

      (<gui> :enable-table-sorting table #f)
      
      (define num-rows (<gui> :get-num-table-rows table))
      (<gui> :add-table-rows table 0 (- (length targets)
                                        num-rows))
      
      (set! doit #f)
      
      (for-each (lambda (n target)
                  (define target-id (target :instrument-id))
                  (define target-effect-num (target :effect-num))
                  (define effect-name (<ra> :get-instrument-effect-name target-effect-num target-id))
                  (create-row! n target-id target-effect-num effect-name))
                (iota (length targets))
                targets)

      (set! doit #t)

      (<gui> :enable-table-sorting table #t)))

  (<ra> :schedule 0
        (lambda ()
          (cond ((not (<ra> :instrument-is-open-and-audio instrument-id))
                 (<gui> :close gui)
                 #f)
                ((not (<gui> :is-open gui))
                 #f)
                (else
                 (update-rows!)
                 200))))

  (for-each (lambda (phasenum)
              (define button (<gui> :child gui (<-> "phase_" phasenum "4")))
              (<gui> :add-callback button (lambda ()
                                            (when (<ra> :instrument-is-open-and-audio instrument-id)
                                              (<ra> :undo-instrument-effect instrument-id "Phase shift")
                                              (<ra> :set-instrument-effect instrument-id "Phase shift" (/ phasenum 4))))))
            (list 0 1 2 3))
  
  (define close-button (<gui> :child gui "close_button"))
  (<gui> :add-callback close-button (lambda ()
                                      (<gui> :close gui)))

  (<gui> :set-takes-keyboard-focus gui #f)
  (<gui> :enable-table-sorting table #t)

  (<gui> :set-parent gui -1) ;; Set parent to the main window.
  
  gui)

  
#!
(let ((gui (FROM_C-create-modulator-gui (first (<ra> :get-modulator-instruments)))))
  (<gui> :show gui))
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blocks table GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-blocks-table-gui)

  (define gui (<gui> :ui "blocks.ui"))
  (<gui> :set-window-title gui "Blocks")

  (define doit #f)

  (define search-string "")

  (define search-text (<gui> :child gui "search_text"))
  (<gui> :hide (<gui> :child gui "search_button"))

  (define blocknum->rownum-map (vector))

  (<gui> :add-realtime-callback search-text
         (lambda (new-text)
           (when doit
             (set! search-string new-text)
             (c-display "SETTING search to" new-text)
             (update-rows!))))

  (<gui> :set-layout-spacing (<gui> :child gui "searchWidget") 0 0 0 0 2)

  (let ((width (floor (* 3 (<gui> :text-width "Block # Long name LInes Tracks Delete")))))
    (<gui> :set-size gui width (floor (* width 0.7))))

  (define table (create-table-gui (list (make-table-row "Block #" "Block #" #f)
                                        (make-table-row "Name" "Long name of a block" #t)
                                        (make-table-row "Lines" "Tracks9" #f)
                                        (make-table-row "Tracks" "Tracks9" #f)
                                        (make-table-row "Delete" #f #f))
                                  :hide-callback (lambda (table)
                                                   (<gui> :close table))
                                  :curr-selected-row-changed-callback (lambda (table row-num row-content)
                                                                        (when doit
                                                                          (c-display "ROW_CONTENT:" row-content row-num)
                                                                          (define blocknum (to-integer (string->number (first row-content))))
                                                                          (when (not (= blocknum (<ra> :current-block)))
                                                                            (<ra> :select-block blocknum)
                                                                            (update-rows!)
                                                                            (c-display (integer? blocknum) blocknum "row num" row-num "selected. Content:" row-content))))))
                                                                        

  
  (let ((table-parent (<gui> :child gui "tableParent")))
    (<gui> :set-layout-spacing table-parent 0 0 0 0 2)
    (<gui> :set-layout-spacing gui 0 2 2 2 2)
    (<gui> :add table-parent table))

  (define (create-name blocknum curr)
    (define ret (<gui> :line curr
                       (lambda (value)
                         (when doit
                           (c-display "VALUE:" value)
                           (when (not (string=? value curr))
                             (<ra> :add-undo-block blocknum)
                             (<ra> :set-block-name value blocknum)
                             (update-rows!))))))
    (define color (<gui> :mix-colors
                         (<ra> :get-block-color blocknum)
                         (<gui> :get-background-color -1)
                         0.9))
    (<gui> :set-background-color ret color)
    (define layout (<gui> :vertical-layout))
    (<gui> :add layout ret)
    (<gui> :set-layout-spacing layout 2 2 2 2 2)
    layout)

  (define (create-num-lines blocknum curr)
    (<gui> :int-text 2 curr (max curr 9999)
           (lambda (value)
             (when doit
               (c-display "VALUE:" value)
               (<ra> :add-undo-block blocknum)
               (<ra> :schedule 10
                     (lambda ()
                       (<ra> :set-num-lines value blocknum)
                       #f))
               ;;(update-rows!)
               ))))

  (define (create-num-tracks blocknum curr)
    (<gui> :int-text 1 curr (max curr 999)
           (lambda (value)
             (when doit
               (<ra> :add-undo-block blocknum)
               (<ra> :set-num-tracks value blocknum)
               (update-rows!)))))

  (define (create-delete blocknum)
    (<gui> :button "Delete"
           (lambda ()
             (<ra> :delete-block blocknum)
             (update-rows!))))

  (define (create-row! blockinfo rownum curr-blocknum)
    (define blocknum (car blockinfo))
    (define blockname (cadr blockinfo))
    (define num-lines (caddr blockinfo))
    (define num-tracks (cadddr blockinfo))
    
    (define time1 (time))
    (<gui> :add-table-int-cell table blocknum 0 rownum)
    (define time2 (time))

    (define blocknamegui (create-name blocknum blockname))
    (<gui> :add-table-gui-cell table blocknamegui 1 rownum)
    (define time3 (time))
    
    (<gui> :add-table-gui-cell table (create-num-lines blocknum num-lines) 2 rownum)
    (define time4 (time))
    
    (<gui> :add-table-gui-cell table (create-num-tracks blocknum num-tracks) 3 rownum)
    (define time5 (time))
    
    (define delete (create-delete blocknum))
    (define time6 (time))
    (<gui> :add-table-gui-cell table delete 4 rownum)
    (define time7 (time))

    (if (= blocknum curr-blocknum)
        (<gui> :set-value table rownum))
    ;;(c-display "Created " blocknum blockname (- time2 time1) (- time3 time2) (- time4 time3) (- time5 time4) (- time6 time5) (- time7 time6))
    )

  (define curr-data #f)

  (define (get-data)
    (cons (<ra> :current-block)
          (keep identity
                (map (lambda (blocknum)
                       (define blockname (<ra> :get-block-name blocknum))
                       (and (or (string=? search-string "")
                                (string-case-insensitive-contains? blockname search-string))
                            (list blocknum
                                  blockname
                                  (<ra> :get-num-lines blocknum)
                                 (<ra> :get-num-tracks blocknum))))
                     (iota (<ra> :get-num-blocks))))))
  
  (define (update-rows!)
    (define data (get-data))
    (when (not (morally-equal? data curr-data))
      (set! curr-data data)
      
      (define curr-blocknum (car data))
      (define blockdata (cdr data))

      (c-display "DATA:" (pp data))

      (<gui> :enable-table-sorting table #f)
      
      (define num-rows (<gui> :get-num-table-rows table))

      (set! doit #f)
      
      (disable-gui-updates-block ;; Speed up.
       table
       (lambda ()

         (<gui> :add-table-rows table 0 (- (length blockdata)
                                           num-rows))
         
         (<gui> :hide table) ;; Major speedup when the block list is updated due to user interaction in the gui. Probably just a workaround for bad qt performance. Tested on Qt 5.4.1. Not unlikely that this is not necessary in newer versions of Qt. (TODO: Test newer versions of Qt)

        (<gui> :set-value table -1) ;; unselect current row.

         (for-each (lambda (rowdata rownum)
                     (create-row! rowdata rownum curr-blocknum))
                   blockdata
                   (iota (length blockdata)))
         (<gui> :show table)

         (<gui> :enable-table-sorting table #t)
         
         ))

      (c-display "HASIT:" (<gui> :has-keyboard-focus search-text))
               
      (set! doit #t)))



  (update-rows!)

  (<ra> :schedule 1100
        (lambda ()
          (cond ((not (<gui> :is-open gui))
                 #f)
                (else
                 (update-rows!)
                 200))))

  (define close-button (<gui> :child gui "close_button"))
  (<gui> :add-callback close-button (lambda ()
                                      (<gui> :close gui)))

  (<gui> :set-takes-keyboard-focus gui #f)

  (<gui> :enable-table-sorting table #t)

  (<gui> :set-parent gui -1) ;; Set parent to the main window.

  (<gui> :show gui)

  (c-display "HASIT2:" (<gui> :has-keyboard-focus search-text))
  (c-display "HASIT3:" (<gui> :has-keyboard-focus gui))

  (<ra> :schedule 100
        (lambda ()
          (c-display "HASIT4:" (<gui> :has-keyboard-focus search-text))
          #f))
  gui)


(if (not *is-initializing*)
    (create-blocks-table-gui))


(define *blocks-table-gui* #f)  

(define (FROM_C-create-blocks-table-gui)
  (if (or (not *blocks-table-gui*)
          (not (<gui> :is-open *blocks-table-gui*)))
      (set! *blocks-table-gui* (create-blocks-table-gui))
      (<gui> :raise *blocks-table-gui*)))

  

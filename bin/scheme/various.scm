(provide 'various.scm)


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

#!
(pretty-print (<ra> :get-modulator-targets (first (<ra> :get-modulator-instruments))))
;;(<ra> :get-modulator-targets (second (<ra> :get-modulator-instruments)))
!#
  
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
(let ((gui (create-modulator-gui (first (<ra> :get-modulator-instruments)))))
  (<gui> :show gui))
!#

  

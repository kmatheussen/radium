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

  (define gui (<gui> :ui (<ra> :get-path "modulatorgui.ui")))
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
                                                   (<gui> :close gui))))
  
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
                  (define effect-name (target :effect-name))
                  (create-row! n target-id target-effect-num effect-name))
                (iota (length targets))
                targets)

      (set! doit #t)

      (<gui> :enable-table-sorting table #t)))

  (<gui> :add-deleted-callback gui
         (lambda (radium-runs-custom-exec)
           (<ra> :internal_instrument-gui-has-been-hidden instrument-id)))

  ;;(<gui> :add-close-callback gui
  ;;       (lambda (radium-runs-custom-exec)
  ;;         (if (<ra> :instrument-is-open-and-audio instrument-id)
  ;;             (begin
  ;;               (<gui> :hide gui)
  ;;               (<ra> :internal_instrument-gui-has-been-hidden instrument-id)
  ;;               #f)
  ;;             #t)))
  
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

  ;;(<gui> :set-parent gui -1) ;; Set parent to the main window. (caller are responsible for doing this, if needed)
  
  gui)

  
#!
(let ((gui (FROM_C-create-modulator-gui (first (<ra> :get-modulator-instruments)))))
  (<gui> :show gui))
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blocks table GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-blocks-table-gui)

  (define gui (<gui> :ui (<ra> :get-path "blocks.ui")))
  (<gui> :set-window-title gui "Blocks")

  (define doit #f)

  (define search-string "")

  (define search-text (<gui> :child gui "search_text"))
  (<gui> :hide (<gui> :child gui "search_button"))

  (<gui> :add-realtime-callback search-text
         (lambda (new-text)
           (when doit
             (set! search-string new-text)
             ;;(c-display "SETTING search to" new-text)
             (update-rows!))))

  (<gui> :set-layout-spacing (<gui> :child gui "searchWidget") 0 0 0 0 2)

  (let ((width (floor (* 3 (<gui> :text-width "Block # Long name LInes Tracks Delete")))))
    (<gui> :set-size gui width (floor (* width 0.7))))

  (define table (create-table-gui (list (make-table-row "Block #" "Block #" #f)
                                        (make-table-row "Usage #" "Usage #" #f)
                                        (make-table-row "Name" "Long name of a block" #t)
                                        (make-table-row "Lines" "Tracks9" #f)
                                        (make-table-row "Tracks" "Tracks9" #f)
                                        (make-table-row "Delete" #f #f))
                                  :hide-callback (lambda (table)
                                                   (<gui> :close table))
                                  :curr-selected-row-changed-callback (lambda (table row-num row-content)
                                                                        (when doit
                                                                          ;;(c-display "ROW_CONTENT:" row-content row-num)
                                                                          (define blocknum (to-integer (string->number (first row-content))))
                                                                          (when (not (= blocknum (<ra> :current-block)))
                                                                            (when (not (<ra> :is-playing-song))
                                                                              (<ra> :select-block blocknum)
                                                                              (update-rows!))
                                                                            ;;(c-display (integer? blocknum) blocknum "row num" row-num "selected. Content:" row-content)
                                                                            )))))
                                                                        

  
  (let ((table-parent (<gui> :child gui "tableParent")))
    (<gui> :set-layout-spacing table-parent 0 0 0 0 2)
    (<gui> :set-layout-spacing gui 0 2 2 2 2)
    (<gui> :add table-parent table))

  (define (create-name blocknum curr rownum)
    (define ret (<gui> :line curr "black"
                       (lambda (value)
                         (when doit
                           ;;(c-display "BLOCKNAME. New VALUE:" value)
                           (when (not (string=? value (<ra> :get-block-name blocknum)))
                             (<ra> :add-undo-block blocknum)
                             (<ra> :set-block-name value blocknum)
                             (update-rows!))))))

    (<gui> :add-focus-in-callback ret (lambda ()
                                        (if (not (<ra> :is-playing-song))
                                            (begin
                                              (<ra> :select-block blocknum)
                                              (update-rows!))
                                            (begin
                                              (<gui> :enable-table-sorting table #f)
                                              (<gui> :set-value table rownum)
                                              (<gui> :enable-table-sorting table #t)))))

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
               ;;(c-display "VALUE:" value)
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

  (define last-time 0)
  (define start-time 0)
  
  (define (create-row! blockinfo rownum curr-blocknum blockusage)
    (define blocknum (car blockinfo))
    (define usage (blockusage blocknum))
    (define blockname (cadr blockinfo))
    (define num-lines (caddr blockinfo))
    (define num-tracks (cadddr blockinfo))
    
    (define time1 (time))
    (<gui> :add-table-int-cell table blocknum 0 rownum)
    (define time2 (time))

    (<gui> :add-table-int-cell table usage 1 rownum)

    (define blocknamegui (create-name blocknum blockname rownum))
    (<gui> :add-table-gui-cell table blocknamegui 2 rownum)
    (define time3 (time))

    (<gui> :add-table-gui-cell table (create-num-lines blocknum num-lines) 3 rownum)
    (define time4 (time))
    
    (<gui> :add-table-gui-cell table (create-num-tracks blocknum num-tracks) 4 rownum)
    (define time5 (time))
    
    (define delete (create-delete blocknum))
    (define time6 (time))
    (<gui> :add-table-gui-cell table delete 5 rownum)
    (define time7 (time))

    (if (= blocknum curr-blocknum)
        (<gui> :set-value table rownum))

    (define time8 (time))
    (c-display "Created " blocknum blockname (- time8 last-time) "-" (- time8 start-time) ":" (- time2 time1) (- time3 time2) (- time4 time3) (- time5 time4) (- time6 time5) (- time7 time6) (- time8 time7))
    (set! last-time time8)
    )

  (define curr-data #f)

  (define (get-data)
    (list (<ra> :current-block)
          (<ra> :get-block-usage-in-sequencer)
          (keep identity
                (map (lambda (blocknum)
                       (define blockname (<ra> :get-block-name blocknum))
                       ;;(c-display "BLOCKNAME" blocknum "\"" blockname "\"")
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
      (define blockusage (cadr data))
      (define blockdata (caddr data))

      ;;(c-display "DATA:" (pp data))

      (<gui> :enable-table-sorting table #f)
      
      (define num-rows (<gui> :get-num-table-rows table))

      (set! doit #f)

      (set! start-time (time))
      
      (disable-gui-updates-block ;; Speed up.
       table
       (lambda ()

         (<gui> :add-table-rows table 0 (- (length blockdata)
                                           num-rows))

         ;; Hiding the table is a major speedup when the block list is updated due to user interaction in the gui.
         ;; It's mainly a workaround for very bad qt performance on Qt 5.4.1. (I hadn't discovered it on newer Qt versions)
         ;; On Qt 5.9.0, it only increases performance with around 20-30%.
         (<gui> :hide table)

        (<gui> :set-value table -1) ;; unselect current row.

         (for-each (lambda (rowdata rownum)
                     (create-row! rowdata rownum curr-blocknum blockusage))
                   blockdata                   
                   (iota (length blockdata)))

         ;; show it again.
         (<gui> :show table)

         (<gui> :enable-table-sorting table #t)
         
         ))

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

  gui)


;(if (not *is-initializing*)
;    (create-blocks-table-gui))


(define *blocks-table-gui* #f)  

(define (FROM_C-create-blocks-table-gui)
  (if (or (not *blocks-table-gui*)
          (not (<gui> :is-open *blocks-table-gui*)))
      (set! *blocks-table-gui* (create-blocks-table-gui))
      (<gui> :raise *blocks-table-gui*)))

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instruments table GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-instruments-table-gui)

  (define gui (<gui> :ui (<ra> :get-path "blocks.ui")))
  (<gui> :set-window-title gui "Instruments")

  (define doit #f)

  (define search-string "")

  (define search-text (<gui> :child gui "search_text"))
  (<gui> :hide (<gui> :child gui "search_button"))

  (<gui> :add-realtime-callback search-text
         (lambda (new-text)
           (when doit
             (set! search-string new-text)
             ;;(c-display "SETTING search to" new-text)
             (update-rows!))))

  (<gui> :set-layout-spacing (<gui> :child gui "searchWidget") 0 0 0 0 2)

  (let ((width (floor (* 3 (<gui> :text-width "Instrument # Long name LInes Tracks Delete")))))
    (<gui> :set-size gui width (floor (* width 0.7))))

  (define table (create-table-gui (list (make-table-row "#" "123" #f)
                                        (make-table-row "# Conn." "# Conn." #f)
                                        (make-table-row "Name" "Long name of a instrument" #t)
                                        (make-table-row "# in" "# in" #f)
                                        (make-table-row "# out" "# out" #f)
                                        (make-table-row "Type" "GAkk gkkakkg kakkg / asdf" #t)
                                        (make-table-row "Recv. ext. MIDI" "Recv. ext. MIDI" #f)
                                        (make-table-row "GUI" "GUI" #f)
                                        (make-table-row "Delete" #f #f))
                                  :hide-callback (lambda (table)
                                                    (<gui> :close gui))
                                  :curr-selected-row-changed-callback (lambda (table row-num row-content)
                                                                        (when doit
                                                                          ;;(c-display "ROW_CONTENT:" row-content row-num)
                                                                          (define instrument-id (<ra> :get-audio-instrument-id (to-integer (string->number (first row-content)))))
                                                                          (when (not (equal? instrument-id (<ra> :get-current-instrument)))
                                                                            (<ra> :set-current-instrument instrument-id)
                                                                            (update-rows!)
                                                                            ;;(c-display (integer? instrumentnum) instrumentnum "row num" row-num "selected. Content:" row-content)
                                                                            )))))
                                                                        

  
  (let ((table-parent (<gui> :child gui "tableParent")))
    (<gui> :set-layout-spacing table-parent 0 0 0 0 2)
    (<gui> :set-layout-spacing gui 0 2 2 2 2)
    (<gui> :add table-parent table))
  
  (define (create-name instrument-id curr)
    (define ret (<gui> :line curr
                       (lambda (value)
                         (when doit
                           ;;(c-display "VALUE:" value)
                           (when (not (string=? value curr))
                             (<ra> :set-instrument-name value instrument-id)
                             (update-rows!))))))    
    
    (<gui> :add-focus-in-callback ret (lambda ()
                                        (<ra> :set-current-instrument instrument-id)
                                        (update-rows!)
                                        ))
    
    (define color (<gui> :mix-colors
                         (<ra> :get-instrument-color instrument-id)
                         (<gui> :get-background-color -1)
                         0.9))
    (<gui> :set-background-color ret color)
    (define layout (<gui> :vertical-layout))
    (<gui> :add layout ret)
    (<gui> :set-layout-spacing layout 2 2 2 2 2)
    (<gui> :set-size-policy layout #t #f)
    layout)

  (define (create-gui-onoff instrument-id)
    (define has-gui (<ra> :has-native-instrument-gui instrument-id))
    (if (not has-gui)
        #f
        (mid-horizontal-layout (<gui> :checkbox "" (<ra> :instrument-gui-is-visible instrument-id table)
                                      (lambda (onoff)
                                        (if onoff
                                            (<ra> :show-instrument-gui instrument-id gui)
                                            (<ra> :hide-instrument-gui instrument-id)))))))
  
  (define (create-delete instrument-id)
    (<gui> :button "Delete"
           (lambda ()
             (<ra> :delete-instrument instrument-id)
             (update-rows!))))

  (define last-time 0)
  (define start-time 0)
  
  (define (create-row! instrumentinfo rownum curr-instrument-id)
    (define instrument-id (car instrumentinfo))
    (define usage (cadr instrumentinfo))
    (define instrumentname (caddr instrumentinfo))
    (define num-inputs (<ra> :get-num-input-channels instrument-id))
    (define num-outputs (<ra> :get-num-output-channels instrument-id))
    (define type (cadddr instrumentinfo))
    
    (define time1 (time))
    (<gui> :add-table-int-cell table (<ra> :get-audio-instrument-num instrument-id) 0 rownum)
    (define time2 (time))

    (<gui> :add-table-int-cell table usage 1 rownum)
    
    (define instrumentnamegui (create-name instrument-id instrumentname))
    (<gui> :add-table-gui-cell table instrumentnamegui 2 rownum)
    (define time3 (time))

    (<gui> :add-table-int-cell table num-inputs 3 rownum)
    (define time4 (time))
    
    (<gui> :add-table-int-cell table num-outputs 4 rownum)
    (define time5 (time))
    
    (<gui> :add-table-string-cell table type 5 rownum)

    (define midienabledgui (<gui> :checkbox "" (<ra> :instrument-always-receive-midi-input instrument-id)
                                  (lambda (onoff)
                                    (<ra> :set-instrument-always-receive-midi-input instrument-id onoff))))
    (<gui> :set-tool-tip midienabledgui "Receive MIDI from external input")
    (<gui> :add-table-gui-cell table (mid-horizontal-layout midienabledgui) 6 rownum)

    (define onoffgui (create-gui-onoff instrument-id))
    (if onoffgui
        (<gui> :add-table-gui-cell table onoffgui 7 rownum)
        (<gui> :add-table-string-cell table "" 7 rownum)) ;; clear the cell. TODO: create a clear-cell function.

    (define delete (create-delete instrument-id))
    (define time6 (time))
    (<gui> :add-table-gui-cell table delete 8 rownum)
    (define time7 (time))

    (if (equal? instrument-id curr-instrument-id)
        (<gui> :set-value table rownum))

    (define time8 (time))
    (c-display "Created " instrument-id instrumentname (- time8 last-time) "-" (- time8 start-time) ":" (- time2 time1) (- time3 time2) (- time4 time3) (- time5 time4) (- time6 time5) (- time7 time6) (- time8 time7))
    (set! last-time time8)
    )

  (define curr-data #f)

  (define (get-data)
    (list (<ra> :get-current-instrument)
          (keep identity
                (map (lambda (instrument-id)
                       (define instrumentname (<ra> :get-instrument-name instrument-id))
                       (define type (<-> (<ra> :get-instrument-type-name instrument-id) " / " (<ra> :get-instrument-plugin-name instrument-id)))
                       (and (or (string=? search-string "")
                                (string-case-insensitive-contains? instrumentname search-string)
                                (string-case-insensitive-contains? type search-string))
                            (list instrument-id
                                  (+ (<ra> :get-num-in-audio-connections instrument-id)
                                     (<ra> :get-num-out-audio-connections instrument-id)
                                     (<ra> :get-num-in-event-connections instrument-id)
                                     (<ra> :get-num-out-event-connections instrument-id))
                                  instrumentname
                                  type
                                  (<ra> :instrument-always-receive-midi-input instrument-id)

                                  ;; Commented out since instrument-gui-is-visible only returns true if the supplied gui argument is the same as the parent of the instrument gui. Should probably make an is-visible-on-any-gui function.
                                  ;;(and (<ra> :has-native-instrument-gui instrument-id)
                                  ;;     (<ra> :instrument-gui-is-visible instrument-id table))

                                  )))
                     (append ;;(get-all-midi-instruments)
                             (get-all-audio-instruments))))))
  
  (define (update-rows!)
    (define data (get-data))
    (when (not (morally-equal? data curr-data))
      (set! curr-data data)
      
      (define curr-instrument-id (car data))
      (define instrumentdata (cadr data))

      ;;(c-display "DATA:" (pp data))

      (<gui> :enable-table-sorting table #f)
      
      (define num-rows (<gui> :get-num-table-rows table))

      (set! doit #f)

      (set! start-time (time))
      
      (disable-gui-updates-block ;; Speed up.
       table
       (lambda ()

         (<gui> :add-table-rows table 0 (- (length instrumentdata)
                                           num-rows))

         ;; Hiding the table is a major speedup when the instrument list is updated due to user interaction in the gui.
         ;; It's mainly a workaround for very bad qt performance on Qt 5.4.1. (I hadn't discovered it on newer Qt versions)
         ;; On Qt 5.9.0, it only increases performance with around 20-30%.
         (<gui> :hide table)

        (<gui> :set-value table -1) ;; unselect current row.

         (for-each (lambda (rowdata rownum)
                     (create-row! rowdata rownum curr-instrument-id))
                   instrumentdata                   
                   (iota (length instrumentdata)))

         ;; show it again.
         (<gui> :show table)

         (<gui> :enable-table-sorting table #t)
         
         ))

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
  (<gui> :sort-table-by table 0 #t)

  (<gui> :set-parent gui -1) ;; Set parent to the main window.

  (<gui> :show gui)

  gui)


(if (not *is-initializing*)
    (create-instruments-table-gui))


(define *instruments-table-gui* #f)  

(define (FROM_C-create-instruments-table-gui)
  (if (or (not *instruments-table-gui*)
          (not (<gui> :is-open *instruments-table-gui*)))
      (set! *instruments-table-gui* (create-instruments-table-gui))
      (<gui> :raise *instruments-table-gui*)))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blocklist / Playlist 
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: may be used for keybinding
(define (show-set-current-seqtrack-menu)
  (popup-menu (map (lambda (seqtracknum)
                     (list (<ra> :get-seqtrack-name seqtracknum)
                           (lambda ()
                             (<ra> :set-curr-seqtrack seqtracknum))))
                   (iota (<ra> :get-num-seqtracks)))))


(<declare-variable> get-delete-all-pauses-menu-entry) ;; in sequencer.scm

(define (get-blocklist/playlist-common-entries)
  (list

   (get-delete-all-pauses-menu-entry (<ra> :get-curr-seqtrack))

   "Set current seqtrack"
   show-set-current-seqtrack-menu

   "Show blocklist"
   ra:show-blocklist-gui))

             

(define (get-blocklist-popup-menu-entries)
  (define blocknum (<ra> :current-block))

  (define (get-block-entries)
    (list
     
     (<-> "-------" blocknum ": \"" (<ra> :get-block-name blocknum) "\"")
     
     (list "Rename"
           (lambda ()
             (define old-name (<ra> :get-block-name blocknum))
             (define new-name (<ra> :request-string "New name:" #t old-name))
             (c-display "NEWNAME" (<-> "-" new-name "-"))
             (when (and (not (string=? new-name ""))
                        (not (string=? new-name old-name)))
               (<ra> :add-undo-block blocknum)
               (<ra> :set-block-name new-name blocknum))))

     (list "Configure color"
           (lambda ()
             (if blocknum
                 (<ra> :color-dialog (<ra> :get-block-color blocknum -1 #f) -1
                       (lambda (color)
                         (<ra> :set-block-color color blocknum))))))
     
     (list "Generate new color"
           :shortcut ra:generate-new-color-for-all-selected-seqblocks
           (lambda ()
             (let ((color (<ra> :generate-new-block-color 1.0)))
               (<ra> :set-block-color color blocknum))))
     
     "Delete"
     ra:delete-block
     
     "-------Editor blocks"

     "Insert new block"
     ra:insert-block
     
     "Append new block"
     ra:append-block
     
     "---------------"
     
     "Load Block (BETA!)"
     ra:load-block
     
     "Save Block"
     ra:save-block

     "---------------"
     
     ))
  (get-block-entries))

(define (show-blocklist-popup-menu)
  (popup-menu (get-blocklist-popup-menu-entries)
              (get-blocklist/playlist-common-entries)))
  
(define (get-audiofile-entries filename)
  (list
   "Add new audio file(s)"
   (lambda ()
     (create-file-requester "Choose audio file(s)" (<ra> :create-illegal-filepath) "audio files" (<ra> :get-audiofile-postfixes) #t "" #t #f -1
                            (lambda (filenames)
                              (c-display "FILENAMES:" filenames)
                              (for-each (lambda (filename)
                                          (c-display "ADDING" filename)
                                          (<ra> :add-audiofile filename))
                                        filenames))))
   
   (list "Configure color"
         :enabled filename
         (lambda ()
           (<ra> :color-dialog (<ra> :get-audiofile-color filename #f) -1
                 (lambda (color)
                   (<ra> :set-audiofile-color color filename)))))
   
   (list "Generate new color"
         :enabled filename
         :shortcut ra:generate-new-color-for-all-selected-seqblocks
         (lambda ()
           (let ((color (<ra> :generate-new-block-color 1.0)))
             (<ra> :set-audiofile-color color filename))))
   
   ))

(define (show-audiolist-popup-menu filename)
  (popup-menu (get-audiofile-entries filename)))

(define (FROM_C-show-blocklist-popup-menu)
  (define seqtracknum (<ra> :get-curr-seqtrack))
  (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define for-blocks (not for-audiofiles))

  (popup-menu
   (if for-blocks
       (get-blocklist-popup-menu-entries)
       (get-audiofile-entries #f))

   "---------------Playlist"

   (get-blocklist/playlist-common-entries)

   "Hide"
   (lambda ()
     (<ra> :show-hide-playlist -1)
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load song, "are you sure?" requester 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define (create-are-you-sure?-loading-song-requester callback)
;;  (define gui (<gui> :vertical-layout))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bottom bar CPU / XRUNS
;;;;;;;;;;;;;;;;;;;;;;;;;;;


#!!
(<ra> :get-num-xruns)
!!#

(define (get-xruns-area-X-width)
  (* 1.2 (<gui> :text-width "X:")))

(define (get-xruns-area-number-width)
  (* 1.2 (<gui> :text-width "00")))

(define (get-xruns-area-width)
  (+ (get-xruns-area-X-width)
     (get-xruns-area-number-width)))

(define (get-cpu-area-single-width)
  (* 1.2 (<gui> :text-width "00")))
(define (get-cpu-area-dash-width)
  (* 1.2 (<gui> :text-width "-")))
(define (get-cpu-area-width)
  (+ (* 3 (get-cpu-area-single-width))
     (* 2 (get-cpu-area-dash-width))))

(define *zero-num-xruns* (<ra> :get-num-xruns))
(define *num-xruns* 0)

(define *cpu-usage-guis* '())

(define *cpu-usage* (<ra> :get-cpu-usage))

(define (reset-cpu-usage!)
  (set! *zero-num-xruns* (<ra> :get-num-xruns))
  (set! *num-xruns* 0))

(define *cpu-usage-poller-has-started #f)

(define (maybe-start-cpu-usage-poller)
  (when (not *cpu-usage-poller-has-started)
    (set! *cpu-usage-poller-has-started #t)
    (reset-cpu-usage!)
    (<ra> :schedule 1000
          (lambda ()
            ;;(c-display "..update")
            (set! *num-xruns* (max 0 (- (<ra> :get-num-xruns) *zero-num-xruns*)))
            (set! *cpu-usage* (<ra> :get-cpu-usage))
            (set! *cpu-usage-guis* (keep (lambda (area)
                                           (if (<gui> :is-open area)
                                               (begin
                                                 (<gui> :update area)
                                                 #t)
                                               #f))
                                         *cpu-usage-guis*))
            1000))))

(def-area-subclass (<cpu-usage-area> :gui :x1 :y1 :x2 :y2)
  (define dash-width (get-cpu-area-dash-width))
  (define single-width (get-cpu-area-single-width))
  (define c1-x1 0)
  (define d1-x1 single-width)
  (define c2-x1 (+ d1-x1 dash-width))
  (define d2-x1 (+ c2-x1 single-width))
  (define c3-x1 (+ d2-x1 dash-width))
  (define c3-x2 (+ c3-x1 single-width))

  (define xruns-X-x1 (+ c3-x2 2))
  (define xruns-X-x2 (+ xruns-X-x1 (get-xruns-area-X-width)))
  (define xruns-number-x1 xruns-X-x2)
  (define xruns-number-x2 (+ xruns-X-x2 (get-xruns-area-number-width)))
  
  (<ra> :schedule 0
        (lambda ()
          (if (<gui> :is-open gui)
              (set-fixed-width gui (ceiling xruns-number-x2)))
          #f))
  
  (c-display "      CREATING")
  
  (define cpu-area-x2 (get-cpu-area-width))

  (add-mouse-cycle! :press-func (lambda (button x* y*)
                                  (reset-cpu-usage!)
                                  (update-me!)
                                  #f
                                  ))
                    
  (add-raw-mouse-cycle!
   :enter-func (lambda (button x* y*)
                 (<gui> :tool-tip
                        (<->"<html><head/><body><p>The first three numbers, from left to right, show:</p>"
                            "<p>1. The <span style=\" font-size:11pt; font-weight:600; text-decoration: underline;\">lowest</span> amount of CPU measured for processing an audio block during the last second. (%)</p>"
                            "<p>2. The <span style=\" font-size:11pt; font-weight:600; text-decoration: underline;\">average</span> amount of CPU measured for processing audio blocks during the last second. (%)</p>"
                            "<p>3. The <span style=\" font-size:11pt; font-weight:600; text-decoration: underline;\">highest</span> amount of CPU measured for processing an audio block during the last second. (%)</p>"
                            "<p>Note that the sum of average numbers for all instruments is likely to be higher than the average CPU you see in the bottom bar due to processing instruments in parallel.</p>"
                            "<p>The last number shows number of soundcard Xruns. Click to reset.</p>"
                            "</body></html>")
                        )))

  (define dascolor (<gui> :mix-colors "green" *text-color* 0.5))

  (define-override (paint) ;; workaround for osx.
    ;;(c-display "GAKK")
    (<gui> :filled-box gui "high_background" x1 y1 x2 y2 0 0)
    
    (define (draw-text text color x1 x2 scale-font-size)
      (<gui> :draw-text gui color text
             x1
             y1
             x2
             y2
             #f ;; wrap-lines
             #f ;; align-top
             #f ;; align-left
             0 ;; rotate
             #f ;;cut-text-to-fit
             scale-font-size ;;scale-font-size
             ))

    ;; cpu usage
    ;;;;;;;;;;;;;;;;;
    (define (draw-number i x1 x2)
      (define n (round (*cpu-usage* i)))
      (define text (if (< n 10)
                       (<-> "0" (number->string n))
                       (number->string n)))
      (define color (cond ((>= n 90)
                           "red")
                          ((>= n 60)
                           "yellow")
                          (else
                           dascolor)))
      (draw-text text color x1 x2 (> n 99)))
    
    (define (draw-dash x1 x2)
      (draw-text "-" *text-color* x1 x2 #f))
    
    (draw-number 0 c1-x1 d1-x1)
    
    (draw-dash d1-x1 c2-x1)
    
    (draw-number 1 c2-x1 d2-x1)
    
    (draw-dash d2-x1 c3-x1)
    
    ;;(c-display d1-x1 c2-x1 d2-x1 c3-x1)
    
    (draw-number 2 c3-x1 c3-x2)

    ;; xruns
    ;;;;;;;;;;;;;;;;;
    (draw-text "X:" dascolor xruns-X-x1 xruns-X-x2 #f)
    
    (let ((xruns *num-xruns*))
      (draw-text (if (< xruns 10)
                     (<-> "0" xruns)
                     (number->string xruns))
                 (if (> xruns 0)
                     "red"
                     dascolor)
                 xruns-number-x1
                 xruns-number-x2
                 (> xruns 99)))
    )
  )

(define (FROM_C-create-cpu-usage-widget)
  (maybe-start-cpu-usage-poller)
  (define testarea (make-qtarea :width 160 :height 20
                                :sub-area-creation-callback
                                (lambda (gui width height state)
                                  (<new> :cpu-usage-area gui 0 0 width height))))
  (push-back! *cpu-usage-guis* (testarea :get-gui))
  (testarea :get-gui))

#!!
(let ()
  (define gui (FROM_C-create-cpu-usage-widget))
  (<gui> :set-parent gui -1)
  (<gui> :show gui))
!!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various popup menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Instruments
  

(define (FROM_C-show-lock-instrument-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-set-current-instrument-locked"
                                                    '()
                                                    "")
   "-------------"
   "Set current instrument" show-set-current-instrument-popup-menu
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
 


;;; Bottom bar

(define (FROM_C-show-bottom-bar-octave-down-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:dec-key-add"
                                                    '(12)
                                                    "FOCUS_EDITOR")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))


(define (FROM_C-show-bottom-bar-octave-up-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:inc-key-add"
                                                    '(12)
                                                    "FOCUS_EDITOR")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-bottom-bar-undo-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:undo"
                                                    '()
                                                    "")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-bottom-bar-redo-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:redo"
                                                    '()
                                                    "")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-bottom-bar-switch-drunk_velocity-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-drunk-velocity-on-off"
                                                    '()
                                                    "FOCUS_EDITOR")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-bottom-bar-switch-edit-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-edit-on-off"
                                                    '()
                                                    "FOCUS_EDITOR")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-bottom-bar-switch-click-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-metronome"
                                                    '()
                                                    "")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-bottom-bar-switch-play-cursor-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-play-cursor-on-off"
                                                    '()
                                                    "FOCUS_EDITOR")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-bottom-bar-switch-editor-follows-play-cursor-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-editor-follows-play-cursor"
                                                    '()
                                                    "FOCUS_EDITOR")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (place-to-string place)
  (if (eq? 'same-place place)
      'same-place
      (<-> (* 1.0 place))))


  


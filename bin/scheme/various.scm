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
                  (define effect-name (<ra> :get-instrument-effect-name target-effect-num target-id))
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

  (define gui (<gui> :ui "blocks.ui"))
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
    (define ret (<gui> :line curr
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

  (define gui (<gui> :ui "blocks.ui"))
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
                                        (make-table-row "GUI" "GUI" #f)
                                        (make-table-row "Delete" #f #f))
                                  :hide-callback (lambda (table)
                                                   (<gui> :close gui))
                                  :curr-selected-row-changed-callback (lambda (table row-num row-content)
                                                                        (when doit
                                                                          ;;(c-display "ROW_CONTENT:" row-content row-num)
                                                                          (define instrument-id (to-integer (string->number (first row-content))))
                                                                          (when (not (= instrument-id (<ra> :get-current-instrument)))
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
    (<gui> :add-table-int-cell table instrument-id 0 rownum)
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

    (define onoffgui (create-gui-onoff instrument-id))
    (if onoffgui
        (<gui> :add-table-gui-cell table onoffgui 6 rownum)
        (<gui> :add-table-string-cell table "" 6 rownum)) ;; clear the cell. TODO: create a clear-cell function.

    (define delete (create-delete instrument-id))
    (define time6 (time))
    (<gui> :add-table-gui-cell table delete 7 rownum)
    (define time7 (time))

    (if (= instrument-id curr-instrument-id)
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
                                  type)))
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

(define (FROM_C-show-blocklist-popup-menu)
  (define seqtracknum (<ra> :get-curr-seqtrack))
  (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define for-blocks (not for-audiofiles))

  (define (get-block-entries)
    (list 
     "Insert new block"
     ra:insert-block
     
     "Append new block"
     ra:append-block
     
     "Delete block"
     ra:delete-block

     "---------------"
     
     "Load Block (BETA!)"
     ra:load-block
     
     "Save Block"
     ra:save-block

     "---------------"
     
     "Show block list"
     ra:show-blocklist-gui
     ))
  
  (define (get-audiofile-entries)
    (list
     "Add new audio file"
     (lambda ()
       (create-file-requester "Choose audio file" "" "audio files" (<ra> :get-audiofile-postfixes) #t #f -1
                              (lambda (filename)
                                (<ra> :add-audiofile filename))))
     ))

  (popup-menu

   (if for-blocks
       (get-block-entries)
       (get-audiofile-entries))

   "---------------"
   
   "Hide"
   (lambda ()
     (<ra> :show-hide-playlist -1)
     )))

(define (have-pauses-in-seqtrack? seqtracknum)
    (let loop ((seqblocks (to-list (<ra> :get-seqblocks-state seqtracknum)))
               (time 0))
      (if (null? seqblocks)
          #f
          (let* ((seqblock (car seqblocks))
                 (start (seqblock :start-time))
                 (end (seqblock :end-time)))
            (if (not (= start time))
                #t
                (loop (cdr seqblocks)
                      end))))))
  
(define (delete-all-pauses-in-seqtrack seqtracknum)
  (define new-seqblocks
    (let loop ((seqblocks (to-list (<ra> :get-seqblocks-state seqtracknum)))
               (time 0))
      (if (null? seqblocks)
          '()
          (let* ((seqblock (car seqblocks))
                 (start (seqblock :start-time))
                 (end (seqblock :end-time))
                 (duration (- end start))
                 (new-end (+ time duration)))
            (cons (copy-hash seqblock
                             :start-time time
                             :end-time new-end)
                  (loop (cdr seqblocks)
                        new-end))))))
  (try-finally
   :try (lambda ()
          (<ra> :create-gfx-seqblocks-from-state new-seqblocks seqtracknum)
          (<ra> :undo-sequencer)
          (<ra> :apply-gfx-seqblocks seqtracknum))
   :failure (lambda ()
              (<ra> :cancel-gfx-seqblocks seqtracknum)))
  )

#!
(pp (delete-all-pauses-in-seqtrack 1))
!#

(define (get-delete-all-pauses-menu-entry seqtracknum)
  (list
   "Delete all pauses"
   :enabled (have-pauses-in-seqtrack? seqtracknum)
   (lambda ()
     (delete-all-pauses-in-seqtrack seqtracknum))))

(define (FROM_C-show-playlist-popup-menu)
  (define seqtracknum (<ra> :get-curr-seqtrack))
  (popup-menu
   (get-delete-all-pauses-menu-entry seqtracknum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load song, "are you sure?" requester 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define (create-are-you-sure?-loading-song-requester callback)
;;  (define gui (<gui> :vertical-layout))
  


(define (place-to-string place)
  (if (eq? 'same-place place)
      'same-place
      (<-> (* 1.0 place))))

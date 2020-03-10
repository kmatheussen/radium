
(provide 'notem.scm)

(my-require 'notes.scm)
(my-require 'keybindings.scm)
(my-require 'randomize-note-durations.scm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit tab in the lower tabs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define first-time? (not (defined? '*notem-gui*)))




;;    HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;


(define (notem-group-name name qualifier)
  name)
;;  (<-> name "  -  " (get-displayable-qualifier qualifier) ""))
  

(define (create-under-construction)
  (mid-horizontal-layout (<gui> :text "Under construction.")))


(define (create-notem-layout . elements)
  (define ret (<gui> :horizontal-layout))
  (for-each (lambda (element)
              (<gui> :add ret element))
            elements)
  (<gui> :set-layout-spacing ret 18 9 0 9 0)
  (<gui> :set-size-policy ret #t #t)
  ret)

(define (create-notem-flow-layout . elements)
  (define vertical-layout (<gui> :vertical-layout))
  (<gui> :add vertical-layout (<gui> :widget 3 3) 1)
  (define flow-layout (<gui> :flow-layout))
  (for-each (lambda (element)
              (<gui> :add flow-layout element))
            elements)
  (<gui> :add vertical-layout flow-layout 3)
  (<gui> :add vertical-layout (<gui> :widget 3 3) 3)
  
  (<gui> :scroll-area #t #t vertical-layout))
  
  


;;        MAIN EDIT TAB
;;;;;;;;;;;;;;;;;;;;;;;;;


(define-constant *notem-gui* (if first-time?
                                 (begin
                                   (let ((gui (my-tabs #t)))
                                     (<gui> :set-static-toplevel-widget gui #t)
                                     
                                     ;; Just hide window when closing it.
                                     (<gui> :add-close-callback gui
                                            (lambda (radium-runs-custom-exec)
                                              ;;(<gui> :set-parent *notem-gui* -3)
                                              (c-display "              GAKK GAKK GAKK")
                                              (<gui> :hide *notem-gui*)
                                              #f))
                                     gui))
                                 *notem-gui*))


(define (add-notem-tab name gui)

  (<gui> :add-tab *notem-gui* name gui)

  ;;(reopen-gui-at-curr-pos *notem-gui*)
  ;;(<gui> :update *notem-gui*)
  )

#!!
(add-notem-tab "testing2" (<gui> :button "hello hello2"))
(<gui> :set-background-color *notem-gui* "blue")
!!#



;;          TRANSPOSE
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-transpose-buttons groupname ra-funcname)
  (define funcname-contains-range (string-contains? ra-funcname "range"))
  (define ra-func (eval-string ra-funcname))
  (define (func . args)
    (if (and funcname-contains-range
             (not (<ra> :has-range)))
        (show-async-message :text "No range in block. Select range by using Left Meta + b")
        (apply ra-func args)))
  
  (define (create-button how-much)
    (define arrow (if (> how-much 0) "Up" "Down")) ;; "↑" "↓"))
    (define button (<gui> :button
                          ""
                          (lambda ()
                            (func how-much))))
    (define (set-button-text!)
      (<gui> :set-text button (let ((a (get-displayable-keybinding ra-funcname (list how-much))))
                                (if (string=? "" a)
                                    "Click me"
                                    a))))

    (set-button-text!)
    
    (define gui (<gui> :group (<-> arrow " " (abs how-much) ": ")
                       button))

    (define (reloaded-keybinding-callback)
      (if (not (<gui> :is-open gui))
          (remove-reload-keybindings-callback reloaded-keybinding-callback)
          (set-button-text!)))
    
    (add-reload-keybindings-callback reloaded-keybinding-callback)

    (add-keybinding-configuration-to-gui gui ra-funcname (list how-much))

    gui)

  (define horizontal (<gui> :horizontal-layout
                            (<gui> :vertical-layout
                                   (create-button 1)
                                   (create-button -1))
                            (<gui> :vertical-layout
                                   (create-button 7)
                                   (create-button -7))
                            (<gui> :vertical-layout
                                   (create-button 12)
                                   (create-button -12))))
  (<gui> :set-layout-spacing horizontal 6 9 9 9 9)
  
  (define ret (<gui> :group groupname horizontal))
                     
  (<gui> :set-layout-spacing ret 6 9 0 9 0)
  ;;(<gui> :set-background-color ret "low_background")
  
  ret)

(define *transpose-tab* #f)

(define (create-transpose-notem)
  (define ret (create-notem-flow-layout (create-transpose-buttons (notem-group-name "Note" "ALT_R")    "ra:transpose-note")
                                        (create-transpose-buttons (notem-group-name "Range" "EXTRA_L") "ra:transpose-range")
                                        (create-transpose-buttons (notem-group-name "Track" "ALT_L")   "ra:transpose-track")
                                        (create-transpose-buttons (notem-group-name "Block" "CTRL_L")  "ra:transpose-block")))
  (set! *transpose-tab* ret)
  ret)


(when (not first-time?)
  ;;(<gui> :show (create-transpose-notem))
  #t
  )





;;        Various
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define *various-tab* #f)



;; shuffle

(define (shuffle-notes notes duration shuffle-func)
  
  (define (pitch-in-range note pitch)
    (define place (+ (note :place)
                     (pitch :place)))
    (and (>= place 0)
         (< place duration)))
             
  (define (get-pitch-values notes)
    (keep (lambda (pitch)
            (> pitch 0))
          (let loop ((notes notes))
            (if (null? notes)
                '()
                (let ((note (car notes)))
                  (if (>= (note :place) duration)
                      (begin
                        (if (not (<ra> :release-mode))
                            (assert #f))
                        '())
                      (append (map (lambda (pitch)
                                     (pitch :value))
                                   (keep (lambda (pitch)
                                           (pitch-in-range note pitch))
                                         (note :pitches)))
                              (loop (cdr notes)))))))))

  (define (replace-pitch-values notes pitch-values)
    (let loop ((notes notes)
               (pitch-values pitch-values))
      (if (null? notes)
          '()
          (let ((note (car notes)))
            ;;(c-display "notestart:" (note :place) ", pitch-values:" (length notes) (length pitch-values) pitch-values)
            (let loop2 ((pitches (note :pitches))
                        (new-pitches '())
                        (pitch-values pitch-values))
              (if (null? pitches)
                  (cons (copy-note note
                                   :pitches (reverse new-pitches))
                        (loop (cdr notes)
                              pitch-values))
                  (let ((pitch (car pitches)))
                    (if (or (= 0 (pitch :value))
                            (not (pitch-in-range note pitch)))
                        (loop2 (cdr pitches)
                               (cons pitch new-pitches)
                               pitch-values)
                        (loop2 (cdr pitches)
                               (cons (copy-pitch pitch
                                                 :value (car pitch-values))
                                     new-pitches)
                               (cdr pitch-values))))))))))

  ;;(c-display "pitch-values:" (get-pitch-values notes))
  (replace-pitch-values notes (shuffle-func (get-pitch-values notes))))



;; light shuffle
;;;;;;;;;;;;;;;;

(define *default-shuffle-chance* 0.5)

(define (shuffle-notes! area chance)
  (undo-editor-area area)
  (define start-place (area :start-place))
  (define end-place (area :end-place))
  (replace-notes! (map (lambda (tracknum track-notes)
                         (shuffle-notes track-notes (- end-place start-place) (lambda (seq)
                                                                                (light-shuffle seq chance))))
                       (integer-range (area :start-track) (1- (area :end-track)))
                       (get-area-notes area))
                  area))

(delafina (shuffle-range :chance *default-shuffle-chance* :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (shuffle-notes! (get-ranged-editor-area blocknum) chance)))

(delafina (shuffle-track :chance *default-shuffle-chance* :tracknum -1 :blocknum -1)
  (shuffle-notes! (get-track-editor-area tracknum blocknum) chance))

(delafina (shuffle-block :chance *default-shuffle-chance* :blocknum -1)
  (shuffle-notes! (get-block-editor-area blocknum) chance))



;; distribute notes evenly
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *default-keep-note-durations* #f)

(define (distribute-notes-evenly! area keep-note-durations)
  (undo-editor-area area)
  (define track-notes (map (lambda (track-notes)
                             (let ((new-note-length (/ (- (area :end-place) (area :start-place))
                                                       (max 1 (length track-notes)))))
                               (map (lambda (i note)
                                      (let ((note (copy-note note
                                                             :place (* i new-note-length))))
                                        (if keep-note-durations
                                            note
                                            (set-new-note-end note
                                                              new-note-length))))
                                    (iota (length track-notes))
                                    track-notes)))
                           (get-area-notes area)))  
  ;;(c-display (pp track-notes))
  (replace-notes! track-notes area)
  )


(delafina (distribute-range-evenly :keep-note-durations *default-keep-note-durations* :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (distribute-notes-evenly! (get-ranged-editor-area blocknum) keep-note-durations)))

#!!
(distribute-range-evenly #t)
(distribute-range-evenly #f)
!!#

(delafina (distribute-track-evenly :keep-note-durations *default-keep-note-durations* :tracknum -1 :blocknum -1)
  (distribute-notes-evenly! (get-track-editor-area tracknum blocknum) keep-note-durations))

(delafina (distribute-block-evenly :keep-note-durations *default-keep-note-durations*)
  (distribute-notes-evenly! (get-block-editor-area) keep-note-durations))

  
;; heavy shuffle
;;;;;;;;;;;;;;;;

(define (fullshuffle-notes! area chance)
  (undo-editor-area area)
  (define start-place (area :start-place))
  (define end-place (area :end-place))
  (replace-notes! (map (lambda (tracknum track-notes)
                         (shuffle-notes track-notes (- end-place start-place) random-shuffle))
                       (integer-range (area :start-track) (1- (area :end-track)))
                       (get-area-notes area))
                  area))

(delafina (fullshuffle-range :chance 1 :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (fullshuffle-notes! (get-ranged-editor-area blocknum) chance)))

(delafina (fullshuffle-track :chance 1 :tracknum -1 :blocknum -1)
  ;;(<ra> :play-song-from-start)
  (c-display "fs-track")
  (fullshuffle-notes! (get-track-editor-area tracknum blocknum) chance))

;;(fullshuffle-track)

(delafina (fullshuffle-block :chance 1)
  (fullshuffle-notes! (get-block-editor-area) chance))



;; moduloskew
;;;;;;;;;;;;;

(define (moduloskew-track-notes notes how-much start-place end-place tracknum)
  (set! how-much (/ how-much
                    (<ra> :get-line-zoom-block-ratio)))

  (define duration (- end-place start-place))
  (define was-polyphonic (any? (lambda (note)
                                 (> (<ra> :get-note-subtrack (note :id) tracknum)
                                    0))
                               notes))
  (define notes-moduloed (map (lambda (note)
                                (define place (note :place))
                                (define new-place (modulo (+ place how-much) duration))
                                (copy-note note
                                           :place new-place))
                              notes))
  (define sorted-notes (sort notes-moduloed (lambda (note1 note2)
                                              (< (note1 :place) (note2 :place)))))
  (define (make-monophonic notes)
    (let loop ((notes notes))
      (if (or (null? notes)
              (null? (cdr notes)))
          notes
          (let* ((note1 (car notes))
                 (note1-start (note1 :place))
                 (note1-end (get-note-end note1))
                 (note2 (cadr notes))
                 (note2-start (note2 :place)))
            (cons (if (> note1-end note2-start)
                        (cut-note-keep-start note1 note2-start)
                        note1)
                  (loop (cdr notes)))))))
    
  (if was-polyphonic
      sorted-notes ;; We don't force a polyphonic track to be monophonic.
      (make-monophonic sorted-notes)))
        
(define (moduloskew-notes! area how-much)
  (undo-editor-area area)
  (define start-place (area :start-place))
  (define end-place (area :end-place))
  (replace-notes! (map (lambda (tracknum track-notes)
                         (moduloskew-track-notes track-notes how-much start-place end-place tracknum))
                       (integer-range (area :start-track) (1- (area :end-track)))
                       (get-area-notes area
                                       :include-starting-before #f))
                  area
                  :include-starting-before #f))

(delafina (moduloskew-range :how-much 1 :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (moduloskew-notes! (get-ranged-editor-area blocknum) how-much)))

(delafina (moduloskew-track :how-much 1 :tracknum -1 :blocknum -1)
  (moduloskew-notes! (get-track-editor-area tracknum blocknum) how-much))

(delafina (moduloskew-block :how-much 1)
  (moduloskew-notes! (get-block-editor-area) how-much))


(define *default-microtonal-randomize-pitch* #f)

(define (replace-with-random-pitches! area)  
  (undo-editor-area area)  
  (replace-notes! (map-area-notes (get-area-notes area)
                                  (lambda (note)
                                    (define (changepitch pitch)
                                      (if (in-editor-area (+ (area :start-place)
                                                             (note :place)
                                                             (pitch :place))
                                                          :area area)
                                          (begin
                                            (define pitchvalue (pitch :value))
                                            (define range (* 12 (- 1 (sqrt (myrand 0 1))))) ;; A number between 0 and 12, but on average closer to 0.
                                            (define max-pitch (min 127 (+ pitchvalue range)))
                                            (define min-pitch (max 1 (- pitchvalue range)))
                                            (copy-pitch pitch :value (let ((r (myrand min-pitch max-pitch)))
                                                                       (if *default-microtonal-randomize-pitch*
                                                                           r
                                                                           (between 1 (round r) 127)))))
                                                                         
                                          pitch))
                                    (c-display "NOTE2:" note)
                                    (copy-note note
                                               :pitches (if (= 0 ((last (note :pitches)) :value))
                                                            (map-butlast (note :pitches)
                                                                         changepitch)
                                                            (map changepitch (note :pitches))))))
                  area))

(delafina (replace-with-random-notes-in-range :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (replace-with-random-pitches! (get-ranged-editor-area blocknum))))

(delafina (replace-with-random-notes-in-track :tracknum -1 :blocknum -1)
  (replace-with-random-pitches! (get-track-editor-area tracknum blocknum)))

(define (replace-with-random-notes-in-block)
  (replace-with-random-pitches! (get-block-editor-area)))

#!!
(* 5 0.5)
(/ 5 0.5)
(replace-with-random-notes-in-track)
(<ra> :get-num-notes)
!!#

(define (replace-with-random-velocities! area)  
  (undo-editor-area area)  
  (replace-notes! (map-area-notes (get-area-notes area)
                                  (lambda (note)
                                    (define (changevelocity velocity)
                                      (if (in-editor-area (+ (area :start-place)
                                                             (note :place)
                                                             (velocity :place))
                                                          :area area)
                                          (begin
                                            (define velocityvalue (velocity :value))
                                            (define range (* 1.0 (- 1 (sqrt (myrand 0 1))))) ;; A number between 0 and 1.0, but on average closer to 0.
                                            (define max-velocity (min 1.0 (+ velocityvalue range)))
                                            (define min-velocity (max 0.0 (- velocityvalue range)))
                                            (copy-velocity velocity :value (myrand min-velocity max-velocity)))
                                          velocity))                                    
                                    (copy-note note
                                               :velocities (map changevelocity (note :velocities)))))
                  area))

(delafina (replace-with-random-velocities-in-range :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (replace-with-random-velocities! (get-ranged-editor-area blocknum))))

(delafina (replace-with-random-velocities-in-track :tracknum -1 :blocknum -1)
  (replace-with-random-velocities! (get-track-editor-area tracknum blocknum)))

(define (replace-with-random-velocities-in-block)
  (replace-with-random-velocities! (get-block-editor-area)))




;; Randomize note positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delafina (randomize-note-durations-range :blocknum -1)
  (randomize-note-durations! (get-ranged-editor-area blocknum)))

(delafina (randomize-note-durations-track :tracknum -1 :blocknum -1)
  (randomize-note-durations! (get-track-editor-area tracknum blocknum)))

(define (randomize-note-durations-block)
  (randomize-note-durations! (get-block-editor-area)))


;; Randomize pitch octave

(define (set-random-octave-pitches! area  min-oct max-oct)
  (undo-editor-area area)  
  (replace-notes! (map-area-notes (get-area-notes area)
                                  (lambda (note)
                                    (define (changepitch pitch)
                                      (if (in-editor-area (+ (area :start-place)
                                                             (note :place)
                                                             (pitch :place))
                                                          :area area)
                                          (begin
                                            (define pitchvalue (pitch :value))
                                            (define note pitchvalue)
                                            (define octave (floor (/ note 12)))
                                            (define chroma (- note (* octave 12)))                                            
                                            (define max-note (+ 120 chroma))
                                            (if (> max-note 127)
                                                (set! max-note (- max-note 12)))
                                            ;;(c-display "chroma:" chroma)
                                            (copy-pitch pitch :value (between (if (< chroma 0.01)
                                                                                  (+ chroma 12)
                                                                                  chroma)
                                                                              (+ pitchvalue (* 12 (floor (myrand min-oct (+ 1 max-oct)))))
                                                                              max-note)))
                                          pitch))                                    
                                    (copy-note note
                                               :pitches (if (= 0 ((last (note :pitches)) :value))
                                                            (map-butlast (note :pitches)
                                                                         changepitch)
                                                            (map changepitch (note :pitches))))))
                  area))

(define *default-randomize-min-octave* -1)
(define *default-randomize-max-octave* 1)

(delafina (set-random-octaves-for-notes-in-range  :min-oct *default-randomize-min-octave* :max-oct *default-randomize-max-octave* :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (set-random-octave-pitches! (get-ranged-editor-area blocknum) min-oct max-oct)))

(delafina (set-random-octaves-for-notes-in-track :min-oct *default-randomize-min-octave* :max-oct *default-randomize-max-octave* :tracknum -1 :blocknum -1)
  (set-random-octave-pitches! (get-track-editor-area tracknum blocknum) min-oct max-oct))

(delafina (set-random-octaves-for-notes-in-block :min-oct *default-randomize-min-octave* :max-oct *default-randomize-max-octave*)
  (set-random-octave-pitches! (get-block-editor-area) min-oct max-oct))

#!!
(set-random-octaves-for-notes-in-block -2 3)
!!#

;; randomly-delete
;;;;;;;;;;;;;;;;;;;
 
(define *default-randomly-delete-chance* 0.2)

;; lengthen notes?
(define (randomly-delete-notes notes chance)  
  (let loop ((notes notes))
    (cond ((null? notes)
           notes)
          ((< (myrand 0 1) chance)
           (loop (cdr notes)))
          (else
           (cons (car notes)
                 (loop (cdr notes)))))))


(define (randomly-delete-notes! area chance)
  (undo-editor-area area)
  (define start-place (area :start-place))
  (define end-place (area :end-place))
  (replace-notes! (map (lambda (track-notes)
                         (randomly-delete-notes track-notes chance))
                       (get-area-notes area :include-starting-before #f))
                  area))

(delafina (randomly-delete-notes-range :chance *default-randomly-delete-chance* :blocknum -1)
  (if (not (<ra> :has-range blocknum))
      (show-async-message :text "No range in block. Select range by using Left Meta + b")
      (randomly-delete-notes! (get-ranged-editor-area blocknum) chance)))

(delafina (randomly-delete-notes-track :chance *default-randomly-delete-chance* :tracknum -1 :blocknum -1)
  (randomly-delete-notes! (get-track-editor-area tracknum blocknum) chance))

(delafina (randomly-delete-notes-block :chance *default-randomly-delete-chance* :blocknum -1)
  (randomly-delete-notes! (get-block-editor-area blocknum) chance))




;; Randomize/skew/shuffle tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *randomize/skew-notem-tab* #f)

(define (create-randomize/skew-notem)

  (define random-layout (create-notem-layout (<gui> :checkbox "Microtonal" *default-microtonal-randomize-pitch* #f
                                                    (lambda (is-on)
                                                      (set! *default-microtonal-randomize-pitch* is-on)))
                                             (create-keybinding-button "Range" "ra:eval-scheme" '("(replace-with-random-notes-in-range)"))
                                             (create-keybinding-button "Track" "ra:eval-scheme" '("(replace-with-random-notes-in-track)"))
                                             (create-keybinding-button "Block" "ra:eval-scheme" '("(replace-with-random-notes-in-block)"))))

  (define random-velocities-layout (create-notem-layout (create-keybinding-button "Range" "ra:eval-scheme" '("(replace-with-random-velocities-in-range)"))
                                                        (create-keybinding-button "Track" "ra:eval-scheme" '("(replace-with-random-velocities-in-track)"))
                                                        (create-keybinding-button "Block" "ra:eval-scheme" '("(replace-with-random-velocities-in-block)"))))

  (define moduloskew-notes-layout (create-notem-layout (<gui> :vertical-layout
                                                              (create-keybinding-button "Range Up" "ra:eval-scheme" '("(moduloskew-range -1)"))
                                                              (create-keybinding-button "Range Down" "ra:eval-scheme" '("(moduloskew-range 1)")))
                                                       (<gui> :vertical-layout
                                                              (create-keybinding-button "Track Up" "ra:eval-scheme" '("(moduloskew-track -1)"))
                                                              (create-keybinding-button "Track Down" "ra:eval-scheme" '("(moduloskew-track 1)")))
                                                       (<gui> :vertical-layout
                                                              (create-keybinding-button "Block Up" "ra:eval-scheme" '("(moduloskew-block -1)"))
                                                              (create-keybinding-button "Block Down" "ra:eval-scheme" '("(moduloskew-block 1)")))))

  (define shuffle-notes-layout (create-notem-layout (<gui> :horizontal-int-slider "chance %: "
                                                           0 (floor (* 100 *default-shuffle-chance*)) 100
                                                           (lambda (val)
                                                             (set! *default-shuffle-chance* (/ val 100))))
                                                    (<gui> :vertical-layout
                                                           (create-keybinding-button "Range" "ra:eval-scheme" '("(shuffle-range)")))
                                                    (<gui> :vertical-layout
                                                           (create-keybinding-button "Track" "ra:eval-scheme" '("(shuffle-track)")))
                                                    (<gui> :vertical-layout
                                                           (create-keybinding-button "Block" "ra:eval-scheme" '("(shuffle-block)")))))
  
  (define fullshuffle-notes-layout (create-notem-layout (<gui> :vertical-layout
                                                               (create-keybinding-button "Range" "ra:eval-scheme" '("(fullshuffle-range)")))
                                                        (<gui> :vertical-layout
                                                               (create-keybinding-button "Track" "ra:eval-scheme" '("(fullshuffle-track)")))
                                                        (<gui> :vertical-layout
                                                               (create-keybinding-button "Block" "ra:eval-scheme" '("(fullshuffle-block)")))))

  (define randomize-note-durations-layout (create-notem-layout (<gui> :vertical-layout
                                                                    (create-keybinding-button "Range" "ra:eval-scheme" '("(randomize-note-durations-range)")))
                                                             (<gui> :vertical-layout
                                                                    (create-keybinding-button "Track" "ra:eval-scheme" '("(randomize-note-durations-track)")))
                                                             (<gui> :vertical-layout
                                                                    (create-keybinding-button "Block" "ra:eval-scheme" '("(randomize-note-durations-block)")))))
  
  (define randomly-delete-notes-layout (create-notem-layout (let ((slider (<gui> :horizontal-int-slider "chance %: "
                                                                                 0 (floor (* 100 *default-randomly-delete-chance*)) 100
                                                                                 (lambda (val)
                                                                                   (set! *default-randomly-delete-chance* (/ val 100))))))
                                                              ;;(<gui> :set-min-width slider (<gui> :text-width "chance %: "))
                                                              slider)
                                                            (<gui> :vertical-layout
                                                                   (create-keybinding-button "Range" "ra:eval-scheme" '("(randomly-delete-notes-range)")))
                                                            (<gui> :vertical-layout
                                                                   (create-keybinding-button "Track" "ra:eval-scheme" '("(randomly-delete-notes-track)")))
                                                            (<gui> :vertical-layout
                                                                   (create-keybinding-button "Block" "ra:eval-scheme" '("(randomly-delete-notes-block)")))))
  
  (define ret (create-notem-flow-layout (<gui> :group "Randomize pitch" random-layout)
                                        (<gui> :group "Randomize velocities" random-velocities-layout)
                                        (<gui> :group "Randomize note positions and durations" randomize-note-durations-layout)
                                        (<gui> :group "Randomly delete notes" randomly-delete-notes-layout)
                                        (<gui> :group "Modulo skew" moduloskew-notes-layout)
                                        (<gui> :group "Lightly shuffle pitches" shuffle-notes-layout)
                                        (<gui> :group "Heavily shuffle pitches" fullshuffle-notes-layout)
                                        ))

  (set! *randomize/skew-notem-tab* ret)
  ret)


(define (create-various-notem)

  (define lines-layout (create-notem-layout (create-keybinding-button (notem-group-name "Range" "EXTRA_L") "ra:expand-range")
                                            (create-keybinding-button (notem-group-name "Block" "CTRL_L")  "ra:expand-block")))
  
  
  (define pitches-layout (create-notem-layout (create-keybinding-button (notem-group-name "Range" "EXTRA_L") "ra:pexpand-range")
                                              (create-keybinding-button (notem-group-name "Track" "ALT_L") "ra:pexpand-track")
                                              (create-keybinding-button (notem-group-name "Block" "CTRL_L")  "ra:pexpand-block")))
  
  (define invert-layout (create-notem-layout (create-keybinding-button (notem-group-name "Range" "EXTRA_L") "ra:invert-range")
                                             (create-keybinding-button (notem-group-name "Track" "ALT_L") "ra:invert-track")
                                             (create-keybinding-button (notem-group-name "Block" "CTRL_L")  "ra:invert-block")))
  
  
  (define backwards-layout (create-notem-layout (create-keybinding-button (notem-group-name "Range" "EXTRA_L") "ra:backwards-range")
                                                (create-keybinding-button (notem-group-name "Track" "ALT_L") "ra:backwards-track")
                                                (create-keybinding-button (notem-group-name "Block" "CTRL_L")  "ra:backwards-block")))
  
  (define glissando-layout (create-notem-layout (create-keybinding-button "Apply glissando between two notes" "ra:glissando")))
  
  (define monophonic-layout (create-notem-layout (create-keybinding-button "Make track monophonic" "ra:make-track-monophonic")
                                                 (create-keybinding-button "Split track into several monophonic tracks" "ra:split-track-into-monophonic-tracks")))

  (define distribute-notes-evenly-layout (create-notem-layout (<gui> :checkbox "Keep note durations"
                                                                     *default-keep-note-durations*
                                                                     (lambda (val)
                                                                       (set! *default-keep-note-durations* val)))
                                                              (<gui> :vertical-layout
                                                                     (create-keybinding-button "Range" "ra:eval-scheme" '("(distribute-range-evenly)")))
                                                              (<gui> :vertical-layout
                                                                     (create-keybinding-button "Track" "ra:eval-scheme" '("(distribute-track-evenly)")))
                                                              (<gui> :vertical-layout
                                                                     (create-keybinding-button "Block" "ra:eval-scheme" '("(distribute-block-evenly)")))))

  (define distribute-notes-evenly-layout (create-notem-layout (<gui> :checkbox "Keep note durations"
                                                                     *default-keep-note-durations*
                                                                     (lambda (val)
                                                                       (set! *default-keep-note-durations* val)))
                                                              (<gui> :vertical-layout
                                                                     (create-keybinding-button "Range" "ra:eval-scheme" '("(distribute-range-evenly)")))
                                                              (<gui> :vertical-layout
                                                                     (create-keybinding-button "Track" "ra:eval-scheme" '("(distribute-track-evenly)")))
                                                              (<gui> :vertical-layout
                                                                     (create-keybinding-button "Block" "ra:eval-scheme" '("(distribute-block-evenly)")))))

  (define set-random-octaves-for-notes-layout (create-notem-layout (<gui> :text "min/max:")
                                                                   (<gui> :int-text -10 *default-randomize-min-octave* 10 (lambda (val)
                                                                                                                            (set! *default-randomize-min-octave* val)))
                                                                   (<gui> :int-text -10 *default-randomize-max-octave* 10 (lambda (val)
                                                                                                                            (set! *default-randomize-max-octave* val)))
                                                                   (<gui> :vertical-layout
                                                                          (create-keybinding-button "Range" "ra:eval-scheme" '("(set-random-octaves-for-notes-in-range)")))
                                                                   (<gui> :vertical-layout
                                                                          (create-keybinding-button "Track" "ra:eval-scheme" '("(set-random-octaves-for-notes-in-track)")))
                                                                   (<gui> :vertical-layout
                                                                          (create-keybinding-button "Block" "ra:eval-scheme" '("(set-random-octaves-for-notes-in-block)")))))
    
  (define ret (create-notem-flow-layout (<gui> :group "Expand/shrink Pitch" pitches-layout)
                                        (<gui> :group "Expand/shrink Lines" lines-layout)
                                        (<gui> :group "Invert Pitches" invert-layout)
                                        (<gui> :group "Reverse notes" backwards-layout)
                                        (<gui> :group "Glissando" glissando-layout)
                                        (<gui> :group "Polyphonic tracks" monophonic-layout)
                                        (<gui> :group "Distribute notes evenly" distribute-notes-evenly-layout)
                                        (<gui> :group "Randomize octaves" set-random-octaves-for-notes-layout)                                        
                                        ))
  
  ;;(<gui> :set-size-policy vertical-layout #t #t)
  (set! *various-tab* ret)
  ret)

(when (not first-time?)
  ;;(<gui> :show (create-expand/shrink-notem))
  #t
  )




;;         INIT
;;;;;;;;;;;;;;;;;;;;;;;;;

(define *quantitize-tab* #f)

(define (add-edit-tabs)
  (set! *quantitize-tab* (create-quantitize-gui-for-tab))
  (add-notem-tab "Quantization" *quantitize-tab*)
  (add-notem-tab "Transpose" (create-transpose-notem))
  (add-notem-tab "Randomize/Skew/Shuffle" (create-randomize/skew-notem))
  (add-notem-tab "Various" (create-various-notem))
  ;;(add-notem-tab "More" (mid-vertical-layout (create-under-construction)))
  (if (not (<ra> :release-mode))
      (<gui> :set-current-tab *notem-gui* 2))
  )

(define (replace-edit-tabs)
  (while (> (<gui> :num-tabs *notem-gui*) 0)
    (<gui> :remove-tab *notem-gui* 0))
  (add-edit-tabs))

(if first-time?
    (add-edit-tabs)
    (replace-edit-tabs))


    



;;      DRODL
;;;;;;;;;;;;;;;;;;;;;;;;;



#!!
(<gui> :height *various-tab*)
(<gui> :height *notem-gui*)
(<gui> :height *transpose-tab*)
(<gui> :height *quantitize-tab*)
(<gui> :minimize-as-much-as-possible *transpose-tab*)

(begin
  (define group (<gui> :group "hello"))
  (define widg (<gui> :widget))
  (<gui> :set-background-color widg "low_background")
  (<gui> :add widg (create-transpose-notem) 0 0 500 500)
  (<gui> :show widg))

                   
(define transp (create-transpose-notem))
(<gui> :show transp)
(<gui> :set-background-color transp "low_background")

(add-notem-tab "Transpose" (create-transpose-notem))
(add-notem-tab "Transpose2" (<gui> :flow-layout
                                  (<gui> :button "b1")
                                  (<gui> :button "b2")))

(<gui> :set-style-sheet group
       (<-> "QGroupBox {"
            "background-color: rgba(40, 80, 0, 40);"
            "border: 1px solid rgba(10, 10, ff, 50);"
            "border-radius: 20px;"
            "margin-top: 1.5em;"
            "}"))


(<gui> :set-style-sheet-recursively widg "")

;; main stylesheet:
(<gui> :set-style-sheet-recursively group "QGroupBox {    background-color: rgba(0, 0, 0, 10);    border: 1px solid rgba(10, 10, 10, 50);    border-radius: 2px;    margin-top: 1.5em;}QGroupBox::title {    subcontrol-origin: margin;    padding: 2px 2px;    background-color: transparent;}QScrollArea { background: transparent; }QScrollArea > QWidget > QWidget { background: transparent; }QScrollArea > QWidget > QScrollBar { background: rgba(ff, ff, ff, 50); }QTabBar::pane { border: 0; }")


(<gui> :set-style-sheet-recursively transp
       (<-> "QScrollArea"
            "{"
            "  background-color: transparent;"
            "}"
            "QScrollArea > QWidget > QWidget { background: transparent; }"
            ))

(<gui> :set-style-sheet-recursively transp
       (<-> "QAbstractScrollArea"
            "{"
            "  background-color: transparent;"
            "}"
            ))

(let ((tab (create-transpose-notem)))
  (<gui> :set-style-sheet *notem-gui*
         (<-> "QAbstractScrollArea"
              "{"
              "  background-color: transparent;"
              "}"
              "QWidget#scrollAreaWidgetContents{"
              "  background-color: transparent;"
              "}"
              ))
  (add-notem-tab "Transpose" tab))

(define tabWidget (<gui> :child ui "tabWidget"))
(<gui> :add-tab tabWidget "aiai" (<gui> :flow-layout
                                  (<gui> :button "b1")
                                  (<gui> :button "b2")))
(<gui> :add-tab tabWidget "aiai" (create-transpose-notem))

(define (create-callback-creator func)
  (lambda (how-much)
    (lambda ()
      (func how-much))))

(add-notem-tab "hepp"
       (<gui> :flow-layout              
              (create-transpose-buttons "Note" (create-callback-creator ra:transpose-note))
              (create-transpose-buttons "Range" (create-callback-creator ra:transpose-range))
              (create-transpose-buttons "Track" (create-callback-creator ra:transpose-track))
              (create-transpose-buttons "Block" (create-callback-creator ra:transpose-block))))

(let* ((w (<gui> :horizontal-layout))
       (flow (<gui> :flow-layout              
              (create-transpose-buttons "Note" (create-callback-creator ra:transpose-note))
              (create-transpose-buttons "Range" (create-callback-creator ra:transpose-range))
              (create-transpose-buttons "Track" (create-callback-creator ra:transpose-track))
              (create-transpose-buttons "Block" (create-callback-creator ra:transpose-block)))))
  (<gui> :add w flow)
  (add-notem-tab "hepp" w))
       


!!#


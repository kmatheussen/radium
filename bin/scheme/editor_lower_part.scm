(provide 'editor_lower_part.scm)

(my-require 'gui.scm)
(my-require 'area.scm)

#!!
(<ra> :get-editor-x1)
(<ra> :get-editor-x2)
(<ra> :get-reltempo-slider-x1)
(<ra> :get-track-slider-x2)
(<ra> :get-reltempo)
!!#

(define (reset-tempo-multiplier)
  (<ra> :undo-reltempo)
  (<ra> :set-reltempo 1.0))


(define (apply-tempo-multiplier-to-block)
  (define (apply-it callback)
    (let* ((reltempo (<ra> :get-reltempo))
           (bpms (get-BPMs))
           (scale-bpm (lambda (bpm)
                        (round (* reltempo bpm)))))
      (for-each (lambda (place-and-bpm)
                  (let ((place (car place-and-bpm))
                        (bpm (cadr place-and-bpm)))
                    (callback (scale-bpm bpm) place)))
                bpms)
      (if (or (null? bpms)
              (> (car (car bpms)) 0))
          (callback (scale-bpm (<ra> :get-main-bpm)) 0))))
  (define lowest 1000)
  (define highest 0)
  (apply-it (lambda (value place)
              (set! highest (max value highest))
              (set! lowest (min value lowest))))
  (cond ((> highest 999)
         (show-async-message (<gui> :get-editor-gui)
                             (<-> "Can not set BPM higher than 999. (" highest ")")))
        ((< lowest 1)
         (show-async-message (<gui> :get-editor-gui)
                             (<-> "Can not set BPM lower than 1. (" lowest ")")))
        (else
         (undo-block
          (lambda ()
            (apply-it (lambda (value place)
                        (<ra> :add-bpm value place)))
            (reset-tempo-multiplier))))))

#!!
(define (apply-bpm-glide bpmnum)
  (undo-block
   (lambda ()
     (define bpms (get-BPMs))
     (define bpm1 (list-ref bpms bpmnum))
     (define bpm2 (list-ref bpms (1+ bpmnum)))
     (define temponodes (get-temponodes))
     (set! temponodes (nodelist-add-same-value-at-place (car bpm1) 0))
     (set! temponodes (nodelist-add-same-value-at-place (car bpm2) 0))
     (set! temponodes (nodelist-mix temponodes (list (create-node (car bpm1) 0)
                                                     (create-node (-line (car bpm2)) 2)))) ;; Fix 2.
     )))
!!#

(define (show-reltempo-slider-popup-menu)
  (define reltempo (<ra> :get-reltempo))
  (popup-menu (list "Reset"
                    :enabled (not (= 1.0 reltempo))
                    reset-tempo-multiplier)
              (list "Apply tempo (and Reset)"
                    :enabled (not (= 1.0 reltempo))
                    apply-tempo-multiplier-to-block)
              (list "Add MIDI learn"
                    :enabled (not (<ra> :has-block-multiplier-midi-learn))
                    ra:add-block-multiplier-midi-learn)
              (list "Remove MIDI learn"
                    :enabled (<ra> :has-block-multiplier-midi-learn)
                    ra:remove-block-multiplier-midi-learn)))

(define *ignore-reconf* #f)

(def-area-subclass (<reltempo-slider> :gui :x1 :y1 :x2 :y2)

  (define blocknum -1)
  (define has-made-undo #f)
  
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (set! has-made-undo #t)
      (<ra> :undo-reltempo blocknum)))

  (define* (get-curr-slider-value)
    (define reltempo (<ra> :get-reltempo))
    (if (< reltempo 1.0) ;; check if this is correct
        (scale reltempo 0 1 0.0 0.5)
        (scale reltempo 1 6 0.5 1.0)))

  (define (get-reltempo slider-value)
    (if (< slider-value 0.5)
        (scale slider-value 0 0.5 0 1)
        (scale slider-value 0.5 1.0 1 6)))

  (define (get-curr-statusbar-text)
    (<-> "Tempo multiplier: " (two-decimal-string (<ra> :get-reltempo))))
  
  (define-override (paint)
    (paint-horizontal-slider gui
                             (get-curr-slider-value)
                             (get-curr-statusbar-text)
                             (+ 1 x1) (+ y1 0.0)
                             (- x2 1) (- y2 0.0)
                             :color "tempo_multiplier_slider";color12"
                             :color2 "black" ;#f ;;(<gui> :mix-colors "color0" "black" 0.7)
                             :text-color "tempo_multiplier_slider_text"
                             :border-color "#222222"
                             :border-width 0
                             :cut-text-to-fit #f
                             :wrap-lines #f
                             )
    )
  
  (define start-mouse-value #f)

  (add-statusbar-text-handler "Block tempo multiplier: Right-click for options")
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set! has-made-undo #f)
     (cond ((= button *right-button*)
            (show-reltempo-slider-popup-menu)
            'eat-mouse-cycle)
           ((= button *left-button*)
            (set! *ignore-reconf* #t)
            (set! blocknum (<ra> :current-block))
            (set! start-mouse-value (get-curr-slider-value))
            (set-statusbar-text! (get-curr-statusbar-text))
            #t)
           (else
            #f)))
   (lambda (button x* y* dx dy)
     (assert start-mouse-value)
     (maybe-make-undo)
     (define slider-value (between 0 (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                   1))
     (<ra> :set-reltempo (get-reltempo slider-value) blocknum)
     (set-statusbar-text! (get-curr-statusbar-text))
     (update-me!)
     )
   (lambda (button x* y* dx dy)
     ;;(c-display "release button/x/y" x* y*)
     (set! *ignore-reconf* #f)
     #f
     ))
  )

#!!
(<ra> :set-track-slider-pos 0.5)
(<ra> :get-track-slider-pos)
(<ra> :get-track-x1 0)
(<ra> :get-track-x2 0)
(<ra> :get-track-x1 1)
!!#  

(define (create-editor-track-scrollbar-area gui x1 y1 x2 y2)

  (define slider-color "track_slider")
 
  (define (get-slider-length)
    (define total-width (- (<ra> :get-track-x2 (- (<ra> :get-num-tracks) 1))                           
                           (<ra> :get-track-x1 0)))
    (define editor-width (- x2 x1))
    (/ editor-width total-width))
  
  (<new> :scrollbar gui x1 y1 x2 y2
         (lambda (slider-pos1 slider-pos2)
           (define new-value (between 0
                                      slider-pos1
                                      (- 1.0 (get-slider-length))))
           ;;(c-display "new-value:" new-value ". pos1/pos2:" (two-decimal-string slider-pos1) (two-decimal-string slider-pos2))
           (<ra> :set-track-slider-pos new-value)
           )
                      
         :slider-pos (<ra> :get-track-slider-pos) ;;(scale (<ra> :get-track-slider-scroller-x1) x1 x2 0 1)
         :slider-length get-slider-length
         :vertical #f
         :background-color #f
         :border-color slider-color
         :slider-color slider-color
         :slider-pressed-color (<gui> :mix-colors "#010101" slider-color 0.5)
         :border-width 2
         :mouse-press-callback (lambda ()
                                 (set! *ignore-reconf* #t))
         :mouse-release-callback (lambda ()
                                   (set! *ignore-reconf* #f))
         )

  )

;; Note: used as shortcut
(define (set-current-block)
  (popup-menu
   (map (lambda (blocknum)
          (list (<-> blocknum ": " (<ra> :get-block-name blocknum))
                (lambda ()
                  (<ra> :select-block blocknum))))
        (iota (<ra> :get-num-blocks)))))

(define (create-editor-lock-checkbox gui x1 y1 x2 y2)
  (define enabled #t)
  (define-optional-func area (key . args))
  
  (set! area (<new> :checkbox gui x1 y1 x2 y2
                    (lambda ()
                      (not (<ra> :allow-automatically-changing-current-block)))
                    (lambda (val)
                      (<ra> :set-allow-automatically-changing-current-block (not val)))
                    ;;:paint-func
                    ;;(lambda (gui x1 y1 x2 y2 is-selected is-hovering)
                    :text (lambda ()
                            (if (<ra> :allow-automatically-changing-current-block)
                                "unlocked.svg"
                                "locked.svg"))
                    ;;"🔓" "🔒"
                    :selected-color "#225522"
                    :prepend-checked-marker #f
                    :right-mouse-clicked-callback
                    (lambda ()
                      (popup-menu
                       (list
                        (list "Set current block" set-current-block)
                        "--------------Keybindings"
                        (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-allow-automatically-changing-current-block"
                                                                         :args '()
                                                                         :focus-keybinding "FOCUS_EDITOR"
                                                                         :gui-or-area area)
                        "Help keybindings" show-keybinding-help-window)))))
  
  (area :add-statusbar-text-handler "Prevent program from automatically changing block")
  
  area)


#!!
(c-display "hepp:" (<ra> :allow-automatically-changing-current-block))
!!#

(define (create-editor-midi-record-checkbox gui x1 y1 x2 y2)
  (define area (<new> :checkbox gui x1 y1 x2 y2
                      ra:record-accurately-from-midi
                      ra:set-record-accurately-from-midi
                      :text "R"
                      :selected-color "red"
                      ))
  (area :add-statusbar-text-handler "Enable polyphonic and time-accurate recording from MIDI")
  (add-keybinding-configuration-to-gui area
                                       "ra:switch-record-accurately-from-midi"
                                       '())
  area)



(def-area-subclass (<editor-lower-part-area> :gui :x1 :y1 :x2 :y2)
  (define (add-sub-areas!)
    (define reltempo-x1 (<ra> :get-reltempo-slider-x1))
    (define reltempo-x2 (<ra> :get-reltempo-slider-x2))
    
    (define track-x1 (<ra> :get-track-slider-x1))
    (define track-x2 (<ra> :get-track-slider-x2))

    (define lock-x2 (/ reltempo-x1 2))
    
    (add-sub-area-plain! (create-editor-lock-checkbox gui 0 y1 lock-x2 y2))
    (add-sub-area-plain! (create-editor-midi-record-checkbox gui lock-x2 y1 reltempo-x1 y2))
    (add-sub-area-plain! (<new> :reltempo-slider gui reltempo-x1 y1 reltempo-x2 y2))
    (add-sub-area-plain! (create-editor-track-scrollbar-area gui track-x1 y1 track-x2 y2)))

  (add-sub-areas!)

  (add-method! :reconfigure!
               (lambda ()
                 (when (not *ignore-reconf*)
                   ;;(c-display "RECONF " (<ra> :get-track-slider-x1))
                   (remove-sub-areas!)
                   (add-sub-areas!)
                   (update-me!) ;; If not, background between sub areas are not updated.
                   )))

  ;; Workaround. TODO: Investigate why this is necessary.
  (define-override (paint)
    (<gui> :filled-box gui "high_background" x1 y1 x2 y2 -1 -1 *no-gradient*))
  )

(define *last-created-editor-lower-part-area* #f)

(define (FROM_C-create-editor-lower-part-gui)
  (define qtarea (make-qtarea :width (floor (- (<ra> :get-editor-x2) (<ra> :get-editor-x1))) :height (floor (- (<ra> :get-track-slider-y2) (<ra> :get-track-slider-y1)))
                              :sub-area-creation-callback (lambda (gui width height state)
                                                            (set! *last-created-editor-lower-part-area*
                                                                  (<new> :editor-lower-part-area gui 0 0 width height))
                                                            *last-created-editor-lower-part-area*)
                              :enable-mouse-callbacks #t))
  (define gui (qtarea :get-gui))

  ;; TODO: Investigate why this doesn't work on OSX.
  ;;(<gui> :set-background-color gui "high_background")
  
  (<gui> :dont-autofill-background gui)
  gui)

(define (FROM_C-reconfigure-editor-lower-part-gui!)
  (if *last-created-editor-lower-part-area*
      (*last-created-editor-lower-part-area* :reconfigure!)))
  
(define *temp-test-area* (if (defined? '*temp-test-area*) *temp-test-area* #f))
(when (defined? '*editor-lower-part-has-loaded*)
  (when *temp-test-area*
    (c-display "CLOSING" *temp-test-area*)
    (if (<gui> :is-open *temp-test-area*)
        (<gui> :close *temp-test-area*))
    (set! *temp-test-area* #f))
  (set! *temp-test-area* (FROM_C-create-editor-lower-part-gui))
  (<gui> :set-parent *temp-test-area* -1)
  (<gui> :show *temp-test-area*))


(define *editor-lower-part-has-loaded* #t)

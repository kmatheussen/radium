(provide 'editor_track_headers.scm)

(my-require 'gui.scm)
(my-require 'area.scm)

(define *editor-track-background-color* "high_editor")

         
(define (show-editor-track-popup-menu tracknum)
  (popup-menu (<-> "---------Track " tracknum ":")
              (list "Pan slider"
                    :check (<ra> :get-track-pan-on-off tracknum)
                    :shortcut ra:switch-track-pan-on-off
                    (lambda (onoff)
                      (<ra> :set-track-pan-on-off onoff tracknum)))
              (list "Volume slider"
                    :check (<ra> :get-track-volume-on-off tracknum)
                    :shortcut ra:switch-track-volume-on-off
                    (lambda (onoff)
                      (<ra> :set-track-volume-on-off onoff tracknum))
                    )
              "-----------"
              (let ((instrument-id (<ra> :get-instrument-for-track tracknum)))
                (list "Rename instrument"
                      :enabled (<ra> :is-legal-instrument instrument-id)
                      :shortcut (list ra:eval-scheme "(FROM_C-request-rename-instrument)")
                      (lambda ()
                        (FROM_C-request-rename-instrument instrument-id))))
                            
              "-----------"
              (list "Enabled/Muted"
                    :check (<ra> :track-on tracknum)
                    :shortcut ra:switch-track-on
                    (lambda (onoff)
                      (<ra> :set-track-on onoff tracknum)))
              "-----------"
              (list "Pianoroll visible"
                    :check (<ra> :pianoroll-visible tracknum)
                    :shortcut ra:show-hide-pianoroll
                    (lambda (onoff)
                      (<ra> :show-pianoroll onoff tracknum)))
              "-----------"
              (list "Keybindings"
                    (list
                     "---------------Pan slider on/off"
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-track-pan-on-off"
                                                                      :args '()
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     (<-> "---------------Pan slider on/off for track #" tracknum)
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-track-pan-on-off"
                                                                      :args (list tracknum)
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     "---------------Volume slider on/off"
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-track-volume-on-off"
                                                                      :args '()
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     (<-> "---------------Volume slider on/off for track #" tracknum)
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-track-volume-on-off"
                                                                      :args (list tracknum)
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     "---------------Rename instrument"
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:eval-scheme"
                                                                      :args (list "(FROM_C-request-rename-instrument)")
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     "---------------Enabled/Muted"
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-track-on"
                                                                      :args '()
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     (<-> "---------------Enabled/Muted for track #" tracknum)
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:switch-track-on"
                                                                      :args (list tracknum)
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     "---------------Pianoroll on/off"
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:show-hide-pianoroll"
                                                                      :args ()
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     (<-> "---------------Pianoroll on/off for track #" tracknum)
                     (get-keybinding-configuration-popup-menu-entries :ra-funcname "ra:show-hide-pianoroll"
                                                                      :args (list tracknum)
                                                                      :focus-keybinding "FOCUS_EDITOR")
                     "-------------"
                     "Help keybindings" show-keybinding-help-window)))
  )

                      
(def-area-subclass (<track-volume-slider> :gui :x1 :y1 :x2 :y2
                                          :tracknum
                                          :is-audio-instrument
                                          :instrument-id
                                          :instrument-color)

  (define (get-statusbar-text)
    (if (< tracknum (<ra> :get-num-tracks))
        (<-> "Track volume: " (two-decimal-string (<ra> :get-track-volume tracknum)))))
  
  (add-statusbar-text-handler get-statusbar-text)
  
  (define has-made-undo #f)
  
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (<ra> :undo-track-volume tracknum)
      (set! has-made-undo #t)
      ))

  (define start-mouse-value #f)
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     ;;(c-display "x/y" x* y*)
     (set! has-made-undo #f)
     (cond ((= button *right-button*)
            ;;(show-sequencer-header-popup-menu seqtracknum instrument-id "System Volume" gui)
            ;;'eat-mouse-cycle
            ;;(c-display "volume slider right")
            #f
            )
           ((= button *left-button*)
            (set! start-mouse-value (<ra> :get-track-volume tracknum))
            (set-statusbar-text! (get-statusbar-text))
            #t)
           (else
            #f)))
   (lambda (button x* y* dx dy)
     (assert start-mouse-value)
     (maybe-make-undo)
     (define slider-value (between 0 (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                   1))
     ;;(c-display "slider-value:" slider-value dx)
     (<ra> :set-track-volume slider-value tracknum)
     (set-statusbar-text! (get-statusbar-text))
     ;;(update-me!)
     )
   (lambda (button x* y* dx dy)
     ;;(c-display "release button/x/y" x* y*)
     (set! start-mouse-value #f)
     #f
     ))

  (detect-hovering!)
  
  (define-override (paint)
    (when (< tracknum (<ra> :get-num-tracks))
      (define b 1)
      (define value (<ra> :get-track-volume tracknum))
      
      (paint-horizontal-slider gui
                               value
                               (two-decimal-string value)
                               x1 y1 x2 y2
                               :color (or instrument-color
                                          *editor-track-background-color*)
                               :is-hovering is-hovering
                               )))

  )

                                          
(def-area-subclass (<track-pan-slider> :gui :x1 :y1 :x2 :y2
                                       :tracknum
                                       :is-audio-instrument
                                       :instrument-id
                                       :instrument-color)

  (define (get-degree-value)
    (if (< tracknum (<ra> :get-num-tracks))
        (round (scale (<ra> :get-track-pan tracknum)
                      -1 1
                      -90 90))))
  
  (define (get-statusbar-text)
    (<-> "Track pan: " (get-degree-value)))
  
  (add-statusbar-text-handler get-statusbar-text)
  
  (define has-made-undo #f)
  
  (define (maybe-make-undo)
    (when (not has-made-undo)
      (set! has-made-undo #t)
      (<ra> :undo-track-pan tracknum)
      ))

  (define start-mouse-value #f)
  
  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     ;;(c-display "x/y" x* y*)
     (set! has-made-undo #f)
     (cond ((= button *right-button*)
            ;;(show-sequencer-header-popup-menu seqtracknum instrument-id "System Volume" gui)
            #f)
           ((= button *left-button*)
            (set! start-mouse-value (<ra> :get-track-pan tracknum))
            (set-statusbar-text! (get-statusbar-text))
            #t)
           (else
            #f)))
   (lambda (button x* y* dx dy)
     (assert start-mouse-value)
     (maybe-make-undo)
     (define slider-value (between -1 (+ start-mouse-value
                                         (scale dx 0 width 0 2))
                                   1))
     (<ra> :set-track-pan slider-value tracknum)
     ;;(c-display "Pan. slider-value:" slider-value ". dx" dx ". start:" start-mouse-value)
     (set-statusbar-text! (get-statusbar-text))
     ;;(update-me!)
     )
   (lambda (button x* y* dx dy)
     ;;(c-display "release button/x/y" x* y*)
     (set! start-mouse-value #f)
     #f
     ))

  (detect-hovering!)
  
  (define-override (paint)
    (when (< tracknum (<ra> :get-num-tracks))
      (define b 1)
      (define value (<ra> :get-track-pan tracknum))
      ;;(c-display "degree:" (get-degree-value) ", " value)
      (paint-pan-slider gui x1 y1 x2 y2
                        (get-degree-value)
                        #t
                        :background-color (or instrument-color
                                              *editor-track-background-color*)
                        :is-hovering is-hovering
                        )
      
      '(paint-horizontal-slider gui
                                value
                                (<-> (get-degree-value))
                                x1 y1 x2 y2
                                )
      )
    )

  )

                                          
(def-area-subclass (<track-header-lower-part> :gui :x1 :y1 :x2 :y2
                                              :tracknum
                                              :is-audio-instrument
                                              :instrument-id
                                              :instrument-color
                                              :show-vam
                                              )
  (set! show-vam #f)
  (define b 1)
  
  ;;(define pianoroll-visible (<ra> :pianoroll-visible tracknum))
  
  (define volume-visible (<ra> :get-track-volume-on-off tracknum))
  (define pan-visible (<ra> :get-track-pan-on-off tracknum))
  
  (define pan-slider-y1 y1)
  (define pan-slider-y2 (if volume-visible
                            (average y1 y2)
                            y2))
  
  (define volume-slider-y1 (if pan-visible
                               (average y1 y2)
                               y1))
  
  (define volume-slider-y2 y2)

  (define meter-x2 (if show-vam
                       (+ x1 (<gui> :get-gfx-scale 9))
                       x1))
  
  (define vam #f)

  ;;(c-display "INSTRUMENT-id:" instrument-id)
  
  (if show-vam
      (set! vam (<gui> :add-vertical-audio-meter gui (find-meter-instrument-id instrument-id) x1 y1 (+ b meter-x2) y2 -2)));; instrument-id)))
  
  (define-override (about-to-be-removed-callback)
    (if vam
        (let ((removed (<gui> :remove-vertical-audio-meter vam #f)))
          (if (not (<ra> :release-mode))
              (assert removed)))))

  (if volume-visible
      (add-sub-area-plain! (<new> :track-volume-slider gui
                                  meter-x2 volume-slider-y1
                                  x2 volume-slider-y2
                                  tracknum
                                  is-audio-instrument
                                  instrument-id
                                  instrument-color)))

  (if pan-visible
      (add-sub-area-plain! (<new> :track-pan-slider gui
                                  meter-x2 pan-slider-y1
                                  x2 pan-slider-y2
                                  tracknum
                                  is-audio-instrument
                                  instrument-id
                                  instrument-color)))

  
  )


(define (get-min/max-pitch tracknum blocknum)
  (define minkey (floor (<ra> :get-lowest-key tracknum blocknum)))
  (define maxkey (+ 1 (floor (<ra> :get-highest-key tracknum blocknum))))
  (if (not (>= minkey 0))
      (list 48 60)
      (let loop ((minkey minkey)
                 (maxkey maxkey))
        (if (>= (- maxkey minkey) 5)
            (list minkey maxkey)
            (loop (if (> minkey 0)
                      (- minkey 1)
                      minkey)
                  (if (< maxkey 127)
                      (+ maxkey 2)
                      maxkey))))))

#!!
(get-min/max-pitch 4 -1)
!!#

(define (FROM_C-set-pianoroll-autorange tracknum blocknum)
  (define min/max (get-min/max-pitch tracknum blocknum))
  (when min/max
    (define minkey (car min/max))
    (define maxkey (cadr min/max))
    (<ra> :set-pianoroll-range minkey maxkey tracknum blocknum)))

  
(def-area-subclass (<track-header-pianoroll> :gui :x1 :y1 :x2 :y2
                                             :tracknum
                                             :instrument-id
                                             :instrument-color)

  (define x-split1 (scale 1 0 3 x1 x2))
  (define x-split2 (scale 2 0 3 x1 x2))

  (define minmax-color (<gui> :mix-colors
                              (or instrument-color
                                  *editor-track-background-color*)
                              *editor-track-background-color*
                              0.8))

  (define (legalize-low keynum)
    (between 0
             (floor keynum)
             (- (<ra> :get-pianoroll-high-key tracknum)
                5)))
  
  (define (legalize-high keynum)
    (between (+ (<ra> :get-pianoroll-low-key tracknum)
                5)
             (floor keynum)
             128))
  
  (define low-button (<new> :line-input gui (- x1 1) (- y1 1) x-split1 y2
                            :text (<ra> :get-note-name3 (<ra> :get-pianoroll-low-key tracknum))
                            :background-color minmax-color
                            :callback (lambda (new-name)
                                        (define keynum (<ra> :get-note-name-value new-name))
                                        ;;(c-display "keynum:" keynum)
                                        (<ra> :get-note-name3 (if (= -1 keynum)
                                                                  (<ra> :get-pianoroll-low-key tracknum)
                                                                  (let ((keynum (legalize-low keynum)))
                                                                    (<ra> :set-pianoroll-low-key keynum tracknum)
                                                                    keynum))))
                            ))
  (add-sub-area-plain! low-button)

  (define auto-button (<new> :button gui x-split1 y1 x-split2 y2
                             :text "A"
                             :statusbar-text "Automatically set pianoroll range"
                             :callback-release (lambda ()
                                                 (<ra> :set-pianoroll-auto-range tracknum))
                             ))
  (add-sub-area-plain! auto-button)
  (add-keybinding-configuration-to-gui auto-button
                                       "ra:set-pianoroll-auto-range"
                                       (list -1))
  
  (define high-button (<new> :line-input gui x-split2 (- y1 1) (+ x2 1) y2
                            :text (<ra> :get-note-name3 (<ra> :get-pianoroll-high-key tracknum))
                            :background-color minmax-color
                            :callback (lambda (new-name)
                                        (define keynum (<ra> :get-note-name-value new-name))
                                        ;;(c-display "keynum:" keynum)
                                        (<ra> :get-note-name3 (if (= -1 keynum)
                                                                  (<ra> :get-pianoroll-high-key tracknum)
                                                                  (let ((keynum (legalize-high keynum)))
                                                                    (<ra> :set-pianoroll-high-key keynum tracknum)
                                                                    keynum))))
                            ))
  (add-sub-area-plain! high-button)
   
  (low-button :override-method! :mouse-wheel-moved-last
              (lambda (is-up x* y*)
                (<ra> :set-pianoroll-low-key (legalize-low (+ (<ra> :get-pianoroll-low-key tracknum)
                                                              (if is-up
                                                                  1
                                                                  -1)))
                      tracknum)
                #t))
  (high-button :override-method! :mouse-wheel-moved-last
               (lambda (is-up x* y*)
                 (<ra> :set-pianoroll-high-key (legalize-high (+ (<ra> :get-pianoroll-high-key tracknum)
                                                                 (if is-up
                                                                     1
                                                                     -1)))
                       tracknum)
                 #t))
  )

(def-area-subclass (<track-headers> :gui :x1 :y1 :x2 :y2
                                    :start-tracknum
                                    :end-tracknum
                                    )

  (define b 1)
  
  (define y-split (average y1 y2))

  (define instrument-id (<ra> :get-instrument-for-track start-tracknum))

  (define is-audio-instrument (and (<ra> :is-legal-instrument instrument-id)
                                   (<ra> :instrument-is-audio instrument-id)))

  (define instrument-color (and (<ra> :is-legal-instrument instrument-id)
                                (get-instrument-background-color gui instrument-id)))
  ;;(define meter-x2 x1)

  (define vam #f)
    
  (define meter-x2 (if is-audio-instrument
                       (+ x1 (<gui> :get-gfx-scale 9))
                       x1))

  (define (get-text-area-background-color)
    (define color (or instrument-color
                      *editor-track-background-color*))
    (if (name :is-hovering)
        (set! color (<gui> :make-color-lighter color 1.1)))
    color)
              
  (define name (<new> :text-area gui meter-x2 y1 x2 y-split
                      (<-> (if (= start-tracknum end-tracknum)
                                       start-tracknum
                                       (<-> start-tracknum "->" end-tracknum))
                           ": "
                           (if (<ra> :is-legal-instrument instrument-id)
                               (<ra> :get-instrument-name instrument-id)
                               "(click me)"))
                      :background-color get-text-area-background-color
                      :align-left #t
                      :scale-font-size #f
                      :cut-text-to-fit #t
                      ))
  
  (add-sub-area-plain! name)

  (name :detect-hovering!)
  
  (add-mouse-cycle! (lambda (button x* y*)
                      ;;(c-display "hepp:" button x* y*)
                      (define tracknum (get-track-num x* y*))
                      (when (and tracknum
                                 (>= tracknum 0)
                                 (or (= button *middle-button*)
                                     (= button *left-button*)
                                     (= button *right-button*)))
                        (<ra> :select-track tracknum)
                        (cond ((= button *right-button*)
                               (show-editor-track-popup-menu tracknum))
                              ((= button *left-button*)
                               (<ra> :select-instrument-for-track tracknum))))
                      #f))

  
  (when is-audio-instrument
    ;;(c-display "INSTRUMENT-id 2:" instrument-id is-audio-instrument (<ra> :get-instrument-name instrument-id))
    ;;(c-display "   HEPP: " instrument-id (find-meter-instrument-id instrument-id))
    (set! vam (<gui> :add-vertical-audio-meter gui (find-meter-instrument-id instrument-id) x1 0 (+ b meter-x2) y2 -2))) ;;instrument-id)))

  ;;(c-display "    INSTRUMENT-id 3")
  
  (define-override (about-to-be-removed-callback)
    (if vam
        (let ((removed (<gui> :remove-vertical-audio-meter vam #f)))
          (if (not (<ra> :release-mode))
              (assert removed)))))
  
  (let loop ((tracknum start-tracknum))
    (when (<= tracknum end-tracknum)
      (define x1 (max (+ 1 meter-x2) (<ra> :get-track-x1 tracknum)))
      (define x2 (+ 2 (<ra> :get-track-x2 tracknum)))
      (when (> x2 x1)
        (define pianoroll-visible (<ra> :pianoroll-visible tracknum))
        (define pianoroll-x2 (if pianoroll-visible
                                 (- (<ra> :get-track-pianoroll-x2 tracknum) 1)
                                 x1))
        (if pianoroll-visible
            (add-sub-area-plain! (<new> :track-header-pianoroll gui
                                        x1 y-split (- pianoroll-x2 1) y2
                                        tracknum
                                        instrument-id
                                        instrument-color)))
        (add-sub-area-plain! (<new> :track-header-lower-part gui
                                    (- pianoroll-x2 0) y-split
                                    (+ x2 1) (- y2 1)
                                    tracknum
                                    is-audio-instrument
                                    instrument-id
                                    instrument-color
                                    :show-vam (and is-audio-instrument
                                                   (= tracknum start-tracknum))
                                    )))
      (loop (+ tracknum 1))))
  
  )


#!!
(<ra> :track-on 0)
(<ra> :track-on 1)

!!#

(def-area-subclass (<editor-track-headers-area-only-tracks> :gui :x1 :y1 :x2 :y2)
  (define num-tracks (<ra> :get-num-tracks))

  (define-override (post-paint)
    ;;(c-display "POASTPAINT")
    (let loop ((tracknum 0))
      (when (< tracknum (<ra> :get-num-tracks))
        (define track-x1 (- (<ra> :get-track-x1 tracknum) 1))
        (define track-x2 (+ 4 (<ra> :get-track-x2 tracknum)))
        (when (and (< track-x1 x2)
                   (> track-x2 x1)
                   (not (<ra> :track-on tracknum)))
          ;;(c-display "NOT:" tracknum)
          (<gui> :filled-box gui "#d088aa88"
                 track-x1 y1 track-x2 y2))
        (loop (+ 1 tracknum)))))
      
  (let loop ((start-tracknum 0)
             (tracknum 1))
    
    (define (create!)
      (define end-tracknum (- tracknum 1))
      ;;(c-display "CREATE:" start-tracknum tracknum)
      (define track-x1 (- (<ra> :get-track-x1 start-tracknum) 1))
      (define track-x2 (+ 4 (<ra> :get-track-x2 end-tracknum)))
      (if (and (< track-x1 x2)
               (> track-x2 x1))
          (add-sub-area-plain! (<new> :track-headers gui
                                      track-x1 y1
                                      track-x2 y2
                                      start-tracknum end-tracknum)))
      (if (< tracknum num-tracks)
          (loop tracknum
                (+ tracknum 1))))
    

    (if (or (= tracknum num-tracks)
            (not (<ra> :has-instrument-for-track tracknum)))
        (create!)
        (let ((start-instrument (<ra> :get-instrument-for-track start-tracknum))
              (end-instrument (<ra> :get-instrument-for-track tracknum)))
          (if (not (equal? start-instrument end-instrument))
              (create!)
              (loop start-tracknum
                    (+ tracknum 1)))))))

  
(def-area-subclass (<editor-track-headers-area> :gui :x1 :y1 :x2 :y2)
  (define (add-sub-areas!)

    ;;(define height (- y2 y1))) ;;(ceiling (<ra> :get-track-volume-slider-y2)))
    ;;(set-fixed-height gui height)

    (define tracks-x1 (<ra> :get-track-slider-x1))
    (define tracks-x2 (<ra> :get-track-slider-x2))

    (add-sub-area-plain! (<new> :editor-track-headers-area-only-tracks gui tracks-x1 0 tracks-x2 height)))

  (add-sub-areas!)

  (add-method! :reconfigure!
               (lambda ()
                 ;;(c-display "   reconfigure! called. *ignore-reconf*:" *ignore-reconf*)
                 ;;(c-display "RECONF " (<ra> :get-track-slider-x1))
                 (remove-sub-areas!)
                 (add-sub-areas!)
                 (update-me!) ;; If not, background between sub areas are not updated.
                 ))

  ;; Workaround. TODO: Investigate why this is necessary.
  (define-override (paint)
    (<gui> :filled-box gui "high_background" x1 y1 x2 y2 -1 -1 *no-gradient*))
  )

(define *last-created-editor-track-headers-area* (if (defined? '*last-created-editor-track-headers-area*)
                                                     *last-created-editor-track-headers-area*
                                                     #f))

(define (FROM_C-create-editor-track-headers-gui)
  (define qtarea (make-qtarea :width (ceiling (<ra> :get-editor-x2))
                              :height (+ 1 (ceiling (<ra> :get-editor-scrollbar-y1)))
                              :sub-area-creation-callback (lambda (gui width height state)
                                                            (set! *last-created-editor-track-headers-area*
                                                                  (<new> :editor-track-headers-area gui 0 0 width height))
                                                            *last-created-editor-track-headers-area*)
                              :enable-mouse-callbacks #t))

  (qtarea :add-mouse-pointerhandler ra:set-normal-mouse-pointer)

  (define gui (qtarea :get-gui))
  
  (<gui> :dont-autofill-background gui)
  gui)

(define (FROM_C-reconfigure-editor-track-headers-gui!)
  ;;(c-display "           RECONFIGURE called")
  (if *last-created-editor-track-headers-area*
      (*last-created-editor-track-headers-area* :reconfigure!)))

#!!
(FROM_C-reconfigure-editor-track-headers-gui!)
!!#

(define *temp-test-area2* (if (defined? '*temp-test-area2*) *temp-test-area2* #f))

(when (and #f (defined? '*editor-track-headers-has-loaded*))
  (if (and *temp-test-area2*
           (<gui> :is-open *temp-test-area2*))
      (FROM_C-reconfigure-editor-track-headers-gui!)          
      (begin
        (set! *temp-test-area2* (FROM_C-create-editor-track-headers-gui))
        (<gui> :set-parent *temp-test-area2* -1)
        (<gui> :show *temp-test-area2*))))


(if (defined? '*editor-track-headers-has-loaded*)
    (FROM_C-reconfigure-editor-track-headers-gui!))

(define *editor-track-headers-has-loaded* #t)

#!!
(FROM_C-reconfigure-editor-track-headers-gui!)
(<ra> :get-editor-x1)
(<ra> :get-editor-x2)
(<ra> :get-track-x1 1)
(<ra> :get-leftmost-track-num)
(<ra> :get-track-slider-x1 0)
!!#

(provide 'seqtracks_config.scm)


#!!
(let ((box (<new> :checkbox gui x1 y1 x2 y2
                  :is-selected-func (lambda ()
                                      #t)
                  :value-changed-callback (lambda (new-value)
                                            #t)
                  :text "\\u2713")))
  (<gui> :show box))
!!#

(def-area-subclass (<seqtrack-config-entry> :gui :x1 :y1 :x2 :y2
                                            :seqtracknum)
  
  (define is-current (= (<ra> :get-curr-seqtrack) seqtracknum))
  
  (define for-audiofiles (<ra> :seqtrack-for-audiofiles seqtracknum))
  (define for-blocks (not for-audiofiles))
  (define instrument-id (if for-blocks
                            -1
                            (<ra> :get-seqtrack-instrument seqtracknum)))
  
  (define text-x1 (+ x1 (get-fontheight)))
  (define mutesolo-x1 (- x2 (get-mutesolo-width for-audiofiles)))
  
  (define enabled-button (<new> :checkbox gui x1 y1 text-x1 y2
                                :is-selected-func (lambda ()
                                                    (<ra> :get-seqtrack-visible seqtracknum))
                                :value-changed-callback (lambda (new-value)
                                                          ;;(c-display "New:" new-value)
                                                          (<ra> :set-seqtrack-visible seqtracknum new-value)
                                                          #t)
                                :text (lambda ()
                                        (if (<ra> :get-seqtrack-visible seqtracknum)
                                            "✓"
                                            " "))))
  (define name-area (<new> :text-area gui text-x1 y1 mutesolo-x1 y2;;text-x1 y1 x2 y2
                           :text (<-> seqtracknum ". " (<ra> :get-seqtrack-name seqtracknum))
                           :background-color (lambda ()
                                               (get-seqtrack-background-color gui seqtracknum))
                           :text-color *text-color*
                           :align-left #t
                           :paint-border #f
                           :cut-text-to-fit #t
                           ))

  (name-area :add-mouse-cycle!
             (lambda (button x* y*)
               (if (inside? x* y*)
                   (begin
                     (if (or (> seqtracknum (<ra> :get-lowest-visible-seqtrack))
                             (< seqtracknum (<ra> :get-topmost-visible-seqtrack)))
                         (<ra> :set-topmost-visible-seqtrack seqtracknum))
                     (<ra> :set-curr-seqtrack seqtracknum)
                     #t)
                   #f)))

  '(define-override (get-mouse-cycle button x* y*)
    (define ret (super:get-mouse-cycle button x* y*))
    (if (not ret)
        (when (inside? x* y*)
          ;;(c-display "____HEADER seqtracknum:" seqtracknum)
          (<ra> :schedule 0
                (lambda ()
                  (if (or (> seqtracknum (<ra> :get-lowest-visible-seqtrack))
                          (< seqtracknum (<ra> :get-topmost-visible-seqtrack)))
                      (<ra> :set-topmost-visible-seqtrack seqtracknum))
                  (<ra> :set-curr-seqtrack seqtracknum)
                  #f))
          ))
    ret)

  
  '(define-override (paint)
    (c-display "ENTRY" seqtracknum ": " x1 y1 x2 y2 width height)
    (for-each (lambda (sub-area)
                (sub-area :get-position
                          (lambda (x1 y1 x2 y2 width height)
                            (c-display "        " (sub-area :class-name) ": " x1 y1 x2 y2 width height))))
              sub-areas))

  (define mutesolo-area (<new> :mute-solo-buttons gui
                               mutesolo-x1 y1
                               x2 y2
                               instrument-id
                               :use-single-letters #t
                               :stack-horizontally #t
                               :seqtracknum seqtracknum))
  
  (add-sub-area-plain! enabled-button)
  (add-sub-area-plain! name-area)
  (add-sub-area-plain! mutesolo-area)
  
  ;;(c-display seqtracknum text-x1 y1 x2 y2)
  
  )

#!!
(for-each (lambda (i)
            (<ra> :append-seqtrack #t #f)
            (define instrument (<ra> :get-seqtrack-instrument (- (<ra> :get-num-seqtracks) 1)))
            (<ra> :generate-new-instrument-color instrument))
          (iota 50))
!!#

(def-area-subclass (<seqtracks-config-area> :gui :x1 :y1 :x2 :y2)

  (define num-settings-buttons 8)

  (define curr-entry-num 0)

  (define-optional-func vertical-list-area (key . rest))
  
  (define (update-areas!)
    ;;(c-display "\n\n\n---------------------- num entries:" (length entries) "-----------------------\n\n\n")
    (remove-sub-areas!)

    (define border 2)
  
    (define scroll-y1 (+ y1 (get-fontheight) border))
    (define radio-x1 (+ x1 (/ width (+ 1 num-settings-buttons))))
    (define radio-y2 (- scroll-y1 border))

    (define reset-button (<new> :button gui x1 y1 (- radio-x1 (/ border 2)) radio-y2
                                :text "↝"
                                :background-color "#88228833"
                                :statusbar-text (list #t "Reset A/B")
                                :callback (lambda ()
                                            (<ra> :reset-seqtrack-config))))
    (add-sub-area-plain! reset-button)
    
    (add-sub-area-plain! (<new> :radiobuttons gui (+ radio-x1 (/ border 2)) y1 x2 radio-y2
                                num-settings-buttons
                                (<ra> :get-curr-seqtrack-config-num)
                                (lambda (num is-on)
                                  ;;(c-display "radio:" num is-on)
                                  (when is-on
                                    (apply-state! (get-state))
                                    (<ra> :set-curr-seqtrack-config-num num))
                                  #t)
                                :layout-horizontally #t
                                :text-func (lambda (num)
                                             (if (<ra> :seqtrack-config-is-used num)
                                                 (vector-ref (vector "A*" "B*" "C*" "D*" "E*" "F*" "G*" "H*") num)
                                                 (vector-ref (vector "A" "B" "C" "D" "E" "F" "G" "H") num)))))

    (define curr-x1 -10000000)
    (define curr-x2 -10000000)
    (define curr-entries #f)

    (define entry-height (* 1.2 (get-fontheight)))
    
    (set! vertical-list-area (<new> :vertical-list-area2 gui x1 scroll-y1 x2 y2
                                    :num-sub-areas (<ra> :get-num-seqtracks)
                                    :get-sub-area-height entry-height
                                    :create-sub-area
                                    (lambda (seqtracknum x1 x2)
                                      (<new> :seqtrack-config-entry gui x1 0 x2 entry-height seqtracknum))))
                                      
    (add-sub-area-plain! vertical-list-area)
    )

  (update-areas!)

  (define-override (get-state)
    (vertical-list-area :get-state))

  (define-override (apply-state! state)
    ;;(c-display "apply-state:" state)
    (when state
      (vertical-list-area :apply-state! state)
      )
    ;;(update-areas!)
    )
  

  )

(define recreate-seqtracks-config-area #f)

(define (create-seqtracks-config-area gui x1 y1 x2 y2 state)

  (define (recreate x1 y1 x2 y2 state)
    (define area (<new> :seqtracks-config-area gui x1 y1 x2 y2))
    (if state
        (area :apply-state! state))
    area)
  
  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))
  
  '(area :add-mouse-cycle! (lambda (button x* y*)
                             (and (not (<ra> :shift-pressed))
                                  (= button *right-button*)
                                  (begin
                                    (show-blocklist-popup-menu)
                                    #t))))
  
  ;;(c-display "state:" state)
  
  (define (update)
    (define state (area :get-state))
    (area :remove-sub-areas!)
    (area :get-position
          (lambda (x1 y1 x2 y2 width height)
            (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state)))))

  (set! recreate-seqtracks-config-area update)
  area
  )

#!!
(<ra> :get-seqtrack-solo 1)
(<ra> :set-seqtrack-solo #f 1)
(<ra> :set-seqtrack-solo #t 1)

(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                              (create-seqtracks-config-area gui 0 0 width height state))))
  (<gui> :show (testarea :get-gui)))


!!#




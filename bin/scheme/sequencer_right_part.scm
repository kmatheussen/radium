(provide 'sequencer_right_part.scm)

(my-require 'gui.scm)
(my-require 'area.scm)

(define (create-audio-files-browser-area gui x1 y1 x2 y2 state)

  (define curr-audiofile #f)

  (define (recreate x1 y1 x2 y2 state)
    (define audiofiles (to-list (<ra> :get-audio-files)))
    (define area
      (<new> :vertical-list-area gui x1 y1 x2 y2
             (map (lambda (i audiofile)
                    (define color (<ra> :get-audiofile-color audiofile))
                    (define col1 color)
                    ;;(set! color (<gui> :make-color-lighter color 1.5))
                    (set! color (<gui> :set-alpha-for-color color 0.5))
                    (c-display "With alpha:" col1 ". without:" color)
                    (define file-info (<ra> :get-file-info audiofile))
                    (<new> :sequencer-drag-entry-area gui 10 0 100 (* 1.2 (get-fontheight))
                           :is-current (lambda ()
                                         ;;(c-display "curr:" curr-audiofile)
                                         (and curr-audiofile
                                            (string=? audiofile curr-audiofile)))
                           :entry-num i
                           :file-info file-info
                           :allow-dragging #t
                           :background-color color
                           :callback (lambda (button x y entry-num)
                                       (set! curr-audiofile audiofile)
                                       (area :update-me!)
                                       (and (= button *right-button*)
                                            (begin
                                              (show-audiolist-popup-menu audiofile)
                                              #t)))
                           ))
                  (iota (length audiofiles))
                  audiofiles)))
    (if state
        (area :apply-state! state))
    area)

  
  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))

  (area :add-mouse-cycle! (lambda (button x* y*)
                            (when curr-audiofile
                              (set! curr-audiofile #f)
                              (area :update-me!))
                            (and (= button *right-button*)
                                 (begin
                                   (show-audiolist-popup-menu #f) ;;curr-audiofile)
                                   #t))))


  (define generation (<ra> :get-audio-files-generation))
  (<ra> :schedule (random 1000)
        (lambda ()
          (if (or (not (<gui> :is-open gui))
                  (not (area :is-alive)))
              #f
              (begin
                (define new-generation (<ra> :get-audio-files-generation))
                (when (not (= generation new-generation))
                  (set! generation new-generation)
                  (define state (area :get-state))
                  (area :remove-sub-areas!)
                  (area :get-position
                        (lambda (x1 y1 x2 y2 width height)
                          (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state)))))
                200))))
  area
  )

#!!
(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                              (create-audio-files-browser-area gui 0 0 width height state))))
  (<gui> :show (testarea :get-gui)))
!!#


(define (create-blocks-browser-area gui x1 y1 x2 y2 state)

  (define (recreate x1 y1 x2 y2 state)
    (define area
      (<new> :vertical-list-area gui x1 y1 x2 y2
             (map (lambda (blocknum)
                    (define color (<ra> :get-block-color blocknum))
                  ;;;(set! color (<gui> :make-color-lighter color 1.5))
                    (set! color (<gui> :set-alpha-for-color color 0.5))
                    (<new> :sequencer-drag-entry-area gui 10 0 100 (* 1.2 (get-fontheight))
                           :callback (lambda (button x y entry-num)
                                       (if (and (<ra> :shift-pressed)
                                                (= button *right-button*))
                                           (<ra> :delete-block blocknum)
                                           (begin
                                             (<ra> :select-block blocknum)))
                                       (update)
                                       #f)
                           :is-current (= (<ra> :current-block) blocknum)
                           :entry-num blocknum
                           :blocknum blocknum
                           :background-color color ;(if (= (<ra> :current-block) blocknum)
                                        ;(<gui> :mix-colors color "green" 0.1)
                                               ;color)
                           :allow-dragging #t))
                  (iota (<ra> :get-num-blocks)))))
    (if state
        (area :apply-state! state))
    area)
  
  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))

  (area :add-mouse-cycle! (lambda (button x* y*)
                            (and (not (<ra> :shift-pressed))
                                 (= button *right-button*)
                                 (begin
                                   (show-blocklist-popup-menu)
                                   #t))))
  
  ;;(c-display "state:" state)

  (define generation (<ra> :get-editor-blocks-generation))
  (define curr-block (<ra> :current-block))

  (define (update)
    (if (or (not (<gui> :is-open gui))
            (not (area :is-alive)))
        #f
        (begin
          (define new-generation (<ra> :get-editor-blocks-generation))
          (define new-curr-block (<ra> :current-block))
          (when (or (not (= generation new-generation))
                    (not (= curr-block new-curr-block)))
            (set! generation new-generation)
            (set! curr-block new-curr-block)
            (define state (area :get-state))
            (area :remove-sub-areas!)
            (area :get-position
                  (lambda (x1 y1 x2 y2 width height)
                    (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state)))))
          200)))
    
  (<ra> :schedule (random 1000)
        update)

  area
  )

#!!
(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                              (create-blocks-browser-area gui 0 0 width height state))))
  (<gui> :show (testarea :get-gui)))
!!#


(define (create-sequencer-right-part-area gui x1 y1 x2 y2 state)
  ;;(c-display "    CREATE SEQUENCER RIGHT AREA. State:" (pp state))
  (define (recreate gui x1 y1 x2 y2)
    (define list-area (<new> :tabs gui x1 y1 x2 y2
                             :is-horizontal #f
                             :curr-tab-num 0
                             :tab-names '("Hide" "Blocks" "Sounds" "Browser")
                             :state state
                             :get-tab-area-func
                             (lambda (tab-num x1 y1 x2 y2 state)
                               ;;(<gui> :disable-updates gui) <-- Caused flickering for all of the sequencer when resizing.
                               (<ra> :schedule 0 ;; Run in next event cycle to avoid set-sequencer-right-part-empty calling a reconfigure and so forth.
                                     (lambda ()
                                       ;;(<gui> :enable-updates gui)
                                       (<ra> :set-sequencer-right-part-empty (= tab-num 0))
                                       #f))
                               (cond ((= tab-num 0)
                                      (c-display "EMPTY")
                                      (<new> :area gui x1 y1 x2 y2))
                                     ((= tab-num 1)
                                      (create-blocks-browser-area gui x1 y1 x2 y2 state))
                                     ((= tab-num 2)
                                      (create-audio-files-browser-area gui x1 y1 x2 y2 state))
                                     ((= tab-num 3)
                                      ;;(c-display "    CREATING FILEBROWSER. state:" state)
                                      (<new> :file-browser gui x1 y1 x2 y2
                                             :path (<ra> :get-home-path)
                                             :id-text "sequencer-right-part"
                                             :only-audio-files #t
                                             :state state
                                             ))))
                             ))

    (list-area :add-nonpress-mouse-cycle!
               (lambda (x* y)
                 (set-mouse-pointer ra:set-normal-mouse-pointer gui)
                 #f))
    
    list-area)
  (recreate gui x1 y1 x2 y2))

#!!
(let ()
  (define testarea (make-qtarea :width 450 :height 750
                                :sub-area-creation-callback (lambda (gui width height state)
                                                               (create-sequencer-right-part-area gui 0 0 width height state))))
  (<gui> :show (testarea :get-gui)))
!!#

(define (get-sequencer-right-part-position kont)
  (define header-box (<ra> :get-box sequencer-right-part))
  (kont (header-box :x1) (header-box :y1)
        (header-box :x2) (header-box :y2)))

(define (get-sequencer-right-part-area)
  (when (not *sequencer-right-part-area*)
    (set! *sequencer-right-part-area* (if *use-testgui*
                                         *testarea*
                                         (get-sequencer-right-part-position
                                          (lambda (x1 y1 x2 y2)
                                            (<new> :area (<gui> :get-sequencer-gui)
                                                   x1 y1 x2 y2))))))
  *sequencer-right-part-area*)

(define (FROM_C-reconfigure-sequencer-right-part)
  ;;(c-display "   Scheme: Reconfiguring right part")

  (get-sequencer-right-part-area)

  (define gui (if *use-testgui*
                  *testgui*
                  (<gui> :get-sequencer-gui)))

  (get-sequencer-right-part-position
   (lambda (x1 y1 x2 y2)
     (define sub-areas (*sequencer-right-part-area* :get-sub-areas))
     (define state (and (not (null? sub-areas))
                        ((car sub-areas) :get-state)))
     ;;(c-display "das state:" state)
     (*sequencer-right-part-area* :reset! x1 y1 x2 y2)
     (*sequencer-right-part-area* :add-sub-area-plain! (create-sequencer-right-part-area gui x1 y1 x2 y2 state))
     ))
  )


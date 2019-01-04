(provide 'sequencer_right_part.scm)

(my-require 'gui.scm)
(my-require 'area.scm)

(define (create-audio-files-browser-area gui x1 y1 x2 y2 state)
  (define (recreate x1 y1 x2 y2 state)
    (define audiofiles (to-list (<ra> :get-audio-files)))
    (define area
      (<new> :vertical-list-area gui x1 y1 x2 y2
           (map (lambda (i audiofile)
                  (define file-info (<ra> :get-file-info audiofile))
                  (<new> :file-browser-entry gui 10 0 100 (* 1.2 (get-fontheight))
                         :is-current #f
                         :entry-num i
                         :file-info file-info
                         :background-color (<gui> :mix-colors
                                                  (<ra> :get-audiofile-color audiofile)
                                                  "white"
                                                  0.80)
                         :allow-dragging #t))
                (iota (length audiofiles))
                audiofiles)))
    (if state
        (area :apply-state! state))
    area)

  
  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))

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
                  (area :remove-sub-areas!)
                  (area :get-position
                        (lambda (x1 y1 x2 y2 width height)
                          (area :add-sub-area-plain! (recreate x1 y1 x2 y2)))))
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

  (define (get-data)
    (hash-table* :curr-block (<ra> :current-block)
                 :block-names (map (lambda (i)
                                     (<ra> :get-block-name i))
                                   (iota (<ra> :get-num-blocks)))))

  (define curr-data (get-data))

  (define (recreate x1 y1 x2 y2 state)
    (define area 
      (<new> :vertical-list-area gui x1 y1 x2 y2
           (map (lambda (blocknum blockname)
                  (define color ;;(<ra> :get-block-color blocknum))
                    (<gui> :mix-colors
                           (<ra> :get-block-color blocknum)
                           "white" ;;(<gui> :get-background-color -1)
                           0.95))
                  (define line
                    (<new> :text-area gui
                           10 0 100 (* 1.2 (get-fontheight))
                           blockname
                           :text-color "sequencer_text_color"
                           :background-color (lambda ()
                                               (if (= (<ra> :current-block) blocknum)
                                                   (<gui> :mix-colors color "green" 0.1)
                                                   color))
                           :align-left #t))
                  (line :add-mouse-cycle!
                        (lambda (button x* y*)
                          ;;(c-display "hepp" i)
                          (line :get-position
                                (lambda (x1 y1 x2 y2 width height)
                                  (<gui> :create-block-drag-icon gui (floor width) (floor height) (floor (- x* x1)) (floor (- y* y1)) blocknum
                                         (lambda (gui width height)
                                           ;;(c-display "-------w2: " width height)
                                           (line :paint-text-area gui 0 0 width height)
                                           ;;(<gui> :draw-line gui "black" 5 3 10 5 20)
                                           ))))
                          #t)
                        (lambda (button x* y*)
                          #t)
                        (lambda (button x* y*)
                          #t))
                  line)
                (iota (length (curr-data :block-names)))
                (curr-data :block-names))))
    (if state
        (area :apply-state! state))
    area)

  (define area (<new> :use-first-subarea-state-as-state-area gui x1 y1 x2 y2))
  (area :add-sub-area-plain! (recreate x1 y1 x2 y2 state))
  
  ;;(c-display "state:" state)

  (<ra> :schedule (random 1000)
        (lambda ()
          (if (or (not (<gui> :is-open gui))
                  (not (area :is-alive)))
              #f
              (let ((new-data (get-data)))
                (when (not (morally-equal? curr-data new-data))
                  (set! curr-data new-data)
                  (area :remove-sub-areas!)
                  (area :get-position
                        (lambda (x1 y1 x2 y2 width height)
                          (area :add-sub-area-plain! (recreate x1 y1 x2 y2 #f)))))
                200))))
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


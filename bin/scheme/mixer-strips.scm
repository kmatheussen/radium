(provide 'mixer-strips.scm)

;; TOPIC: Should the pan slider work for the last instrument in the path?
;; In case, we need to copy pan values when deleting / removing plulgins.
;;
;; When adding a new mixer strip, we could slice the various mixer strips windows.




(define (get-fontheight)
  (+ 4 (<gui> :get-system-fontheight)))

(define *text-color* "#cccccc")
(define *arrow-text* "↳")

(define (db-to-slider db)
  (if (<= db *min-db*)
      0
      (let ((scaled (scale db *min-db* *max-mixer-db* 0 1)))
        (* scaled scaled))))

(define (slider-to-db slider)
  (define scaled (sqrt slider))
  (scale scaled 0 1 *min-db* *max-mixer-db*))

(define (db-to-text db add-dB-string)
  (cond ((<= db *min-db*)
         "~inf")
        (add-dB-string
         (<-> (one-decimal-string db) "dB"))
        (else
         (one-decimal-string db))))

(define (set-fixed-height gui height)
  (<gui> :set-size gui (<gui> :width gui) height)
  (<gui> :set-min-height gui height)
  (<gui> :set-max-height gui height)
  (<gui> :set-size-policy gui #t #f))

(define (set-fixed-width gui width)
  (<gui> :set-size gui width (<gui> :height gui))
  (<gui> :set-min-width gui width)
  (<gui> :set-max-width gui width)
  (<gui> :set-size-policy gui #f #t))

(define (create-custom-checkbox paint-func value-changed is-selected min-height)
  (define checkbox (<gui> :widget))
  (<gui> :set-min-height checkbox min-height)

  (define (repaint)
    (paint-func checkbox is-selected 
                (<gui> :width checkbox)
                (<gui> :height checkbox)))

  (<gui> :add-mouse-callback checkbox (lambda (button state x y)
                                        ;;(c-display "state" state)
                                        (when (and (= button *left-button*)
                                                   (= state *is-pressing*))
                                          (set! is-selected (not is-selected))
                                          (value-changed is-selected)
                                          (<gui> :update checkbox))
                                        #t))
  (<gui> :add-paint-callback checkbox
         (lambda (newwidth newheight)
           ;;(c-display "             RESIZE callback" newwidth newheight)
           ;;(assert (> newwidth 0))
           ;;(assert (> newheight 0))
           ;;(set! width 100);;newwidth)
           ;;(set! height 100);;newheight)
           (repaint)))

  ;;(repaint)

  (list (lambda (new-value)
          (set! is-selected new-value)
          (<gui> :update checkbox))
        checkbox))


(define (add-gui-effect-monitor gui instrument-id effect-name callback)
  (define effect-monitor (<ra> :add-effect-monitor effect-name instrument-id callback))
  (<gui> :add-close-callback gui
         (lambda ()
           (<ra> :remove-effect-monitor effect-monitor))))



(define (create-mixer-strip-name-line instrument-id height)
  (define name (<gui> :line (<ra> :get-instrument-name instrument-id) (lambda (edited)
                                                                        (if (<ra> :instrument-is-open instrument-id)
                                                                            (<ra> :set-instrument-name edited instrument-id)))))
  (<gui> :set-background-color name (<ra> :get-instrument-color instrument-id))

  (<gui> :add-mouse-callback name (lambda (button state x y)
                                    (if (and (= button *right-button*)
                                             (= state *is-pressing*))
                                        (if (<ra> :shift-pressed)
                                            (<ra> :delete-instrument instrument-id)
                                            (create-default-mixer-path-popup instrument-id)))
                                    #f))

  (set-fixed-height name height)

  name)

(define (create-mixer-strip-name instrument-id is-minimized)
  (define name (<ra> :get-instrument-name instrument-id))
  (define color (<ra> :get-instrument-color instrument-id))

  (define label (<gui> :widget))
  (<gui> :add-paint-callback label
         (lambda (width height)
           (<gui> :filled-box label color 0 0 width height)
           (if is-minimized
               (<gui> :draw-vertical-text label *text-color* name 2 5 (+ width 0) height #f #t)
               (<gui> :draw-text label *text-color* name 5 0 width height #f #t #f))
           (<gui> :draw-box label "#202020" 0 0 width height 1.0 2 2)))

  (<gui> :add-mouse-callback label (lambda (button state x y)
                                    (when (= state *is-pressing*)
                                      (if (= button *right-button*)
                                          (if (<ra> :shift-pressed)
                                              (<ra> :delete-instrument instrument-id)
                                              (create-default-mixer-path-popup instrument-id))
                                          (when (= button *left-button*)
                                            (<ra> :set-wide-instrument-strip instrument-id is-minimized)
                                            (remake-mixer-strips instrument-id))))
                                    #f))

  label)


(define (request-send-instrument instrument-id callback)
  (define is-bus-descendant (<ra> :instrument-is-bus-descendant instrument-id))
  (define buses (get-buses))

  (define (create-entry-text instrument-id)
    (<-> *arrow-text* " " (<ra> :get-instrument-name instrument-id)))
    
  (popup-menu

   ;; buses
   (map (lambda (bus-effect-name bus-onoff-effect-name bus-id)
          (list (create-entry-text bus-id)
                :enabled (and (not is-bus-descendant)
                              (< (<ra> :get-instrument-effect instrument-id bus-onoff-effect-name) 0.5))
                (lambda ()
                  (callback (lambda (gain)
                              (undo-block (lambda ()
                                            (<ra> :undo-instrument-effect instrument-id bus-onoff-effect-name)
                                            (if gain
                                                (<ra> :undo-instrument-effect instrument-id bus-effect-name))
                                            (<ra> :set-instrument-effect instrument-id bus-onoff-effect-name 1.0)
                                            (if gain
                                                (<ra> :set-instrument-effect instrument-id bus-effect-name (scale (<ra> :gain-to-db gain)
                                                                                                                  *min-db* *max-db*
                                                                                                                  0 1))))))))))
        *bus-effect-names*
        *bus-effect-onoff-names*
        buses)

   "------------"

   ;; audio connections
   (map (lambda (send-id)
          (list (create-entry-text send-id)
                :enabled (and (not (= send-id instrument-id))
                              (not (<ra> :has-audio-connection instrument-id send-id))
                              (> (<ra> :get-num-input-channels send-id) 0))
                (lambda ()
                  (callback (lambda (gain)
                              (<ra> :undo-mixer-connections)
                              (<ra> :create-audio-connection instrument-id send-id gain))))))
        (sort-instruments-by-mixer-position
         (keep (lambda (id)
                 (not (member id buses)))
               (get-all-instruments-that-we-can-send-to instrument-id))))))
          

(define (show-mixer-path-popup first-instrument-id
                               parent-instrument-id
                               instrument-id

                               is-send?
                               is-sink?
                               upper-half?

                               delete-func
                               replace-func
                               reset-func
                               )

  (popup-menu (list "Delete"
                     :enabled delete-func
                    (lambda ()
                      (delete-func)))
              (list "Replace"
                    :enabled replace-func
                    (lambda ()
                      (replace-func)))
              "-----------"
              (list "Insert Plugin"
                    :enabled (or upper-half?
                                 (not is-sink?))
                    (lambda ()
                      (cond (is-send? 
                             (insert-new-instrument-between parent-instrument-id
                                                            (get-instruments-connecting-from-instrument parent-instrument-id)
                                                            #t))
                            (upper-half?
                             (insert-new-instrument-between parent-instrument-id instrument-id #t)) ;; before
                            (else
                             (insert-new-instrument-between instrument-id
                                                            (get-instruments-connecting-from-instrument instrument-id)
                                                            #t))) ;; after
                      (c-display "             first: " (<ra> :get-instrument-name first-instrument-id))                      
                      (<ra> :set-current-instrument first-instrument-id)
                      ))
                    
              (list "Insert Send"
                    :enabled (> (<ra> :get-num-output-channels instrument-id) 0)
                    (lambda ()
                      (let ((instrument-id (cond (is-send?
                                                  parent-instrument-id)
                                                 (upper-half?
                                                  parent-instrument-id)
                                                 (else
                                                  instrument-id))))
                        (request-send-instrument instrument-id
                                                 (lambda (create-send-func)
                                                   (create-send-func 0)
                                                   (<ra> :set-current-instrument first-instrument-id))))))
              "----------"

              (list "Reset value"
                    :enabled reset-func
                    (lambda ()
                      (reset-func)))

              ;;"----------"
              ;;"Convert to standalone strip" (lambda ()
              ;;                                #t)

                "----------"
                "Rename instrument" (lambda ()
                                      (define old-name (<ra> :get-instrument-name instrument-id))
                                      (define new-name (<ra> :request-string "New name:" #t))
                                      (if (and (not (string=? new-name ""))
                                               (not (string=? new-name old-name)))
                                          (<ra> :set-instrument-name new-name instrument-id)))
                "Configure instrument color" (lambda ()
                                               (show-instrument-color-dialog instrument-id))
                "Instrument information" (lambda ()
                                           (<ra> :show-instrument-info instrument-id))
                "Show GUI" :enabled (<ra> :has-native-instrument-gui instrument-id)
                (lambda ()
                  (<ra> :show-instrument-gui instrument-id #f))
                "----------"
                "Set current instrument" (lambda ()
                                           (popup-menu (map (lambda (instrument-id)
                                                              (list (<ra> :get-instrument-name instrument-id)
                                                                    (lambda ()
                                                                      (<ra> :set-current-instrument instrument-id #f)
                                                                      )))
                                                            (sort-instruments-by-mixer-position-and-connections 
                                                             (get-all-audio-instruments)))))
              ))

(define (create-default-mixer-path-popup instrument-id)
  (define is-permanent? (<ra> :instrument-is-permanent instrument-id))

  (define (delete)
    (<ra> :delete-instrument instrument-id))
  (define (replace)
    (replace-instrument instrument-id "" #f #f))
  (define reset #f)

  (show-mixer-path-popup instrument-id
                         instrument-id
                         instrument-id
                         #f
                         (= 0 (<ra> :get-num-output-channels instrument-id))
                         #f
                         (if is-permanent? #f delete)
                         (if is-permanent? #f replace)
                         reset))
  
(define (strip-slider first-instrument-id
                      parent-instrument-id
                      instrument-id
                      is-send?
                      is-sink?
                      make-undo
                      get-scaled-value
                      get-value-text
                      set-value
                      delete-func
                      replace-func
                      reset-func)

  (define instrument-name (<ra> :get-instrument-name instrument-id))
  ;;(define widget (<gui> :widget 100 (get-fontheight)))
  (define widget #f)
  (define is-changing-value #f)

  (define (paintit width height)
    (define color (<ra> :get-instrument-color instrument-id))
    (define value (get-scaled-value))
    ;;(c-display "value: " value)
    (define pos (scale value 0 1 0 width))
    (<gui> :filled-box widget (<gui> :get-background-color widget) 0 0 width height)
    (<gui> :filled-box widget "black" 1 1 (1- width) (1- height) 5 5)
    (<gui> :filled-box widget color 0 0 pos height 5 5)
    (<gui> :draw-box widget "gray" 0 0 width height 0.8)
    (define text (<-> instrument-name ": " (get-value-text value)))
    (when is-changing-value
      (<ra> :set-statusbar-text text)
      (<gui> :tool-tip text))
    (<gui> :draw-text widget *text-color* text
           4 2 width height))

  (set! widget (<gui> :horizontal-slider "" 0 (get-scaled-value) 1.0
                      (lambda (val)
                        ;;(<ra> :set-instrument-effect instrument-id effect-name val)
                        (when widget
                          (set-value val)
                          (<gui> :update widget)))))

  (<gui> :set-min-height widget (get-fontheight))

  (<gui> :add-paint-callback widget paintit)
  
  (<gui> :add-mouse-callback widget (lambda (button state x y)
                                      (when (and (= button *left-button*)
                                                 (= state *is-pressing*))
                                        (set! is-changing-value #t)
                                        (make-undo))
                                      (when (= state *is-releasing*)
                                        (set! is-changing-value #f)
                                        (<gui> :tool-tip "")
                                        ;;(c-display "finished")
                                        )
                                      (when (and (= button *right-button*)
                                                 (= state *is-pressing*))
                                        (if (<ra> :shift-pressed)
                                            (delete-func)
                                            (show-mixer-path-popup first-instrument-id
                                                                   parent-instrument-id
                                                                   instrument-id
                                                                   is-send?
                                                                   is-sink?
                                                                   (< y (/ (<gui> :height widget) 2))
                                                                   delete-func
                                                                   replace-func
                                                                   reset-func)))
                                      #f))

  (<gui> :add-double-click-callback widget (lambda (button x y)
                                             (when (= button *left-button*)
                                               ;;(c-display " Double clicking" button)
                                               (<ra> :cancel-last-undo) ;; Undo the added undo made at th mouse callback above.
                                               (<ra> :show-instrument-gui instrument-id (<ra> :show-instrument-widget-when-double-clicking-sound-object))
                                               )))

  ;;(paintit (<gui> :width widget)
  ;;         (<gui> :height widget))

  (<gui> :set-size-policy widget #t #t)

  widget)



;; Finds the next plugin in a plugin path. 'instrument-id' is the plugin to start searching from.
(define (find-next-plugin-instrument-in-path instrument-id)
  (let loop ((out-instruments (reverse (sort-instruments-by-mixer-position
                                        (get-instruments-connecting-from-instrument instrument-id)))))
    (if (null? out-instruments)
        #f
        (let ((out-instrument (car out-instruments)))
          (if (= 1 (<ra> :get-num-in-audio-connections out-instrument))
              out-instrument
              (loop (cdr out-instruments)))))))


(define (create-mixer-strip-plugin gui first-instrument-id parent-instrument-id instrument-id)
  (define (get-drywet)
    (<ra> :get-instrument-effect instrument-id "System Dry/Wet"))

  (define (delete-instrument)
    (define ids (get-instruments-connecting-from-instrument instrument-id))
    (define gains (map (lambda (to)
                         (<ra> :get-audio-connection-gain instrument-id to))
                       ids))
    (undo-block
     (lambda ()
       (<ra> :delete-instrument instrument-id)
       (for-each (lambda (id gain)
                   (<ra> :create-audio-connection parent-instrument-id id gain))
                 ids
                 gains)))
    ;;(remake-mixer-strips) ;; (makes very little difference in snappiness, and it also causes mixer strips to be remade twice)
    )


  (define (das-replace-instrument)
    (replace-instrument instrument-id "" #t #t))

  (define (reset)
    (<ra> :set-instrument-effect instrument-id "System Dry/Wet" 1))

  (define doit #t)
  (define slider (strip-slider first-instrument-id
                               parent-instrument-id
                               instrument-id
                               #f #f
                               (lambda ()
                                 (<ra> :undo-instrument-effect instrument-id "System Dry/Wet"))
                               get-drywet
                               (lambda (scaled-value)
                                 (<-> (round (* 100 scaled-value)) "%"))
                               (lambda (new-scaled-value)
                                 (if (and doit (not (= new-scaled-value (get-drywet))))                                     
                                     (<ra> :set-instrument-effect instrument-id "System Dry/Wet" new-scaled-value)))
                               delete-instrument
                               das-replace-instrument
                               reset
                               ))

  (add-gui-effect-monitor slider instrument-id "System Dry/Wet"
                          (lambda ()
                            (set! doit #f)
                            (<gui> :set-value slider (get-drywet))
                            (set! doit #t)))

  (<gui> :add gui slider))


;; A sink plugin. For instance "System Out".
(define (create-mixer-strip-sink-plugin gui first-instrument-id parent-instrument-id instrument-id)

  (define (make-undo)
    (<ra> :undo-instrument-effect instrument-id "System In"))
    
  (define (delete)
    (<ra> :delete-instrument instrument-id))

  (define (replace)
    (c-display "   name: " (<ra> :get-instrument-name parent-instrument-id))
    (for-each (lambda (id)
                (c-display "      " (<ra> :get-instrument-name id)))
              (get-all-instruments-that-we-can-send-to parent-instrument-id))
    (request-send-instrument parent-instrument-id
                             (lambda (create-send-func)
                               (undo-block
                                (lambda ()
                                  (define db (get-db-value))
                                  (define gain (<ra> :db-to-gain db))
                                  ;;(delete)
                                  (<ra> :undo-mixer-connections)
                                  (<ra> :delete-audio-connection parent-instrument-id instrument-id)
                                  (create-send-func gain))))))

  (define (set-db-value db)
    (<ra> :set-instrument-effect instrument-id "System In" (scale db *min-db* *max-db* 0 1)))
  
  (define (reset)
    (make-undo)
    (set-db-value 0))

  (define (get-db-value)
    (scale (<ra> :get-instrument-effect instrument-id "System In")
           0 1
           *min-db* *max-db*))

  (define last-value (get-db-value))
  
  (define doit #t)
  (define slider (strip-slider first-instrument-id
                               parent-instrument-id
                               instrument-id
                               #f #t

                               make-undo

                               ;; get-scaled-value
                               (lambda ()
                                 (db-to-slider (get-db-value)))
                               
                               ;; get-value-text
                               (lambda (slider-value)
                                 (db-to-text (slider-to-db slider-value) #t))

                               ;; set-value
                               (lambda (new-slider-value)
                                 (define db (slider-to-db new-slider-value))
                                 ;;(c-display "new-db:" db ", old-db:" last-value)
                                 (when (and doit (not (= last-value db)))
                                   (set! last-value db)
                                   (set-db-value db)))

                               delete
                               replace
                               reset
                               ))
                                     

  (add-gui-effect-monitor slider instrument-id "System In"
                          (lambda ()
                            (define new-value (db-to-slider (get-db-value)))
                            (when (not (= new-value (<gui> :get-value slider)))
                              (set! doit #f)
                              (<gui> :set-value slider new-value)
                              (set! doit #t))))

  (<gui> :add gui slider))



(define (get-mixer-strip-send-horiz gui)
  (define horiz (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing horiz 1 1 0 1 0)

  (define text-gui #f)

  (define background-color (<gui> :get-background-color gui))
  
  (define text-gui (<gui> :text *arrow-text*))
  (define width (floor (* 1.5 (<gui> :text-width *arrow-text*))))
  
  #||
  (define text-checkbox (create-custom-checkbox (lambda (gui is-selected with height)
                                                  (<gui> :filled-box
                                                         gui
                                                         background-color
                                                         0 0 width height)
                                                  (<gui> :draw-text gui *text-color* *arrow-text* 0 0 width height #f))
                                                (lambda (is-selected)
                                                  #t)
                                                (lambda ()
                                                  #t)))
  
  (define width (floor (* 2 (<gui> :text-width *arrow-text*))))
  
  (set! text-gui (cadr text-checkbox))
  (define set-text-func (car text-checkbox)) 

 ||#
  
  (<gui> :set-min-width text-gui width)
  (<gui> :set-max-width text-gui width)
  
  (<gui> :set-size-policy text-gui #f #t)
  (<gui> :add horiz text-gui)

  (<gui> :add gui horiz)

  horiz)

(define (create-mixer-strip-send gui
                                 first-instrument-id
                                 parent-instrument-id
                                 target-instrument-id
                                 make-undo
                                 get-db-value
                                 set-db-value
                                 add-monitor
                                 delete
                                 replace)

  (define horiz (get-mixer-strip-send-horiz gui))

  (define (reset)
    (make-undo)
    (set-db-value 0)
    (remake-mixer-strips))

  (define doit #t)

  (define last-value (get-db-value))

  (define slider (strip-slider first-instrument-id
                               parent-instrument-id
                               target-instrument-id
                               #t #f

                               make-undo

                               ;; get-scaled-value
                               (lambda ()
                                 (db-to-slider (if add-monitor ;; minor optimization.
                                                   (get-db-value)
                                                   last-value)))

                               ;; get-value-text
                               (lambda (slider-value)                                 
                                 (db-to-text (slider-to-db slider-value) #t))

                               ;; set-value
                               (lambda (new-slider-value)
                                 (define db (slider-to-db new-slider-value))
                                 ;;(c-display "new-db:" db ", old-db:" last-value)
                                 (when (and doit (not (= last-value db)))
                                   (set! last-value db)
                                   (set-db-value db)))
                               
                               delete
                               replace
                               reset))
  
  (if add-monitor
      (add-monitor slider
                   (lambda ()
                     (define new-value (db-to-slider (get-db-value)))
                     (when (not (= new-value (<gui> :get-value slider)))
                       (set! doit #f)
                       (<gui> :set-value slider new-value)
                       (set! doit #t)))))
  
  (<gui> :add horiz slider))


(define (create-mixer-strip-bus-send gui first-instrument-id instrument-id bus-num)
  (define effect-name (list-ref *bus-effect-names* bus-num))
  (define on-off-effect-name (list-ref *bus-effect-onoff-names* bus-num))

  (define bus-id (<ra> :get-audio-bus-id bus-num))

  (define (make-undo)
    (<ra> :undo-instrument-effect instrument-id effect-name))

  (define (delete)
    (<ra> :undo-instrument-effect instrument-id on-off-effect-name)
    (<ra> :set-instrument-effect instrument-id on-off-effect-name 0))

  (define (replace)
    (request-send-instrument instrument-id
                             (lambda (create-send-func)
                               (undo-block
                                (lambda ()
                                  (define db (get-db-value))
                                  (define gain (<ra> :db-to-gain db))
                                  (delete)
                                  (create-send-func gain))))))
  (define (get-db-value)
    (let ((db (<ra> :get-instrument-effect instrument-id effect-name)))
      (scale db
             0 1
             *min-db* *max-db*)))

  (define (set-db-value db)
    (<ra> :set-instrument-effect instrument-id effect-name (scale db *min-db* *max-db* 0 1)))

  (define (add-monitor slider callback)
    (add-gui-effect-monitor slider instrument-id effect-name callback))

  (create-mixer-strip-send gui
                           first-instrument-id
                           instrument-id
                           bus-id
                           make-undo
                           get-db-value
                           set-db-value
                           add-monitor
                           delete
                           replace))


(define (create-mixer-strip-audio-connection-send gui first-instrument-id source-id target-id)
  (define (make-undo)
    (<ra> :undo-audio-connection-gain source-id target-id))

  (define (delete)
    (<ra> :undo-mixer-connections)
    (<ra> :delete-audio-connection source-id target-id))

  (define (replace)
    (request-send-instrument source-id
                             (lambda (create-send-func)
                               (define gain (<ra> :get-audio-connection-gain source-id target-id))
                               (undo-block
                                (lambda ()
                                  (delete)
                                  (create-send-func gain))))))
  
  (define (get-db-value)
    (define db (<ra> :gain-to-db (<ra> :get-audio-connection-gain source-id target-id)))
    ;;(c-display "getting " db)
    db)

  (define (set-db-value db)
    ;;(c-display "setting db to" db)
    (<ra> :set-audio-connection-gain source-id target-id (<ra> :db-to-gain db) #f))

  (define add-monitor #f)

  (create-mixer-strip-send gui
                           first-instrument-id
                           source-id
                           target-id
                           make-undo 
                           get-db-value
                           set-db-value
                           add-monitor
                           delete
                           replace))


;; Returns a list of parallel plugins that needs their own mixer strip.
(define (get-returned-plugin-buses instrument-id)
  (define returned-plugin-buses '())

  (define out-instruments (sort-instruments-by-mixer-position ;; Needs to be sorted.
                           (get-instruments-connecting-from-instrument instrument-id)))

  (define next-plugin-instrument (find-next-plugin-instrument-in-path instrument-id))

  (define ret (keep (lambda (out-instrument)
                      (if (= 1 (<ra> :get-num-in-audio-connections out-instrument))
                          (if (not next-plugin-instrument)
                              #t
                              (if (= next-plugin-instrument out-instrument)
                                  #f
                                  #t))
                          #f))
                               
                    out-instruments))

  (if next-plugin-instrument
      (append ret 
              (get-returned-plugin-buses next-plugin-instrument))
      ret))


;; Returns the last plugin.
(define (find-meter-instrument-id instrument-id)
  (define next-plugin-instrument (find-next-plugin-instrument-in-path instrument-id))
  (if next-plugin-instrument
      (find-meter-instrument-id next-plugin-instrument)
      instrument-id))

(define (get-mixer-strip-path-instruments instrument-id kont)
  (define bus-nums (if (<ra> :instrument-is-bus-descendant instrument-id)
                       '()
                       (get-buses-connecting-from-instrument instrument-id #t)))
  
  (define out-instruments (sort-instruments-by-mixer-position
                           (get-instruments-connecting-from-instrument instrument-id)))
  (define next-plugin-instrument (find-next-plugin-instrument-in-path instrument-id))

  (define instrument-sends (keep (lambda (out-instrument)
                                   (or (not next-plugin-instrument)
                                       (not (= next-plugin-instrument out-instrument))))
                                 out-instruments))
  
  (kont bus-nums
        instrument-sends
        next-plugin-instrument))

;; Returns the last plugin.
(define (create-mixer-strip-path gui first-instrument-id instrument-id)
  (get-mixer-strip-path-instruments instrument-id
                                    (lambda (bus-sends instrument-sends next-plugin-instrument)
                                      (for-each (lambda (bus-num)
                                                  (create-mixer-strip-bus-send gui
                                                                               instrument-id
                                                                               instrument-id
                                                                               bus-num))
                                                bus-sends)
                                      
                                      (for-each (lambda (out-instrument)
                                                  (create-mixer-strip-audio-connection-send gui
                                                                                            first-instrument-id
                                                                                            instrument-id
                                                                                            out-instrument))
                                                instrument-sends)

                                      (if next-plugin-instrument
                                          (begin
                                            (if (= 0 (<ra> :get-num-output-channels next-plugin-instrument))
                                                (create-mixer-strip-sink-plugin gui instrument-id instrument-id next-plugin-instrument)
                                                (create-mixer-strip-plugin gui first-instrument-id instrument-id next-plugin-instrument))
                                            (create-mixer-strip-path gui first-instrument-id next-plugin-instrument))
                                          instrument-id))))

(define (get-all-instruments-used-in-mixer-strip instrument-id)
  (get-mixer-strip-path-instruments instrument-id
                                    (lambda (bus-sends instrument-sends next-plugin-instrument)
                                      (append (list instrument-id)
                                              bus-sends
                                              instrument-sends
                                              (if next-plugin-instrument
                                                  (get-all-instruments-used-in-mixer-strip next-plugin-instrument)
                                                  '())))))
                                              

(define (create-mixer-strip-pan instrument-id system-background-color background-color height)
  (define (pan-enabled?)
    (>= (<ra> :get-instrument-effect instrument-id "System Pan On/Off") 0.5))
  
  (define (get-pan)
    (floor (scale (<ra> :get-instrument-effect instrument-id "System Pan")
                  0 1
                  -90 90)))
  (define doit #t)

  (define paint #f)

  (define last-slider-val (get-pan))

  (define slider #f)
  (set! slider (<gui> :horizontal-int-slider
                      "pan: "
                      -90 (get-pan) 90
                      (lambda (degree)
                        (when (and doit (not (= last-slider-val degree))) ;; (pan-enabled?))
                          (set! last-slider-val degree)
                          (<ra> :set-instrument-effect instrument-id "System Pan On/Off" 1.0)
                          (<ra> :set-instrument-effect instrument-id "System Pan" (scale degree -90 90 0 1))
                          (if slider
                              (<gui> :update slider))))))

  (set-fixed-height slider height)

  (set! paint
        (lambda ()
          (define width (<gui> :width slider))
          (define value (get-pan))
          (define is-on (pan-enabled?))
          (<gui> :filled-box slider system-background-color 0 0 width height)
          (define background (if is-on
                                 (<gui> :mix-colors background-color "black" 0.39)
                                 (<gui> :mix-colors background-color "white" 0.95)))
          (<gui> :filled-box slider background 0 0 width height 5 5)
          (define col1 (<gui> :mix-colors "white" background 0.4))
          (define col2 (<gui> :mix-colors "#010101" background 0.5))

          (define inner-width/2 (scale 1 0 18 0 (get-fontheight)))
          (define outer-width/2 (* inner-width/2 2))

          (define middle (scale value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))

          (<gui> :filled-box slider col1 (- middle inner-width/2) 2 (+ middle inner-width/2) (- height 3))
          (<gui> :filled-box slider col2 (- middle inner-width/2 outer-width/2) 2 (- middle inner-width/2) (- height 3))
          (<gui> :filled-box slider col2 (+ middle inner-width/2) 2 (+ middle inner-width/2 outer-width/2) (- height 3))
          ;;(<gui> :draw-text slider "white" (<-> value "o") 0 0 width height #t)
          (<gui> :draw-box slider "#404040" 0 0 width height 2)
          ))

  (<gui> :add-paint-callback slider (lambda x (paint)))

  ;;(paint)

  (add-gui-effect-monitor slider instrument-id "System Pan"
                          (lambda ()
                            (set! doit #f)
                            (<gui> :set-value slider (get-pan))
                            (<gui> :update slider)
                            (set! doit #t)))
  
  (add-gui-effect-monitor slider instrument-id "System Pan On/Off" (lambda ()
                                                                     (<gui> :update slider)))

  (<gui> :add-mouse-callback slider
         (lambda (button state x y)
           (cond ((and (= button *left-button*)
                       (= state *is-pressing*))
                  (undo-block
                   (lambda ()
                     (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
                     (<ra> :undo-instrument-effect instrument-id "System Pan"))))

                 ((and (= button *right-button*)
                       (= state *is-releasing*))
                  
                  (popup-menu "Reset" (lambda ()
                                        (<ra> :undo-instrument-effect instrument-id "System Pan")
                                        (<ra> :set-instrument-effect instrument-id "System Pan" 0.5))
                              (list "Enabled"
                                    :check (pan-enabled?)
                                    (lambda (onoff)
                                      (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
                                      (<ra> :set-instrument-effect instrument-id "System Pan On/Off" (if onoff 1.0 0.0)))))))
           #f))

  slider)

;;(define (create-mixer-strip-checkbox text sel-color unsel-color width height callback)
;;  (define button (<gui> :widget width height))

(define (create-mixer-strip-mutesolo instrument-id background-color height is-minimized)
  
  (define (draw-mutesolo checkbox is-selected is-implicitly-selected text color width height)
    (<gui> :filled-box
           checkbox
           background-color
           0 0 width height)
    (<gui> :filled-box
           checkbox
           (if is-selected
               color
               background-color)
           2 2 (- width 2) (- height 2)
           5 5)
    (<gui> :draw-text
           checkbox
           "black"
           text
           3 2 (- width 3) (- height 2)
           )
    
    (<gui> :draw-box
           checkbox
           (if is-implicitly-selected
               color
               "#404040")
           2 2 (- width 2) (- height 2)
           (if is-implicitly-selected
               2.0
               1.0)
           5 5)
    )

  (define (get-muted)
    (< (<ra> :get-instrument-effect instrument-id "System Volume On/Off") 0.5))
  (define (get-soloed)
    (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
           
  (define (turn-off-all-mute except)
    (for-each (lambda (instrument-id)
                (when (and (not (= instrument-id except))
                           (< (<ra> :get-instrument-effect instrument-id "System Volume On/Off") 0.5))
                  (<ra> :undo-instrument-effect instrument-id "System Volume On/Off")
                  (<ra> :set-instrument-effect instrument-id "System Volume On/Off" 1)
                  ))
              (get-all-audio-instruments)))
  
  (define (turn-off-all-solo except)
    (for-each (lambda (instrument-id)
                (when (and (not (= instrument-id except))
                           (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
                  ;;(<ra> :undo-instrument-effect instrument-id "System Solo On/Off")
                  (<ra> :set-instrument-effect instrument-id "System Solo On/Off" 0)
                  ))
              (get-all-audio-instruments)))

  
  (define implicitly-muted (<ra> :instrument-is-implicitly-muted instrument-id)) ;; Mutable variable
  (define (draw-mute mute is-muted width height)
    (draw-mutesolo mute is-muted implicitly-muted (if is-minimized "M" "Mute") "green" width height))

  (define mute (create-custom-checkbox draw-mute
                                       (lambda (is-muted)
                                         (undo-block
                                          (lambda ()
                                            (<ra> :undo-instrument-effect instrument-id "System Volume On/Off")
                                            (<ra> :set-instrument-effect instrument-id "System Volume On/Off" (if is-muted 0.0 1.0))
                                            ;;(c-display "mute: " is-muted)
                                            (if (<ra> :ctrl-pressed)
                                                (turn-off-all-mute instrument-id))
                                            )))
                                       (get-muted)
                                       height))

  (<ra> :schedule (random 1000) (let ((mute (cadr mute)))
                                  (lambda ()
                                    (if (and (<gui> :is-open mute) (<ra> :instrument-is-open instrument-id))
                                        (let ((last-implicitly-muted implicitly-muted))
                                          (set! implicitly-muted (<ra> :instrument-is-implicitly-muted instrument-id))
                                          (when (not (eq? implicitly-muted last-implicitly-muted))
                                            ;;(draw-mute mute (get-muted) (<gui> :width mute) (<gui> :height mute))
                                            (<gui> :update mute)
                                            )
                                          100)
                                        #f))))
  
  (define solo (create-custom-checkbox (lambda (solo is-soloed width height)
                                         (draw-mutesolo solo is-soloed #f (if is-minimized "S" "Solo") "yellow" width height))
                                       (lambda (is-selected)
                                         (undo-block
                                          (lambda ()
                                            ;;(<ra> :undo-instrument-effect instrument-id "System Solo On/Off")
                                            (<ra> :set-instrument-effect instrument-id "System Solo On/Off" (if is-selected 1.0 0.0))
                                            (if (<ra> :ctrl-pressed)
                                                (turn-off-all-solo instrument-id)))))
                                       (get-soloed)
                                       height))
  
  (add-gui-effect-monitor (cadr mute) instrument-id "System Volume On/Off"
                          (lambda ()
                            ((car mute) (get-muted))))
  
  (add-gui-effect-monitor (cadr solo) instrument-id "System Solo On/Off"
                          (lambda ()
                            (c-display "Solo changed for" instrument-id)
                            ((car solo) (get-soloed))))
  
  (define gui (if is-minimized
                  (<gui> :vertical-layout)
                  (<gui> :horizontal-layout)))
  (<gui> :set-layout-spacing gui 0 0 0 0 0)

  (if is-minimized
      (begin
        (set-fixed-height (cadr mute) height)
        (set-fixed-height (cadr solo) height))
      (set-fixed-height gui height))

  (<gui> :add gui (cadr mute) 1)
  (<gui> :add gui (cadr solo) 1)
  gui
  )

(define (create-mixer-strip-volume instrument-id meter-instrument-id background-color is-minimized)
  (define fontheight (get-fontheight))
  (define voltext-height fontheight)

  (define (get-border-size width)
    (scale 0.05 0 1 0 width))

  (define show-voltext #t)
  (define show-peaktext (not is-minimized))

#||
  (define middle (floor (average x1 x2)))

  (define voltext_x2 (+ x1 middle))
  (define voltext_y1 (round (+ y1 border-size/2)))
  (define voltext_y2 (+ voltext_y1 voltext-height))

  (define peaktext_y1 voltext_y1)
  (define peaktext_x1 middle)
  (define peaktext_x2 x2)
  (define peaktext_y2 voltext_y2)

  (define volslider_x1 x1)
  (define volslider_y1 (round (+ border-size/2 voltext_y2)))
  (define volslider_x2 middle)
  (define volslider_y2 (round (- y2 border-size)))

  (define peak_x1 peaktext_x1)
  (define peak_y1 volslider_y1)
  (define peak_x2 peaktext_x2)
  (define peak_y2 volslider_y2)
||#

  (define peaktexttext "-inf")

  (define is-sink? (= 0 (<ra> :get-num-output-channels instrument-id)))

  (define effect-name (if is-sink?
                          "System In"
                          "System Volume"))
  
  (define (get-volume)
    ;(c-display "           got"
    ;           (<ra> :get-instrument-effect instrument-id effect-name)
    ;           (scale (<ra> :get-instrument-effect instrument-id effect-name)
    ;                  0 1
    ;                  *min-db* *max-db*)
    ;           " for " (<ra> :get-instrument-name instrument-id))
    (scale (<ra> :get-instrument-effect instrument-id effect-name)
           0 1
           *min-db* *max-db*))

  (define doit #f)

  (define paint-voltext #f)
  (define paint-peaktext #f)

  (define voltext (<gui> :widget))
  (define peaktext (<gui> :widget))

  (define paint-slider #f)

  (define last-vol-slider (get-volume))
  (define volslider #f)
  (set! volslider (<gui> :vertical-slider
                         ""
                         0 (db-to-slider (get-volume)) 1
                         (lambda (val)
                           (define db (slider-to-db val))
                           (when (and doit (not (= last-vol-slider db)))
                             (set! last-vol-slider db)
                             ;;(c-display "             hepp hepp")
                             (<ra> :set-instrument-effect instrument-id effect-name (scale db *min-db* *max-db* 0 1))
                             (if paint-voltext
                                 (<gui> :update voltext)))
                           (if volslider
                               (<gui> :update volslider)))))
    
  (define (paint-text gui text)
    (define width (<gui> :width gui))
    (define height (<gui> :height gui))
    
    (define border-size (get-border-size width))

    (define col1 (<gui> :mix-colors "#010101" background-color 0.7))
    
    ;; background
    (<gui> :filled-box gui background-color 0 0 width height)
    
    ;; rounded
    (<gui> :filled-box gui col1 border-size 0 (- width border-size) height 5 5)
    
    ;; text
    (<gui> :draw-text gui *text-color* text 0 0 width height))

    
  (when show-voltext
    (set! paint-voltext
          (lambda ()
            (paint-text voltext (db-to-text (get-volume) #f))))
    
    (<gui> :add-paint-callback voltext (lambda x (paint-voltext)))
    ;;(paint-voltext)
    )

  (when show-peaktext
    (set! paint-peaktext
          (lambda ()
            (paint-text peaktext peaktexttext)))
    
    (<gui> :add-paint-callback peaktext (lambda x (paint-peaktext)))
    ;;(paint-peaktext)
    )

  (set! paint-slider
        (lambda ()
          (define width (<gui> :width volslider))
          (define height (<gui> :height volslider))
          (define border-size (get-border-size width))
          (define x1 border-size) ;;(scale 0.1 0 1 0 width))
          (define x2 (- width border-size)) ;;(scale 0.9 0 1 0 width))
          (define middle_y (scale (db-to-slider (get-volume)) 0 1 height 0))
          
          ;; background
          (<gui> :filled-box volslider background-color 0 0 width height)
          
          (define col1 (<gui> :mix-colors
                              (if is-sink? "#f0f0f0" "#010101")
                              background-color 0.2)) ;; down
          (define col2 (<gui> :mix-colors "#010101" background-color 0.9)) ;; up

          ;; slider
          (<gui> :filled-box volslider col2 x1 0 x2 height 5 5) ;; up (fill everything)
          (<gui> :filled-box volslider col1 x1 middle_y x2 height 5 5) ;; down

          ;; slider border
          (<gui> :draw-box volslider "black" x1 0 x2 height 1.0)

          ;; slider 0db, white line
          (define middle_0db (scale (db-to-slider 0) 0 1 height 0))
          (<gui> :draw-line volslider "#eeeeee" (1+ x1) middle_0db (1- x2) middle_0db 0.3)
          ))

  (<gui> :add-paint-callback volslider (lambda x (paint-slider)))


  (define volmeter (<gui> :vertical-audio-meter meter-instrument-id))
  
  (add-gui-effect-monitor volslider instrument-id effect-name
                          (lambda ()
                            (set! doit #f)
                            (<gui> :set-value volslider (db-to-slider (get-volume)))
                            ;;(<gui> :set-value voltext (get-volume))
                            (if paint-voltext
                                (<gui> :update voltext))
                            (<gui> :update volslider)
                            (set! doit #t)))

  (<gui> :add-mouse-callback volslider
         (lambda (button state x y)
           (cond ((and (= button *left-button*)
                       (= state *is-pressing*))
                  (<ra> :undo-instrument-effect instrument-id effect-name))
                 ((and (= button *right-button*)
                       (= state *is-releasing*))
                  (popup-menu "Reset" (lambda ()
                                        (<ra> :undo-instrument-effect instrument-id effect-name)
                                        (<ra> :set-instrument-effect instrument-id effect-name (scale 0 *min-db* *max-db* 0 1))))))
           #f))

  (when show-voltext
    (<gui> :add-mouse-callback voltext (lambda (button state x y)
                                         (when (and (= button *left-button*)
                                                    (= state *is-pressing*))
                                           (let ((maybe (<ra> :request-float "" *min-db* *max-db* #t)))
                                             (when (>= maybe *min-db*)
                                               (<ra> :undo-instrument-effect instrument-id effect-name)
                                               (<ra> :set-instrument-effect instrument-id effect-name (scale maybe *min-db* *max-db* 0 1)))))
                                         #t))
    )

  (when show-peaktext

    (<gui> :add-audio-meter-peak-callback volmeter (lambda (text)
                                                     (set! peaktexttext text)
                                                     (<gui> :update peaktext)))

    (<gui> :add-mouse-callback peaktext (lambda (button state x y)
                                          (when (and (= button *left-button*)
                                                     (= state *is-pressing*))
                                            (set! peaktexttext "-inf")
                                            (<gui> :reset-audio-meter-peak volmeter)
                                            (<gui> :update peaktext))
                                          #t)))

  ;; horiz 1 (voltext and peaktext)
  ;;
  (define horizontal1 (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing horizontal1 0 0 0 0 0)

  (if (or show-voltext show-peaktext)
      (set-fixed-height horizontal1 voltext-height))

  (if show-voltext
      (<gui> :add horizontal1 voltext 1))

  (if show-peaktext
    (<gui> :add horizontal1 peaktext 1))


  ;; horiz 2 (volume slider and audio meter)
  ;;
  (define horizontal2 (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing horizontal2 0 0 0 0 0)
  (<gui> :set-size-policy horizontal2 #t #t)
  (<gui> :set-size-policy volslider #t #t)
  (<gui> :set-size-policy volmeter #t #t)
  (<gui> :add horizontal2 volslider 1)
  (<gui> :add horizontal2 volmeter 1)

  ;; vertical
  (define vertical (<gui> :vertical-layout))
  (<gui> :set-layout-spacing vertical 5 0 0 0 0)

  (<gui> :add vertical horizontal1)
  (<gui> :add vertical horizontal2)

  (set! doit #t)

  vertical
  )


(define (get-mixer-strip-background-color gui instrument-id)
  (<gui> :mix-colors
         (<ra> :get-instrument-color instrument-id)
         (<gui> :get-background-color gui)
         0.3))


(define (create-mixer-strip-comment instrument-id height)
  (define comment-edit (<gui> :line (<ra> :get-instrument-comment instrument-id)
                              (lambda (new-name)
                                (<ra> :set-instrument-comment new-name instrument-id))))    
  (<gui> :set-background-color comment-edit (<ra> :get-instrument-color instrument-id))
  (set-fixed-height comment-edit height)
  comment-edit)

(define (draw-mixer-strips-border gui width height)
  (<gui> :draw-box gui "#bb222222" 0 0 width height 2 3 3))

(define (create-mixer-strip-minimized instrument-id)
  (define color (<ra> :get-instrument-color instrument-id))

  ;;(define gui (<gui> :vertical-layout)) ;; min-width height))
  ;;(<gui> :add gui (<gui> :checkbox "" #f))
  ;;(<gui> :add gui (<gui> :text (<ra> :get-instrument-name instrument-id)))
  
  ;(define gui (<gui> :checkbox (<ra> :get-instrument-name instrument-id) #f #t))
  ;(<gui> :set-background-color gui (<ra> :get-instrument-color instrument-id))
  ;;(set-fixed-width gui (get-fontheight))

  (define gui (<gui> :vertical-layout)) ;; min-width height))
  (<gui> :set-layout-spacing gui 5 5 5 5 5);;2 2 2 2)

  (define background-color (get-mixer-strip-background-color gui instrument-id))
  (<gui> :set-background-color gui color)
  
  ;;(define gui (<gui> :widget))
  (set-fixed-width gui (floor (* 1.2 (get-fontheight))))
  
  (define label (create-mixer-strip-name instrument-id #t))

  (define meter-instrument-id (find-meter-instrument-id instrument-id))

  (<gui> :add gui label 1)
  (<gui> :add gui (create-mixer-strip-mutesolo instrument-id background-color (get-fontheight) #t))

  (define volume-gui (create-mixer-strip-volume instrument-id meter-instrument-id background-color #t))
  (<gui> :add gui volume-gui 1)
  
  (<gui> :add-paint-callback gui
         (lambda (width height)
           ;;(set-fixed-height volume-gui (floor (/ height 2)))
           (<gui> :filled-box gui background-color 0 0 width height 0 0)
           (draw-mixer-strips-border gui width height)
           #t
           )
         )

  gui)


(define (create-mixer-strip-wide instrument-id min-width)
  (define gui (<gui> :vertical-layout)) ;; min-width height))
  (<gui> :set-min-width gui min-width)
  ;;(<gui> :set-max-width gui width)
  ;;(<gui> :set-size-policy gui #f #t)
  (<gui> :set-layout-spacing gui 2 4 4 4 4)

  (define background-color (get-mixer-strip-background-color gui instrument-id))

  (define system-background-color (<gui> :get-background-color gui))
  ;;(c-display "               backgroudn: " (<gui> :get-background-color gui))
  (<gui> :set-background-color gui background-color) ;;(<ra> :get-instrument-color instrument-id))
  
#||
  (define y1 0)
  (define x1 0)
  (define x2 min-width)
  (define y2 height)
  (define top-y y1)
  ||#

  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define name-height fontheight-and-borders)
  (define pan-height fontheight-and-borders)
  (define mutesolo-height fontheight-and-borders)
  (define comment-height fontheight-and-borders)

#||
  (define min-send-height (* 2 fontheight-and-borders))
  (define min-volume-height (* 3 fontheight-and-borders))

  (define height-left (- height
                         (+ name-height pan-height mutesolo-height comment-height)))

  (define sends-height (max min-send-height
                            (floor (/ height-left 2))))
  (define volume-height (max min-volume-height
                             (1+ (floor (/ height-left 2)))))
||#

#||
  (define name_y1 y1)
  (define name_y2 (+ y1 name-height))

  (define sends_y1 name_y2)
  (define sends_y2 (+ sends_y1 sends-height))

  (define pan_y1 sends_y2)
  (define pan_y2 (+ pan_y1 pan-height))

  (define mutesolo_y1 pan_y2)
  (define mutesolo_y2 (+ mutesolo_y1 mutesolo-height))

  (define volume_y1 mutesolo_y2)
  (define volume_y2 (+ volume_y1 volume-height))

  (define comment_y1 volume_y2)
  (define comment_y2 (+ comment_y1 comment-height))
||#

  (define name (create-mixer-strip-name instrument-id #f))
  (set-fixed-height name name-height)

  (<gui> :add gui name)

  (define mixer-strip-path-gui (<gui> :vertical-scroll))
  (<gui> :set-layout-spacing mixer-strip-path-gui 5 5 5 5 5)


  (define hepp (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing hepp 0 5 2 5 2)

  ;;(<gui> :add-layout-space hepp 2 2 #f #f)
  (<gui> :add hepp mixer-strip-path-gui)
  ;;(<gui> :add-layout-space hepp 2 2 #f #f)

  (<gui> :add gui hepp 1)

  ;(set-fixed-width mixer-strip-path-gui (- (<gui> :width gui) 26))

  '(<gui> :add-paint-callback gui
         (lambda (width height)
           (set-fixed-width mixer-strip-path-gui (- width 26))))

  
  (<gui> :add-mouse-callback mixer-strip-path-gui (lambda (button state x y)
                                                    (if (and (= button *right-button*)
                                                             (= state *is-pressing*))
                                                        (if (<ra> :shift-pressed)
                                                            (<ra> :delete-instrument instrument-id)
                                                            (create-default-mixer-path-popup instrument-id)))
                                                    #f))

  (define meter-instrument-id (create-mixer-strip-path mixer-strip-path-gui instrument-id instrument-id))

  (<gui> :add gui (create-mixer-strip-pan instrument-id system-background-color background-color pan-height))
  (<gui> :add gui (create-mixer-strip-mutesolo instrument-id background-color mutesolo-height #f))
  (<gui> :add gui (create-mixer-strip-volume instrument-id meter-instrument-id background-color #f) 1)
  (<gui> :add gui (create-mixer-strip-comment instrument-id comment-height))

  (<gui> :add-paint-callback gui
         (lambda (width height)
           (<gui> :filled-box gui background-color 0 0 width height 0 0)
           (draw-mixer-strips-border gui width height)))
  gui)

(define (create-mixer-strip instrument-id min-width)
  (if (<ra> :has-wide-instrument-strip instrument-id)
      (create-mixer-strip-wide instrument-id min-width)
      (create-mixer-strip-minimized instrument-id)))


;; Stored mixer strips.
;;
;; These are used for caching so that we don't have to recreate all strips when recreating a mixer strips window
(define (create-stored-mixer-strip instrument-id mixer-strip)
  (list instrument-id mixer-strip))

(define (get-instrument-id-from-stored-mixer-strip stored-mixer-strip)
  (car stored-mixer-strip))

(define (get-mixer-strip-from-stored-mixer-strip stored-mixer-strip)
  (cadr stored-mixer-strip))

(define (get-stored-mixer-strip stored-mixer-strips instrument-id)
  (assv instrument-id stored-mixer-strips))

(define (stored-mixer-strip-is-valid? stored-mixer-strip list-of-modified-instrument-ids)
  (if (or (not stored-mixer-strip)
          (null? list-of-modified-instrument-ids))
      #f
      (let ((instrument-id (get-instrument-id-from-stored-mixer-strip stored-mixer-strip)))
        (not (or (memv instrument-id list-of-modified-instrument-ids)
                 (any? (lambda (instrument-id)
                         (memv instrument-id list-of-modified-instrument-ids))
                       (get-all-instruments-used-in-mixer-strip instrument-id)))))))



(define (create-standalone-mixer-strip instrument-id width height)
  ;;(define parent (<gui> :horizontal-layout))
  ;;(<gui> :set-layout-spacing parent 0 0 0 0 0)

  (define parent (<gui> :widget))

  ;;(define width (floor (* 1 (<gui> :text-width "MUTE SOLO"))))

  (set-fixed-width parent width)
  ;;(set-fixed-height parent height)
  ;;(<gui> :set-min-width parent 100)
  ;;(<gui> :set-max-width parent 100)
  
  (define das-mixer-strip-gui #f)

  (define (remake width height)
    (define instrument-is-open (<ra> :instrument-is-open instrument-id))
    
    (c-display "    remaking mixer-strip" instrument-id parent width height)
    (catch #t
           (lambda ()
             ;;(<gui> :disable-updates parent)
             
             (define new-mixer-strip (and instrument-is-open (create-mixer-strip instrument-id width)))
             
             (when das-mixer-strip-gui
               (<gui> :close das-mixer-strip-gui)
               (set! das-mixer-strip-gui #f))

             (when instrument-is-open
               (<gui> :add parent new-mixer-strip 0 0 width height)
               (<gui> :show new-mixer-strip)             
               (set! das-mixer-strip-gui new-mixer-strip))
             )
           
           (lambda args
             (display (ow!))))
  
    ;;(<gui> :enable-updates parent)
    )

  (remake width height)

  ;;(<gui> :add-paint-callback parent remake)
  (<gui> :add-paint-callback parent
         (lambda (width height)
           (when das-mixer-strip-gui
             (set-fixed-height das-mixer-strip-gui height))))
  
  (define mixer-strips-object (make-mixer-strips-object :gui parent
                                                        :remake (lambda (list-of-modified-instrument-ids)
                                                                  (remake (<gui> :width parent) (<gui> :height parent)))))
  
  ;;(<ra> :inform-about-gui-being-a-mixer-strips parent) // Must only be called for standalone windows.

  (push-back! *mixer-strips-objects* mixer-strips-object)
  
  (<gui> :add-close-callback parent
         (lambda ()
           (set! *mixer-strips-objects*
                 (remove (lambda (a-mixer-strip-object)
                           (= (a-mixer-strip-object :gui)
                              parent))
                         *mixer-strips-objects*))))

  parent)



#!
(begin
  (define pos-x (or (and (defined? 'mixer-strips) (<gui> :get-x mixer-strips))
                    400))
  (define pos-y (or (and (defined? 'mixer-strips) (<gui> :get-y mixer-strips))
                    50))
  (if (defined? 'mixer-strips)
      (<gui> :close mixer-strips))
  (define mixer-strips (<gui> :widget 300 800))
  ;;(<gui> :draw-box mixer-strips "black" (- 20 1) (- 20 1) (1+ 220) (1+ 700) 1.0)
  ;;(<gui> :filled-box mixer-strips "black" (- 20 1) (- 20 1) (1+ 220) (1+ 700))
  (create-mixer-strip mixer-strips (get-instrument-from-name "Sample Player 1") 220)
  (<gui> :show mixer-strips)

  (<gui> :set-pos mixer-strips pos-x pos-y)
  )
!#

#!
(define mixer-strips (<gui> :widget 300 800))
!#

;;!#

(define (get-mixer-strip-instrument-ids)
  (define instruments (keep (lambda (id)
                              (or (> (<ra> :get-num-input-channels id)
                                     0)
                                  (> (<ra> :get-num-output-channels id)
                                     0)))
                            (get-all-instruments-with-no-input-connections)))

  (define instrument-plugin-buses (apply append (map (lambda (instrument-id)
                                                       (get-returned-plugin-buses instrument-id))
                                                     instruments)))

  (define buses (append (get-all-instruments-with-at-least-two-input-connections)
                        (get-buses)))

  (define buses-plugin-buses (apply append (map (lambda (instrument-id)
                                                  (get-returned-plugin-buses instrument-id))
                                                (append buses
                                                        instrument-plugin-buses))))
  (define all-buses (append instrument-plugin-buses
                            buses
                            buses-plugin-buses))


  (list instruments
        all-buses))


(define (create-mixer-strips num-rows stored-mixer-strips list-of-modified-instrument-ids kont)
  ;;(set! num-rows 3)
  (define strip-separator-width 5)
  (define instruments-buses-separator-width (* (get-fontheight) 2))

  ;;(define mixer-strips (<gui> :widget 800 800))
  ;;(define mixer-strips (<gui> :horizontal-scroll)) ;;widget 800 800))
  (define mixer-strips-gui (<gui> :scroll-area #t #t))
  (<gui> :set-layout-spacing mixer-strips-gui 0 0 0 0 0)

  (define vertical-layout (<gui> :vertical-layout))
  (<gui> :set-layout-spacing vertical-layout strip-separator-width 0 0 0 0)

  (define layout (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing layout strip-separator-width 0 0 0 0)

  (<gui> :add vertical-layout layout)
  (<gui> :add mixer-strips-gui vertical-layout)


  (define mixer-strip-instrument-ids (get-mixer-strip-instrument-ids))
  (define instruments (car mixer-strip-instrument-ids))
  (define all-buses (cadr mixer-strip-instrument-ids))

  (define num-strips (+ (length instruments)
                        (length all-buses)))


  (define min-mixer-strip-width (1+ (floor (max (<gui> :text-width " -14.2 -23.5 ")
                                                (<gui> :text-width " Mute Solo ")))))

  (define mixer-strip-num 0)
  (define num-strips-per-row (ceiling (/ num-strips num-rows)))

  (define (add-strips id-instruments)
    (map (lambda (instrument-id)
           (define stored-mixer-strip (get-stored-mixer-strip stored-mixer-strips instrument-id))
           (define mixer-strip (if (stored-mixer-strip-is-valid? stored-mixer-strip list-of-modified-instrument-ids)
                                   (get-mixer-strip-from-stored-mixer-strip stored-mixer-strip)
                                   (create-mixer-strip instrument-id min-mixer-strip-width)))
           '(c-display "   Creating" instrument-id ". Stored is valid?" (stored-mixer-strip-is-valid? stored-mixer-strip list-of-modified-instrument-ids)
                      "stored-mixer-strip:" stored-mixer-strip
                      "list-of-modified:" list-of-modified-instrument-ids)
           (<gui> :add layout mixer-strip)
           (set! mixer-strip-num (1+ mixer-strip-num))
           (if (= num-strips-per-row mixer-strip-num)
               (begin
                 (set! layout (<gui> :horizontal-layout))
                 (<gui> :set-layout-spacing layout strip-separator-width 0 0 0 0)
                 (<gui> :add vertical-layout layout)
                 (set! mixer-strip-num 0)))
           (create-stored-mixer-strip instrument-id
                                      mixer-strip))
         id-instruments))

  (define instrument-mixer-strips (add-strips (sort-instruments-by-mixer-position
                                               instruments)))

  (if (> mixer-strip-num 0)
      (<gui> :add-layout-space layout instruments-buses-separator-width 10 #f #f))

  (define bus-mixer-strips (add-strips (sort-instruments-by-mixer-position-and-connections
                                        all-buses)))

  (kont (append instrument-mixer-strips
                bus-mixer-strips)
        mixer-strips-gui)
  )

#!
(<gui> :show (create-mixer-strips 1000 800))
!#

(define-struct mixer-strips-object
  :gui
  :remake
  :is-full-screen #f
  :pos #f)

(define *mixer-strips-objects* '())

(delafina (create-mixer-strips-gui :num-rows 1
                                   :is-full-screen #f
                                   :pos #f)
  ;;(define parent (<gui> :horizontal-layout))
  ;;(define parent (<gui> :scroll-area #t #t))
  ;;(define parent (<gui> :widget))
  (define parent (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing parent 0 0 0 0 0)

  (define width (if pos (caddr pos) 1000))
  (define height (if pos (cadddr pos) 800))

  (<gui> :set-size parent width height)
  (<gui> :set-pos
         parent
         (if pos (car pos) 600)
         (if pos (cadr pos) 50))
  ;;(<gui> :set-layout-spacing parent 0 0 0 0 0)

  (if (not is-full-screen)
      (<gui> :set-always-on-top parent)
      (<gui> :set-full-screen parent))
  
  (<gui> :show parent)
      
  ;;(<gui> :set-full-screen parent)

  (define das-stored-mixer-strips '())
  (define das-mixer-strips-gui #f)
  
  (define (remake list-of-modified-instrument-ids)
    (catch #t
           (lambda ()
;;             (<gui> :disable-updates parent)
             
             (create-mixer-strips num-rows das-stored-mixer-strips list-of-modified-instrument-ids
                                  (lambda (mixer-strips mixer-strips-gui)
             
                                    (if das-mixer-strips-gui
                                        (<gui> :close das-mixer-strips-gui))
                                    
                                    (<gui> :add parent mixer-strips-gui)
                                    
                                    (<gui> :show mixer-strips-gui)

                                    (set! das-stored-mixer-strips mixer-strips)
                                    (set! das-mixer-strips-gui mixer-strips-gui)
                                    )))

           (lambda args
             (display (ow!))))
  ;;  (<gui> :enable-updates parent)
    )

  (define mixer-strips-object (make-mixer-strips-object :gui parent
                                                        :is-full-screen is-full-screen
                                                        :remake remake
                                                        :pos pos))
  
  (remake '())

  (<ra> :inform-about-gui-being-a-mixer-strips parent)
  (push-back! *mixer-strips-objects* mixer-strips-object)

  (<gui> :add-close-callback parent
         (lambda ()
           (set! *mixer-strips-objects*
                 (remove (lambda (a-mixer-strips-object)
                           (= (a-mixer-strips-object :gui)
                              parent))
                         *mixer-strips-objects*))))

  mixer-strips-object
  )

(define (remake-mixer-strips . list-of-modified-instrument-ids)
  ;;(c-display "\n\n\n             REMAKE MIXER STRIPS " list-of-modified-instrument-ids "\n\n\n")
  (for-each (lambda (a-mixer-strips-object)
              ((a-mixer-strips-object :remake) list-of-modified-instrument-ids))
            *mixer-strips-objects*))

(define (toggle-all-mixer-strips-fullscreen)
  (define set-to 0)
  (for-each (lambda (a-mixer-strips-object)
              (if (number? set-to)
                  (set! set-to (not (<gui> :is-full-screen (a-mixer-strips-object :gui)))))
              (<gui> :set-full-screen (a-mixer-strips-object :gui) set-to))
            *mixer-strips-objects*))

(define (toggle-current-mixer-strips-fullscreen)

  ;; fallback solution if qt fights back too much.
  (define (toggle-by-recreating mixer-strips)
    (define gui (mixer-strips :gui))
    (<gui> :close gui)
    (if (mixer-strips :is-full-screen)
        (begin
          (define pos (mixer-strips :pos))
          (create-mixer-strips-gui 1 #f pos))
        (begin
          (define x (<gui> :get-x gui))
          (define y (<gui> :get-y gui))
          (define width (<gui> :width gui))
          (define height (<gui> :height gui))
          (define pos (list x y width height))
          (create-mixer-strips-gui 1 #t pos))))

  (define (toggle mixer-strips)
    ;;(c-display "         About to toggle" (mixer-strips :gui) ". is fullscreen?" (<gui> :is-full-screen (mixer-strips :gui)))
    (<gui> :set-full-screen (mixer-strips :gui) (not (<gui> :is-full-screen (mixer-strips :gui)))))
  
  (let loop ((mixer-strips *mixer-strips-objects*))
    (let ((first-mixer-strips (and (not (null? mixer-strips))
                                   (car mixer-strips))))
      (cond ((and (null? mixer-strips)
                  (not (null? *mixer-strips-objects*)))
             (toggle (car *mixer-strips-objects*)))
            ((null? mixer-strips)
             #f)
            ((<gui> :mouse-points-mainly-at (first-mixer-strips :gui))
             ;;(toggle-by-recreating first-mixer-strips) ;; This one is just as fast the 'toggle' function, plus that gui is closed immediately, so it's actually better.
             (toggle first-mixer-strips)
             )
            (else
             (loop (cdr mixer-strips)))))))



;; main
(when (not *is-initializing*)
  (let ((start (time)))
    (set! *mixer-strips-objects* '())
    (create-mixer-strips-gui 2)
    (c-display "   Time used to open mixer:" (- (time) start))))





#!
(remake-mixer-strips)

(get-instrument-from-name "Sample Player 1")
(get-buses-connecting-from-instrument (get-instrument-from-name "Sample Player 1") #t)


(get-all-audio-instruments)
(define widget (<gui> :widget 200 200))
(<gui> :show widget)
!#

;; Option to have two rows
;; Options to turn on/off instruments
;;option to select +35 or +6


;; Background color should probably be a mix between system background color and instrument color.
;; Color type should probably be int64_t, not string. Don't see any advantages using string.


#||
(let ()
  (define start (time))

  (define mixer-strip-instrument-ids (get-mixer-strip-instrument-ids))
  (define instruments (car mixer-strip-instrument-ids))
  (define all-buses (cadr mixer-strip-instrument-ids))

  (define num-strips (+ (length instruments)
                        (length all-buses)))

  (c-display "time" (- (time) start))
  (c-display "instruments" instruments)
  (c-display "all-buses" all-buses)
  )


||#

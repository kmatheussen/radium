(provide 'mixer-strips.scm)

;; TOPIC: Should the pan slider work for the last instrument in the path?
;; In case, we need to copy pan values when deleting / removing plulgins.
;;
;; When adding a new mixer strip, we could slice the various mixer strips windows.




(define (get-fontheight)
  (+ 4 (<gui> :get-system-fontheight)))

(define-constant *text-color* "#cccccc")
(define-constant *arrow-text* "↳")
(define-constant *arrow-text2* "→")

(define *current-mixer-strip-is-wide* #f)


(define (db-to-slider db)
  (if (<= db *min-db*)
      0
      (let ((scaled (scale db *min-db* *max-mixer-db* 0 1)))
        (* scaled scaled)))) ;; Seems to work okay

(define (slider-to-db slider)
  (define scaled (sqrt slider)) ;; Seems to work okay
  (scale scaled 0 1 *min-db* *max-mixer-db*))

(define (db-to-text db add-dB-string)
  (if  (<= db *min-db*)
       "~inf"
       (let* ((val1 (one-decimal-string db))
              (val (if (string=? val1 "-0.0") "0.0" val1)))
         (if add-dB-string
             (<-> val "dB")
             val))))


;; callback is not called after an instrument (any instrument) has been deleted.
(define (add-safe-callback gui add-func callback)
  (define deletion-generation (<ra> :get-audio-instrument-deletion-generation))
  (add-func gui
            (lambda args
              (if (= deletion-generation (<ra> :get-audio-instrument-deletion-generation))
                  (apply callback args)
                  #t))))
           
(define (add-safe-paint-callback gui callback)
  (add-safe-callback gui ra:gui_add-paint-callback callback))

(define (add-safe-resize-callback gui callback)
  (add-safe-callback gui ra:gui_add-resize-callback callback))

(define (add-safe-mouse-callback gui callback)
  (add-safe-callback gui ra:gui_add-mouse-callback callback))

(define (add-safe-double-click-callback gui callback)
  (add-safe-callback gui ra:gui_add-double-click-callback callback))


(define (get-global-mixer-strips-popup-entries instrument-id strips-config)
  (list (list "Hide mixer strip" :enabled (and instrument-id strips-config)
              (lambda ()
                (set! (strips-config :is-enabled instrument-id) #f)))
        (list "Configure mixer strips on/off" :enabled strips-config
              (lambda ()
                (strips-config :show-config-gui)))
        "----------"
        (if strips-config
            (list "Set number of rows"
                  (lambda ()
                    (define num-rows-now (strips-config :num-rows))
                    (popup-menu (map (lambda (num-rows)
                                       (list (number->string num-rows)
                                             :enabled (not (= num-rows num-rows-now))
                                             (lambda ()
                                               (set! (strips-config :num-rows) num-rows))))
                                     (map 1+ (iota 6))))))
            '())
        "----------"
        "Help" (lambda ()
                 (<ra> :show-mixer-help-window))
        "Set current instrument" (lambda ()
                                   (popup-menu (map (lambda (instrument-id)
                                                      (list (<ra> :get-instrument-name instrument-id)
                                                            (lambda ()
                                                              (<ra> :set-current-instrument instrument-id #f)
                                                              )))
                                                    (sort-instruments-by-mixer-position-and-connections 
                                                     (get-all-audio-instruments)))))))



(define (create-custom-checkbox instrument-id strips-config paint-func value-changed is-selected min-height)
  (define checkbox (<gui> :widget))
  (<gui> :set-min-height checkbox min-height)

  (add-safe-mouse-callback checkbox (lambda (button state x y)
                                        ;;(c-display "state" state)
                                        (if (and (= button *right-button*)
                                                 (= state *is-pressing*))
                                            (if (<ra> :shift-pressed)
                                                (<ra> :delete-instrument instrument-id)
                                                (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config))))
                                        (when (and (= button *left-button*)
                                                   (= state *is-pressing*))
                                          (set! is-selected (not is-selected))
                                          (value-changed is-selected)
                                          (<gui> :update checkbox))
                                        #t))
  
  (add-safe-paint-callback checkbox
                           (lambda (width height)
                             (paint-func checkbox is-selected width height)))

  (list (lambda (new-value)
          (set! is-selected new-value)
          (<gui> :update checkbox))
        checkbox))


(define (add-gui-effect-monitor gui instrument-id effect-name callback)
  (let ((effect-monitor (<ra> :add-effect-monitor effect-name instrument-id callback)))
    (<gui> :add-deleted-callback gui
           (lambda (radium-runs-custom-exec)
             (<ra> :remove-effect-monitor effect-monitor))))) ;; This function should be safe to call also when 'radium-runs-custom-exec' is true.


(define (get-mixer-strip-name instrument-id strips-config)
  (let ((name (<ra> :get-instrument-name instrument-id)))
    (if (or (not strips-config)
            (strips-config :is-unique instrument-id))
        name
        (<-> "[" name "]"))))


;; STRIPS-CONFIG
;;;;;;;;;;;;;;;;;

                    
;        (list instruments
;              all-buses))))


(define (create-is-enabled-gui strips-config)

  (define vertical-layout (<gui> :vertical-layout))

  ;;(define instruments (strips-config :instruments))
  ;;(define all-buses (strips-config :buses))
  
  (define row-separator-width 10)
  (define strip-separator-width 0)

  (<gui> :set-layout-spacing vertical-layout row-separator-width 0 0 0 0)

  (define num-instruments-per-row (1+ (floor (sqrt (<ra> :get-num-audio-instruments)))))
  
  (define row-num -1)
  (define column-num 0)
  (define horizontal-row-layout #f)
  (define horizontal-layout #f)

  (define (add-new-horizontal-layout! inc-row-num)
    (when inc-row-num
      (inc! row-num 1)
      (define layout (<gui> :horizontal-layout))
      ;;(<gui> :add layout (<gui> :text (<-> "Row " (1+ row-num) ":")) 0) ;; Just create confution.
      (set! horizontal-row-layout (<gui> :vertical-layout))
      (<gui> :add layout horizontal-row-layout 1)
      (<gui> :add vertical-layout layout))

    (set! horizontal-layout (<gui> :horizontal-layout))
    (<gui> :set-layout-spacing horizontal-layout strip-separator-width 0 0 0 0)

    (<gui> :add horizontal-row-layout horizontal-layout)
    (set! column-num 0))
  
  (add-new-horizontal-layout! #t)

  (define (add-guis instruments)
    (for-each (lambda (instrument-id)
                (define is-initing #t) ;; this is stupid. Should fix immediately after 5.0.
                ;;(c-display "row-num for" (<ra> :get-instrument-name instrument-id) ": " (strips-config :row-num instrument-id))
                (define name (get-mixer-strip-name instrument-id strips-config))
                (define button (<gui> :checkbox name
                                      (strips-config :is-enabled instrument-id)
                                      #t
                                      (lambda (is-on)
                                        (if (not is-initing)
                                            (set! (strips-config :is-enabled instrument-id) is-on)))))
                (define background-color (<gui> :get-background-color vertical-layout))
                (define instrument-color (<ra> :get-instrument-color instrument-id))
                (define unselected-color (<gui> :mix-colors instrument-color background-color 0.1))
                (add-safe-paint-callback button
                                         (lambda (width height)
                                           (define is-enabled (strips-config :is-enabled instrument-id))
                                           (define x1 0)
                                           (define y1 0)
                                           (define x2 width)
                                           (define y2 height)
                                           
                                           (if (not is-enabled)
                                               (begin
                                                 (<gui> :filled-box
                                                        button
                                                        background-color
                                                        0 0 width height)
                                                 (let ((border 5))
                                                   (set! x1 border) ;;(floor (/ width border)))
                                                   (set! y1 border) ;;(floor (/ height border)))
                                                   (set! x2 (max 1 (- width border))) ;;(floor (- width (/ width border))))
                                                   (set! y2 (max 1 (- height border))))));;(floor (- height (/ height border)))))))
                                           
                                           (<gui> :filled-box
                                                  button
                                                  (if is-enabled
                                                      instrument-color
                                                      unselected-color)
                                                  x1 y1 x2 y2)

                                           (<gui> :draw-text
                                                  button
                                                  (if is-enabled
                                                      *text-color*
                                                      "gray")
                                                  name
                                                  (+ x1 3) (+ y1 2) (- x2 3) (- y2 2)
                                                  )

                                           (<gui> :draw-box button "#202020" 0 0 width height 1.0 2 2)))

                (set! is-initing #f)
                (<gui> :set-size-policy button #t #t)
                (inc! column-num 1)
                (if (= column-num num-instruments-per-row)
                    (add-new-horizontal-layout! #f))

                (<gui> :add horizontal-layout button))

              instruments))

  (define cat-instruments (<new> :get-cat-instruments))
  
  (add-guis (sort-instruments-by-mixer-position-and-connections (cat-instruments :instrument-instruments)))

  ;;(c-display "bef:" (map ra:get-instrument-name (cat-instruments :instrument-instruments)))
  ;;(c-display "aft:" (map ra:get-instrument-name (sort-instruments-by-mixer-position-and-connections (cat-instruments :instrument-instruments))))

  (let ((text (<gui> :text *arrow-text2*)))
    (define w1 (<gui> :widget))
    (define w2 (<gui> :widget))
    (define layout (<gui> :horizontal-layout w1 text w2))
    (set-fixed-width layout (* 2 (<gui> :width text)))
    (<gui> :set-background-color layout (<gui> :mix-colors (<gui> :get-background-color layout) "black" 0.5))
    (<gui> :add horizontal-layout layout))

  (add-guis (sort-instruments-by-mixer-position-and-connections (cat-instruments :bus-instruments)))

  ;;(<gui> :show vertical-layout)
  ;;
  vertical-layout
  )


#!!
(let ((strips-config (create-strips-config -2)))
  (strips-config :scan-instruments!)
  (create-is-enabled-gui strips-config))

(map (lambda (key . rest)
       (c-display "key" key ", rest" rest)) 
     (hash-table->alist (hash-table* :a 1 :b 2)))

(let ((h (make-hash-table)))
  (set! (h :a) 5)
  (set! (h :b) 2)
  (c-display "alist" (hash-table->alist h))
  (map (lambda (key)
         (display "key:")(display key)(newline))
       h))

       (hash-table->alist h)))

(c-display "hepp" (hash-table->alist (hash-table '(a . 5) '(b . 2))))
(c-display "hepp" (hash-table->alist (hash-table* 'a 5 'b 2)))

!!#
  
(define (create-strips-config initial-instruments num-rows remake parentgui)
  (define-struct conf
    :instrument-id
    :is-bus
    :row-num
    :is-enabled
    :is-unique)

  (define first-time #t)
  (define confs (make-hash-table 100 =))

  (define (scan-instruments!)

    (define cat-instruments (<new> :get-cat-instruments))

    (for-each (lambda (id)
                 (let* ((is-instrument (cat-instruments :is-instrument? id))
                        (is-bus (cat-instruments :is-bus? id))
                        (is-strip (or is-instrument is-bus))
                        (has-conf (confs id))
                        (is-enabled (get-bool (cond (has-conf
                                                     (confs id :is-enabled))
                                                    (initial-instruments
                                                     (and first-time
                                                          (memv id initial-instruments)))
                                                    (else
                                                     is-strip))))
                        (is-unique (or (cat-instruments :has-no-inputs-or-outputs? id)
                                       is-strip)))
                   (set! (confs id) (make-conf :instrument-id id
                                               :is-bus is-bus
                                               :row-num 0
                                               :is-enabled is-enabled
                                               :is-unique is-unique))))
              (get-all-audio-instruments))

    (if first-time
        (set! first-time #f))
    )
  

  (define (reset!)
    (set! first-time #t)
    (set! confs (make-hash-table 100 =))
    (set! initial-instruments #f)
    (scan-instruments!))
  
  (define is-enabled-content (<gui> :widget))

  (define config-gui (<gui> :vertical-layout))
  (<gui> :set-parent config-gui -2)

  (<gui> :add config-gui is-enabled-content 3) ;; stretch 3
  (<gui> :add config-gui
         (let ((horiz (<gui> :horizontal-layout)))
           ;;(<gui> :add-layout-space horiz 0 0 #t #f)
           (define close-button (<gui> :button "Close" (lambda ()
                                                         (<gui> :close config-gui))))
           (<gui> :set-size-policy close-button #t #t)
           (<gui> :add horiz close-button)
           horiz)
         1) ;; stretch 1


  (<gui> :add-close-callback config-gui
         (lambda (radium-runs-custom-exec)
           (if (<gui> :is-open parentgui)
               (begin
                 (try-finally :try (lambda ()
                                     (<gui> :hide config-gui)))
                 #f)
               #t)))

  
  (<gui> :add-deleted-callback parentgui
         (lambda (runs-custom-exec)
           (<gui> :close config-gui)))

  (define (recreate-config-gui-content)
    (define old-content is-enabled-content)
    (set! is-enabled-content (create-is-enabled-gui this))
    (<gui> :replace config-gui old-content is-enabled-content)
    (<gui> :close old-content))

  (define has-shown #f)
  
  (define (show-config-gui)
    ;;(<gui> :show config-gui))
    (recreate-config-gui-content)
    (<gui> :show config-gui)
    (when (not has-shown)
      (<gui> :move-to-centre-of config-gui parentgui)
      (<gui> :set-size config-gui
             (* 2 (<gui> :width config-gui))
             (* 3 (<gui> :height config-gui)))
      (set! has-shown #t)))


  (define (set-conf-var! instrument-id keyword new-value)
    (set! (confs instrument-id)
          (let ((conf (confs instrument-id)))
            (<copy-conf> conf keyword new-value))))

  (define this
    (dilambda (lambda (keyword . rest)
                ;;(c-display "THIS called. keyword:" keyword)
                (case keyword
                  ((:row-num) (confs (car rest) :row-num))
                  ((:is-enabled) (confs (car rest) :is-enabled))
                  ((:is-unique) (confs (car rest) :is-unique))
                  ((:scan-instruments!) (scan-instruments!))
                  ((:reset!) (begin
                               (reset!)
                               (remake :non-are-valid)))
                  ((:show-config-gui) (show-config-gui))
                  ((:recreate-config-gui-content) (recreate-config-gui-content))
                  ((:num-rows) num-rows)
                  ((:get) (hash-table* :num-rows num-rows
                                       :instrument-settings (map (lambda (entry)
                                                                   (define instrument-id (car entry))
                                                                   (define conf (cdr entry))
                                                                   (hash-table* :instrument-id instrument-id
                                                                                :is-enabled (conf :is-enabled)))
                                                                 confs)))
                  ((:set!) (let ((settings (car rest)))
                             (reset!)
                             (set! num-rows (settings :num-rows))
                             (for-each (lambda (conf)
                                         (set-conf-var! (conf :instrument-id) :is-enabled (conf :is-enabled)))
                                       (to-list (settings :instrument-settings)))
                             (remake :non-are-valid)))
                  (else
                   (error (<-> "Unknown keyword1 " keyword)))))
              (lambda (keyword first-arg . rest-args)
                (if (null? rest-args)
                    (case keyword
                      ((:num-rows) (begin
                                     (set! num-rows first-arg)
                                     (remake :all-are-valid)))
                      (else
                       (error (<-> "Unknown keyword3 " keyword))))                      
                    (let ((instrument-id first-arg)
                          (new-value (car rest-args)))
                      (case keyword
                        ((:row-num) (set-conf-var! instrument-id :row-num new-value))
                        ((:is-enabled) (begin
                                         (set-conf-var! instrument-id :is-enabled new-value)
                                         ;;(c-display "..........calling remake from is-enabled")
                                         (remake :all-are-valid)
                                         ;;(recreate-config-gui-content) ;; <- Not necessary since remake (called above) calls that function.
                                         ))
                        (else
                         (error (<-> "Unknown keyword2 " keyword)))))))))

  this)


;; GUIS
;;;;;;;;

#||
(define (create-mixer-strip-name-line instrument-id strips-config height)
  (define name (<gui> :line (<ra> :get-instrument-name instrument-id) (lambda (edited)
                                                                        (if (<ra> :instrument-is-open instrument-id)
                                                                            (<ra> :set-instrument-name edited instrument-id)))))
  (<gui> :set-background-color name (<ra> :get-instrument-color instrument-id))

  (add-safe-mouse-callback name (lambda (button state x y)
                                    (if (and (= button *right-button*)
                                             (= state *is-pressing*))
                                        (begin
                                          (if (<ra> :shift-pressed)
                                              (<ra> :delete-instrument instrument-id)
                                              (create-default-mixer-path-popup instrument-id strips-config name))
                                          #t)
                                        #f)))

  (set-fixed-height name height)

  name)
||#

(define (create-mixer-strip-name instrument-id strips-config is-minimized is-current-mixer-strip)
  (define name (get-mixer-strip-name instrument-id strips-config))
  (define color (<ra> :get-instrument-color instrument-id))

  (define label (<gui> :widget))
  
  (if is-minimized
      (<gui> :set-min-height label (* 2 (<gui> :get-system-fontheight))))
  
  (add-safe-paint-callback label
         (lambda (width height)
           (<gui> :filled-box label color 0 0 width height)
           (if is-minimized
               (<gui> :draw-vertical-text label *text-color* name 2 7 (+ width 0) height #t #f #t)
               (<gui> :draw-text label *text-color* name 5 0 width height #f #f #f))
           (<gui> :draw-box label "#202020" 0 0 width height 1.0 2 2)))
  
  (add-safe-mouse-callback label (lambda (button state x y)
                                     (if (= state *is-pressing*)
                                         (if (= button *right-button*)
                                             (if (<ra> :shift-pressed)
                                                 (<ra> :delete-instrument instrument-id)
                                                 (create-default-mixer-path-popup instrument-id strips-config label))
                                             (when (= button *left-button*)
                                               (cond (is-current-mixer-strip
                                                      (set! *current-mixer-strip-is-wide* is-minimized)
                                                      (remake-mixer-strips instrument-id))
                                                     ((= instrument-id (<ra> :get-current-instrument))
                                                      (<ra> :set-wide-instrument-strip instrument-id is-minimized)
                                                      (remake-mixer-strips instrument-id))
                                                     (else
                                                      (<ra> :set-current-instrument instrument-id))))))
                                     #t))

  label)

#!!
(define (trysomething)
  (define id (last (get-all-audio-instruments)))
  (<ra> :set-wide-instrument-strip id (not (<ra> :has-wide-instrument-strip id)))
  (remake-mixer-strips))
!!#

(define (request-send-instrument instrument-id callback)
  (define is-bus-descendant (<ra> :instrument-is-bus-descendant instrument-id))
  (define buses (get-buses))
  (define (create-entry-text instrument-id)
    (<-> *arrow-text* " " (<ra> :get-instrument-name instrument-id)))

  (define (apply-changes changes)
    (<ra> :undo-mixer-connections)
    (<ra> :change-audio-connections changes))
  
  (define args
    (run-instrument-data-memoized
     (lambda()
       (list
     
        ;; buses
        (map (lambda (bus-effect-name bus-onoff-effect-name bus-id)
               (list (create-entry-text bus-id)
                     :enabled (and (not is-bus-descendant)
                                   (< (<ra> :get-instrument-effect instrument-id bus-onoff-effect-name) 0.5))
                     (lambda ()
                       (callback (lambda (gain changes)
                                   (undo-block (lambda ()
                                                 ;;(c-display "GAIN: " gain ", db: " *min-db* (<ra> :gain-to-db gain) *max-db*)
                                                 (<ra> :undo-instrument-effect instrument-id bus-onoff-effect-name)
                                                 (if gain
                                                     (<ra> :undo-instrument-effect instrument-id bus-effect-name))
                                                 (<ra> :set-instrument-effect instrument-id bus-onoff-effect-name 1.0)
                                                 (if gain
                                                     (<ra> :set-instrument-effect instrument-id bus-effect-name (scale (<ra> :gain-to-db gain)
                                                                                                                       *min-db* *max-db*
                                                                                                                       0 1)))
                                                 (apply-changes changes))))))))

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
                       (callback (lambda (gain changes)
                                   (push-audio-connection-change! changes (list :type "connect"
                                                                                :source instrument-id
                                                                                :target send-id
                                                                                :gain gain))
                                   (apply-changes changes))))))
             (sort-instruments-by-mixer-position-and-connections
              (keep (lambda (id)
                      (not (member id buses)))
                    (begin
                      (define ret (get-all-instruments-that-we-can-send-to instrument-id))
                      ret))))))))
  (apply popup-menu args))


(define (show-mixer-path-popup first-instrument-id
                               parent-instrument-id
                               instrument-id
                               strips-config
                               
                               is-send?
                               is-sink?
                               upper-half?

                               delete-func
                               replace-func
                               reset-func

                               parentgui
                               )

  (set! parentgui (<gui> :get-parent-window parentgui))

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
                      (define (finished new-instrument)
                        ;;(c-display "             first: " (<ra> :get-instrument-name first-instrument-id) ", new:" (and new-instrument (<ra> :get-instrument-name new-instrument)))
                        (<ra> :set-current-instrument first-instrument-id))
                      (cond (is-send?
                             (insert-new-instrument-between parent-instrument-id
                                                            (get-instruments-connecting-from-instrument parent-instrument-id)
                                                            #t
                                                            parentgui
                                                            finished))
                            (upper-half?
                             ;; before
                             (insert-new-instrument-between parent-instrument-id
                                                            instrument-id
                                                            #t
                                                            parentgui
                                                            finished))
                            (else
                             ;; after
                             (insert-new-instrument-between instrument-id
                                                            (get-instruments-connecting-from-instrument instrument-id)
                                                            #t
                                                            parentgui
                                                            finished)))
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
                                                   (create-send-func 0 '())
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
                (list "Set as current instrument"
                      :enabled (not (= (<ra> :get-current-instrument)
                                       instrument-id))
                      (lambda ()
                        (<ra> :set-current-instrument instrument-id #f)))
                "Rename" (lambda ()
                           (define old-name (<ra> :get-instrument-name instrument-id))
                           (define new-name (<ra> :request-string "New name:" #t old-name))
                           (c-display "NEWNAME" (<-> "-" new-name "-"))
                           (if (and (not (string=? new-name ""))
                                    (not (string=? new-name old-name)))
                               (<ra> :set-instrument-name new-name instrument-id)))
                "Show Info" (lambda ()
                              (<ra> :show-instrument-info instrument-id parentgui))
                "Configure color" (lambda ()
                                    (show-instrument-color-dialog parentgui instrument-id))
                (list "Show GUI"
                      :enabled (<ra> :has-native-instrument-gui instrument-id)
                      (lambda ()
                        (<ra> :show-instrument-gui instrument-id #f)))
                "----------"
                (list "Wide mode"
                      :check (<ra> :has-wide-instrument-strip parent-instrument-id)
                      (lambda (enabled)
                        (<ra> :set-wide-instrument-strip parent-instrument-id enabled)
                        (remake-mixer-strips parent-instrument-id)))
                ;;(remake-mixer-strips parent-instrument-id)))
                (get-global-mixer-strips-popup-entries first-instrument-id strips-config)
                ))

(define (create-default-mixer-path-popup instrument-id strips-config gui)
  (define is-permanent? (<ra> :instrument-is-permanent instrument-id))

  (define (delete)
    (<ra> :delete-instrument instrument-id))
  (define (replace)
    (async-replace-instrument instrument-id "" (make-instrument-conf :must-have-inputs #f :must-have-outputs #f :parentgui gui)))
  
  (define reset #f)

  (show-mixer-path-popup instrument-id
                         instrument-id
                         instrument-id
                         strips-config
                         #f
                         (= 0 (<ra> :get-num-output-channels instrument-id))
                         #f
                         (if is-permanent? #f delete)
                         (if is-permanent? #f replace)
                         reset
                         gui))
  
(define (strip-slider first-instrument-id
                      parent-instrument-id
                      instrument-id
                      strips-config
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

    ;;(if (= (<ra> :get-current-instrument) instrument-id)
    ;;    (<gui> :filled-box widget "#aa111144" 1 1 (1- width) (1- height) 5 5))

    (if (= (<ra> :get-current-instrument) instrument-id)
        (let* ((w 1.2)
               (w*2 (* w 3)))
          (<gui> :draw-box widget *current-mixer-strip-border-color* w w (- width 2) (- height 2) w*2 5 5)) ;; "#aa111144"
        (<gui> :draw-box widget "gray"      0 0 width height 0.8 5 5))
  
    (define text (<-> instrument-name ": " (get-value-text value)))
    (when is-changing-value
      (<ra> :set-statusbar-text text)
      (<gui> :tool-tip text))
    (<gui> :draw-text widget *text-color* text
           4 0 (- width 4) height))

  (set! widget (<gui> :horizontal-slider "" 0 (get-scaled-value) 1.0
                      (lambda (val)
                        ;;(<ra> :set-instrument-effect instrument-id effect-name val)
                        (when widget
                          (set-value val)
                          (<gui> :update widget)
                          ;;(<ra> :set-current-instrument first-instrument-id)
                          ))))

  (<gui> :set-min-height widget (get-fontheight))

  (add-safe-paint-callback widget paintit)

  (add-safe-mouse-callback widget (lambda (button state x y)
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
                                                                 strips-config
                                                                 is-send?
                                                                 is-sink?
                                                                 (< y (/ (<gui> :height widget) 2))
                                                                 delete-func
                                                                 replace-func
                                                                 reset-func
                                                                 widget)))
                                    #f))
  
  (add-safe-double-click-callback widget (lambda (button x y)
                                           (when (= button *left-button*)
                                             ;;(c-display " Double clicking" button)
                                             (<ra> :cancel-last-undo) ;; Undo the added undo made at th mouse callback above.
                                             (<ra> :show-instrument-gui instrument-id (<ra> :show-instrument-widget-when-double-clicking-sound-object))
                                             )))

  ;;(paintit (<gui> :width widget)
  ;;         (<gui> :height widget))

  (<gui> :set-size-policy widget #t #t)

  widget)



(define (create-mixer-strip-plugin gui first-instrument-id parent-instrument-id instrument-id strips-config)
  (define (get-drywet)
    (<ra> :get-instrument-effect instrument-id "System Dry/Wet"))
  
  (define (delete-instrument)

    (c-display "HIDING" slider)
    (<gui> :hide slider)

    (<ra> :schedule 10 ;; Wait for slider to be removed, and (for some reason) it also prevents some flickering. Looks much better if the slider disappears immediately.
          (lambda ()
            
            (define child-ids (get-instruments-connecting-from-instrument instrument-id))
            (define child-gains (map (lambda (to)
                                       (<ra> :get-audio-connection-gain instrument-id to))
                                     child-ids))
            (undo-block
             (lambda ()
               (define changes '())
               
               ;; Disconnect parent -> me
               (push-audio-connection-change! changes (list :type "disconnect"
                                                            :source parent-instrument-id
                                                            :target instrument-id))
               
               (for-each (lambda (child-id child-gain)
                           ;; Disconnect me -> child
                           (push-audio-connection-change! changes (list :type "disconnect"
                                                                        :source instrument-id
                                                                        :target child-id))
                           ;; Connect parent -> child
                           (push-audio-connection-change! changes (list :type "connect"
                                                                        :source parent-instrument-id
                                                                        :target child-id
                                                                        :gain child-gain)))
                         child-ids
                         child-gains)
               (<ra> :undo-mixer-connections)
               (<ra> :change-audio-connections changes) ;; Apply all changes simultaneously
               (<ra> :delete-instrument instrument-id)
               ;;(remake-mixer-strips) ;; (makes very little difference in snappiness, and it also causes mixer strips to be remade twice)
               ))
            
            #f)))

  (define (das-replace-instrument)
    (async-replace-instrument instrument-id "" (make-instrument-conf :must-have-inputs #t :must-have-outputs #t :parentgui gui)))

  (define (reset)
    (<ra> :set-instrument-effect instrument-id "System Dry/Wet" 1))

  (define doit #t)
  (define slider (strip-slider first-instrument-id
                               parent-instrument-id
                               instrument-id
                               strips-config
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



;; A sink plugin. For instance "System Out".
(define (create-mixer-strip-sink-plugin gui first-instrument-id parent-instrument-id instrument-id strips-config)

  (define horiz (get-mixer-strip-send-horiz gui))
  
  (define (make-undo)
    (<ra> :undo-instrument-effect instrument-id "System In"))
    
  (define (delete)
    (<gui> :hide horiz)
    (<ra> :undo-mixer-connections)
    (<ra> :delete-audio-connection parent-instrument-id instrument-id)
    #f)
  
  (define (replace)
    ;;(c-display "   name: " (<ra> :get-instrument-name parent-instrument-id))
    ;;(for-each (lambda (id)
    ;;            (c-display "      " (<ra> :get-instrument-name id)))
    ;;          (get-all-instruments-that-we-can-send-to parent-instrument-id))
    (request-send-instrument parent-instrument-id
                             (lambda (create-send-func)
                               (undo-block
                                (lambda ()
                                  (define db (get-db-value))
                                  (define gain (<ra> :db-to-gain db))
                                  ;;(delete)
                                  (define changes '())
                                  (push-audio-connection-change! changes (list :type "disconnect"
                                                                               :source parent-instrument-id
                                                                               :target instrument-id))
                                  (create-send-func gain changes))))))

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
                               strips-config
                               #t #t

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

  (<gui> :add horiz slider)
  horiz)



(define (create-mixer-strip-send gui
                                 first-instrument-id
                                 parent-instrument-id
                                 target-instrument-id
                                 strips-config
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
                               strips-config
                               #t #f

                               make-undo

                               ;; get-scaled-value
                               (lambda ()
                                 (db-to-slider (if add-monitor ;; minor optimization.
                                                   (or (get-db-value)
                                                       last-value)
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
  
  (<gui> :add horiz slider)
  horiz)


(define (create-mixer-strip-bus-send gui first-instrument-id instrument-id strips-config bus-num)
  (define effect-name (list-ref *bus-effect-names* bus-num))
  (define on-off-effect-name (list-ref *bus-effect-onoff-names* bus-num))

  (define bus-id (<ra> :get-audio-bus-id bus-num))

  (define (make-undo)
    (<ra> :undo-instrument-effect instrument-id effect-name))

  (define send-gui #f)
  
  (define (delete)
    (<gui> :hide send-gui)
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
                                  (create-send-func gain '()))))))
  (define (get-db-value)
    (let ((db (<ra> :get-instrument-effect instrument-id effect-name)))
      (scale db
             0 1
             *min-db* *max-db*)))

  (define (set-db-value db)
    (<ra> :set-instrument-effect instrument-id effect-name (scale db *min-db* *max-db* 0 1)))
  
  (define (add-monitor slider callback)
    (add-gui-effect-monitor slider instrument-id effect-name callback))
  
  (set! send-gui (create-mixer-strip-send gui
                                          first-instrument-id
                                          instrument-id
                                          bus-id
                                          strips-config
                                          make-undo
                                          get-db-value
                                          set-db-value
                                          add-monitor
                                          delete
                                          replace))
  send-gui)


(define *send-callbacks* '())

(define (create-mixer-strip-audio-connection-send gui first-instrument-id source-id target-id strips-config)
  (define (make-undo)
    (<ra> :undo-audio-connection-gain source-id target-id))

  (define send-gui #f)

  (define (delete)
    (<gui> :hide send-gui)
    (<ra> :undo-mixer-connections)
    (<ra> :delete-audio-connection source-id target-id))

  (define (replace)
    (request-send-instrument source-id
                             (lambda (create-send-func)
                               (define gain (<ra> :get-audio-connection-gain source-id target-id))
                               (undo-block
                                (lambda ()
                                  (define changes '())
                                  (push-audio-connection-change! changes (list :type "disconnect"
                                                                               :source source-id
                                                                               :target target-id))
                                  (create-send-func gain changes))))))
  
  (define (get-db-value)
    (and (<ra> :has-audio-connection source-id target-id)
         (<ra> :gain-to-db (<ra> :get-audio-connection-gain source-id target-id))))

  (define (set-db-value db)
    ;;(c-display "setting db to" db)
    (<ra> :set-audio-connection-gain source-id target-id (<ra> :db-to-gain db) #f)
    (for-each (lambda (send-callback)
                (send-callback gui source-id target-id))
              *send-callbacks*))
  
  (define (add-monitor slider callback)
    (define send-callback
      (lambda (maybe-gui maybe-source-id maybe-target-id)
        (if (and (not (= gui maybe-gui))
                 (= maybe-source-id source-id)
                 (= maybe-target-id target-id)
                 (<ra> :instrument-is-open source-id)
                 (<ra> :instrument-is-open target-id)
                 (<ra> :has-audio-connection source-id target-id))
            (callback))))
  
    (push-back! *send-callbacks* send-callback)
    
    (<gui> :add-deleted-callback gui
           (lambda (radium-runs-custom-exec)
             (set! *send-callbacks*
                   (remove (lambda (callback)
                             (equal? callback send-callback))
                           *send-callbacks*)))))
    
  ;; Also works fine, but is less efficient. (cleaner code though)
  ;(define (add-monitor slider callback)
  ;  (<ra> :schedule (random 1000) (lambda ()
  ;                                  (if (and (<gui> :is-open gui)
  ;                                           (<ra> :instrument-is-open source-id)
  ;                                           (<ra> :instrument-is-open target-id))                                             
  ;                                      (begin
  ;                                        (callback)
  ;                                        100)
  ;                                      #f))))

  ;;(set! add-monitor #f)

  (set! send-gui (create-mixer-strip-send gui
                                          first-instrument-id
                                          source-id
                                          target-id
                                          strips-config
                                          make-undo 
                                          get-db-value
                                          set-db-value
                                          add-monitor
                                          delete
                                          replace))
  send-gui)


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
(define (create-mixer-strip-path gui first-instrument-id instrument-id strips-config)
  (get-mixer-strip-path-instruments instrument-id
                                    (lambda (bus-sends instrument-sends next-plugin-instrument)
                                      (for-each (lambda (bus-num)
                                                  (create-mixer-strip-bus-send gui
                                                                               instrument-id
                                                                               instrument-id
                                                                               strips-config
                                                                               bus-num))
                                                bus-sends)
                                      
                                      (for-each (lambda (out-instrument)
                                                  (create-mixer-strip-audio-connection-send gui
                                                                                            first-instrument-id
                                                                                            instrument-id
                                                                                            out-instrument
                                                                                            strips-config))
                                                instrument-sends)

                                      (if next-plugin-instrument
                                          (begin
                                            (if (= 0 (<ra> :get-num-output-channels next-plugin-instrument))
                                                (create-mixer-strip-sink-plugin gui instrument-id instrument-id next-plugin-instrument strips-config)
                                                (create-mixer-strip-plugin gui first-instrument-id instrument-id next-plugin-instrument strips-config))
                                            (create-mixer-strip-path gui first-instrument-id next-plugin-instrument strips-config))
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
                                                
  
(define (create-mixer-strip-pan instrument-id strips-config system-background-color background-color height)
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

  (add-safe-paint-callback slider (lambda x (paint)))

  ;;(paint)

  (add-gui-effect-monitor slider instrument-id "System Pan"
                          (lambda ()
                            (set! doit #f)
                            (<gui> :set-value slider (get-pan))
                            (<gui> :update slider)
                            (set! doit #t)))
  
  (add-gui-effect-monitor slider instrument-id "System Pan On/Off" (lambda ()
                                                                     (<gui> :update slider)))

  (add-safe-mouse-callback slider
         (lambda (button state x y)
           (cond ((and (= button *left-button*)
                       (= state *is-pressing*))
                  (undo-block
                   (lambda ()
                     (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
                     (<ra> :undo-instrument-effect instrument-id "System Pan")))
                  #f)

                 ((and (= button *right-button*)
                       (= state *is-releasing*))
                  
                  (popup-menu "Reset" (lambda ()
                                        (<ra> :undo-instrument-effect instrument-id "System Pan")
                                        (<ra> :set-instrument-effect instrument-id "System Pan" 0.5))
                              (list "Enabled"
                                    :check (pan-enabled?)
                                    (lambda (onoff)
                                      (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
                                      (<ra> :set-instrument-effect instrument-id "System Pan On/Off" (if onoff 1.0 0.0))))
                              "------------"
                              (get-global-mixer-strips-popup-entries instrument-id strips-config))
                  #t)
                 (else
                  #f))))
  
  slider)

;;(define (create-mixer-strip-checkbox text sel-color unsel-color width height callback)
;;  (define button (<gui> :widget width height))

(define (create-mixer-strip-mutesolo instrument-id strips-config background-color height is-minimized)
  
  (define (draw-mutesolo checkbox is-selected is-implicitly-selected text color width height)
    (<gui> :filled-box
           checkbox
           background-color
           0 0 width height)
    (define b (if is-minimized 2 5))
    (<gui> :filled-box
           checkbox
           (if is-selected
               color
               background-color)
           2 2 (- width 2) (- height 2)
           b b)
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
           b b)
    )

  (define volume_on_off_name (get-instrument-volume-on/off-effect-name instrument-id))

  (define (get-muted)
    (< (<ra> :get-instrument-effect instrument-id volume_on_off_name) 0.5))
  (define (get-soloed)
    (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
           
  (define (turn-off-all-mute except)
    (for-each (lambda (instrument-id)
                (when (and (not (= instrument-id except))
                           (< (<ra> :get-instrument-effect instrument-id (get-volume-on-off-name instrument-id)) 0.5))
                  (<ra> :undo-instrument-effect instrument-id (get-volume-on-off-name instrument-id))
                  (<ra> :set-instrument-effect instrument-id (get-volume-on-off-name instrument-id) 1)
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

  (define mute (create-custom-checkbox instrument-id strips-config
                                       draw-mute
                                       (lambda (is-muted)
                                         (undo-block
                                          (lambda ()
                                            (<ra> :undo-instrument-effect instrument-id volume_on_off_name)
                                            (<ra> :set-instrument-effect instrument-id volume_on_off_name (if is-muted 0.0 1.0))
                                            ;;(c-display "mute: " is-muted)
                                            (if (<ra> :control-pressed)
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
  
  (define solo (create-custom-checkbox instrument-id strips-config
                                       (lambda (solo is-soloed width height)
                                         (draw-mutesolo solo is-soloed #f (if is-minimized "S" "Solo") "yellow" width height))
                                       (lambda (is-selected)
                                         (undo-block
                                          (lambda ()
                                            ;;(<ra> :undo-instrument-effect instrument-id "System Solo On/Off")
                                            (<ra> :set-instrument-effect instrument-id "System Solo On/Off" (if is-selected 1.0 0.0))
                                            (if (<ra> :control-pressed)
                                                (turn-off-all-solo instrument-id)))))
                                       (get-soloed)
                                       height))
  
  (add-gui-effect-monitor (cadr mute) instrument-id volume_on_off_name
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

(define (create-mixer-strip-volume instrument-id meter-instrument-id strips-config background-color is-minimized)
  (define fontheight (get-fontheight))
  (define voltext-height fontheight)

  (define horizontal-spacing 4) ;; must be an even number.
  (define horizontal-spacing/2 (/ horizontal-spacing 2))
  
  (define show-voltext (not is-minimized))
  (define show-peaktext #t)

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

  (define voltext (and show-voltext (<gui> :widget)))
  (define peaktext (and show-peaktext (<gui> :widget)))

  (define paint-slider #f)

  (define last-vol-slider (get-volume))
  (define volslider (<gui> :vertical-slider
                           ""
                           0 (db-to-slider (get-volume)) 1
                           (lambda (val)
                             (define db (slider-to-db val))
                             (when (and doit (not (= last-vol-slider db)))
                               (set! last-vol-slider db)
                               ;;(c-display "             hepp hepp")
                               (<ra> :set-instrument-effect instrument-id effect-name (scale db *min-db* *max-db* 0 1))
                               (if paint-voltext
                                   (<gui> :update voltext))))))

  (<gui> :set-size-policy volslider #t #t)
  ;;(<gui> :set-min-width volslider 1) ;; ?? Why is this necessary?
  
  (define (paint-text gui text)
    (define width (<gui> :width gui))
    (define height (<gui> :height gui))
    
    (define col1 (<gui> :mix-colors "#010101" background-color 0.7))
    
    ;; background
    (<gui> :filled-box gui background-color 0 0 width height)
    
    ;; rounded
    (if is-minimized
        (<gui> :filled-box gui col1 0 0 width height 0 0)
        (<gui> :filled-box gui col1 0 0 width height 5 5))
    
    ;; text
    (<gui> :draw-text gui *text-color* text 2 2 (- width 2) (- height 2)))

    
  (when show-voltext
    (set! paint-voltext
          (lambda ()
            (paint-text voltext (db-to-text (get-volume) #f))))
    
    (add-safe-paint-callback voltext (lambda x (paint-voltext)))
    ;;(paint-voltext)
    )

  (when show-peaktext
    (set! paint-peaktext
          (lambda ()
            (paint-text peaktext peaktexttext)))
    
    (add-safe-paint-callback peaktext (lambda x (paint-peaktext)))
    ;;(paint-peaktext)
    )

  (define volslider-rounding (if is-minimized 0 2))
  
  (set! paint-slider
        (lambda ()
          (define volslider-width (<gui> :width volslider))
          (define mid (floor (/ volslider-width 2)))
          (define spacing (if is-minimized
                              0
                              horizontal-spacing/2))
          (define width (- mid spacing))
          (define height (<gui> :height volslider))
          (define x1 0)
          (define x2 width)
          (define middle_y (scale (db-to-slider (get-volume)) 0 1 height 0))
          
          ;; background
          (<gui> :filled-box volslider background-color 0 0 volslider-width height)
          
          (define col1 (<gui> :mix-colors
                              (if is-sink? "#f0f0f0" "#010101")
                              background-color 0.2)) ;; down
          (define col2 (<gui> :mix-colors "#010101" background-color 0.9)) ;; up

          ;; slider
          (<gui> :filled-box volslider col2 x1 0 x2 height volslider-rounding volslider-rounding) ;; up (fill everything)
          (<gui> :filled-box volslider col1 x1 middle_y x2 height volslider-rounding volslider-rounding) ;; down

          ;; slider border
          (<gui> :draw-box volslider "#222222" (+ 0.5 x1) 0.5 (- x2 0.5) (- height 0.5) 1.0 volslider-rounding volslider-rounding)
          ;;(<gui> :filled-box volslider "black" 0 0 (1+ x2) height)

          ;; slider 0db, white line
          (define middle_0db (scale (db-to-slider 0) 0 1 height 0))
          (<gui> :draw-line volslider "#eeeeee" (1+ x1) middle_0db (1- x2) middle_0db 0.3)
          ))

  (add-safe-paint-callback volslider (lambda x (paint-slider)))


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

  (add-safe-mouse-callback volslider
         (lambda (button state x y)
           (cond ((and (= button *left-button*)
                       (= state *is-pressing*))
                  (<ra> :undo-instrument-effect instrument-id effect-name))
                 ((and (= button *right-button*)
                       (= state *is-pressing*))
                  (popup-menu "Reset" (lambda ()
                                        (<ra> :undo-instrument-effect instrument-id effect-name)
                                        (<ra> :set-instrument-effect instrument-id effect-name (scale 0 *min-db* *max-db* 0 1)))
                              "------------"
                              (get-global-mixer-strips-popup-entries instrument-id strips-config))))
           #f))


  (<gui> :add volslider volmeter 0 0 5 5) ;; Set parent of volmeter to volslider (gui_setParent should be renamed to something else, this is awkward)
  
  (add-safe-resize-callback volslider (lambda (width height) 
                                        (define mid (floor (/ width 2)))
                                        (define spacing (if is-minimized
                                                            0
                                                            horizontal-spacing/2))
                                        (define volmeter-x1 (+ mid spacing))
                                        (<gui> :set-pos volmeter volmeter-x1 0)
                                        (<gui> :set-size volmeter (- width volmeter-x1) height)))

  (when show-voltext
    (add-safe-mouse-callback voltext (lambda (button state x y)                                       
                                         (cond ((and (= button *right-button*)
                                                     (= state *is-pressing*))                                                
                                                (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config))
                                                #t)
                                               ((and (= button *left-button*)
                                                     (= state *is-pressing*))
                                                (define old-volume (let ((v (two-decimal-string (get-volume))))
                                                                     (if (or (string=? v "-0.0")
                                                                             (string=? v "-0.00"))
                                                                         "0.00"
                                                                         v)))
                                                (let ((maybe (<gui> :requester-operations
                                                                    (<-> "Set new volume for " (<ra> :get-instrument-name instrument-id))
                                                                    (lambda ()
                                                                      (<ra> :request-float "dB: " *min-db* *max-db* #f old-volume)))))
                                                  (when (>= maybe *min-db*)
                                                    (<ra> :undo-instrument-effect instrument-id effect-name)
                                                    (<ra> :set-instrument-effect instrument-id effect-name (scale maybe *min-db* *max-db* 0 1))))
                                                #t)
                                               (else
                                                #f))))
    )

  (when show-peaktext

    (<gui> :add-audio-meter-peak-callback volmeter (lambda (text)
                                                     (set! peaktexttext text)
                                                     (<gui> :update peaktext)))

    (add-safe-mouse-callback volmeter (lambda (button state x y)
                                          (cond ((and (= button *right-button*)
                                                      (= state *is-pressing*))
                                                 (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config))
                                                 #t)
                                                (else
                                                 #f))))

    (add-safe-mouse-callback peaktext (lambda (button state x y)
                                          (cond ((and (= button *right-button*)
                                                      (= state *is-pressing*))
                                                 (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config))
                                                 #t)
                                                ((and (= button *left-button*)
                                                      (= state *is-pressing*))
                                                 (set! peaktexttext "-inf")
                                                 (<gui> :reset-audio-meter-peak volmeter)
                                                 (<gui> :update peaktext)
                                                 #t)
                                                (else
                                                 #f)))))

  ;; horiz 1 (voltext and peaktext)
  ;;
  (define horizontal1 (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing horizontal1 horizontal-spacing 0 0 0 0)

  (if (or show-voltext show-peaktext)
      (set-fixed-height horizontal1 voltext-height))

  (if show-voltext
      (<gui> :add horizontal1 voltext 1))

  (if show-peaktext
    (<gui> :add horizontal1 peaktext 1))


  ;; vertical
  (define vertical (<gui> :vertical-layout))
  (<gui> :set-layout-spacing vertical 5 0 0 0 0)

  (<gui> :add vertical horizontal1)
  (<gui> :add vertical volslider)

  (set! doit #t)

  vertical
  )

(define (get-mixer-strip-background-color gui instrument-id)
  (<gui> :mix-colors
         (<ra> :get-instrument-color instrument-id)
         (<gui> :get-background-color gui)
         0.3))


(define (create-mixer-strip-comment instrument-id strips-config height)
  (define comment-edit (<gui> :line (<ra> :get-instrument-comment instrument-id)
                              (lambda (new-name)
                                (<ra> :set-instrument-comment new-name instrument-id))))    
  (<gui> :set-background-color comment-edit (<ra> :get-instrument-color instrument-id))
  (set-fixed-height comment-edit height)

  (add-safe-mouse-callback comment-edit (lambda (button state x y)
                                            (if (and (= button *right-button*)
                                                     (= state *is-pressing*))
                                                (begin
                                                  (if (<ra> :shift-pressed)
                                                      (<ra> :delete-instrument instrument-id)
                                                      (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config)))
                                                  #t)
                                                #f)))
  comment-edit)

(define-constant *mixer-strip-border-color* "#bb222222")
(define *current-mixer-strip-border-color* "mixerstrips_selected_object_color_num")

(define (mydrawbox gui color x1 y1 x2 y2 line-width rounding)
  (define w (/ line-width 2))
  (define w*3 (* w 3))
  (<gui> :draw-box
         gui color
         (+ x1 w) (+ y1 w)
         (- x2 w) (- y2 w)
         line-width
         rounding rounding))

(define (draw-mixer-strips-border gui width height instrument-id is-standalone-mixer-strip border-width)
  ;;(c-display "    Draw mixer strips border called for " instrument-id)
  (if (= (<ra> :get-current-instrument) instrument-id)
      (if (not is-standalone-mixer-strip)
          (mydrawbox gui *current-mixer-strip-border-color* 0 0 width height border-width 0)
          )))

(define (create-current-instrument-border gui instrument-id)
  (define rubberband-resize (gui-rubberband gui 5 "#bb111144" (lambda ()
                                                                (= (<ra> :get-current-instrument) instrument-id))))
  (add-safe-resize-callback gui (lambda (width height)
                                  (rubberband-resize 0 0 width height))))

  
(define (create-mixer-strip-minimized instrument-id strips-config is-standalone-mixer-strip)
  (define color (<ra> :get-instrument-color instrument-id))

  ;;(define gui (<gui> :vertical-layout)) ;; min-width height))
  ;;(<gui> :add gui (<gui> :checkbox "" #f))
  ;;(<gui> :add gui (<gui> :text (<ra> :get-instrument-name instrument-id)))
  
  ;(define gui (<gui> :checkbox (<ra> :get-instrument-name instrument-id) #f #t))
  ;(<gui> :set-background-color gui (<ra> :get-instrument-color instrument-id))
  ;;(set-fixed-width gui (get-fontheight))

  (define border-width 2)
  
  (define bsize (if is-standalone-mixer-strip 0 border-width))
  
  (define gui (<gui> :vertical-layout)) ;; min-width height))
  (<gui> :set-layout-spacing gui 5 bsize bsize bsize bsize)
      
  (define background-color (get-mixer-strip-background-color gui instrument-id))
  (<gui> :set-background-color gui color)
  
  ;;(define gui (<gui> :widget))
  (set-fixed-width gui (+ (* bsize)
                          (max (floor (<gui> :text-width "-14.2"))
                               (get-fontheight))))

  (define label (create-mixer-strip-name instrument-id strips-config #t is-standalone-mixer-strip))


  (<gui> :add gui label 1)
  (<gui> :add gui (create-mixer-strip-mutesolo instrument-id strips-config background-color (get-fontheight) #t))

  (define meter-instrument-id (find-meter-instrument-id instrument-id))

  (define volume-gui (create-mixer-strip-volume instrument-id meter-instrument-id strips-config background-color #t))
  (<gui> :add gui volume-gui 1)

  ;;(if (not is-standalone-mixer-strip)
  ;;    (create-current-instrument-border gui instrument-id))

  (add-safe-paint-callback gui
         (lambda (width height)
           ;;(set-fixed-height volume-gui (floor (/ height 2)))
           (<gui> :filled-box gui background-color 0 0 width height 0 0)
           (draw-mixer-strips-border gui width height instrument-id is-standalone-mixer-strip bsize)
           #t
           )
         )

  gui)


(define (create-mixer-strip-wide instrument-id strips-config min-width is-standalone-mixer-strip)
  (define gui (<gui> :vertical-layout)) ;; min-width height))
  (<gui> :set-min-width gui min-width)
  ;;(<gui> :set-max-width gui width)
  ;;(<gui> :set-size-policy gui #f #t)

  (define bsize (if is-standalone-mixer-strip 0 5))
  
  (<gui> :set-layout-spacing gui 2 bsize bsize bsize bsize)

  (define background-color (get-mixer-strip-background-color gui instrument-id))

  (define system-background-color (<gui> :get-background-color gui))
  (<gui> :set-background-color gui background-color) ;;(<ra> :get-instrument-color instrument-id))
  
  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define name-height fontheight-and-borders)
  (define pan-height fontheight-and-borders)
  (define mutesolo-height fontheight-and-borders)
  (define comment-height fontheight-and-borders)

  (define name (create-mixer-strip-name instrument-id strips-config #f is-standalone-mixer-strip))
  (set-fixed-height name name-height)

  (<gui> :add gui name)

  (define mixer-strip-path-gui (<gui> :vertical-scroll))
  (<gui> :set-layout-spacing mixer-strip-path-gui 5 5 5 5 5)
  (<gui> :set-style-sheet mixer-strip-path-gui
         (<-> "QScrollArea {"
              "  background: " system-background-color ";"
              "  border: 1px solid rgba(10, 10, 10, 50);"
              "  border-radius:3px;"
              "}"))
 

  (define hepp (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing hepp 0 5 2 5 2)

  ;;(<gui> :add-layout-space hepp 2 2 #f #f)
  (<gui> :add hepp mixer-strip-path-gui)
  ;;(<gui> :add-layout-space hepp 2 2 #f #f)

  (<gui> :add gui hepp 1)

  ;(set-fixed-width mixer-strip-path-gui (- (<gui> :width gui) 26))

  '(add-safe-paint-callback gui
         (lambda (width height)
           (set-fixed-width mixer-strip-path-gui (- width 26))))

  
  (add-safe-mouse-callback mixer-strip-path-gui (lambda (button state x y)
                                                    (if (and (= button *right-button*)
                                                             (= state *is-pressing*))
                                                        (begin (if (<ra> :shift-pressed)
                                                                   (<ra> :delete-instrument instrument-id)
                                                                   (create-default-mixer-path-popup instrument-id strips-config mixer-strip-path-gui bsize))
                                                               #t)
                                                        #f)))

  (define meter-instrument-id (create-mixer-strip-path mixer-strip-path-gui instrument-id instrument-id strips-config))

  (<gui> :add gui (create-mixer-strip-pan instrument-id strips-config system-background-color background-color pan-height))
  (<gui> :add gui (create-mixer-strip-mutesolo instrument-id strips-config background-color mutesolo-height #f))
  (<gui> :add gui (create-mixer-strip-volume instrument-id meter-instrument-id strips-config background-color #f) 1)
  (define comment (create-mixer-strip-comment instrument-id strips-config comment-height))
  (<gui> :add gui comment)

  ;;(if (not is-standalone-mixer-strip)
  ;;    (create-current-instrument-border gui instrument-id))

  (add-safe-paint-callback gui
                           (lambda (width height)
                             (<gui> :filled-box gui background-color 0 0 width height 0 0)
                             (draw-mixer-strips-border gui width height instrument-id is-standalone-mixer-strip bsize)))
  
  gui)

(delafina (create-mixer-strip :instrument-id
                              :min-width
                              :strips-config #f
                              :is-standalone-mixer-strip #f)
  (define is-wide (if is-standalone-mixer-strip
                      *current-mixer-strip-is-wide*
                      (<ra> :has-wide-instrument-strip instrument-id)))
  (if is-wide
      (create-mixer-strip-wide instrument-id strips-config min-width is-standalone-mixer-strip)
      (create-mixer-strip-minimized instrument-id strips-config is-standalone-mixer-strip)))


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
  (cond ((not stored-mixer-strip)
         #f)
        ((eq? :all-are-valid list-of-modified-instrument-ids)
         #t)
        ((eq? :non-are-valid list-of-modified-instrument-ids)
         #f)
        (else
         (let ((instrument-id (get-instrument-id-from-stored-mixer-strip stored-mixer-strip)))
           (not (or (memv instrument-id list-of-modified-instrument-ids)
                    (any? (lambda (instrument-id)
                            (memv instrument-id list-of-modified-instrument-ids))
                          (get-all-instruments-used-in-mixer-strip instrument-id))))))))


(define (create-standalone-mixer-strip instrument-id width height)
  ;;(define parent (<gui> :horizontal-layout))
  ;;(<gui> :set-layout-spacing parent 0 0 0 0 0)

  (define parent (<gui> :widget width height)) ;; Lots of trouble using a widget as parent instead of a layout. However, it's an easy way to avoid flickering when changing current instrument.
  
  ;;(define width (floor (* 1 (<gui> :text-width "MUTE SOLO"))))
  
  (set-fixed-width parent width)
  ;;(set-fixed-height parent height)
  ;;(<gui> :set-min-width parent 100)
  ;;(<gui> :set-max-width parent 100)
  
  (define das-mixer-strip-gui #f)

  (define org-width width)

  (define (remake width height)
    (define instrument-is-open (<ra> :instrument-is-open instrument-id))
    
    (run-instrument-data-memoized
     (lambda()
       (when das-mixer-strip-gui
         (<gui> :close das-mixer-strip-gui)
         (set! das-mixer-strip-gui #f))
       
       (when instrument-is-open                
         (set! das-mixer-strip-gui (create-mixer-strip instrument-id width :is-standalone-mixer-strip #t))
         (if *current-mixer-strip-is-wide*
             (set! width org-width)
             (set! width (<gui> :width das-mixer-strip-gui)))
         (set-fixed-width parent width)
         (set-fixed-width das-mixer-strip-gui width)
         (<gui> :add parent das-mixer-strip-gui 0 0 width height)
         )
       
       )))

  
  (remake width height)
  

  (define is-resizing #f)
  
  (<gui> :add-resize-callback parent
         (lambda (width height)
           (when (not is-resizing) ;; Unfortunately, remake triggers a new resize, and we get a recursive call here. TODO: Fix this. Resize callback should never call itself.
             (set! is-resizing #t)             
             (<gui> :disable-updates parent)
             (remake width height) ;; Don't need to use safe callback here. 'remake' checks that the instrument is open.
             (<gui> :enable-updates parent)
             (set! is-resizing #f))))
  
  (define mixer-strips-object (make-mixer-strips-object :gui parent
                                                        :remake (lambda (list-of-modified-instrument-ids)
                                                                  (remake (<gui> :width parent) (<gui> :height parent)))))
  
  ;;(<ra> :inform-about-gui-being-a-mixer-strips parent) // Must only be called for standalone windows.

  (push-back! *mixer-strips-objects* mixer-strips-object)

  (<gui> :add-deleted-callback parent
         (lambda (radium-runs-custom-exec)
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

(define (create-mixer-strips num-rows stored-mixer-strips strips-config list-of-modified-instrument-ids kont)
  ;;(c-display "\n\n      ============ CREATE-MIXER-STRIPS ============\n\n")

  ;;(set! num-rows 3)
  (define strip-separator-width 1)
  (define border-color *mixer-strip-border-color*)
  (define instruments/buses-separator-width (max 2 (floor (* (get-fontheight) 0.2))))

  ;;(define mixer-strips (<gui> :widget 800 800))
  ;;(define mixer-strips-gui (<gui> :horizontal-scroll)) ;;widget 800 800))
  (define mixer-strips-gui (<gui> :scroll-area #t #t))
  (<gui> :set-layout-spacing mixer-strips-gui 0 0 0 0 0)
  (<gui> :set-background-color mixer-strips-gui border-color)
  
  (define vertical-layout (<gui> :vertical-layout))
  (<gui> :set-layout-spacing vertical-layout strip-separator-width 0 0 0 0)

  (define row-num -1)
  (define column-num 0)
  (define horizontal-layout #f)

  (define (add-new-horizontal-layout!)
    (set! horizontal-layout (<gui> :horizontal-layout))
    (<gui> :set-layout-spacing horizontal-layout strip-separator-width 0 0 0 0)
    (<gui> :add vertical-layout horizontal-layout)
    (inc! row-num 1)
    (set! column-num 0))

  (add-new-horizontal-layout!)


  (<gui> :add mixer-strips-gui vertical-layout)

  ;;(c-display "...scan1")
  (strips-config :scan-instruments!)

  (define num-visible-strips (length (keep (lambda (id)
                                             (strips-config :is-enabled id))
                                           (get-all-audio-instruments))))

  (define min-mixer-strip-width (1+ (floor (max (<gui> :text-width " -14.2 -23.5 ")
                                                (<gui> :text-width " Mute Solo ")))))

  (define num-strips-per-row (ceiling (/ num-visible-strips num-rows)))

  (define (add-strips id-instruments)
    (map (lambda (instrument-id)
           (set! (strips-config :row-num instrument-id) row-num)
           ;;(c-display "adding strips for" instrument-id)
           (define stored-mixer-strip (get-stored-mixer-strip stored-mixer-strips instrument-id))
           (define mixer-strip (if (stored-mixer-strip-is-valid? stored-mixer-strip list-of-modified-instrument-ids)
                                   (get-mixer-strip-from-stored-mixer-strip stored-mixer-strip)
                                   (create-mixer-strip instrument-id min-mixer-strip-width :strips-config strips-config)))
           '(c-display "   Creating" instrument-id ". Stored is valid?" (stored-mixer-strip-is-valid? stored-mixer-strip list-of-modified-instrument-ids)
                       "stored-mixer-strip:" stored-mixer-strip
                       "list-of-modified:" list-of-modified-instrument-ids)
           
           (<gui> :add horizontal-layout mixer-strip)
           
           (set! column-num (1+ column-num))
           (if (= num-strips-per-row column-num)
               (add-new-horizontal-layout!))
           (create-stored-mixer-strip instrument-id
                                      mixer-strip))
         (keep (lambda (id)
                 (strips-config :is-enabled id))
               id-instruments)))


  (define cat-instruments (<new> :get-cat-instruments))
  
  (define instrument-mixer-strips (add-strips (sort-instruments-by-mixer-position-and-connections (cat-instruments :instrument-instruments))))
  
  (let ((text (<gui> :text *arrow-text2* "" #f #f)))
    ;;(<gui> :set-background-color text "blue") ;; doesn't work.
    (<gui> :add horizontal-layout text))
  
  (define bus-mixer-strips (add-strips (sort-instruments-by-mixer-position-and-connections (cat-instruments :bus-instruments))))
  
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
  :strips-config #f
  :is-full-screen #f
  :pos #f)

(define *mixer-strips-objects* (if *is-initializing*
                                   '()
                                   *mixer-strips-objects*))

(delafina (create-mixer-strips-gui :num-rows 1
                                   :instrument-ids #f
                                   :is-full-screen #f
                                   :pos #f)
  
  (set! instrument-ids (to-list instrument-ids))
  
  (c-display "   IDS:" instrument-ids num-rows)
  ;;(set! instrument-ids #f)
  
  ;;(define gui (<gui> :horizontal-layout))
  ;;(define gui (<gui> :scroll-area #t #t))
  ;;(define gui (<gui> :widget))
  (define gui (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing gui 0 0 0 0 0)

  (define width (if pos (caddr pos) 1000))
  (define height (if pos (cadddr pos) 800))

  (<gui> :set-size gui width height)
  (if pos
      (<gui> :set-pos
             gui
             (if pos (car pos) 600)
             (if pos (cadr pos) 50)))
  ;;(<gui> :set-layout-spacing gui 0 0 0 0 0)

  (if (not is-full-screen)
      (<gui> :set-parent gui -1)
      (<gui> :set-full-screen gui))
  
  ;;(<gui> :show gui)
      
  ;;(<gui> :set-full-screen gui)

  (define das-stored-mixer-strips '())
  (define das-mixer-strips-gui #f)
  
  (define (remake list-of-modified-instrument-ids)
    ;;(c-display "REMAKE" list-of-modified-instrument-ids)
    (define start-time (time))
    (set! g-total-time 0)
    (set! g-total-time2 0)
    (set! g-total-num-calls 0)
    (set! g-total-sort-time 0)
    
    (run-instrument-data-memoized
     (lambda()
       (<gui> :disable-updates gui)
       ;;(c-display "   Size of das-stored:" (length das-stored-mixer-strips))
       (create-mixer-strips (strips-config :num-rows) das-stored-mixer-strips strips-config list-of-modified-instrument-ids
                            (lambda (new-mixer-strips new-mixer-strips-gui)
                              (if das-mixer-strips-gui
                                  (begin
                                    (<gui> :replace gui das-mixer-strips-gui new-mixer-strips-gui)
                                    (<gui> :close das-mixer-strips-gui))
                                  (begin
                                    (<gui> :add gui new-mixer-strips-gui)
                                    ;;(<gui> :show mixer-strips-gui)
                                    ))
                              
                              ;;(c-display "...scan2")
                              ;;(strips-config :scan-instruments!) ;; :scan-instruments! was called in 'create-mixer-strips'.
                              (strips-config :recreate-config-gui-content)
                              (set! das-stored-mixer-strips new-mixer-strips)
                              (set! das-mixer-strips-gui new-mixer-strips-gui)
                              ))
       ))
    
    ;; prevent some flickering
    (<ra> :schedule 15 (lambda ()
                         (<gui> :enable-updates gui)
                         #f))
    
    (c-display "   remake-gui duration: " (- (time) start-time) g-total-time "("g-total-num-calls ")" g-total-time2 g-total-sort-time)
    )


  (define strips-config (create-strips-config instrument-ids num-rows remake gui))

  (add-safe-mouse-callback gui
         (lambda (button state x y)
           (cond ((and (= button *right-button*)
                       (= state *is-releasing*))
                  (popup-menu
                   (get-global-mixer-strips-popup-entries #f strips-config))))
           #f))

    
  (define mixer-strips-object (make-mixer-strips-object :gui gui
                                                        :is-full-screen is-full-screen
                                                        :remake remake
                                                        :strips-config strips-config
                                                        :pos pos))
  (remake :non-are-valid)
  
  (<ra> :inform-about-gui-being-a-mixer-strips gui)

  (push-back! *mixer-strips-objects* mixer-strips-object)

  (<gui> :add-deleted-callback gui
         (lambda (radium-runs-custom-exec)
           (set! *mixer-strips-objects*
                 (remove (lambda (a-mixer-strips-object)
                           (= (a-mixer-strips-object :gui)
                              gui))
                         *mixer-strips-objects*))))

  ;;mixer-strips-object
  
  gui
  )

#!!
(define gui (create-mixer-strips-gui 2
                                     (get-all-audio-instruments)))
(length (get-all-audio-instruments))
(<gui> :show gui)
!!#

(define (remake-mixer-strips . list-of-modified-instrument-ids)
  ;;(c-display "\n\n\n             REMAKE MIXER STRIPS " (sort list-of-modified-instrument-ids <) (length *mixer-strips-objects*) "\n\n\n")
  ;;(c-display "all:" (sort (get-all-audio-instruments) <))
  (let ((list-of-modified-instrument-ids (cond ((or (null? list-of-modified-instrument-ids)
                                                    (not (null? ((<new> :container (get-all-audio-instruments)) ;;i.e. if a modified instrument is deleted.
                                                                 :set-difference
                                                                 list-of-modified-instrument-ids))))
                                                :non-are-valid)
                                               ((true-for-all? (lambda (id)
                                                                 (= id -2))
                                                               list-of-modified-instrument-ids)
                                                :all-are-valid)
                                               (else
                                                (remove (lambda (id)
                                                          (= id -2))
                                                        list-of-modified-instrument-ids)))))
    (for-each (lambda (a-mixer-strips-object)
                ((a-mixer-strips-object :remake) list-of-modified-instrument-ids))
              *mixer-strips-objects*)))

(define (redraw-mixer-strips . list-of-modified-instrument-ids)
  ;;(c-display "\n\n\n             REDRAW MIXER STRIPS " list-of-modified-instrument-ids "\n\n\n")
  (for-each (lambda (a-mixer-strips-object)
              ;;(c-display "       updating " (a-mixer-strips-object :gui))
              (<gui> :update-recursively (a-mixer-strips-object :gui)))
            *mixer-strips-objects*))


(define (get-mixer-strips-object-from-gui mixer-strips-gui)
  (let loop ((objects *mixer-strips-objects*))
    (if (= ((car objects) :gui)
           mixer-strips-gui)
        (car objects)
        (loop (cdr objects)))))
                   

(define (mixer-strips-get-num-rows mixer-strips-gui)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (object :strips-config :num-rows)))

(define (mixer-strips-change-num-rows mixer-strips-gui num-rows)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (set! (object :strips-config :num-rows) num-rows)))

#!!
(mixer-strips-change-num-rows ((car *mixer-strips-objects*) :gui) 6)
!!#


(define (mixer-strips-reset-configuration! mixer-strips-gui)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (object :strips-config :reset!)))

#!!
(mixer-strips-reset-configuration! ((car *mixer-strips-objects*) :gui))
!!#

(define (mixer-strips-get-configuration mixer-strips-gui)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (object :strips-config :get)))
  
(define (mixer-strips-set-configuration! mixer-strips-gui configuration)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (object :strips-config :set! configuration)))
  

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
          (create-mixer-strips-gui 1 #f #f pos))
        (begin
          (define x (<gui> :get-x gui))
          (define y (<gui> :get-y gui))
          (define width (<gui> :width gui))
          (define height (<gui> :height gui))
          (define pos (list x y width height))
          (create-mixer-strips-gui 1 #f #t pos))))

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



;; main (practical, but if it fails, we must restart radium, so not always very practical)
#||
(when (not *is-initializing*)
  (let ((start (time)))
    (set! *mixer-strips-objects* '())
    (define gui (create-mixer-strips-gui 2))
    (<gui> :show gui)
    (c-display "   Time used to open mixer:" (- (time) start))))
||#




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

(fill! (*s7* 'profile-info) #f)
(show-profile 100)

||#

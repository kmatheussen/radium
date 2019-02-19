(provide 'mixer-strips.scm)

(my-require 'gui.scm)

;; TOPIC: Should the pan slider work for the last instrument in the path?
;; In case, we need to copy pan values when deleting / removing plulgins.
;;
;; When adding a new mixer strip, we could slice the various mixer strips windows.




(define (get-fontheight)
  (+ 4 (<gui> :get-system-fontheight))) ;; If changing this one, change the "font_height" variable in Seqtracks_widget::get_heights, and paintVamps in api_gui.cpp, too.

(define-constant *text-color* "#cccccc")
(define-constant *arrow-text* "↳")
(define-constant *arrow-text2* "→")

(define *pan-mutesolo-voltext-scale-factor* 0.75)

(define *current-mixer-strip-is-wide* #f)


(define (mixer-normalized-to-slider mixer-normalized) ;; TODO: Rename "scaled" into "normalized" in the whole program.
  (* mixer-normalized mixer-normalized)) ;; Seems to work okay

(define (radium-normalized-to-mixer-normalized radium-normalized)
  (db-to-mixer-normalized
   (radium-normalized-to-db
    radium-normalized)))
         
(define (radium-normalized-to-slider radium-normalized)
  (mixer-normalized-to-slider (radium-normalized-to-mixer-normalized radium-normalized)))

(define (radium-normalized-to-db radium-normalized)
  (scale radium-normalized 0 1 *min-db* *max-db*))

(define (mixer-normalized-to-db mixer-normalized)
  (scale mixer-normalized 0 1 *min-db* *max-mixer-db*))

(define (db-to-mixer-normalized db)
  (scale db *min-db* *max-mixer-db* 0 1))

(define (db-to-radium-normalized db)
  (scale db *min-db* *max-db* 0 1))

(define (db-to-slider db)
  (if (<= db *min-db*)
      0
      (mixer-normalized-to-slider (db-to-mixer-normalized db))))

(define (slider-to-mixer-normalized slider)
  (sqrt slider)) ;; Seems to work okay

(define (slider-to-db slider)
  (define mixer-normalized (slider-to-mixer-normalized slider))
  (mixer-normalized-to-db mixer-normalized))

(define (db-to-text db add-dB-string)
  (if  (<= db *min-db*)
       "~inf"
       (let* ((val1 (one-decimal-string db))
              (val (if (string=? val1 "-0.0") "0.0" val1)))
         (if add-dB-string
             (<-> val " dB")
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

(delafina (get-global-mixer-strips-popup-entries :instrument-id
                                                 :strips-config
                                                 :wide-mode-instrument-id #f
                                                 :effect-name #f)

  (if (not wide-mode-instrument-id)
      (set! wide-mode-instrument-id instrument-id))
  
  (list
   (and effect-name
        (get-effect-popup-entries instrument-id effect-name))
   "------------Mixer"
   ;;(list "Pan Enabled"
   ;;      :check (pan-enabled? instrument-id)
   ;;      (lambda (onoff)
   ;;        (pan-enable! instrument-id onoff)))

   (list "Wide mode"
         :check (and wide-mode-instrument-id
                     (<ra> :has-wide-instrument-strip wide-mode-instrument-id))
         :enabled wide-mode-instrument-id
         (lambda (enabled)
           (<ra> :set-wide-instrument-strip wide-mode-instrument-id enabled)
           (remake-mixer-strips instrument-id)))
   
   (list "Make all strips wide" (lambda ()
                                  (for-each (lambda (i) (<ra> :set-wide-instrument-strip i #t)) (get-all-audio-instruments))
                                  (<ra> :remake-mixer-strips)))
   (list "Make no strips wide" (lambda ()
                                 (for-each (lambda (i) (<ra> :set-wide-instrument-strip i #f)) (get-all-audio-instruments))
                                 (<ra> :remake-mixer-strips)))
   
   "----------"
   
   (list "Mixer strip visible" :enabled (or instrument-id
                                            (not strips-config))
         :check #t
         (lambda (hideit)
           (if (not strips-config)
               (<ra> :show-hide-mixer-strip)
               (set! (strips-config :is-enabled instrument-id) #f))))
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


(delafina (create-instrument-effect-checkbox :instrument-id
                                             :strips-config
                                             :paint-func
                                             :value-changed
                                             :is-selected
                                             :min-height
                                             :effect-name #f
                                             :hovered-callback #f
                                             )
  
  (define checkbox (<gui> :widget))
  (if min-height
      (<gui> :set-min-height checkbox min-height))

  (add-safe-mouse-callback checkbox (lambda (button state x y)
                                      ;;(c-display "state" state)
                                      (if hovered-callback
                                          (hovered-callback button state x y))
                                        (if (and (= button *right-button*)
                                                 (= state *is-pressing*))
                                            (if (<ra> :shift-pressed)
                                                (if effect-name
                                                    (<ra> :reset-instrument-effect instrument-id effect-name)
                                                    (<ra> :delete-instrument instrument-id))
                                                (if strips-config
                                                    (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config :effect-name effect-name)))))
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


  
(define (add-gui-effect-monitor gui instrument-id effect-name monitor-stored monitor-automation callback)
  (let ((effect-monitor (<ra> :add-effect-monitor effect-name instrument-id monitor-stored monitor-automation callback)))
    (<gui> :add-deleted-callback gui
           (lambda (radium-runs-custom-exec)
             (<ra> :remove-effect-monitor effect-monitor #f))))) ;; This function should be safe to call also when 'radium-runs-custom-exec' is true.


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

                                           (<gui> :my-draw-text
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
     (hash-table->alist (hash-table :a 1 :b 2)))

(let ((h (make-hash-table)))
  (set! (h :a) 5)
  (set! (h :b) 2)
  (c-display "alist" (hash-table->alist h))
  (map (lambda (key)
         (display "key:")(display key)(newline))
       h))

       (hash-table->alist h)))

(c-display "hepp" (hash-table->alist (hash-table '(a . 5) '(b . 2))))
(c-display "hepp" (hash-table->alist (hash-table 'a 5 'b 2)))

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
                   ;;(c-display "=======================" (<ra> :get-instrument-name id) ". is-instrument:" is-instrument ". is-enabled:" is-enabled ". is-strip:" is-strip)
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
  (<gui> :set-parent config-gui parentgui)

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
                 (eat-errors :try (lambda ()
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
    (let ((conf (confs instrument-id)))
      (assert conf)
      (set! (confs instrument-id) (<copy-conf> conf keyword new-value))))
                   

  (define this
    (dilambda (lambda (keyword . rest)
                ;;(c-display "THIS called. keyword:" keyword)
                (case keyword
                  ((:parent-gui) parentgui)
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
                  ((:get) (hash-table :num-rows num-rows
                                       :instrument-settings (keep identity
                                                                  (map (lambda (entry)
                                                                         (define instrument-id (car entry))
                                                                         (if (<ra> :instrument-is-open-and-audio instrument-id) ;; When saving song, conf can include deleted instruments.
                                                                             (let ((conf (cdr entry)))
                                                                               (hash-table :instrument-id instrument-id
                                                                                            :is-enabled (conf :is-enabled)))
                                                                             #f))
                                                                       confs))))
                  ((:set!) (let ((settings (car rest)))
                             (reset!)
                             (set! num-rows (settings :num-rows))
                             (for-each (lambda (conf)
                                         (define instrument-id (conf :instrument-id))
                                         (if (<ra> :instrument-is-open-and-audio instrument-id) ;; When loading older songs, settings can include id of deleted instruments
                                             (set-conf-var! instrument-id :is-enabled (conf :is-enabled))))
                                       (to-list (settings :instrument-settings)))
                             (remake '())));;:non-are-valid)))
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
                                                                        (if (<ra> :instrument-is-open-and-audio instrument-id)
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

  (define (show-name-tool-tip)
    (<gui> :set-tool-tip label name))
  
  (if is-minimized
      (show-name-tool-tip))
  
  (if is-minimized
      (<gui> :set-min-height label (* 2 (<gui> :get-system-fontheight))))

  (define instruments-in-path (and is-minimized
                                   (get-all-instruments-used-in-mixer-strip instrument-id)))
  (define num-in-path (length instruments-in-path))
  
  (add-safe-paint-callback label
         (lambda (width height)
           (<gui> :filled-box label color 0 0 width height)
           (if is-minimized
               (begin
                 (define ysplit (if (<= num-in-path 1)
                                    height
                                    (- height (* 1.5 (<gui> :get-system-fontheight)))))                                    
                 (<gui> :draw-vertical-text label *text-color* name 2 7 (+ width 0) ysplit #t #f #t)
                 (if (> num-in-path 1)
                     (<gui> :do-alpha label 0.7
                            (lambda () 
                              (<gui> :draw-text label *text-color* (<-> "(" (- num-in-path 1) ")")
                                     2 ysplit
                                     (+ width 0) height
                                     #f ;; wrap
                                     #f ;; align top
                                     #f ;; align left
                                     )))))
               (if (not (<gui> :my-draw-text label *text-color* name 5 0 width height #f #f #f 0 #t #t))
                   (show-name-tool-tip)
                   (<gui> :set-tool-tip label "")))
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
                                                      ;;(c-display "         SETTING CURRENT")
                                                      (<ra> :set-current-instrument instrument-id)
                                                      ;;(remake-mixer-strips instrument-id)
                                                      )))))
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
                                                     (<ra> :set-instrument-effect instrument-id bus-effect-name (db-to-radium-normalized (<ra> :gain-to-db gain))))
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
                                                                                :connection-type *send-connection-type*
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
                               is-bus?
                               
                               delete-func
                               replace-func
                               reset-func

                               midi-learn-instrument-id
                               effect-name
                               
                               parentgui
                               )

  (set! parentgui (<gui> :get-parent-window parentgui))
  ;;(c-display (<-> "Effect name: -" effect-name "-"))
  ;;(c-display "ins:" instrument-id)

  (popup-menu (<-> "----------Insert")
              (list (<-> "Insert Plugin" (if effect-name " (after this position)" ""))
                    :enabled (not is-sink?)
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
                            (else
                             ;; after
                             (insert-new-instrument-between instrument-id
                                                            (get-instruments-connecting-from-instrument instrument-id)
                                                            #t
                                                            parentgui
                                                            finished)))
                      ))
              
              (list (<-> "Insert Send"  (if effect-name " (after this position)" ""))
                    :enabled (> (<ra> :get-num-output-channels instrument-id) 0)
                    (lambda ()
                      (let ((instrument-id (cond (is-send?
                                                  parent-instrument-id)
                                                 (else
                                                  instrument-id))))
                        (request-send-instrument instrument-id
                                                 (lambda (create-send-func)
                                                   (create-send-func 0 '())
                                                   (<ra> :set-current-instrument first-instrument-id))))))

              ;;(and (not is-send?)
              ;;     (list
              ;;      "----------Plugin"

              (if is-send?
                  (list "------------Send-slider"
                        (list "Delete"
                              :enabled (and is-send? delete-func)
                              (lambda ()
                                (delete-func)))
                        (list "Replace"
                              :enabled (and is-send? replace-func)
                              (lambda ()
                                (replace-func)))
                        "----------------"
                        (list "Set to 0.0 dB"
                              :enabled (and is-send? reset-func)
                              (lambda ()
                                (reset-func))))
                  (and effect-name
                       (list
                        ;;"------------Plugin-slider"
                        (get-effect-popup-entries midi-learn-instrument-id
                                                  effect-name
                                                  :automation-error-message (if effect-name
                                                                                #f
                                                                                "(Connection gain automation not supported yet)")
                                                  :modulation-error-message (if effect-name
                                                                                #f
                                                                                "(Connection gain modulation not supported yet)")))))
              ;;"----------"
              ;;"Convert to standalone strip" (lambda ()
              ;;                                #t)

                            (list (<-> "------------Connection " (<ra> :get-instrument-name parent-instrument-id) "-->" (<ra> :get-instrument-name instrument-id) "")
                    (let ((connection-type (if (or (not parent-instrument-id)
                                                   (not instrument-id))
                                               *auto-connection-type*
                                               (<ra> :get-audio-connection-type parent-instrument-id instrument-id))))
                      (list
                       :radio-buttons
                       (list (<-> "Display as plugin-slider") ;; " " connection-type " - " is-top-instrument)
                             :enabled (and (not is-top-instrument)
                                           (not is-sink?)
                                           (not is-bus?))
                             :check (and (not is-top-instrument)
                                         (not is-bus?)
                                         (= connection-type *plugin-connection-type*))
                             (lambda (ison)
                               (if ison
                                   (<ra> :set-audio-connection-type parent-instrument-id instrument-id *plugin-connection-type*))))
                       (list "Display as send-slider"
                             :enabled (and (not is-top-instrument)
                                           (not is-sink?)
                                           (not is-bus?))
                             :check (and (not is-top-instrument)
                                         (not is-bus?)
                                         (= connection-type *send-connection-type*))
                             (lambda (ison)
                               (if ison
                                   (<ra> :set-audio-connection-type parent-instrument-id instrument-id *send-connection-type*))))
                       (list (<-> "Auto" (if (not (= connection-type *auto-connection-type*)) "" (if is-send? " (send)" " (plugin)")))
                             :enabled (and (not is-top-instrument)
                                           (not is-sink?)
                                           (not is-bus?))
                             :check (and (not is-top-instrument)
                                         (not is-bus?)
                                         (= connection-type *auto-connection-type*))
                             (lambda (ison)
                               (if ison
                                   (<ra> :set-audio-connection-type parent-instrument-id instrument-id *auto-connection-type*)))))))
              

              (get-instrument-popup-entries instrument-id parentgui :must-have-inputs effect-name :must-have-outputs effect-name)

              (get-global-mixer-strips-popup-entries first-instrument-id strips-config parent-instrument-id)
              )

  (<ra> :set-current-instrument instrument-id)
  )
    

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
                         #f ;; is-send
                         (= 0 (<ra> :get-num-output-channels instrument-id)) ;; is-sinc
                         #f ;; is-bus
                         (if is-permanent? #f delete)
                         (if is-permanent? #f replace)
                         reset
                         #f ;; midi-learn instrument-id
                         #f ;; effect-name
                         gui))

(define (paint-horizontal-instrument-slider widget instrument-id value text is-enabled is-current get-automation-data text-x1 x1 y1 x2 y2 color)
  (paint-horizontal-slider :widget widget
                           :value value
                           :text text
                           :x1 x1
                           :y1 y1
                           :x2 x2
                           :y2 y2
                           :color color
                           :is-enabled is-enabled
                           :is-current is-current
                           :get-automation-data get-automation-data
                           :text-x1 text-x1
                           ))
  
(define (strip-slider first-instrument-id
                      parent-instrument-id
                      instrument-id
                      strips-config
                      is-send?
                      is-sink?
                      is-bus?
                      make-undo
                      get-scaled-value
                      get-value-text
                      set-value
                      get-automation-data
                      delete-func
                      replace-func
                      reset-func
                      midi-learn-instrument-id
                      effect-name)

  (define instrument-name (<ra> :get-instrument-name instrument-id))
  ;;(define widget (<gui> :widget 100 (get-fontheight)))
  (define widget #f)
  (define is-changing-value #f)

  (define (is-enabled?)
    ;;(c-display "INSTRUMENT:" instrument-name)
    (if is-send?
        (<ra> :get-connection-enabled parent-instrument-id instrument-id #f) ;; #f means don"t show error if not connected. That might happen if redrawing before remaking after changing configuration.
        (>= (<ra> :get-instrument-effect instrument-id "System Effects On/Off") 0.5)))
  
  (define (is-grayed?)
    (not (is-enabled?)))
    ;;(if (not (is-enabled?))
    ;;    #t
    ;;    (if is-send? ;; Wrong. We don't gray out a plugins sends. The sends are still working.
    ;;        (< (<ra> :get-stored-instrument-effect parent-instrument-id "System Effects On/Off") 0.5)
    ;;        #f)))
    
    
  (define (toggle-enabled)
    (define is-enabled (is-enabled?))
    (if is-send?
        (begin
          (<ra> :undo-connection-enabled parent-instrument-id instrument-id)
          (<ra> :set-connection-enabled parent-instrument-id instrument-id (not is-enabled) #t)
          (<gui> :update widget)
          )
        (begin
          (<ra> :set-instrument-bypass instrument-id (not (not is-enabled)))
          (<gui> :update widget))))
  ;;(<gui> :update (<gui> :get-parent-gui widget)))))

  (define (get-slider-text value)
    (define midi-learn-text (if (and effect-name
                                     (<ra> :instrument-effect-has-midi-learn midi-learn-instrument-id effect-name))
                                "[M] "
                                ""))
    (<-> midi-learn-text instrument-name ": " (get-value-text value)))
  
  (define (paint-slider x1-on/off width height)
    (define value (get-scaled-value))
    (paint-horizontal-instrument-slider widget
                                        instrument-id
                                        value
                                        (get-slider-text value)
                                        (is-enabled?)
                                        (= (<ra> :get-current-instrument) instrument-id)
                                        get-automation-data
                                        x1-on/off
                                        0 0 width height
                                        (<ra> :get-instrument-color instrument-id)
                                        ))
  
  (define fontheight (get-fontheight))
  
  (define (get-on/off-box x1-on/off width height kont)
    ;;(define b1 10)
    (define b1 (scale 1 0 3 0 x1-on/off))
    (define y (/ height 2))
    (define x (/ x1-on/off 2))
    (kont (- x b1)
          (- y b1)
          (+ x b1)
          (+ y b1)))

  (define (paint-on/off x1-on/off width height)
    (get-on/off-box x1-on/off width height
                    (lambda (x1 y1 x2 y2)    
                      (define b2 (scale 1 0 6 0 x1-on/off))
                      (define x1_i (+ x1 b2))
                      (define y1_i (+ y1 b2))
                      (define x2_i (- x2 b2))
                      (define y2_i (- y2 b2))
    
                      ;;(c-display x1-on/off b1 height "-" (* 1.0 x1) (* 1.0 y1) (* 1.0 x2) (* 1.0 y2))
                      ;;(c-display x1_i y2_i x2_i y2_i "\n\n")
                      
                      (cond ((is-enabled?)
                             (<gui> :filled-ellipse widget "black" x1 y1 x2 y2)
                             (<gui> :filled-ellipse widget "#22aa22" x1_i y1_i x2_i y2_i))
                            (else
                             (<gui> :filled-ellipse widget "#000000" x1 y1 x2 y2)
                             (<gui> :filled-ellipse widget "#003f00" x1_i y1_i x2_i y2_i)))
                      
                      ;; border
                      (<gui> :draw-ellipse widget "#88111111" x1 y1 x2 y2 2.0))))

  (define (get-x1-on/off)
    fontheight)
  
  (define (paintit width height)
    (define x1 (get-x1-on/off))
    (paint-slider x1 width height)
    (paint-on/off x1 width height))
  
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

  (define (in-enabled-box? x y)
    (get-on/off-box (get-x1-on/off) (<gui> :width widget) (<gui> :height widget)
                    (lambda (x1 y1 x2 y2)    
                      (and (>= x x1)
                           (>= y y1)
                           (< x x2)
                           (< y y2)))))

  (define has-made-undo #t)
  
  (add-safe-mouse-callback widget (lambda (button state x y)
                                    ;;(c-display "state:" state)
                                    (if (= state *is-pressing*)
                                        (<ra> :set-current-instrument instrument-id))
                                    (define is-left-pressing (and (= button *left-button*)
                                                                  (= state *is-pressing*)))

                                    (if (in-enabled-box? x y)
                                        (begin
                                          (if is-left-pressing
                                              (toggle-enabled))
                                          #t)
                                        (begin
                                          (when is-left-pressing
                                            (set! is-changing-value #t)
                                            (set! has-made-undo #f)
                                            )
                                          (when (and (not has-made-undo)
                                                     (= button *left-button*)
                                                     (= state *is-moving*))                                                     
                                            (make-undo)
                                            (set! has-made-undo #t))
                                          (when (= state *is-releasing*)
                                            (set! is-changing-value #f)
                                            (<gui> :tool-tip "")
                                            ;;(c-display "finished")
                                            )
                                          (let ((status-text (get-slider-text (get-scaled-value))))
                                            (if is-changing-value
                                                (set-tooltip-and-statusbar status-text)
                                                (<ra> :set-statusbar-text status-text)
                                                ))
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
                                                                       is-bus?
                                                                       delete-func
                                                                       replace-func
                                                                       (lambda ()
                                                                         (make-undo)
                                                                         (reset-func))
                                                                       midi-learn-instrument-id
                                                                       effect-name
                                                                       widget)))
                                          #f))))
  
  (add-safe-double-click-callback widget (lambda (button x y)
                                           (when (= button *left-button*)
                                             (if (in-enabled-box? x y)
                                                 (toggle-enabled)
                                                 (begin
                                                   (c-display " Double clicking" button)
                                                   ;;(<ra> :cancel-last-undo) ;; Undo the added undo made at th mouse callback above. (now we wait until slider is moved before making undo)
                                                   (<ra> :show-instrument-gui instrument-id widget (<ra> :show-instrument-widget-when-double-clicking-sound-object))
                                                   )))))

  ;;(paintit (<gui> :width widget)
  ;;         (<gui> :height widget))

  (<gui> :set-size-policy widget #t #t)
  
  (if (not is-send?)
      (add-gui-effect-monitor widget instrument-id "System Effects On/Off" #t #t
                              (lambda (on/off automation)
                                (<gui> :update widget))))
  
  widget)



(define (create-mixer-strip-plugin gui first-instrument-id parent-instrument-id instrument-id strips-config)
  (define (get-drywet)
    (<ra> :get-stored-instrument-effect instrument-id "System Dry/Wet"))
  
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
                                                                        :connection-type (<ra> :get-audio-connection-type instrument-id child-id)
                                                                        :gain child-gain)))
                         child-ids
                         child-gains)
               
               (<ra> :undo-mixer-connections)
               
               (<ra> :change-audio-connections changes) ;; Apply all changes simultaneously

               (if (= 0 (<ra> :get-num-in-audio-connections instrument-id))
                   (<ra> :delete-instrument instrument-id))
               
               ;;(remake-mixer-strips) ;; (makes very little difference in snappiness, and it also causes mixer strips to be remade twice)
               ))
            
            #f)))

  (define (das-replace-instrument)
    (async-replace-instrument instrument-id "" (make-instrument-conf :must-have-inputs #t :must-have-outputs #t :parentgui gui)))

  (define (reset)    
    (<ra> :set-instrument-effect instrument-id "System Dry/Wet" 1))

  (define automation-value #f)
  (define automation-color (<ra> :get-instrument-effect-color instrument-id "System Dry/Wet"))
  (define (get-automation-data kont)
    (if automation-value
        (kont automation-value automation-color)))
  
  (define doit #t)
  
  (define slider (strip-slider first-instrument-id
                               parent-instrument-id
                               instrument-id
                               strips-config
                               #f #f #f
                               (lambda ()
                                 (<ra> :undo-instrument-effect instrument-id "System Dry/Wet"))
                               get-drywet
                               (lambda (scaled-value)
                                 (<-> (round (* 100 scaled-value)) "%"))
                               (lambda (new-scaled-value)
                                 (if (and doit (not (= new-scaled-value (get-drywet))))                                     
                                     (<ra> :set-instrument-effect instrument-id "System Dry/Wet" new-scaled-value)))
                               get-automation-data
                               delete-instrument
                               das-replace-instrument
                               reset
                               instrument-id
                               "System Dry/Wet"
                               ))

  (add-gui-effect-monitor slider instrument-id "System Dry/Wet" #t #t
                          (lambda (drywet automation)
                            (when drywet
                              ;;(c-display "DRYWET:" drywet)
                              (set! doit #f)
                              (<gui> :set-value slider drywet)
                              (set! doit #t))
                            (when automation
                              ;;(c-display "AUTOMATION:" automation)
                              (set! automation-value (if (< automation 0)
                                                         #f
                                                         automation))
                              (<gui> :update slider))))
  
  (<gui> :add gui slider))


(define (get-mixer-strip-send-horiz gui)
  (define horiz (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing horiz 1 1 0 0 0)

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
                                                  (<gui> :my-draw-text gui *text-color* *arrow-text* 0 0 width height #f))
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
    (<ra> :set-instrument-effect instrument-id "System In" (db-to-radium-normalized db)))
  
  (define (reset)
    (set-db-value 0))

  (define (get-db-value)
    (radium-normalized-to-db (<ra> :get-stored-instrument-effect instrument-id "System In")))

  (define last-value (get-db-value))

  (define automation-value #f)
  (define automation-color (<ra> :get-instrument-effect-color instrument-id "System In"))
  (define (get-automation-data kont)
    (if automation-value
        (if (> automation-value 1.0)
            (kont 1.0 automation-color)
            (kont automation-value automation-color))))
  
  (define doit #t)
  (define slider (strip-slider first-instrument-id
                               parent-instrument-id
                               instrument-id
                               strips-config
                               #t #t #f

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

                               get-automation-data
                               
                               delete
                               replace
                               reset

                               instrument-id ;; midi-learn-instrument-id
                               "System In"
                               ))
                                     

  (add-gui-effect-monitor slider instrument-id "System In" #t #t
                          (lambda (system-in automation)
                            (when system-in
                              (define new-value (radium-normalized-to-slider system-in))
                              (when (not (= new-value (<gui> :get-value slider)))
                                (set! doit #f)
                                (<gui> :set-value slider new-value)
                                (set! doit #t)))
                            (when automation
                              (set! automation-value (if (< automation 0)
                                                         #f
                                                         (radium-normalized-to-slider automation)))
                              (<gui> :update slider))))


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
                                 automation-color
                                 delete
                                 replace
                                 midi-learn-instrument-id
                                 effect-name
                                 is-bus)

  (define horiz (get-mixer-strip-send-horiz gui))

  (define (reset)
    (set-db-value 0)
    (<ra> :redraw-mixer-strips))

  (define automation-value #f)
  (define (get-automation-data kont)
    (if automation-value
        (kont (min 1.0 automation-value) automation-color)))

  (define doit #t)

  (define last-value (get-db-value))

  (define slider (strip-slider first-instrument-id
                               parent-instrument-id
                               target-instrument-id
                               strips-config
                               #t #f is-bus ;; is-send is-sink is-bus
                               
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

                               get-automation-data
                               
                               delete
                               replace
                               reset

                               midi-learn-instrument-id
                               effect-name
                               ))
  
  (if add-monitor
      (add-monitor slider
                   (lambda (new-db automation-normalized)
                     (when new-db
                       (define new-slider-value (db-to-slider new-db))
                       (when (not (= new-slider-value (<gui> :get-value slider)))
                         (set! doit #f)
                         (<gui> :set-value slider new-slider-value)
                         (<gui> :update slider)
                         (set! doit #t)))
                     (when automation-normalized
                       (set! automation-value (if (< automation-normalized 0)
                                                  #f
                                                  (radium-normalized-to-slider automation-normalized)))
                       (<gui> :update slider)))))
  
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
    (radium-normalized-to-db (<ra> :get-stored-instrument-effect instrument-id effect-name)))

  (define (set-db-value db)
    (<ra> :set-instrument-effect instrument-id effect-name (db-to-radium-normalized db)))
  
  (define (add-monitor slider callback)
    (add-gui-effect-monitor slider instrument-id effect-name #t #t
                            (lambda (radium-normalized automation)
                              (callback (and radium-normalized (radium-normalized-to-db radium-normalized))
                                        automation
                                        ))))
  
  (set! send-gui (create-mixer-strip-send gui
                                          first-instrument-id
                                          instrument-id
                                          bus-id
                                          strips-config
                                          make-undo
                                          get-db-value
                                          set-db-value
                                          add-monitor
                                          (<ra> :get-instrument-effect-color instrument-id effect-name)
                                          delete
                                          replace
                                          first-instrument-id ;; midi-learn-instrument-id
                                          effect-name
                                          #t))
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
    ;;(c-display "setting db to" db (<ra> :db-to-gain db))

    (<ra> :set-audio-connection-gain source-id target-id (<ra> :db-to-gain db) #f) ;; 2018-12-05: Changed the last argument for set-audio-connection-gain to #f. Seems like the *send-callbacks* updates all that's necessary.

    (for-each (lambda (send-callback)
                (send-callback gui source-id target-id db))
              *send-callbacks*)
    )
  
  (define (add-monitor slider callback)
    (define send-callback
      (lambda (maybe-gui maybe-source-id maybe-target-id db)
        (if (and (not (= gui maybe-gui))
                 (= maybe-source-id source-id)
                 (= maybe-target-id target-id)
                 (<ra> :instrument-is-open-and-audio source-id)
                 (<ra> :instrument-is-open-and-audio target-id)
                 (<ra> :has-audio-connection source-id target-id))
            (callback db #f)))) ;; #f = automation value (automating audio connection gain is not supported yet)
  
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
  ;                                           (<ra> :instrument-is-open-and-audio source-id)
  ;                                           (<ra> :instrument-is-open-and-audio target-id))                                             
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
                                          "white" ;; not used (automation color)
                                          delete
                                          replace
                                          #f ;; midi-learn-instrument-id
                                          #f ;; automation not supported
                                          #f ;; is bus
                                          ))
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
  ;;(c-display "Create path: " (<ra> :get-instrument-name instrument-id))
  
  (define (create-sink dasid)
    (create-mixer-strip-sink-plugin gui
                                    first-instrument-id
                                    instrument-id
                                    dasid
                                    strips-config))
  
  (get-mixer-strip-path-instruments instrument-id
                                    (lambda (bus-sends instrument-sends next-plugin-instrument)
                                      ;;(c-display "    next-plugin-instrument:" (and next-plugin-instrument (<ra> :get-instrument-name next-plugin-instrument)))
                                      ;;(c-display "    instrument-sends:" (map ra:get-instrument-name instrument-sends))
                                      (for-each (lambda (bus-num)
                                                  (create-mixer-strip-bus-send gui
                                                                               instrument-id
                                                                               instrument-id
                                                                               strips-config
                                                                               bus-num))
                                                bus-sends)
                                      
                                      (for-each (lambda (out-instrument)
                                                  (if (= 0 (<ra> :get-num-output-channels out-instrument))
                                                      (create-sink out-instrument)
                                                      (create-mixer-strip-audio-connection-send gui
                                                                                                first-instrument-id
                                                                                                instrument-id
                                                                                                out-instrument
                                                                                                strips-config)))
                                                instrument-sends)

                                      (if next-plugin-instrument
                                          (begin
                                            (if (= 0 (<ra> :get-num-output-channels next-plugin-instrument))
                                                (create-sink next-plugin-instrument)
                                                (create-mixer-strip-plugin gui first-instrument-id instrument-id next-plugin-instrument strips-config))
                                            (create-mixer-strip-path gui first-instrument-id next-plugin-instrument strips-config))
                                          instrument-id))))

#!!

!!#

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

  (define (get-pan-slider-value normalized-value)
    (floor (scale normalized-value
                  0 1
                  -90 90)))
    
  (define (get-pan)
    (get-pan-slider-value (<ra> :get-stored-instrument-effect instrument-id "System Pan")))

  
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

  (define automation-slider-value -100)
  (define pan-automation-color (<ra> :get-instrument-effect-color instrument-id "System Pan"))
                                 

  (set! paint
        (lambda ()
          (define width (<gui> :width slider))
          (define value (get-pan))
          (define is-on (pan-enabled? instrument-id))
          ;;(<gui> :filled-box slider system-background-color 5 5 width height)
          (define background (if is-on
                                 (<gui> :mix-colors background-color "black" 0.39)
                                 (<gui> :mix-colors background-color "white" 0.95)))
          (<gui> :filled-box slider background 0 0 width height 2 2 #f)
          (define col1 (<gui> :mix-colors "white" background 0.4))
          (define col2 (<gui> :mix-colors "#010101" background 0.5))

          (define inner-width/2 (scale 1 0 18 0 (get-fontheight)))
          (define outer-width/2 (* inner-width/2 2))

          (define middle (scale value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))

          (<gui> :filled-box slider col1 (- middle inner-width/2) 2 (+ middle inner-width/2) (- height 3) -1 -1 #f)
          (<gui> :filled-box slider col2 (- middle inner-width/2 outer-width/2) 2 (- middle inner-width/2) (- height 3) -1 -1 #f)
          (<gui> :filled-box slider col2 (+ middle inner-width/2) 2 (+ middle inner-width/2 outer-width/2) (- height 3) -1 -1 #f)
          ;;(<gui> :my-draw-text slider "white" (<-> value "o") 0 0 width height #t)

          (when (> automation-slider-value -100)
            (define middle (scale automation-slider-value -90 90 (+ inner-width/2 outer-width/2) (- width (+ inner-width/2 outer-width/2))))
            (<gui> :draw-line slider pan-automation-color middle 2 middle (- height 3) 2.0))
          
          (<gui> :draw-box slider "#404040" 0 0 width height 1.5 2 2)

          (when (<ra> :instrument-effect-has-midi-learn instrument-id "System Pan")
            (define midi-learn-color (<gui> :mix-colors *text-color* background 0.2))
            (<gui> :my-draw-text slider midi-learn-color "[M]" 2 2 (- width 2) (- height 2)
                   #f ;; wrap text
                   #f ;; align left
                   #f ;; align top
                   0 ;; rotate
                   #f ;; cut text to fit
                   #t ;; scale font size
                   ))
          
          ))

  (add-safe-paint-callback slider (lambda x (paint)))

  ;;(paint)

  (add-gui-effect-monitor slider instrument-id "System Pan" #t #t
                          (lambda (normalized-value automation)
                            (when normalized-value
                              (set! doit #f)
                              (<gui> :set-value slider (get-pan-slider-value normalized-value))
                              ;;(<gui> :update slider)
                              (set! doit #t))
                            (when automation
                              (if (< automation 0)
                                  (set! automation-slider-value -100)
                                  (set! automation-slider-value (get-pan-slider-value automation)))
                              (<gui> :update slider))))
  
  (add-gui-effect-monitor slider instrument-id "System Pan On/Off" #t #t
                          (lambda (on/off automation)
                            (<gui> :update slider)))

  (define has-made-undo #t)

  (define (enable! onoff)
    (pan-enable! instrument-id onoff))
    
  (add-safe-mouse-callback
   slider
   (lambda (button state x y)
     (<ra> :set-statusbar-text (<-> "Pan: " (get-pan)))
     (cond ((and (= button *left-button*)
                 (= state *is-pressing*))
            (set! has-made-undo #f)
            #f)
           ((and (not has-made-undo)
                       (= button *left-button*)
                       (= state *is-moving*))
            (undo-block
             (lambda ()
               (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
               (<ra> :undo-instrument-effect instrument-id "System Pan")))
            (set! has-made-undo #t)
            #f)
           ((and (= button *right-button*)
                 (= state *is-releasing*))
            (define pan-enabled (pan-enabled? instrument-id))
            (if (<ra> :shift-pressed)
                (undo-block
                 (lambda ()                  
                   (<ra> :reset-instrument-effect instrument-id "System Pan On/Off")
                   (<ra> :reset-instrument-effect instrument-id "System Pan")))
                (popup-menu ;(list "Reset" (lambda ()
                                        ;                (<ra> :undo-instrument-effect instrument-id "System Pan")
                                        ;                (<ra> :set-instrument-effect instrument-id "System Pan" 0.5)))
                 (list "Pan Enabled"
                       :check pan-enabled
                       enable!)                              
                 (get-effect-popup-entries instrument-id "System Pan"
                                           :pre-undo-block-callback (lambda ()
                                                                      (enable! #t)))
                 (get-instrument-popup-entries instrument-id slider)
                 (get-global-mixer-strips-popup-entries instrument-id strips-config)))
            #t)
           (else
            #f))))
  
  slider)

;;(define (create-mixer-strip-checkbox text sel-color unsel-color width height callback)
;;  (define button (<gui> :widget width height))

(delafina (draw-mutesolo :gui
                         :type
                         :instrument-id
                         :x1 :y1 :x2 :y2
                         :is-selected 'undefined
                         :use-single-letters 
                         :background-color #f
                         :border 0
                         :implicit-border 1)

  (define (get-muted)
    (define volume-on-off-name (get-instrument-volume-on/off-effect-name instrument-id))
    (< (<ra> :get-instrument-effect instrument-id volume-on-off-name) 0.5))
  (define (get-soloed)
    (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
  (define (get-recording)
    ;;(<ra> :seqtrack-is-recording seqtracknum)) We don't have seqtracknum here.
    (if (not (<ra> :release-mode))
        (assert #f))
    #f)
  
  (if (eq? is-selected 'undefined)
      (set! is-selected (cond ((eq? type 'height)
                               (if (not (<ra> :release-mode))
                                   (assert #f)))
                              ((eq? type 'record)
                               (get-recording))
                              ((eq? type 'solo)
                               (get-soloed))
                              ((eq? type 'mute)
                               (get-muted))
                              (else
                               (assert #f)))))
  
  (define text (cond ((eq? type 'height)
                      (if use-single-letters
                          "H"
                          "Height"))
                     ((eq? type 'record)
                      (if use-single-letters
                          "R"
                          "Record"))                      
                     ((eq? type 'mute)
                      (if use-single-letters
                          "M"
                          "Mute"))
                     ((eq? type 'solo)
                      (if use-single-letters
                          "S"
                          "Solo"))
                     (else
                      (assert #f))))
  
  (define color (cond ((eq? type 'height)
                       "blue")
                      ((eq? type 'record)
                       "red")
                      ((eq? type 'mute)
                       "green")
                      ((eq? type 'solo)
                       "yellow")
                      (else
                       (assert #f))))

  (define is-implicitly-muted (and (eq? type 'mute)
                                   instrument-id
                                   (>= instrument-id 0)
                                   (<ra> :instrument-is-implicitly-muted instrument-id)))

  ;;(c-display "background-color:" background-color)
  (draw-checkbox gui text is-selected x1 y1 x2 y2 color
                 :x-border border
                 :y-border border
                 :background-color background-color                 
                 :paint-implicit-border is-implicitly-muted
                 :implicit-border-width implicit-border
                 :box-rounding 2)
  )

(delafina (create-mixer-strip-mutesolo :instrument-id 
                                       :strips-config 
                                       :background-color 
                                       :min-height 
                                       :use-single-letters 
                                       :stack-horizontally 
                                       :set-fixed-size #t)
  
  (define volume-on-off-name (get-instrument-volume-on/off-effect-name instrument-id))

  (define (get-muted)
    (< (<ra> :get-instrument-effect instrument-id volume-on-off-name) 0.5))
  (define (get-soloed)
    (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
           
  (define (turn-off-all-mute except)
    (for-each (lambda (instrument-id)
                (when (and (not (= instrument-id except))
                           (< (<ra> :get-instrument-effect instrument-id volume-on-off-name) 0.5))
                  (<ra> :undo-instrument-effect instrument-id volume-on-off-name)
                  (<ra> :set-instrument-effect instrument-id volume-on-off-name 1)
                  ))
              (get-all-audio-instruments)))
  
  (define (turn-off-all-solo except)
    (for-each (lambda (instrument-id)
                (when (and (not (= instrument-id except))
                           (>= (<ra> :get-instrument-effect instrument-id "System Solo On/Off") 0.5))
                  ;;(<ra> :undo-instrument-effect instrument-id "System Solo On/Off")
                  (set-instrument-solo! instrument-id #f)
                  ))
              (get-all-audio-instruments)))

  
  (define (draw-mute mute is-muted width height)
    (draw-mutesolo mute
                   'mute
                   instrument-id
                   0 0 width height
                   is-muted 
                   use-single-letters
                   background-color))

  (define implicitly-muted (<ra> :instrument-is-implicitly-muted instrument-id))
  
  (define mute (create-instrument-effect-checkbox instrument-id strips-config
                                                  draw-mute
                                                  (lambda (is-muted)
                                                    (undo-block
                                                     (lambda ()
                                                       (<ra> :set-instrument-mute instrument-id is-muted)
                                                       ;;(c-display "mute: " is-muted)
                                                       (if (<ra> :control-pressed)
                                                           (turn-off-all-mute instrument-id))
                                                       )))
                                                  (get-muted)
                                                  min-height
                                                  :effect-name "System Volume On/Off"
                                                  :hovered-callback (lambda (button state x y)
                                                                      (<ra> :set-statusbar-text (<-> (<ra> :get-instrument-name instrument-id) ": "
                                                                                                     (cond ((get-muted)
                                                                                                            "Muted")
                                                                                                           (implicitly-muted
                                                                                                            "Implicitly muted")
                                                                                                           (else
                                                                                                            "Not muted")))))
                                                  ))

  ;;(add-safe-mouse-callback (cadr mute)
  ;;                         (lambda (button state x y)
  ;;                           (c-display (<->"gakk" x y))))

  ;; Keep "implicitly-muted" up-to-date.
  (<ra> :schedule (random 1000) (let ((mute (cadr mute)))
                                  (lambda ()
                                    (if (and (<gui> :is-open mute) (<ra> :instrument-is-open-and-audio instrument-id))
                                        (let ((last-implicitly-muted implicitly-muted))
                                          (set! implicitly-muted (<ra> :instrument-is-implicitly-muted instrument-id))
                                          (when (not (eq? implicitly-muted last-implicitly-muted))
                                            ;;(draw-mute mute (get-muted) (<gui> :width mute) (<gui> :height mute))
                                            (<gui> :update mute)
                                            )
                                          100)
                                        #f))))
  
  (define solo (create-instrument-effect-checkbox instrument-id strips-config
                                                  (lambda (solo is-soloed width height)
                                                    (draw-mutesolo solo 
                                                                   'solo
                                                                   instrument-id
                                                                   0 0 width height
                                                                   is-soloed
                                                                   use-single-letters
                                                                   background-color))
                                                  (lambda (is-selected)
                                                    (undo-block
                                                     (lambda ()
                                                       ;;(<ra> :undo-instrument-effect instrument-id "System Solo On/Off")
                                        ;(<ra> :set-instrument-effect instrument-id "System Solo On/Off" (if is-selected 1.0 0.0))
                                                       (<ra> :set-instrument-solo instrument-id is-selected)
                                                       (if (<ra> :control-pressed)
                                                           (turn-off-all-solo instrument-id))
                                                       )))
                                                  (get-soloed)
                                                  min-height
                                                  :effect-name "System Solo On/Off"
                                                  :hovered-callback (lambda (button state x y)
                                                                      (<ra> :set-statusbar-text (<-> (<ra> :get-instrument-name instrument-id) ": " (if (get-soloed) "Solo" "Not solo"))))
                                                  ))
  
  (add-gui-effect-monitor (cadr mute) instrument-id volume-on-off-name #t #t
                          (lambda (on/off automation)
                            ((car mute) (get-muted))))
  
  (add-gui-effect-monitor (cadr solo) instrument-id "System Solo On/Off" #t #t
                          (lambda (on/off automation)
                            ;;(c-display "Solo changed for" instrument-id)
                            ((car solo) (get-soloed))))
  
  (define gui (if stack-horizontally
                  (<gui> :horizontal-layout)
                  (<gui> :vertical-layout)))
  (<gui> :set-layout-spacing gui 0 0 0 0 0)
  ;;(<gui> :set-background-color gui "yellow")
  
  (when set-fixed-size
    (assert min-height)
    (if (not stack-horizontally)
        (begin
          (set-fixed-height (cadr mute) min-height)
          (set-fixed-height (cadr solo) min-height))
        (set-fixed-height gui min-height)))

  (<gui> :add gui (cadr mute) 1)
  (<gui> :add gui (cadr solo) 1)

  gui
  )

(define (create-mixer-strip-volume instrument-id meter-instrument-id strips-config background-color is-minimized)
  (define fontheight (get-fontheight))
  (define voltext-height (ceiling (* *pan-mutesolo-voltext-scale-factor* fontheight)))

  (define horizontal-spacing 4) ;; must be an even number.
  (define horizontal-spacing/2 (/ horizontal-spacing 2))
  
  (define show-voltext (not is-minimized))
  (define show-peaktext #t)

  (define peaktext-is-red #f)
  (define peaktexttext "-inf")

  (define is-sink? (= 0 (<ra> :get-num-output-channels instrument-id)))

  (define effect-name (if is-sink?
                          "System In"
                          "System Volume"))
  
  (define (get-volume)
    (radium-normalized-to-db (<ra> :get-stored-instrument-effect instrument-id effect-name)))

  (define doit #f)

  (define paint-voltext #f)
  (define paint-peaktext #f)

  (define voltext (and show-voltext (<gui> :widget)))
  (define peaktext (and show-peaktext (<gui> :widget)))

  (define paint-slider #f)

  (define last-vol-slider-db-value (get-volume))
  (define volslider (<gui> :vertical-slider
                           ""
                           0 (db-to-slider (get-volume)) 1
                           (lambda (val)
                             (define db (slider-to-db val))
                             (when (and doit (not (= last-vol-slider-db-value db)))
                               (set! last-vol-slider-db-value db)
                               ;;(c-display "             hepp hepp")
                               ;;(c-display "Setting new volume to" (db-to-radium-normalized db))
                               (<ra> :set-instrument-effect instrument-id effect-name (db-to-radium-normalized db))
                               (if paint-voltext
                                   (<gui> :update voltext)
                                   )))))

  (<gui> :set-size-policy volslider #t #t)
  ;;(<gui> :set-min-width volslider 1) ;; ?? Why is this necessary?
  (<gui> :set-min-height volslider (* (get-fontheight) 2)) ;; This is strange. If we don't do this, min-height will be set to approx something that looks very good, but I don't find the call doing that.

  (define (paint-text gui text cut-text-to-fit red-background)
    (define width (<gui> :width gui))
    (define height (<gui> :height gui))
    
    (define col1 (<gui> :mix-colors (if red-background "red" "#010101") background-color 0.7))
    
    ;; background
    ;;(<gui> :filled-box gui (if red-background "red" background-color) 0 0 width height -1 -1 #f)
    
    ;; rounded
    (if is-minimized
        (<gui> :filled-box gui col1 0 0 width height 0 0)
        (<gui> :filled-box gui col1 0 0 width height 5 5))
    
    ;; text
    (<gui> :my-draw-text gui *text-color* text 0 0 (- width 0) (- height 0)
           #f ;;wrap-lines
           #f ;;align-top
           #f ;;align-left
           0  ;; rotate
           cut-text-to-fit ;; cut-text-to-fit
           #t)) ;; scale-font-size

    
  (when show-voltext
    (set! paint-voltext
          (lambda ()
            (paint-text voltext (db-to-text (get-volume) #f) #f #f)))
    
    (add-safe-paint-callback voltext (lambda x (paint-voltext)))
    ;;(paint-voltext)
    )

  (when show-peaktext
    (set! paint-peaktext
          (lambda ()
            (paint-text peaktext peaktexttext #f peaktext-is-red)))
    
    (add-safe-paint-callback peaktext (lambda x (paint-peaktext)))
    ;;(paint-peaktext)
    )

  (define volslider-rounding (if is-minimized 0 2))

  (define automation-radium-normalized-volume -10)
  (define automation-slider-value -10)

  (define volume-automation-color (<ra> :get-instrument-effect-color instrument-id effect-name))
                                 
  (set! paint-slider
        (lambda ()
          (define volslider-width (<gui> :width volslider))
          (define mid (floor (/ volslider-width 2)))
          (define spacing (if is-minimized
                              0
                              horizontal-spacing/2))
          (define width volslider-width) ;;(- mid spacing))
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

          ;; Automation value
          (if (> automation-slider-value 0)
              (let ((automation-y (max 0 (scale automation-slider-value 0 1 height 0))))
                (<gui> :draw-line volslider volume-automation-color x1 automation-y x2 automation-y 2.0)))

          (when (<ra> :instrument-effect-has-midi-learn instrument-id effect-name)
            (define midi-learn-color (<gui> :mix-colors *text-color* background-color 0.2))
            (<gui> :my-draw-text volslider midi-learn-color "[M]" 2 2 (- width 2) (- height 2)
                   #f ;; wrap text
                   #f ;; align left
                   #f ;; align top
                   0 ;; rotate
                   #f ;; cut text to fit
                   #t ;; scale font size
                   ))
          ))

  (add-safe-paint-callback volslider (lambda x (paint-slider)))

  (define has-inputs-or-outputs (or (> (<ra> :get-num-input-channels instrument-id) 0)
                                    (> (<ra> :get-num-output-channels instrument-id) 0)))
  (define volmeter (if has-inputs-or-outputs
                       (<gui> :vertical-audio-meter meter-instrument-id instrument-id)
                       (<gui> :widget)))

  (<gui> :set-size-policy volmeter #t #t)
  
  (add-gui-effect-monitor volslider instrument-id effect-name #t #t
                          (lambda (radium-normalized-volume radium-normalized-automation-volume)
                            (when radium-normalized-volume
                              (set! doit #f)
                              (<gui> :set-value volslider (radium-normalized-to-slider radium-normalized-volume))
                              ;;(<gui> :set-value voltext (get-volume))
                              (if paint-voltext
                                  (<gui> :update voltext))
                              ;;(<gui> :update volslider)
                              (set! doit #t))
                            (when radium-normalized-automation-volume
                              (set! automation-radium-normalized-volume radium-normalized-automation-volume)
                              (if (< radium-normalized-automation-volume 0)
                                  (set! automation-slider-value -10)
                                  (set! automation-slider-value (radium-normalized-to-slider radium-normalized-automation-volume)))
                              ;;(c-display "got automation value" radium-normalized-automation-volume automation-slider-value)
                              (<gui> :update volslider))))

  (define has-made-undo #t)

  (define (create-vol-mouse-callback is-slider)    
    (lambda (button state x y)
      (if is-slider
          (<ra> :set-statusbar-text (<-> (<ra> :get-instrument-name instrument-id) ": " (db-to-text (get-volume) #t))))
      ;;(<ra> :set-statusbar-text (<-> "volume:" (db-to-text (get-volume) #t))))
      (cond ((and (= button *left-button*)
                  (= state *is-pressing*))
             (set! has-made-undo #f))
            ((and (not has-made-undo)
                  (= button *left-button*)
                  (= state *is-moving*))
             (<ra> :undo-instrument-effect instrument-id effect-name)
             (set! has-made-undo #t))
            ((and (= button *right-button*)
                  (= state *is-pressing*))
             (cond ((and is-slider
                         effect-name
                         (<ra> :shift-pressed))
                    (<ra> :reset-instrument-effect instrument-id effect-name))
                   ((and (not is-slider)
                         (<ra> :shift-pressed))
                    (reset-peak!))
                   (else
                    (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config :effect-name effect-name))))))
      #f))
  
  (add-safe-mouse-callback volslider (create-vol-mouse-callback #t))
  (add-safe-mouse-callback volmeter (create-vol-mouse-callback #f))


  ;;(<gui> :add volslider volmeter 0 0 5 5) ;; Set parent of volmeter to volslider (gui_setParent should be renamed to something else, this is awkward)
  
  '(add-safe-resize-callback volslider (lambda (width height) 
                                        (define mid (floor (/ width 2)))
                                        (define spacing (if is-minimized
                                                            0
                                                            horizontal-spacing/2))
                                        (define volmeter-x1 (+ mid spacing))
                                        (<gui> :set-pos volmeter volmeter-x1 0)
                                        (<gui> :set-size volmeter (- width volmeter-x1) height)))

  (when show-voltext
    (add-safe-mouse-callback voltext
                             (lambda (button state x y)
                               (<ra> :set-statusbar-text "Click to set new volume.") ;;(<-> (<ra> :get-instrument-name instrument-id) ": " (db-to-text (get-volume) #t) ". Click to set new value"))
                               (cond ((and (= button *right-button*)
                                           (= state *is-pressing*))
                                      (if (and effect-name
                                               (<ra> :shift-pressed))
                                          (<ra> :reset-instrument-effect instrument-id effect-name)
                                          (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config :effect-name effect-name)))
                                      #t)
                                     ((and (= button *left-button*)
                                           (= state *is-pressing*))
                                      (define old-volume (let ((v (two-decimal-string (get-volume))))
                                                           (if (or (string=? v "-0.0")
                                                                   (string=? v "-0.00"))
                                                               "0.00"
                                                               v)))
                                      (let ((maybe (<gui> :requester-operations
                                                          (<-> "Set new volume for "
                                                               (<ra> :get-instrument-name instrument-id)
                                                               "\n[" (one-decimal-string *min-db*) " -> " (one-decimal-string *max-db*) "]")
                                                          (lambda ()
                                                            (<ra> :request-float "dB: " *min-db* *max-db* #f old-volume)))))
                                        (when (>= maybe *min-db*)
                                          (<ra> :undo-instrument-effect instrument-id effect-name)
                                          (<ra> :set-instrument-effect instrument-id effect-name (db-to-radium-normalized maybe))))
                                      #t)
                                     (else
                                      #f))))
    )

  (when (and show-peaktext has-inputs-or-outputs)

    (<gui> :add-audio-meter-peak-callback volmeter (lambda (db)
                                                     (set! peaktext-is-red (>= db 4))
                                                     (set! peaktexttext (db-to-text db #f))
                                                     (<gui> :update peaktext)))

    (define (reset-peak!)
      (set! peaktexttext "-inf")
      (<gui> :reset-audio-meter-peak volmeter)
      (<gui> :update peaktext))
    
    (add-safe-mouse-callback peaktext
                             (lambda (button state x y)
                               (<ra> :set-statusbar-text "Click to reset peak.") ;;(<-> (<ra> :get-instrument-name instrument-id) ": " (db-to-text (get-volume) #t) ". Click to set new value"))
                               (cond ((and (= button *right-button*)
                                           (= state *is-pressing*))
                                      (if (<ra> :shift-pressed)
                                          (reset-peak!)
                                          (popup-menu (get-global-mixer-strips-popup-entries instrument-id strips-config :effect-name effect-name)))
                                      #t)
                                     ((and (= button *left-button*)
                                           (= state *is-pressing*))
                                      (reset-peak!)
                                      #t)
                                     (else
                                      #f)))))
  
  #||
  ;; disable this code since graphics isn't different when disabled.
  (when (not has-inputs-or-outputs)
    (<gui> :set-enabled volmeter #f)
    (<gui> :set-enabled voltext #f)
    (<gui> :set-enabled volslider #f)
    (<gui> :set-enabled peaktext #f))
  ||#
    
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
  (<gui> :set-layout-spacing vertical 1 0 1 0 1)

  (<gui> :add vertical horizontal1)

  (define horizontal2 (<gui> :horizontal-layout))  
  (<gui> :add horizontal2 volslider)
  (<gui> :add horizontal2 volmeter)

  (<gui> :add vertical horizontal2)

  (set! doit #t)

  vertical
  )

(define (get-mixer-strip-background-color gui instrument-id)
  (<gui> :mix-colors
         (<ra> :get-instrument-color instrument-id)
         (<gui> :get-background-color gui)
         0.3))

(define (get-instrument-background-color gui instrument-id)
  (if (>= instrument-id 0)
      (get-mixer-strip-background-color gui instrument-id)
      "#666660"))

(define (paint-instrument-background-color gui x1 y1 x2 y2 instrument-id)
  (define background-color (get-instrument-background-color gui instrument-id))
  (<gui> :filled-box gui background-color x1 y1 x2 y2))


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
  (<gui> :set-layout-spacing gui bsize bsize bsize bsize bsize)
      
  (define background-color (get-mixer-strip-background-color gui instrument-id))
  (<gui> :set-background-color gui color)
  
  ;;(define gui (<gui> :widget))
  (set-fixed-width gui (+ (* bsize)
                          (max (floor (<gui> :text-width "-14.2"))
                               (get-fontheight))))

  (define label (create-mixer-strip-name instrument-id strips-config #t is-standalone-mixer-strip))

  (define mutesolo-height (ceiling (* 0.9 (get-fontheight) *pan-mutesolo-voltext-scale-factor*)))

  (<gui> :add gui label 1)
  (<gui> :add gui (create-mixer-strip-mutesolo instrument-id strips-config background-color mutesolo-height #t #f))

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


(define *min-mixer-strip-with-sends-width* 100)
(define *min-mixer-strip-without-sends-width* 80)
(define *min-mixer-strip-without-plugins-width* 60)

;; we call this each time we create mixer strips in case font size has changed.
(define (set-minimum-mixer-strip-widths!)
  (set! *min-mixer-strip-with-sends-width* 100)
  (define base1 (<gui> :text-width " -14.2 -23.5 "))
  (define base2 (<gui> :text-width " Mute Solo "))
  
  (set! *min-mixer-strip-with-sends-width* (1+ (floor (max (* 1.8 base1)
                                                           (* 1.8 base2)))))
  
  (set! *min-mixer-strip-without-sends-width* (1+ (floor (max (* 1.4 base1)
                                                              (* 1.4 base2)))))
  
  (set! *min-mixer-strip-without-plugins-width* (1+ (floor (max (* 0.9 base1)
                                                                (* 0.9 base2))))))
  

(define (get-min-mixer-strip-width instrument-id)
  (let loop ((instrument-id instrument-id)
             (result-so-far *min-mixer-strip-without-plugins-width*))
    (get-mixer-strip-path-instruments instrument-id
                                      (lambda (bus-sends instrument-sends next-plugin-instrument)
                                        (cond ((not (null? bus-sends))
                                               *min-mixer-strip-with-sends-width*)
                                              ((not (null? instrument-sends))
                                               *min-mixer-strip-with-sends-width*)
                                              (next-plugin-instrument
                                               (loop next-plugin-instrument
                                                     *min-mixer-strip-without-sends-width*))
                                              (else
                                               result-so-far))))))

(define (create-mixer-strip-wide instrument-id strips-config is-standalone-mixer-strip)
  (define gui (<gui> :vertical-layout)) ;; min-width height))  
  (<gui> :set-min-width gui (get-min-mixer-strip-width instrument-id))
  ;;(<gui> :set-max-width gui width)
  ;;(<gui> :set-size-policy gui #f #t)

  (define bsize (if is-standalone-mixer-strip 0 2)) ;; Width of the ligth green border around current instrument.
  
  (<gui> :set-layout-spacing gui 0 bsize bsize bsize bsize)

  (define background-color (get-mixer-strip-background-color gui instrument-id))

  (define system-background-color (<gui> :get-background-color gui))
  (<gui> :set-background-color gui background-color) ;;(<ra> :get-instrument-color instrument-id))
  
  (define fontheight (get-fontheight))
  (define fontheight-and-borders (+ 4 fontheight))

  (define name-height fontheight-and-borders)
  (define pan-height (ceiling (* *pan-mutesolo-voltext-scale-factor* fontheight-and-borders)))
  (define mutesolo-height (ceiling (* 0.9 *pan-mutesolo-voltext-scale-factor* fontheight-and-borders)))
  (define comment-height fontheight-and-borders)

  (define pan-enabled (and instrument-id
                           (pan-enabled? instrument-id)))
  
  ;;(define total-fixed-height-without-pan (+ name-height mutesolo-height (if (<ra> :mixer-strip-comments-visible) comment-height 0)))
  ;;(define total-fixed-height-with-pan (+ total-fixed-height-without-pan pan-height))
  
  (define name (create-mixer-strip-name instrument-id strips-config #f is-standalone-mixer-strip))
  (set-fixed-height name name-height)

  (<gui> :add gui name)

  (define mixer-strip-path-gui (<gui> :vertical-scroll #f))
  ;;(<gui> :set-layout-spacing mixer-strip-path-gui 5 5 5 5 5)
  (<gui> :set-layout-spacing mixer-strip-path-gui 1 0 0 2 0)
  (<gui> :set-style-sheet mixer-strip-path-gui
         (<-> "QScrollArea {"
              "  background: " system-background-color ";"
              "  border: 1px solid rgba(10, 10, 10, 50);"
              "  border-radius:3px;"
              "}"))
 

  (define hepp (<gui> :horizontal-layout))
  (<gui> :set-layout-spacing hepp 0 1 1 1 1)

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
                                                                   (create-default-mixer-path-popup instrument-id strips-config mixer-strip-path-gui))
                                                               #t)
                                                        #f)))

  ;; create path gui
  (define last-instrument-id-in-path (create-mixer-strip-path mixer-strip-path-gui instrument-id instrument-id strips-config))
  
  (define meter-instrument-id last-instrument-id-in-path)

  (if (or #t pan-enabled) ;; Always #t. Couldn't figure out the math to adjust stretch values for path and volume so that all mute/solo buttons buttons align.
      (<gui> :add gui (create-mixer-strip-pan instrument-id strips-config system-background-color background-color pan-height)))
  
  (<gui> :add gui (create-mixer-strip-mutesolo instrument-id strips-config background-color mutesolo-height #f #t))
  (<gui> :add gui (create-mixer-strip-volume instrument-id meter-instrument-id strips-config background-color #f) 1)

  (when (<ra> :mixer-strip-comments-visible)
    (define comment (create-mixer-strip-comment instrument-id strips-config comment-height))
    (<gui> :add gui comment))

  ;;(if (not is-standalone-mixer-strip)
  ;;    (create-current-instrument-border gui instrument-id))

  (add-safe-paint-callback gui
                           (lambda (width height)
                             (<gui> :filled-box gui background-color 0 0 width height 0 0)
                             (draw-mixer-strips-border gui width height instrument-id is-standalone-mixer-strip bsize)))
  
  gui)

(delafina (create-mixer-strip :instrument-id
                              :strips-config #f
                              :is-standalone-mixer-strip #f)
  (define is-wide (if is-standalone-mixer-strip
                      *current-mixer-strip-is-wide*
                      (<ra> :has-wide-instrument-strip instrument-id)))
  (if is-wide
      (create-mixer-strip-wide instrument-id strips-config is-standalone-mixer-strip)
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

  (set-minimum-mixer-strip-widths!)
  
  (define parent (<gui> :widget width height)) ;; Lots of trouble using a widget as parent instead of a layout. However, it's an easy way to avoid flickering when changing current instrument.
  
  ;;(define width (floor (* 1 (<gui> :text-width "MUTE SOLO"))))
  
  (set-fixed-width parent width)
  ;;(set-fixed-height parent height)
  ;;(<gui> :set-min-width parent 100)
  ;;(<gui> :set-max-width parent 100)
  
  (define das-mixer-strip-gui #f)

  (define org-width width)

  (define is-calling-remake #f)
  
  (define (remake width height)
    (when (not is-calling-remake)
      (set! is-calling-remake #t)
      (define instrument-is-open (<ra> :instrument-is-open-and-audio instrument-id))
      
      (run-instrument-data-memoized
       (lambda()
         (when das-mixer-strip-gui
           (<gui> :close das-mixer-strip-gui)
           (set! das-mixer-strip-gui #f))
         
         (when instrument-is-open                
           (set! das-mixer-strip-gui (create-mixer-strip instrument-id :is-standalone-mixer-strip #t))
           (if *current-mixer-strip-is-wide*
               (set! width org-width)
               (set! width (<gui> :width das-mixer-strip-gui)))
           (set-fixed-width parent width)         
           (set-fixed-width das-mixer-strip-gui width)
           (<gui> :add parent das-mixer-strip-gui 0 0 width height)
           )       
         ))
      (set! is-calling-remake #f)))


  ;;(c-display "  Calling remake from constructor")
  (remake width height)
  

  (define is-resizing #f)
  
  (<gui> :add-resize-callback parent
         (lambda (width height)
           (when (and (not is-resizing) ;; Unfortunately, remake triggers a new resize, and we get a recursive call here. TODO: Fix this. Resize callback should never call itself.
                      (not is-calling-remake))
             (set! is-resizing #t)             
             (<gui> :disable-updates parent)
             ;;(c-display "  Calling remake from resize callback")
             (remake width height) ;; Don't need to use safe callback here. 'remake' checks that the instrument is open.
             (<gui> :enable-updates parent)
             (set! is-resizing #f))))
  
  (define mixer-strips-object (make-mixer-strips-object :gui parent
                                                        :remake (lambda (list-of-modified-instrument-ids)
                                                                  ;;(c-display "  Calling remake from global callback")
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
  (<gui> :set-background-color mixer-strips-gui *mixer-strip-background-color*)
  
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

  (define num-strips-per-row (ceiling (/ num-visible-strips num-rows)))

  (define (add-strips id-instruments)
    (map (lambda (instrument-id)
           (set! (strips-config :row-num instrument-id) row-num)
           ;;(c-display "adding strips for" instrument-id)
           (define stored-mixer-strip (get-stored-mixer-strip stored-mixer-strips instrument-id))
           (define stored-valid? (stored-mixer-strip-is-valid? stored-mixer-strip list-of-modified-instrument-ids))
           (define mixer-strip (if stored-valid?
                                   (get-mixer-strip-from-stored-mixer-strip stored-mixer-strip)
                                   (create-mixer-strip instrument-id :strips-config strips-config)))
           '(c-display "   Creating" instrument-id ". Stored is valid?" (stored-mixer-strip-is-valid? stored-mixer-strip list-of-modified-instrument-ids)
                       "stored-mixer-strip:" stored-mixer-strip
                       "list-of-modified:" list-of-modified-instrument-ids)
           
           (if stored-valid?
               (<gui> :remove-parent mixer-strip)) ;; remove existing parent of stored mixer strip.

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


  '(<gui> :add-resize-callback mixer-strips-gui
         (lambda (width height)
           (<gui> :disable-updates mixer-strips-gui)
           (<ra> :schedule 1 (lambda ()
                                (<gui> :enable-updates mixer-strips-gui)
                                #f))))
           
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

(define *mixer-strip-gui-num* 1)

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
  
  (<gui> :set-window-title gui (<-> "Radium Mixer #" *mixer-strip-gui-num*))
  (inc! *mixer-strip-gui-num* 1)
  
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

  (define _is-visible #t)
  
  (define (remake list-of-modified-instrument-ids)
    ;;(c-display "REMAKE" list-of-modified-instrument-ids ". gui visible:" (<gui> :is-visible gui) ". _is-visible:" _is-visible)
    
    (when _is-visible
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
      ))


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

  (<gui> :add-visibility-change-callback gui
         (lambda (is-visible)
           (when (not (eq? is-visible _is-visible))
             (set! _is-visible is-visible)
             (if is-visible
                 (begin
                   ;;(c-display "REMAKING" das-mixer-strips-gui)
                   (remake :non-are-valid))
                 (begin
                   ;;(c-display "CLOSING" das-mixer-strips-gui)
                   (<gui> :close das-mixer-strips-gui)
                   (set! das-mixer-strips-gui #f))))))
  
  ;;mixer-strips-object

  (<gui> :set-takes-keyboard-focus gui #f)

  gui
  )

#!!
(define gui (create-mixer-strips-gui 2
                                     (get-all-audio-instruments)))
(length (get-all-audio-instruments))
(<gui> :show gui)
!!#

(define (remake-mixer-strips . list-of-modified-instrument-ids)
  (FROM_C-reconfigure-sequencer-left-part)
  (set-minimum-mixer-strip-widths!)
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
    (run-instrument-data-memoized
     (lambda()
       (for-each (lambda (a-mixer-strips-object)
                   ((a-mixer-strips-object :remake) list-of-modified-instrument-ids))
                 *mixer-strips-objects*)))))

(define (FROM_C-redraw-mixer-strips . list-of-modified-instrument-ids)
  (set-minimum-mixer-strip-widths!)
  ;;(c-display "\n\n\n             REDRAW MIXER STRIPS " list-of-modified-instrument-ids "\n\n\n")
  (for-each (lambda (a-mixer-strips-object)
              (<gui> :update-recursively (a-mixer-strips-object :gui))  ;; In Qt, it's not enough to call update on the parent, for some reason. (Advice for other programmers: It's much faster to draw everything manually in semi-large or larger programs, both in terms of CPU and development time (especially in development time), than to use qt's widget system. It will probably look better too. Not only is Qt's widget system extremely slow, it's also very buggy (widgets "jump" all over the place, plus all the more serious bugs), and when you try to file bug reports they are very likely be marked as "not a bug" by the developers. In addition, you have all these insane workarounds about temporarily turning off updates, forced dealyed redrawing, and so forth.)
              ;;(<gui> :update (a-mixer-strips-object :gui)) ;; Doesn't always work.
              )
            *mixer-strips-objects*))


(define (get-mixer-strips-object-from-gui mixer-strips-gui)
  (let loop ((objects *mixer-strips-objects*))
    (cond ((null? objects)
           (let ((message (<-> "There is no mixer strips gui #" mixer-strips-gui)))
             (<ra> :add-message message)
             (error message)))
          ((= ((car objects) :gui)
              mixer-strips-gui)
           (car objects))
          (else
           (loop (cdr objects))))))
                   

(define (mixer-strips-get-num-rows mixer-strips-gui)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (if object
        (object :strips-config :num-rows)
        #f)))

(define (mixer-strips-change-num-rows mixer-strips-gui num-rows)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (if object
        (set! (object :strips-config :num-rows) num-rows))))

#!!
(mixer-strips-change-num-rows ((car *mixer-strips-objects*) :gui) 6)
!!#


(define (mixer-strips-reset-configuration! mixer-strips-gui)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (if object
        (object :strips-config :reset!))))

#!!
(mixer-strips-reset-configuration! ((car *mixer-strips-objects*) :gui))
!!#

(define (mixer-strips-get-configuration mixer-strips-gui)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (if object
        (object :strips-config :get))))
  
(define (mixer-strips-set-configuration! mixer-strips-gui configuration)
  (let ((object (get-mixer-strips-object-from-gui mixer-strips-gui)))
    (if object
        (object :strips-config :set! configuration))))
  

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

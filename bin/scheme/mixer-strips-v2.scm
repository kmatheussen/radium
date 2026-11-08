(provide 'mixer-strips-v2.scm)

(my-require 'area.scm)
(my-require 'mixer-strips.scm)


;; This file contains an area.scm-based version of the mixer strip path.
;; The old Qt-layout based path in mixer-strips.scm is left untouched.
;;
;; The widget created by get-mixer-strip-path-v2 is a vertical list area
;; containing one slider area per send/plugin/sink in the path.


;; Tokens used to distinguish the different send sliders that share the same
;; Qt gui. See set-db-value in create-mixer-strip-path-v2-send-area.
(define *mixer-strip-path-v2-send-token-counter* 0)

(define (get-mixer-strip-path-v2-send-token)
  (inc! *mixer-strip-path-v2-send-token-counter* -1))

;; Keep the path scroll position when the seqtrack headers are rebuilt, for
;; instance when the seqtrack under the mouse changes during a scrollbar drag.
(define *mixer-strip-path-v2-saved-states* (make-hash-table 100 equal?))

;; The old mixer strips use FROM_C-redraw-mixer-strips to update all their
;; widgets after a connection has been enabled/disabled. Register the new path
;; sliders here so the same redraw also updates them.
(define *mixer-strip-path-v2-update-funcs* '())

(define (add-mixer-strip-path-v2-update-func func)
  (push-back! *mixer-strip-path-v2-update-funcs* func)
  (lambda ()
    (set! *mixer-strip-path-v2-update-funcs*
          (remove (lambda (f)
                    (eq? f func))
                  *mixer-strip-path-v2-update-funcs*))))

(define (update-mixer-strip-path-v2-areas)
  (for-each (lambda (func)
              (func))
            *mixer-strip-path-v2-update-funcs*))

;; Like v1's mixer strip windows, repaint the path sliders when the
;; instrument under the mouse changes, so the light border follows the mouse.
(<ra> :add-current-instrument-under-mouse-changed-callback
      (lambda ()
        (if (null? *mixer-strip-path-v2-update-funcs*)
            #f
            (begin
              (update-mixer-strip-path-v2-areas)
              #t))))


;; Common slider area for plugins, sends, and sinks in the path.
;;
;; This is the area.scm equivalent of strip-slider in mixer-strips.scm. The
;; various get/set callbacks are supplied by the constructors below.
(def-area-subclass (<mixer-strip-path-v2-slider> :gui :x1 :y1 :x2 :y2
                                                 :first-instrument-id
                                                 :parent-instrument-id
                                                 :instrument-id
                                                 :strips-config
                                                 :is-send? #f
                                                 :is-sink? #f
                                                 :is-bus? #f
                                                 :show-arrow #f
                                                 :get-scaled-value
                                                 :get-value-text
                                                 :set-value
                                                 :get-automation-data #f
                                                 :make-undo #f
                                                 :delete-func #f
                                                 :replace-func #f
                                                 :reset-func #f
                                                 :is-enabled-func
                                                 :toggle-enabled-func
                                                 :midi-learn-instrument-id #f
                                                 :effect-name #f
                                                 :add-monitor-func #f)

  (define fontheight (get-fontheight))

  (define arrow-width (if show-arrow
                          (myfloor (* 1.5 (<gui> :text-width *arrow-text*)))
                          0))

  ;; Computed from the current position, since vertical-list-area2 moves the
  ;; area after it has been created.
  (define (get-slider-x1)
    (+ x1 arrow-width))

  (define is-hovering-on/off #f)
  (define start-mouse-value #f)
  (define has-made-undo #t)

  (detect-hovering!)

  (define (get-slider-text value)
    (define midi-learn-text
      (if (and effect-name
               midi-learn-instrument-id
               (<ra> :instrument-effect-has-midi-learn midi-learn-instrument-id effect-name))
          "[M] "
          ""))
    (<-> midi-learn-text
         (<ra> :get-instrument-name instrument-id)
         ": "
         (get-value-text value)))

  (define (get-x1-on/off)
    fontheight)

  (define (get-on/off-box kont)
    (define x1-on/off (get-x1-on/off))
    (define b1 (scale 1 0 3 0 x1-on/off))
    (define center-x (+ (get-slider-x1) (/ x1-on/off 2)))
    (define center-y (+ y1 (/ height 2)))
    (kont (- center-x b1)
          (- center-y b1)
          (+ center-x b1)
          (+ center-y b1)))

  (define (in-enabled-box? x y)
    (get-on/off-box
     (lambda (box-x1 box-y1 box-x2 box-y2)
       (and (>= x box-x1)
            (>= y box-y1)
            (< x box-x2)
            (< y box-y2)))))

  (define (paint-on/off)
    (get-on/off-box
     (lambda (box-x1 box-y1 box-x2 box-y2)
       (define b2 (scale 1 0 6 0 (get-x1-on/off)))
       (define inner-x1 (+ box-x1 b2))
       (define inner-y1 (+ box-y1 b2))
       (define inner-x2 (- box-x2 b2))
       (define inner-y2 (- box-y2 b2))

       (define color (let ((color (if (is-enabled-func)
                                      "#22aa22"
                                      "#003f00")))
                       (if is-hovering-on/off
                           (<gui> :mix-colors color "white" 0.75)
                           color)))

       (<gui> :filled-ellipse gui "black" box-x1 box-y1 box-x2 box-y2)
       (<gui> :filled-ellipse gui color inner-x1 inner-y1 inner-x2 inner-y2)
       (<gui> :draw-ellipse gui
              (if is-hovering-on/off "#aa333333" "#88111111")
              box-x1 box-y1 box-x2 box-y2
              (if is-hovering-on/off 3.0 2.0)))))

  (define-override (paint)
    (define value (get-scaled-value))
    (define x1-on/off (get-x1-on/off))

    (define sx1 (+ x1 arrow-width))

    (when show-arrow
      (<gui> :my-draw-text gui *text-color* *arrow-text*
             x1 y1 sx1 y2
             #f)) ;; wrap-lines

    ;; Use the same slider painting function as v1, but base the border on the
    ;; current instrument instead of the instrument under the mouse.
    (paint-horizontal-slider :widget gui
                             :value value
                             :text (get-slider-text value)
                             :x1 sx1
                             :y1 y1
                             :x2 x2
                             :y2 y2
                             :color (<ra> :get-instrument-color instrument-id)
                             :is-enabled (is-enabled-func)
                             :is-current (equal? (<ra> :get-current-instrument-under-mouse)
                                                 instrument-id)
                             :get-automation-data get-automation-data
                             :text-x1 (+ sx1 x1-on/off)
                             :border-color (get-instrument-border-color instrument-id)
                             :is-hovering is-hovering)

    (paint-on/off))

  ;; Track hovering over the on/off circle separately, so that the slider part
  ;; can still be hover-highlighted as a whole.
  (define (update-on/off-hover x* y*)
    (define new-hovering (and (in-enabled-box? x* y*) #t))
    (when (not (eq? new-hovering is-hovering-on/off))
      (set! is-hovering-on/off new-hovering)
      (update-me!)))

  (add-raw-mouse-cycle!
   :enter-func (lambda (button x* y)
                 (update-on/off-hover x* y)
                 #t)
   :move-func (lambda (button x* y)
                (update-on/off-hover x* y)
                #t)
   :leave-func (lambda (button x y)
                 (when is-hovering-on/off
                   (set! is-hovering-on/off #f)
                   (update-me!))
                 #t))

  (define (maybe-make-undo)
    (when (and (not has-made-undo)
               make-undo)
      (make-undo)
      (set! has-made-undo #t)))

  (define (show-popup)
    (show-mixer-path-popup first-instrument-id
                           parent-instrument-id
                           instrument-id
                           strips-config
                           is-send?
                           is-sink?
                           is-bus?
                           delete-func
                           replace-func
                           (and reset-func
                                (lambda ()
                                  (maybe-make-undo)
                                  (reset-func)))
                           midi-learn-instrument-id
                           effect-name
                           gui))

  (add-delta-mouse-cycle!
   (lambda (button x* y*)
     (set! has-made-undo #f)
     (cond ((and delete-func
                 (delete-button? button))
            (delete-func)
            'eat-mouse-cycle)
           ((in-enabled-box? x* y*)
            (if (= button *left-button*)
                (begin
                  (toggle-enabled-func)
                  (update-me!)
                  (update-me-and-all-parents-and-siblings!)))
            'eat-mouse-cycle)
           ((= button *right-button*)
            (show-popup)
            'eat-mouse-cycle)
           ((= button *left-button*)
            (set-curr-instrument-in-mouse-callback instrument-id gui)
            (set! start-mouse-value (get-scaled-value))
            (set-statusbar-text! (get-slider-text start-mouse-value))
            #t)
           (else
            #f)))
   (lambda (button x* y* dx dy)
     (when start-mouse-value
       (maybe-make-undo)
       (define slider-value (between 0
                                     (+ start-mouse-value
                                        (scale dx 0 width 0 1))
                                     1))
       (set-value slider-value)
       (set-statusbar-text! (get-slider-text slider-value))
       (update-me!)))
   (lambda (button x* y* dx dy)
     (set! start-mouse-value #f)
     #f))

  (add-statusbar-text-handler
   (lambda ()
     (get-slider-text (get-scaled-value))))

  ;; Add the value/automation monitor. add-monitor-func is defined by the
  ;; plugin/send/sink constructor. It returns an optional cleanup function.
  (define remove-monitor-func #f)
  (when add-monitor-func
    (set! remove-monitor-func
          (add-monitor-func add-area-effect-monitor!
                            (lambda () (update-me!)))))

  ;; Make the old mixer strip redraw update this slider too.
  (define remove-update-func
    (add-mixer-strip-path-v2-update-func
     (lambda ()
       (update-me!))))

  (define-override (about-to-be-removed-callback)
    (when remove-monitor-func
      (remove-monitor-func)
      (set! remove-monitor-func #f))
    (when remove-update-func
      (remove-update-func)
      (set! remove-update-func #f))))


;; A plugin in the path. Displays/changes System Dry/Wet.
(define (create-mixer-strip-path-v2-plugin-area gui
                                                x1 y1 x2 y2
                                                first-instrument-id
                                                parent-instrument-id
                                                instrument-id
                                                strips-config)
  (define effect-name "System Dry/Wet")
  (define doit #t)

  (define (get-drywet)
    (<ra> :get-stored-instrument-effect instrument-id effect-name))

  (define (make-undo)
    (<ra> :undo-instrument-effect instrument-id effect-name))

  (define (delete)
    (delete-instrument-plugin parent-instrument-id instrument-id))

  (define (replace)
    (async-replace-instrument instrument-id
                              ""
                              (make-instrument-conf :must-have-inputs #t
                                                    :must-have-outputs #t
                                                    :parentgui gui)))

  (define (reset)
    (<ra> :set-instrument-effect instrument-id effect-name 1))

  (define (is-enabled?)
    (>= (<ra> :get-instrument-effect instrument-id "System Effects On/Off") 0.5))

  (define (toggle-enabled)
    ;; set-instrument-bypass takes "bypass" as argument, so passing the current
    ;; enabled state toggles it.
    (<ra> :set-instrument-bypass (is-enabled?) instrument-id))

  (define automation-value #f)
  (define automation-color (<ra> :get-instrument-effect-color instrument-id effect-name))

  (define (get-automation-data kont)
    (if automation-value
        (kont automation-value automation-color)))

  (define (add-monitor add-effect-monitor! update-me)
    (add-effect-monitor! instrument-id effect-name #t #t
                         (lambda (drywet automation)
                           (when drywet
                             (update-me))
                           (when automation
                             (set! automation-value
                                   (if (< automation 0)
                                       #f
                                       automation))
                             (update-me))))
    (add-effect-monitor! instrument-id "System Effects On/Off" #t #t
                         (lambda (on/off automation)
                           (update-me)))
    #f)

  (<new> :mixer-strip-path-v2-slider gui x1 y1 x2 y2
         :first-instrument-id first-instrument-id
         :parent-instrument-id parent-instrument-id
         :instrument-id instrument-id
         :strips-config strips-config
         :is-send? #f
         :is-sink? #f
         :is-bus? #f
         :get-scaled-value get-drywet
         :get-value-text (lambda (value)
                           (<-> (round (* 100 value)) "%"))
         :set-value (lambda (new-scaled-value)
                      (when (and doit
                                 (not (= new-scaled-value (get-drywet))))
                        (<ra> :set-instrument-effect instrument-id effect-name new-scaled-value)))
         :get-automation-data get-automation-data
         :make-undo make-undo
         :delete-func delete
         :replace-func replace
         :reset-func reset
         :is-enabled-func is-enabled?
         :toggle-enabled-func toggle-enabled
         :midi-learn-instrument-id instrument-id
         :effect-name effect-name
         :add-monitor-func add-monitor))


;; A sink in the path (an instrument without output channels). Displays/changes
;; the System In effect.
(define (create-mixer-strip-path-v2-sink-area gui
                                              x1 y1 x2 y2
                                              first-instrument-id
                                              parent-instrument-id
                                              instrument-id
                                              strips-config)
  (define effect-name "System In")
  (define doit #t)

  (define (get-db-value)
    (radium-normalized-to-db
     (<ra> :get-stored-instrument-effect instrument-id effect-name)))

  (define (set-db-value db)
    (<ra> :set-instrument-effect instrument-id effect-name (db-to-radium-normalized db)))

  (define (make-undo)
    (<ra> :undo-instrument-effect instrument-id effect-name))

  (define (delete)
    (<ra> :undo-mixer-connections)
    (<ra> :delete-audio-connection parent-instrument-id instrument-id)
    #f)

  (define (replace)
    (request-send-instrument
     parent-instrument-id
     (lambda (create-send-func)
       (undo-block
        (lambda ()
          (define db (get-db-value))
          (define gain (<ra> :db-to-gain db))
          (define changes '())
          (push-audio-connection-change! changes (list :type "disconnect"
                                                       :source parent-instrument-id
                                                       :target instrument-id))
          (create-send-func gain changes))))))

  (define (reset)
    (set-db-value 0))

  (define (is-enabled?)
    (and (not (<ra> :get-connection-implicitly-disabled parent-instrument-id instrument-id))
         (<ra> :get-connection-enabled parent-instrument-id instrument-id #f)))

  (define (toggle-enabled)
    (define enabled (is-enabled?))
    (<ra> :undo-connection-enabled parent-instrument-id instrument-id)
    (<ra> :set-connection-enabled parent-instrument-id instrument-id (not enabled) #t))

  (define last-value (get-db-value))

  (define automation-value #f)
  (define automation-color (<ra> :get-instrument-effect-color instrument-id effect-name))

  (define (get-automation-data kont)
    (if automation-value
        (kont automation-value automation-color)))

  (define (add-monitor add-effect-monitor! update-me)
    (add-effect-monitor! instrument-id effect-name #t #t
                         (lambda (system-in automation)
                           (when system-in
                             (update-me))
                           (when automation
                             (set! automation-value
                                   (if (< automation 0)
                                       #f
                                       (radium-normalized-to-slider automation)))
                             (update-me))))
    #f)

  (<new> :mixer-strip-path-v2-slider gui x1 y1 x2 y2
         :first-instrument-id first-instrument-id
         :parent-instrument-id parent-instrument-id
         :instrument-id instrument-id
         :strips-config strips-config
         :is-send? #t
         :is-sink? #t
         :is-bus? #f
         :show-arrow #t
         :get-scaled-value (lambda ()
                             (db-to-slider (get-db-value)))
         :get-value-text (lambda (slider-value)
                           (db-to-text (slider-to-db slider-value) #t))
         :set-value (lambda (new-slider-value)
                      (define db (slider-to-db new-slider-value))
                      (when (and doit (not (= last-value db)))
                        (set! last-value db)
                        (set-db-value db)))
         :get-automation-data get-automation-data
         :make-undo make-undo
         :delete-func delete
         :replace-func replace
         :reset-func reset
         :is-enabled-func is-enabled?
         :toggle-enabled-func toggle-enabled
         :midi-learn-instrument-id instrument-id
         :effect-name effect-name
         :add-monitor-func add-monitor))


;; A send in the path. Also handles bus sends.
(define (create-mixer-strip-path-v2-send-area gui
                                              x1 y1 x2 y2
                                              first-instrument-id
                                              source-id
                                              target-id
                                              strips-config)
  (define bus-effect-name (get-bus-effect-name-from-target-instrument target-id))
  (define send-token (get-mixer-strip-path-v2-send-token))
  (define doit #t)

  (define (get-db-value)
    (if (and (<ra> :instrument-is-open-and-audio source-id)
             (<ra> :instrument-is-open-and-audio target-id)
             (<ra> :has-audio-connection source-id target-id))
        (<ra> :gain-to-db (<ra> :get-audio-connection-gain source-id target-id))
        0.0))

  (define (set-db-value db)
    (<ra> :set-audio-connection-gain source-id target-id (<ra> :db-to-gain db) #f)
    (for-each (lambda (send-callback)
                (send-callback send-token source-id target-id db))
              *send-callbacks*))

  (define (make-undo)
    (<ra> :undo-audio-connection-gain source-id target-id))

  (define (delete)
    (<ra> :undo-mixer-connections)
    (<ra> :delete-audio-connection source-id target-id)
    #f)

  (define (replace)
    (request-send-instrument
     source-id
     (lambda (create-send-func)
       (define gain (<ra> :get-audio-connection-gain source-id target-id))
       (undo-block
        (lambda ()
          (define changes '())
          (push-audio-connection-change! changes (list :type "disconnect"
                                                       :source source-id
                                                       :target target-id))
          (create-send-func gain changes))))))

  (define (reset)
    (set-db-value 0))

  (define (is-enabled?)
    (and (not (<ra> :get-connection-implicitly-disabled source-id target-id))
         (<ra> :get-connection-enabled source-id target-id #f)))

  (define (toggle-enabled)
    (define enabled (is-enabled?))
    (<ra> :undo-connection-enabled source-id target-id)
    (<ra> :set-connection-enabled source-id target-id (not enabled) #t))

  (define last-value (get-db-value))

  (define automation-value #f)
  (define automation-color (if bus-effect-name
                               (<ra> :get-instrument-effect-color source-id bus-effect-name)
                               "white"))

  (define (get-automation-data kont)
    (if (and bus-effect-name automation-value)
        (kont (min 1.0 automation-value) automation-color)))

  (define (add-send-monitor update-me)
    (define send-callback
      (lambda (maybe-token maybe-source-id maybe-target-id db)
        (if (and (not (= send-token maybe-token))
                 (equal? source-id maybe-source-id)
                 (equal? target-id maybe-target-id)
                 (<ra> :instrument-is-open-and-audio source-id)
                 (<ra> :instrument-is-open-and-audio target-id)
                 (<ra> :has-audio-connection source-id target-id))
            (update-me))))
    (push-back! *send-callbacks* send-callback)
    (lambda ()
      (set! *send-callbacks*
            (remove (lambda (callback)
                      (eq? callback send-callback))
                    *send-callbacks*))))

  (define (add-monitor add-effect-monitor! update-me)
    (if bus-effect-name
        (begin
          (add-effect-monitor! source-id bus-effect-name #t #t
                               (lambda (radium-normalized automation)
                                 (when radium-normalized
                                   (update-me))
                                 (when automation
                                   (set! automation-value
                                         (if (< automation 0)
                                             #f
                                             (radium-normalized-to-slider automation)))
                                   (update-me))))
          #f)
        (add-send-monitor update-me)))

  (<new> :mixer-strip-path-v2-slider gui x1 y1 x2 y2
         :first-instrument-id first-instrument-id
         :parent-instrument-id source-id
         :instrument-id target-id
         :strips-config strips-config
         :is-send? #t
         :is-sink? #f
         :is-bus? #f
         :show-arrow #t
         :get-scaled-value (lambda ()
                             (db-to-slider (get-db-value)))
         :get-value-text (lambda (slider-value)
                           (db-to-text (slider-to-db slider-value) #t))
         :set-value (lambda (new-slider-value)
                      (define db (slider-to-db new-slider-value))
                      (when (and doit (not (= last-value db)))
                        (set! last-value db)
                        (set-db-value db)))
         :get-automation-data get-automation-data
         :make-undo make-undo
         :delete-func delete
         :replace-func replace
         :reset-func reset
         :is-enabled-func is-enabled?
         :toggle-enabled-func toggle-enabled
         :midi-learn-instrument-id (and bus-effect-name source-id)
         :effect-name bus-effect-name
         :add-monitor-func add-monitor))


;; Returns a list of (kind parent-instrument-id target-instrument-id), where
;; kind is 'plugin, 'send, or 'sink. The order is the same as in the old
;; create-mixer-strip-path: all sends first, then the next plugin, then the
;; sends/plugins of that plugin, and so on.
(define (get-mixer-strip-path-v2-entries instrument-id)
  (let loop ((instrument-id instrument-id)
             (result '()))
    (get-mixer-strip-path-instruments
     instrument-id
     (lambda (instrument-sends next-plugin-instrument)
       (define result-with-sends
         (append result
                 (map (lambda (out-instrument)
                        (list (if (= 0 (<ra> :get-num-output-channels out-instrument))
                                  'sink
                                  'send)
                              instrument-id
                              out-instrument))
                      instrument-sends)))

       (define result-with-next
         (if next-plugin-instrument
             (append result-with-sends
                     (list (list (if (= 0 (<ra> :get-num-output-channels next-plugin-instrument))
                                     'sink
                                     'plugin)
                                 instrument-id
                                 next-plugin-instrument)))
             result-with-sends))

       (if next-plugin-instrument
           (loop next-plugin-instrument result-with-next)
           result-with-next)))))


;; Returns the last instrument in the path, i.e. the instrument whose meter
;; should be used by the caller.
(define (get-mixer-strip-path-v2-last-instrument-id instrument-id)
  (define entries (get-mixer-strip-path-v2-entries instrument-id))
  (if (null? entries)
      instrument-id
      (caddr (last entries))))


;; Always paint the path background with the default sequencer color, also
;; when the surrounding seqtrack is selected and has an instrument tint.
(def-area-subclass (<mixer-strip-path-v2-background> :gui :x1 :y1 :x2 :y2
                                                     :instrument-id
                                                     :path-area)

  (define-override (paint)
    (<gui> :filled-box gui "high_background" x1 y1 x2 y2))

  (define-override (about-to-be-removed-callback)
    (define state (path-area :get-state))
    (when (and state
               (state :slider-pos))
      (set! (*mixer-strip-path-v2-saved-states* instrument-id) state))))


;; Public function. Returns an area containing the path.
(delafina (get-mixer-strip-path-v2 :gui
                                   :x1 :y1 :x2 :y2
                                   :instrument-id
                                   :first-instrument-id #f
                                   :strips-config #f
                                   :entry-height #f)

  (when (not first-instrument-id)
    (set! first-instrument-id instrument-id))

  (define entries (get-mixer-strip-path-v2-entries instrument-id))
  (define num-entries (length entries))

  (when (not entry-height)
    (set! entry-height (+ 2 (get-fontheight))))

  (define available-height (myfloor (- y2 y1)))
  (define min-total-height (* num-entries entry-height))

  ;; If there is room for all entries, distribute the available height between
  ;; them, so the path fills the whole header. Otherwise keep the minimum entry
  ;; height and let vertical-list-area2 scroll.
  (define can-expand-path (and (> num-entries 0)
                               (>= available-height min-total-height)))
  (define expanded-entry-height
    (if can-expand-path
        (myfloor (/ available-height num-entries))
        entry-height))
  (define expansion-remainder
    (if can-expand-path
        (- available-height (* expanded-entry-height num-entries))
        0))

  (define slider-gap 1)

  (define (get-row-height num)
    ;; Let the last entry absorb the rounding remainder so the path fills the
    ;; header exactly.
    (if (and can-expand-path
             (= num (- num-entries 1)))
        (+ expanded-entry-height expansion-remainder)
        expanded-entry-height))

  (define (get-slider-height num)
    ;; The gap only goes between sliders, never below the last one.
    (define row-height (get-row-height num))
    (if (= num (- num-entries 1))
        row-height
        (max 1 (- row-height slider-gap))))

  (define (create-entry-area num area-x1 area-x2)
    (define entry (list-ref entries num))
    (define kind (car entry))
    (define parent-instrument-id (cadr entry))
    (define target-instrument-id (caddr entry))
    (define slider-height (get-slider-height num))

    (cond ((eq? kind 'plugin)
           (create-mixer-strip-path-v2-plugin-area gui
                                                   area-x1 0 area-x2 slider-height
                                                   first-instrument-id
                                                   parent-instrument-id
                                                   target-instrument-id
                                                   strips-config))
          ((eq? kind 'send)
           (create-mixer-strip-path-v2-send-area gui
                                                 area-x1 0 area-x2 slider-height
                                                 first-instrument-id
                                                 parent-instrument-id
                                                 target-instrument-id
                                                 strips-config))
          ((eq? kind 'sink)
           (create-mixer-strip-path-v2-sink-area gui
                                                 area-x1 0 area-x2 slider-height
                                                 first-instrument-id
                                                 parent-instrument-id
                                                 target-instrument-id
                                                 strips-config))
          (else
           (assert #f))))

  (define path-area
    (<new> :vertical-list-area2 gui x1 y1 x2 y2
           :num-sub-areas num-entries
           :get-sub-area-height expanded-entry-height
           :create-sub-area create-entry-area
           :background-color #f))

  (define saved-state (*mixer-strip-path-v2-saved-states* instrument-id))
  (when (and saved-state
             (saved-state :slider-pos))
    (path-area :apply-state! saved-state))

  (define background-area
    (<new> :mixer-strip-path-v2-background gui x1 y1 x2 y2
           :instrument-id instrument-id
           :path-area path-area))

  (background-area :add-sub-area-plain! path-area)

  background-area)

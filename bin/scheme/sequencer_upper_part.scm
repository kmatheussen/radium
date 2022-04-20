(provide 'sequencer_upper_part.scm)

(my-require 'sequencer.scm)
(my-require 'gui.scm)
(my-require 'area.scm)



#!!
(pp (<ra> :get-sequencer-tempos (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
(pp (<ra> :get-all-sequencer-tempos))
(pp (<ra> :get-all-sequencer-markers))

(pp (<ra> :get-sequencer-signatures (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
!!#


(define (get-sequencer-x-from-time time x1 x2)
  (scale time
         (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)
         x1 x2))

(define (get-time-triangle time area)
  (area :get-position
        (lambda (x1 y1 x2 y2 width height)
          (define tx (get-sequencer-x-from-time time x1 x2))
  
          (define h/2 (/ (- y2 y1) 2))
          (define triangle-x1 (- tx h/2))
          (define triangle (vector triangle-x1 (+ y1 1)
                                   (+ tx h/2) (+ 1 y1)
                                   tx y2))
          triangle)))

(define (get-triangle-x triangle)
  (triangle 4))

(define (inside-triangle? triangle x y)
  (define x1 (triangle 0))
  (define y1 (triangle 1))
  (define x2 (triangle 2))
  (define y2 (triangle 5))
  (and (>= x x1)
       (< x x2)
       (>= y y1)
       (< y y2)))

(define (show-sequencer-timing-help)
  (FROM-C-show-help-window "help/index.html?page=timing"))

(define (get-sequencer-timing-popup-menu-entries)
  (list "-------------------"
        "----Help timing"
        (list "Help"
              show-sequencer-timing-help)
        "----Sequencer"
        (list "Sequencer"
              (get-sequencer-conf-menues))
        ))

(define *show-editor-timing-warning* #t)
(define *editor-timing-warning-is-visible* #f)

(define (show-editor-timing-warning)
  (define buttons '("Help" "Don't show this warning again" "OK"))
  (when (and *show-editor-timing-warning*
             (not *editor-timing-warning-is-visible*))
    (set! *editor-timing-warning-is-visible* #t)
    (show-async-message (<gui> :get-sequencer-gui)
                        "Tempos and signatures can not be edited in the sequencer timeline\nwhen timing is set to editor timing mode."
                        :buttons buttons
                        :callback (lambda (option)
                                    (set! *editor-timing-warning-is-visible* #f)
                                    (cond ((string=? option (car buttons))
                                           (show-sequencer-timing-help))
                                          ((string=? option (cadr buttons))
                                           (set! *show-editor-timing-warning* #f)))))))
#!!
(show-editor-timing-warning)
!!#

(define (get-sequencer-time-from-x x x1 x2)
  (* 1.0 (max 0 (scale x x1 x2 (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))))

(def-area-subclass (<sequencer-timeline-entry-area> :gui :x1 :y1 :x2 :y2
                                                    :value-type-name
                                                    :get-visible-entries
                                                    :get-all-entries
                                                    :set-all-entries!
                                                    :popup-menu*
                                                    :get-entry-info-string
                                                    :entry-color "#66ff0000"
                                                    :curr-entry-color "#660000ff"
                                                    :do-grid #f
                                                    :double-click-callback #f
                                                    :value->y #f
                                                    :is-tempo #f
                                                    :is-marker #f
                                                    :can-be-edited-in-sequencer-timing-mode #f
                                                    )
  
  (define curr-entry (<optional-hash-table>))
  (define curr-pos #f)
  (define curr-id #f)

  (define (update-parent!)
    (parent-area :update-me!))

  (define (remove-curr-entry!)
    (define entries (get-all-entries))
    (<ra> :undo-sequencer)
    (if (= 1 (vector-length entries))
        (set-all-entries! '())
        (begin
          (set! (entries curr-pos) (entries 0))
          (set-all-entries! (cdr (to-list entries)))))
    ;;(<gui> :update (<gui> :get-sequencer-gui))
    ;;(update-parent!)
    )

  (add-method! :remove-curr-entry! remove-curr-entry!)

  (define (update-curr-entry-pos! entries curr-entry)
    (set! curr-pos (list-position entries (lambda (maybe) (morally-equal? maybe curr-entry))))

    (<ra> :set-seq-indicator
          (round (max 0 (round (curr-entry :time))))
          -1
          1
          curr-entry-color)
  
    (when (= curr-pos -1)
      (set! curr-pos 0)
      (c-display "curr-entry:" (pp curr-entry))
      (c-display "entries:" (pp (get-all-entries)))
      (assert (and "curr-pos<0" #f))))

  (define (sort-entries entries)
    (sort (to-list entries) (lambda (a b)
                              (< (a :time) (b :time)))))
  
  (define (replace-curr-entry2! entries curr-pos new-entry)
    (set! (entries curr-pos) new-entry)
    (set! entries (sort-entries entries))
    (set-all-entries! entries)
    (set! curr-entry new-entry)
    (set! curr-id (new-entry :uuid))
    (update-curr-entry-pos! entries new-entry)    
    ;;(update-parent!)
    )

  (define (replace-curr-entry! new-entry)
    (define entries (get-all-entries))
    (<ra> :undo-sequencer)
    (replace-curr-entry2! entries curr-pos new-entry))

  (add-method! :replace-curr-entry! replace-curr-entry!)

  (define (add-entry! new-entry)
    (define new-entries (sort-entries (cons new-entry
                                            (to-list (get-all-entries)))))
    (<ra> :undo-sequencer)
    (set-all-entries! new-entries)
    (update-curr-entry-pos! new-entries new-entry)
    ;;(update-parent!)
    )

  (add-method! :add-entry! add-entry!)

  ;; shift+right click
  (add-mouse-cycle! (lambda (button x* y*)
                      (and (= button *right-button*)
                           (<ra> :shift-pressed)
                           curr-entry
                           (begin
                             (if (and (not can-be-edited-in-sequencer-timing-mode)
                                      (not (<ra> :is-using-sequencer-timing)))
                                 (show-editor-timing-warning)
                                 (remove-curr-entry!))
                             #t))))
  ;; popup menu
  (if popup-menu*
      (add-mouse-cycle! (lambda (button x* y*)
                          (and (= button *right-button*)
                               (begin
                                 (popup-menu* x* curr-entry curr-pos)
                                 #t)))))

  ;; set song pos / double-clicking
  (let ()
    (define click-start-time 0)
    (add-mouse-cycle! (lambda (button x* y*)
                        (and (= button *left-button*)
                             (not curr-entry)
                             (begin
                               (c-display "HEPP")
                               (define time (<ra> :get-ms))
                               (if (and double-click-callback
                                        (< (- time click-start-time)
                                           (<ra> :get-double-click-interval)))
                                   (double-click-callback x* #f)
                                   (<ra> :set-song-pos (round (get-sequencer-time-from-x x* x1 x2))))
                               (set! click-start-time time))
                             #t))))
    
  ;; move entry
  (let ()
    (define curr-mouse-pos #f)
    (define curr-mouse-entry #f)
    (define mouse-entries #f)
    (define click-start-time 0)
    (define mouse-start-x #f)
    (define has-added-undo #f)
    (add-delta-mouse-cycle! (lambda (button x* y*)
                              (set! mouse-start-x x*)
                              (set! has-added-undo #f)
                              (c-display "Press2" x* y*)
                              (if is-marker
                                  (<ra> :set-paint-vertical-markers-in-sequencer #t))
                              (and (= button *left-button*)
                                   curr-entry
                                   (begin
                                     (set! curr-mouse-pos curr-pos)
                                     (set! mouse-entries (get-all-entries))
                                     (set! curr-mouse-entry curr-entry)
                                     (when double-click-callback
                                       (define time (<ra> :get-ms))
                                       (if (< (- time click-start-time)
                                              (<ra> :get-double-click-interval))
                                           (double-click-callback x* curr-entry)
                                           (set! click-start-time time)))
                                     #t)))
                            (lambda (button x* y* dx dy)
                              ;;(c-display "Move" x* y* dx dy "pos:" curr-pos)
                              (if (and (not can-be-edited-in-sequencer-timing-mode)
                                       (not (<ra> :is-using-sequencer-timing)))
                                  (show-editor-timing-warning)
                                  (begin
                                    (define new-time (get-sequencer-time-from-x (+ mouse-start-x dx) x1 x2))
                                    (if do-grid
                                        (if (not (<ra> :control-pressed))
                                            (set! new-time (* 1.0 (<ra> :get-seq-gridded-time (round new-time))))))
                                    (when (not has-added-undo)
                                      (<ra> :undo-sequencer)
                                      (set! has-added-undo #t))
                                    (replace-curr-entry2! mouse-entries curr-mouse-pos (copy-hash curr-mouse-entry :time new-time))
                                    ;;(<gui> :update (<gui> :get-sequencer-gui))
                                    )))
                            (lambda (button x* y* dx dy)
                              (if is-marker
                                  (<ra> :set-paint-vertical-markers-in-sequencer #f))
                              ;;(c-display "Release" x* y* dx dy)
                              (if (and (= 0 dx)
                                       (= 0 dy))
                                  (<ra> :set-song-pos (round (curr-entry :time))))
                              (set! curr-id #f)
                              ;;(update-parent!)                              
                              ;;(<gui> :tool-tip "")
                              )))


  ;; mark current entry
  (add-raw-mouse-cycle!
   :move-func
   (lambda (button x* y*)
     (define had-curr-entry curr-entry)
     (set! curr-entry #f)
     (set! curr-id #f)
     (if is-marker
         (<ra> :set-paint-vertical-markers-in-sequencer #f))

     (define entries (get-all-entries))
     ;;(c-display "Entries:" (pp entries))
     (let loop ((n 0))
       (if (< n (length entries))
           (begin
             (define entry (entries n))
             (define triangle (get-time-triangle (entry :time) this))
             (define triangle-x (get-triangle-x triangle))
             ;;(c-display "n:" n ". Entry:" entry "\ntriangle:" (map floor (to-list triangle)) ". Inside:" (inside-triangle? triangle x* y*) " .x/y:" x* y* "\n")
             (when (inside-triangle? triangle x* y*)
               ;;(<gui> :tool-tip (<-> value-type-name ": " (get-entry-info-string entry n #f #f)))
               (set! curr-entry entry)
               (set! curr-id (entry :uuid))
               (set! curr-pos n)
               (if is-marker
                   (<ra> :set-paint-vertical-markers-in-sequencer #t))
               ;;(update-me!)
               )
             (if (>= x* triangle-x)
                 (begin                   
                   (define next-entry (and (< (+ n 1) (length entries))
                                           (entries (+ n 1))))
                   (define next-time (and next-entry (next-entry :time)))
                   (define next-triangle-x (and next-time (get-sequencer-x-from-time next-time x1 x2)))
                   (define use-next-entry (and is-tempo
                                               next-entry
                                               (<= x* next-triangle-x)
                                               (= *logtype-linear* (entry :logtype))))
                   (update-parent!)
                   (<ra> :set-statusbar-text (<-> value-type-name ": "
                                                  (get-entry-info-string entry n
                                                                         (and use-next-entry next-entry)
                                                                         (and use-next-entry (scale x* triangle-x next-triangle-x 0 100)))))))
             (loop (+ n 1)))))
     (when (and had-curr-entry
                (not curr-entry))
       ;;(<gui> :tool-tip "")
       ;;(update-parent!)
       #f
       )
     ;;(c-display "curr-entry:" curr-entry)
     (let ((time (round (max 0
                             (if curr-entry
                                 (curr-entry :time)
                                 (get-sequencer-time-from-x x* x1 x2))))))
       (<ra> :set-seq-indicator
             (if (or  curr-entry
                      (<ra> :control-pressed))
                 time
                 (<ra> :get-seq-gridded-time time))
             -1
             (if curr-entry
                 1
                 0)
             (if curr-entry
                 curr-entry-color
                 "")))
     #t)
   :leave-func
   (lambda (button x y)
     (when (and (= 0 button)
                curr-entry)
       (set! curr-entry #f)
       (set! curr-id #f)
       (if is-marker
           (<ra> :set-paint-vertical-markers-in-sequencer #f))
       ;;(update-parent!)
       )
     (when (not (= 0 button))
       (<ra> :set-statusbar-text "")
       ;;(<gui> :tool-tip "")
       )))
  
  
  (define paint-entries #f)

  (define (paint-background)

    ;;(<gui> :filled-box gui "sequencer_timeline_background_color" x1 y1 x2 y2 0 0 *no-gradient*)
    ;;(<gui> :filled-box gui "sequencer_background_color" x1 y1 x2 y2 0 0 *no-gradient*)

    (set! paint-entries (to-list (get-visible-entries)))

    (define (get-ty bpm)
      (scale bpm 0 200 y2 y1))

    (if (and is-tempo
             (not (null? paint-entries)))
        (<gui> :filled-polygon gui "#338811"
               (let loop ((entries paint-entries)
                          (last-y #f))
                 (if (null? entries)
                     (list x2 last-y
                           x2 y2)
                     (let ((entry (car entries)))
                       (define tx (get-sequencer-x-from-time (entry :time) x1 x2))
                       (define ty (get-ty (entry :bpm)))
                       (define rest2 (loop (cdr entries)
                                           ty))
                       (define rest (if (and (= (entry :logtype) *logtype-hold*)
                                             (not (null? (cdr entries))))
                                        (let ((next-entry (cadr entries)))
                                          (cons tx
                                                (cons ty
                                                      (cons (get-sequencer-x-from-time (next-entry :time) x1 x2)
                                                            (cons ty
                                                                  rest2)))))
                                        (cons tx
                                              (cons ty
                                                    rest2))))
                       (if (not last-y)
                           (cons tx
                                 (cons y2
                                       rest))
                           rest)))))))

  (add-method! :paint-background paint-background)

  (define-override (post-paint)

    (let loop ((entries (reverse paint-entries))
               (n (- (length paint-entries) 1))
               (max-x2 10000000000000000000000.0)
               (prev-entry (<optional-hash-table>)))
      (when (not (null? entries))
        (define entry (car entries))
        (define text (<-> (get-entry-info-string entry n #f #f)
                          (cond ((or (not is-tempo)
                                     (not prev-entry)
                                     (= (entry :logtype) *logtype-hold*))
                                 "")
                                ((= (entry :bpm) (prev-entry :bpm))
                                 "⇒")
                                ((< (entry :bpm) (prev-entry :bpm))
                                 "⬀")
                                (else
                                 "⬂"))))
        (define tx (get-sequencer-x-from-time (entry :time) x1 x2))

        (define ty1 (if value->y
                        (value->y entry y1 y2)
                        (scale 100 0 200 y2 y1)))
        
        (define h/2 (/ (- y2 y1) 2))
        
        (define triangle (get-time-triangle (entry :time) this))

        (define triangle-x1 (triangle 0))

        (if (and curr-id
                 (string=? curr-id (entry :uuid)))
            (<gui> :filled-polygon gui curr-entry-color triangle)
            (<gui> :filled-polygon gui entry-color triangle))

        (<gui> :draw-polygon gui "black" triangle 0.7)

        (define text-width (+ 2 (<gui> :text-width text))) ;; Add 2 to avoid scaling font size now and then.
        (define text-x1 (+ h/2 tx))
        (define text-x2 (min max-x2 (+ text-x1 text-width)))

        (define ty ty1) ;;(get-ty (entry :bpm)))
        (define ym (/ (+ y1 y2) 2))
        (define b (/ (get-fontheight) 7))
        (<gui> :filled-box gui "#888888";;*text-color*
               (- tx b) (- ty b)
               (+ tx b) (+ ty b)
               ym ym)
        (<gui> :draw-box gui "black"
               (- tx b) (- ty b)
               (+ tx b) (+ ty b)
               1.4
               ym ym)

        (if (or (<= text-x1 tx) ;;(> text-x2 max-x2)
                (< (- text-x2 text-x1) 5))
            (loop (cdr entries)
                  (- n 1)
                  triangle-x1
                  entry)
            (begin
              ;;(<gui> :filled-box gui "#80010101" tx1 y1 tx2 y2 5 5 #t)
              (<gui> :draw-line gui *text-color*
                     tx ty
                     (- text-x1 2) ym
                     2.0)
              (<gui> :draw-text
                     gui
                     *text-color*
                     ;;"black"
                     text
                     text-x1 y1 text-x2 y2
                     #f ; wrap lines
                     #f ; align left
                     #f ; align top
                     0  ; rotate
                     #f ; cut text to fit
                     #t ; scale font size
                     )
              (loop (cdr entries)
                    (- n 1)
                    triangle-x1
                    entry
                    )))))
    
    ))


(define (bpm->string bpm)
  (if (morally-equal? bpm (floor bpm))
      (<-> (floor bpm))
      (one-decimal-string bpm)))

(define (create-sequencer-tempo-area gui x1 y1 x2 y2)
  (define-optional-func area (key . rest))

  (define (request-tempo x old-tempo callback)
    (<ra> :schedule 0
          (lambda ()
            (define time (get-sequencer-time-from-x x x1 x2))
            (define bpm (<ra> :request-float "BPM:" 1 100000 #t (<-> (if old-tempo (old-tempo :bpm) ""))))
            (and (>= bpm 1)
                 (callback (if old-tempo
                               (copy-hash old-tempo
                                          :time (old-tempo :time)
                                          :bpm bpm
                                          :logtype (old-tempo :logtype))
                               (hash-table :time (get-sequencer-time-from-x x x1 x2)
                                            :bpm bpm
                                            :logtype *logtype-hold*
                                            :uuid (<ra> :create-uuid)
                                            ))))
            #f)))

  (set! area
        (<new> :sequencer-timeline-entry-area gui x1 y1 x2 y2
               :value-type-name "BPM"
               :get-visible-entries (lambda ()
                                      (<ra> :get-sequencer-tempos (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
               :get-all-entries (lambda ()
                                  (if (<ra> :is-using-sequencer-timing)
                                      (<ra> :get-all-sequencer-tempos)
                                      (<ra> :get-sequencer-tempos (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time))))
               :set-all-entries! ra:set-sequencer-tempos
               :popup-menu*
               (lambda (x* curr-tempo pos)
                 (popup-menu
                  (list "----------Tempos")
                  (list "Add tempo..."
                        :enabled (and (not curr-tempo)
                                      (<ra> :is-using-sequencer-timing))
                        (lambda ()
                          (request-tempo x* #f                                         
                                         (lambda (tempo)
                                           (area :add-entry! tempo)))))
                  "-------------------"
                  (list "Remove tempo"
                        :enabled (and curr-tempo
                                      (<ra> :is-using-sequencer-timing))
                        :shortcut ra:simulate-delete-mouse-button
                        (lambda ()
                          (area :remove-curr-entry!)))
                  (list "Set new tempo..."
                        :enabled (and curr-tempo
                                      (<ra> :is-using-sequencer-timing))
                        (lambda ()
                          (request-tempo x* curr-tempo
                                         (lambda (tempo)
                                           (area :replace-curr-entry! tempo)))))
                  (get-sequencer-timing-popup-menu-entries)

                  ;;(list "Glide to next tempo"
                  ;;      :enabled curr-tempo
                  ;;      :check (and curr-tempo
                  ;;                  (not (= *logtype-hold* (curr-tempo :logtype))))
                  ;;      (lambda (doglide)
                  ;;        (area :replace-curr-entry! (copy-hash curr-tempo :logtype (if doglide
                  ;;                                                                      *logtype-linear*
                  ;;                                                                      *logtype-hold*)))))
                  ))
               :get-entry-info-string
               (lambda (tempo n next-tempo percentage-first)
                 (bpm->string (if next-tempo
                                  (scale percentage-first 0 100 (tempo :bpm) (next-tempo :bpm))
                                  (tempo :bpm))))
               :entry-color "#66ff0000"
               :curr-entry-color "#ffff4444"
               :double-click-callback
               (lambda (x* curr-tempo)
                 (if (not (<ra> :is-using-sequencer-timing))
                     (show-editor-timing-warning)
                     (request-tempo x* curr-tempo
                                    (lambda (tempo)
                                      (if curr-tempo
                                          (area :replace-curr-entry! tempo)
                                          (area :add-entry! tempo))))))
               :value->y
               (lambda (tempo y1 y2)
                 (scale (tempo :bpm) 0 200 y2 y1))
               :is-tempo #t
               ))
  area)


(define (create-sequencer-signature-area gui x1 y1 x2 y2)
  (define-optional-func area (key . rest))

  (define (get-entry-info-string signature n _a _b)
    (<-> (signature :numerator)
         "/"
         (signature :denominator)))

  (define (request-signature x old-signature callback)
    (<ra> :schedule 0
          (lambda ()
            (define time (get-sequencer-time-from-x x x1 x2))
            (define ratio (<ra> :request-string "Signature:" #t (if old-signature (get-entry-info-string old-signature 0 #f #f) "")))
            (when (not (string=? "" ratio))
              (define numerator (<ra> :get-numerator-from-ratio-string ratio))
              (define denominator (<ra> :get-denominator-from-ratio-string ratio))
              (if (= 0 numerator)
                  (set! numerator 1))
              (callback (hash-table :time (if old-signature
                                               (old-signature :time)
                                               time)
                                     :numerator numerator
                                     :denominator denominator
                                     :uuid (<ra> :create-uuid)
                                     )))
            #f)))

  (set! area
        (<new> :sequencer-timeline-entry-area gui x1 y1 x2 y2
               :value-type-name "Signature"
               :get-visible-entries (lambda ()
                                      (<ra> :get-sequencer-signatures (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time)))
               :get-all-entries ra:get-all-sequencer-signatures
               :set-all-entries! ra:set-sequencer-signatures
               :popup-menu*
               (lambda (x* curr-signature pos)
                 (popup-menu
                  (list "----------Signatures")
                  (list "Add Signature..."
                        :enabled (and (not curr-signature)
                                      (<ra> :is-using-sequencer-timing))
                        (lambda ()
                          (request-signature x* #f    
                                             (lambda (signature)
                                               (area :add-entry! signature)))))
                  "-------------------"
                  (list "Remove signature"
                        :enabled (and curr-signature
                                      (<ra> :is-using-sequencer-timing))
                        (lambda ()
                          (area :remove-curr-entry!)))
                  (list "Set new signature..."
                        :enabled (and curr-signature
                                      (<ra> :is-using-sequencer-timing))
                        (lambda ()                                      
                          (request-signature x* curr-signature
                                             (lambda (signature)
                                               (area :replace-curr-entry! signature)))))
                  (get-sequencer-timing-popup-menu-entries)                  
                  ))
               :get-entry-info-string get-entry-info-string
               :entry-color "#ccff8800"
               :curr-entry-color "#ffffff00"
               :double-click-callback
               (lambda (x* curr-signature)
                 (if (not (<ra> :is-using-sequencer-timing))
                     (show-editor-timing-warning)
                     (request-signature x* curr-signature
                                        (lambda (signature)
                                          (if curr-signature
                                              (area :replace-curr-entry! signature)
                                              (area :add-entry! signature))))))
               ))

  area)

(define (get-markers-jump-popup-menu-entries)
  (list
   (list "Jump prev marker"
         ra:jump-prev-sequencer-mark)
   (list "Jump next marker"
         ra:jump-next-sequencer-mark)
   (list "Jump to marker #"
         (map (lambda (marker-num)
                (list (<-> marker-num)
                      :shortcut (list ra:jump-to-sequencer-mark marker-num)
                      (lambda ()
                        (<ra> :jump-to-sequencer-mark marker-num))))
              (iota 11)))))

(define (create-sequencer-marker-area gui x1 y1 x2 y2)
  (define-optional-func area (key . rest))

  (define (get-entry-info-string marker n _a _b)
    (if (> n -1)
        (<-> (+ 1 n) ": " (marker :name))
        (marker :name)))

  (define (request-marker x old-marker callback)
    (<ra> :schedule 0
          (lambda ()
            (define time (get-sequencer-time-from-x x x1 x2))
            (define name (<ra> :request-string "Marker:" #t (if old-marker (get-entry-info-string old-marker -1 #f #f) "")))
            (when (not (string=? "" name))
              (callback (hash-table :time (if old-marker
                                               (old-marker :time)
                                               time)
                                     :name name
                                     :uuid (<ra> :create-uuid)
                                     )))
            #f)))

  (define (doubleclick-marker x* curr-marker)
    (define (bail-out)
      (request-marker x* curr-marker
                      (lambda (marker)
                        (if curr-marker
                            (area :replace-curr-entry! marker)
                            (area :add-entry! marker)))))

    ;;(c-display "curr:" (pp curr-marker))
    ;;(pretty-print (to-list (<ra> :get-all-sequencer-markers)))(newline)
    ;;(c-display "hepp")
    
    (let loop ((markers (to-list (<ra> :get-all-sequencer-markers))))
      (if (or (null? markers)
              (null? (cdr markers))
              (not curr-marker))
          (bail-out)
          (let ((marker1 (car markers)))
            (if (equal? curr-marker marker1)
                (let ((marker2 (cadr markers)))
                  (<ra> :set-seqlooping #t (floor (marker1 :time)) (floor (marker2 :time))))
                (loop (cdr markers)))))))

  (set! area
        (<new> :sequencer-timeline-entry-area gui x1 y1 x2 y2
               :value-type-name "Marker"
               :get-visible-entries ra:get-all-sequencer-markers
               :get-all-entries ra:get-all-sequencer-markers
               :set-all-entries! ra:set-sequencer-markers
               :popup-menu*
               (lambda (x* curr-marker pos)
                 (popup-menu
                  (list "----------Markers")
                  (list "Add Marker..."
                        :enabled (not curr-marker)
                        (lambda ()
                          (request-marker x* #f    
                                             (lambda (marker)
                                               (area :add-entry! marker)))))
                  "-------------------"
                  (list "Remove marker"
                        :enabled curr-marker
                        (lambda ()
                          (area :remove-curr-entry!)))
                  (list "Set new marker..."
                        :enabled curr-marker
                        (lambda ()                                      
                          (request-marker x* curr-marker
                                             (lambda (marker)
                                               (area :replace-curr-entry! marker)))))
                  "-------------------"
                  (get-markers-jump-popup-menu-entries)
                  (get-sequencer-timing-popup-menu-entries)
                  ))
               :get-entry-info-string get-entry-info-string
               :entry-color "sequencer_marker_color" ;;"#66004488"
               :curr-entry-color (<gui> :make-color-lighter "sequencer_marker_color" 5)
               :do-grid #t
               :double-click-callback doubleclick-marker
               :is-marker #t
               :can-be-edited-in-sequencer-timing-mode #t
               ))

  area)


#!!
(define (create-sequencer-upper-part-area gui x1 y1 x2 y2 state)
  ;;(c-display "    CREATE SEQUENCER UPPER AREA. State:" (pp state))
  (define (recreate gui x1 y1 x2 y2)
    (create-sequencer-tempo-area gui x1 y1 x2 y2))

  (recreate gui x1 y1 x2 y2))

(when (defined? '*sequencer-upper-part-has-loaded*)
  (define testarea (make-qtarea :width (floor (- (<ra> :get-seqtimeline-area-x2) (<ra> :get-seqtimeline-area-x1))) :height (floor (* 1.2 (get-fontheight)))
                                :sub-area-creation-callback (lambda (gui width height state)
                                                               (create-sequencer-upper-part-area gui 0 0 width height state))))
  (<gui> :set-parent (testarea :get-gui) -1)
  (<gui> :show (testarea :get-gui)))
!!#


(define (paint-sequencer-grid gui x1 y1 x2 y2)
  (when *sequencer-timing-area*
    ;;(c-display "ai!" gui x1 y1 x2 y2)
    (define what-to-iterate #f)

    '(let ((last-x -1))
      (<ra> :iterate-sequencer-time (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time) "beat"
            (lambda (time barnum beatnum linenum)
              (define x (get-sequencer-x-from-time time x1 x2))      
              (if (>= last-x 0)
                  (begin
                    (define dist (- x last-x))
                    (if (> dist 100)
                        (set! what-to-iterate "beat")
                        (set! what-to-iterate "bar"))
                    #f)
                  (begin
                    (set! last-x x)
                    #t)))))

    (define bar-dx (<gui> :text-width "123456890abcdefg"))
    (define beat-dx (/ bar-dx 2))

    (define last-painted-x -10000)
    (define last-bar-num -1)
    (define last-painted-bar-num -1)
    (define last-painted-beat-num -1)

    (define visible-start-time (<ra> :get-sequencer-visible-start-time))

    ;;(<ra> :iterate-sequencer-time (<ra> :get-sequencer-visible-start-time) (<ra> :get-sequencer-visible-end-time) "beat" ;;what-to-iterate
    (<ra> :iterate-sequencer-time 0 (<ra> :get-sequencer-visible-end-time) "beat" ;;what-to-iterate
          (lambda (time barnum beatnum linenum)
            (define x (get-sequencer-x-from-time time x1 x2))

            ;; In a bar, either paint all beat lines, or no beat lines.
            (define doit (and (= barnum last-painted-bar-num)
                              (> last-painted-beat-num 1)))

            ;; Always paint bar line if all beats were painted in previous bar
            (when (and (not doit)
                       (= beatnum 1))
              (if (and (> last-painted-beat-num 1)
                       (= barnum (+ 1 last-painted-bar-num)))
                  (set! doit #t)))

            (when (and (not doit)
                       (= beatnum 1))
              (if (> x (+ last-painted-x bar-dx))
                  (set! doit #t)))

            (when (and (not doit)
                       (= beatnum 2))
              (if (and (= barnum last-painted-bar-num)
                       (> x (+ last-painted-x beat-dx)))
                  (set! doit #t)))
                             
            (when doit
              (when (>= time visible-start-time)
                (if (> beatnum 1)                       
                    (begin
                      (define color (if (= beatnum 1)
                                        "#a0010101"
                                        "#400101ff"))
                      (<gui> :draw-line gui color x y1 x y2 0.5);;(if (= beatnum 1) 1.1 1))
                      )
                    (begin
                      (define x1 (floor x))
                      (define x2 (+ x1 0.6))
                      
                      (<gui> :draw-line gui "#ffffffff" x2 y1 x2 y2 0.7)
                      (<gui> :draw-line gui "black" x1 y1 x1 y2 1.2)
                      )))
              (set! last-painted-bar-num barnum)
              (set! last-painted-beat-num beatnum)
              (set! last-painted-x x))
            #t
            ))))
#!!
(<ra> :iterate-sequencer-time 0 (<ra> :get-sequencer-visible-end-time) "beat" ;;what-to-iterate
      (lambda (time barnum beatnum linenum)
        (if (= 1 beatnum)
            (c-display "bar:" barnum ". time:" time (/ time 44100.0)))
        #t))
!!#

(define (FROM_C-paint-sequencer-grid gui x1 y1 x2 y2)
  (paint-sequencer-grid gui x1 y1 x2 y2))

(define (get-sequencer-timeline-tempo-y2 y1 y2)
  (define show-tempos (<ra> :show-tempos-sequencer-lane))

  (if (not show-tempos)
      y1
      (begin
        (define show-signatures (<ra> :show-signatures-sequencer-lane))
        (define show-markers (<ra> :show-markers-sequencer-lane))

        (cond ((and show-signatures show-markers)
               (scale 0.33 0 1 y1 y2)) ;; all three shown
              ((or show-signatures show-markers)
               (scale 0.5 0 1 y1 y2)) ;; two shown
              (else
               y2))))) ;; only tempo shown.

(define (get-sequencer-timeline-signature-y2 y1 y2)
  (define show-markers (<ra> :show-markers-sequencer-lane))
  
  (if (not show-markers)
      y2
      (begin
        (define show-tempos (<ra> :show-tempos-sequencer-lane))
        (define show-signatures (<ra> :show-signatures-sequencer-lane))

        (cond ((and show-tempos show-signatures)
               (scale 0.66 0 1 y1 y2)) ;; all three
              ((and (not show-tempos) (not show-signatures))
               y1) ;; only show markers
              (else
               (scale 0.5 0 1 y1 y2)))))) ;; one of tempos or signatures is shown

(def-area-subclass (<sequencer-timeline-headers> :gui :x1 :y1 :x2 :y2)
  ;;(c-display "   TYIMELINE-HEADERS. " x1 y1 "/" x2 y2)
  (define tempo-y2 (get-sequencer-timeline-tempo-y2 y1 y2))
  (define signature-y2 (get-sequencer-timeline-signature-y2 y1 y2))

  (if (<ra> :show-tempos-sequencer-lane)
      (add-sub-area-plain! (<new> :text-area gui x1 y1 x2 tempo-y2
                                  :text "Tempos"
                                  :background-color "sequencer_background_color"
                                  :align-right #t
                                  :paint-border #f
                                  )))

  (if (<ra> :show-signatures-sequencer-lane)
      (add-sub-area-plain! (<new> :text-area gui x1 tempo-y2 x2 signature-y2
                                  :text "Signatures"
                                  :background-color "sequencer_background_color"
                                  :align-right #t
                                  :paint-border #f
                                  )))

  (if (<ra> :show-markers-sequencer-lane)
      (add-sub-area-plain! (<new> :text-area gui x1 signature-y2 x2 y2
                                  :text "Markers"
                                  :background-color "sequencer_background_color"
                                  :align-right #t
                                  :paint-border #f
                                  )))
  
  (add-mouse-cycle!
   (lambda (button x* y*)
     (if (= button *right-button*)
         (begin
           (popup-menu
            (get-sequencer-conf-menues))
           'eat-mouse-cycle)
         #f)))
  
  )

(def-area-subclass (<sequencer-timeline-area> :gui :x1 :y1 :x2 :y2)
  (define tempo-y2 (get-sequencer-timeline-tempo-y2 y1 y2))
  (define signature-y2 (get-sequencer-timeline-signature-y2 y1 y2))

  (define show-tempos (<ra> :show-tempos-sequencer-lane))
  (define show-signatures (<ra> :show-signatures-sequencer-lane))
  (define show-markers (<ra> :show-markers-sequencer-lane))

  (assert (or show-tempos show-signatures show-markers))

  (if show-tempos
      (add-sub-area-plain! (create-sequencer-tempo-area gui x1 y1 x2 tempo-y2)))

  (if show-signatures
      (add-sub-area-plain! (create-sequencer-signature-area gui x1 tempo-y2 x2 signature-y2)))

  (if show-markers
      (add-sub-area-plain! (create-sequencer-marker-area gui x1 signature-y2 x2 y2)))

  ;;(c-display "    Timeline-area y1/y2:" y1 y2)
  
  (define-override (paint)
    (<gui> :filled-box gui "sequencer_background_color" x1 y1 x2 y2 0 0 *no-gradient*)

    (for-each (lambda (sub-area)
                (sub-area :paint-background))
              sub-areas)
  
    (define (draw-border y)
      (define y1 (floor y))
      (define y2 (+ y1 0.7))
      (<gui> :draw-line gui "black" x1 y1 x2 y1 1.2)
      (<gui> :draw-line gui "#bbffffff" x1 y2 x2 y2 0.7)
      )


    (if (and show-tempos
             (or show-signatures show-markers))             
        (draw-border tempo-y2))

    (if (and (or show-tempos show-signatures)
             show-markers)
        (draw-border signature-y2))
    
    ;; paint grid
    ;;(paint-sequencer-grid gui x1 y1 x2 y2)
    )
  
  )
                       

(define (create-sequencer-upper-part-area gui x1 y1 x2 y2 state)
  ;;(c-display "    CREATE SEQUENCER UPPER AREA. State:" (pp state))
  (define (recreate gui x1 y1 x2 y2)
    (<new> :sequencer-timeline-area gui x1 y1 x2 y2))

  (recreate gui x1 y1 x2 y2))

(define *temp-test-area* (if (defined? '*temp-test-area*) *temp-test-area* #f))
(when (defined? '*sequencer-upper-part-has-loaded*)
  (when *temp-test-area*
    (c-display "CLOSING" *temp-test-area*)
    (if (<gui> :is-open *temp-test-area*)
        (<gui> :close *temp-test-area*))
    (set! *temp-test-area* #f))
  (define qtarea (make-qtarea :width (floor (- (<ra> :get-seqtimeline-area-x2) (<ra> :get-seqtimeline-area-x1))) :height (floor (* 3.0 (get-fontheight)))
                              :sub-area-creation-callback (lambda (gui width height state)
                                                            (create-sequencer-upper-part-area gui 0 0 width height state))))
  (set! *temp-test-area* (qtarea :get-gui))
  (<gui> :set-parent *temp-test-area* -1)
  (<gui> :show *temp-test-area*))


(define *sequencer-timing-area* #f)

(define (FROM_C-reconfigure-sequencer-timing-part x1 y1 x2 y2)
  ;;(c-display "   Scheme: Reconfiguring right part")

  (define gui (<gui> :get-sequencer-gui))

  (set! *sequencer-timing-area*
        (and (or (<ra> :show-tempos-sequencer-lane)
                 (<ra> :show-signatures-sequencer-lane)
                 (<ra> :show-markers-sequencer-lane))
             (create-sequencer-upper-part-area gui 
                                               x1 y1
                                               x2 y2
                                               #f)))
        
  )



(define *sequencer-upper-part-has-loaded* #t)



(provide 'instruments.scm)

#||
  * Sends should be slightly skewed to the left, so that we see that they go in parallel. Arrows indicating this might help too.

  * What if the last sound object in the sequence isn't connected to the main pipe? How should this be visualized?
    Ardour seems to visualize this with a button under the volume slider ("Master"). (that makes sense)

||#

(define *bus-effect-names* (list "System Reverb"
                                 "System Chorus"
                                 "System Aux 1"
                                 "System Aux 2"
                                 "System Aux 3"))

(define *bus-effect-onoff-names* (map (lambda (bus-effect-name)
                                        (<-> bus-effect-name " On/Off"))
                                      *bus-effect-names*))

(define *instrument-memoized-generation* 0)
(define *use-instrument-memoization* #f)

(define (run-instrument-data-memoized func)
  (set! *use-instrument-memoization* #t)
  (inc! *instrument-memoized-generation* 1)
  (catch #t
         func
         (lambda args
           (set! *use-instrument-memoization* #f)
           (display (ow!))
           (throw (car args)) ;; rethrowing
           #f))
  (set! *use-instrument-memoization* #f))


(define-macro (define-instrument-memoized name&arg . body)
  (let ((args (cdr name&arg))
        (func (gensym "func"))
        (args-name (gensym "args"))
	(memo (gensym "memo"))
        (last-generation (gensym "last-generation")))
    `(define ,(car name&arg)
       (let ((,memo (make-hash-table))
             (,last-generation -1))
	 (lambda ,args-name
           (define (,func ,@args)
             ,@body)
           (if *use-instrument-memoization*
               (begin
                 (when (not (= ,last-generation
                               *instrument-memoized-generation*))
                   (set! ,last-generation *instrument-memoized-generation*)
                   (set! ,memo (make-hash-table)))
                 (or (,memo ,args-name)
                     (set! (,memo ,args-name) (apply ,func ,args-name))))
               (apply ,func ,args-name)))))))


(define (for-all-tracks func)
  (for-each (lambda (blocknum)
              (for-each (lambda (tracknum)
                          (func blocknum tracknum))
                        (iota (<ra> :get-num-tracks blocknum))))
            (iota (<ra> :get-num-blocks))))

(define-instrument-memoized (get-all-midi-instruments)
  (map (lambda (instrument-num)
         (<ra> :get-midi-instrument-id instrument-num))
       (iota (<ra> :get-num-midi-instruments))))
         
(define-instrument-memoized (get-all-audio-instruments)
  (map (lambda (instrument-num)
         (<ra> :get-audio-instrument-id instrument-num))
       (iota (<ra> :get-num-audio-instruments))))

(define-instrument-memoized (get-instrument-from-name name)
  (let loop ((instruments (append (get-all-midi-instruments)
                                  (get-all-audio-instruments))))
    (if (null? instruments)
        #f
        (if (string=? name (<ra> :get-instrument-name (car instruments)))
            (car instruments)
            (loop (cdr instruments))))))

#||
(for-each (lambda (i) (<ra> :set-instrument-effect i "System Solo On/Off" 0)) (get-all-audio-instruments))
||#

(define-instrument-memoized (sort-instruments-by-mixer-position instruments)
  (sort instruments
        (lambda (i1 i2)
          (define x1 (<ra> :get-instrument-x i1))
          (define x2 (<ra> :get-instrument-x i2))
          (define y1 (<ra> :get-instrument-y i1))
          (define y2 (<ra> :get-instrument-y i2))
          (cond ((< y1 y2)
                 #t)
                ((> y1 y2)
                 #f)
                ((< x1 x2)
                 #t)
                (else
                 #f)))))

#||
(fill! (*s7* 'profile-info) #f)
(show-profile)
||#

;; TODO: Around 30% of the time used to create a mixer is spent here. Memoization helped a lot (14X speedup), but there's probably more than can be done.
;; Making "visited" global made the function run around 10% slower.
;; "get-instruments-and-buses-connecting-from-instrument" only uses around 2% of the total time creating a mixer, and it's called from another place as well.
(define g-total-time 0)
(define g-total-num-calls 0)
(define-instrument-memoized (instrument-eventually-connects-to i1 i2)
  (inc! g-total-num-calls 1)
  (define start (time))
  (define ret
    (begin
      (define visited (make-hash-table 16 =))        
      (let loop ((i1 i1))
        (if (visited i1)
            #f
            (begin
              (hash-table-set! visited i1 #t)
              ;;(c-display "  instrument-eventually" (<ra> :get-instrument-name i1) "->" (<ra> :get-instrument-name i2))
              (any? (lambda (to)
                      (if (= to i2)
                          #t
                          (loop to)))
                    (get-instruments-and-buses-connecting-from-instrument i1)))))))
  (set! g-total-time (+ g-total-time (- (time) start)))
  ret)

(define g-total-sort-time 0)
(define-instrument-memoized (sort-instruments-by-mixer-position-and-connections instruments)
  (define start (time))
  (define num-comp 0)
  (define ret
    (sort instruments
          (lambda (i1 i2)
            ;;(c-display "comparing" i1 i2 (length instruments) (inc! num-comp 1))
            (cond ((instrument-eventually-connects-to i1 i2)
                   #t)
                  ((instrument-eventually-connects-to i2 i1)
                   #f)
                  (else
                   (define y1 (<ra> :get-instrument-y i1))
                   (define y2 (<ra> :get-instrument-y i2))
                   (cond ((< y1 y2)
                          #t)
                         ((> y1 y2)
                          #f)
                         (else
                          (define x1 (<ra> :get-instrument-x i1))
                          (define x2 (<ra> :get-instrument-x i2))
                          (if (< x1 x2)
                              #t
                              #f))))))))
  (set! g-total-sort-time (+ g-total-sort-time (- (time) start)))
  ret)



(define g-total-time2 0)
(define-instrument-memoized (get-buses-connecting-from-instrument id-instrument include-0db-buses?)
  (define ret (if (= 0 (<ra> :get-num-output-channels id-instrument))
      '()
      (let loop ((bus-num 0)
                 (bus-onoff-names *bus-effect-onoff-names*)
                 (bus-effect-names *bus-effect-names*))
        (if (null? bus-onoff-names)
            '()
            (let ((rest (loop (1+ bus-num)
                              (cdr bus-onoff-names)
                              (cdr bus-effect-names))))
              (if (and (>= (<ra> :get-instrument-effect id-instrument (car bus-onoff-names)) 0.5)
                       (or include-0db-buses?
                           (> (<ra> :get-instrument-effect id-instrument (car bus-effect-names)) 0)))
                  (cons bus-num rest)
                  rest))))))
  ;;(set! g-total-time2 (+ g-total-time2 (- (time) start)))
  ret)

(define-instrument-memoized (get-buses)
  (map (lambda (bus-num)
         (<ra> :get-audio-bus-id bus-num))
       (iota (length *bus-effect-names*))))

(define-instrument-memoized (get-instruments-connecting-to-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-source-instrument in-connection id-instrument))
       (iota (<ra> :get-num-in-audio-connections id-instrument))))

(define-instrument-memoized (get-instruments-connecting-from-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-dest-instrument in-connection id-instrument))
       (iota (<ra> :get-num-out-audio-connections id-instrument))))

(define-instrument-memoized (get-instruments-and-buses-connecting-from-instrument id-instrument)
  (define start (time))
  (define ret (append (map (lambda (in-connection)
                             (<ra> :get-audio-connection-dest-instrument in-connection id-instrument))
                           (iota (<ra> :get-num-out-audio-connections id-instrument)))
                      (map ra:get-audio-bus-id (get-buses-connecting-from-instrument id-instrument #f))))
  (set! g-total-time2 (+ g-total-time2 (- (time) start)))
  ret)
  
  
(define-instrument-memoized (get-instruments-econnecting-to-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-event-connection-source-instrument in-connection id-instrument))
       (iota (<ra> :get-num-in-event-connections id-instrument))))

(define-instrument-memoized (get-instruments-econnecting-from-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-event-connection-dest-instrument in-connection id-instrument))
       (iota (<ra> :get-num-out-event-connections id-instrument))))

(define-instrument-memoized (get-all-instruments-with-no-input-connections)
  (define buses (get-buses))
  (keep (lambda (id-instrument)
          (and (not (member id-instrument buses))
               (= 0 (<ra> :get-num-in-audio-connections id-instrument))))
        (get-all-audio-instruments)))

(define-instrument-memoized (get-all-instruments-with-at-least-two-input-connections)
  (define buses (get-buses))
  (keep (lambda (id-instrument)
          (and (not (member id-instrument buses))
               (>= (<ra> :get-num-in-audio-connections id-instrument)
                   2)))
        (get-all-audio-instruments)))


(define-instrument-memoized (would-this-create-a-recursive-connection? goal-id id)
  (or (begin
        (define visited (make-hash-table 16 =))
        (let loop ((id id))
          (cond ((visited id)
                 #f)
                ((= goal-id id)
                 #t)
                (else
                 (hash-table-set! visited id #t)
                 (any? loop
                       (get-instruments-and-buses-connecting-from-instrument id))))))
      (begin
        (define visited (make-hash-table 16 =))
        (let loop ((id id))
          (cond ((visited id)
                 #f)
                ((= goal-id id)
                 #t)
                (else
                 (hash-table-set! visited id #t)
                 (any? loop
                       (get-instruments-econnecting-from-instrument id))))))))
        

(define-instrument-memoized (get-all-instruments-that-we-can-send-to from-id)
  (remove (lambda (to-id)
            ;;(c-display "      " (<ra> :get-instrument-name from-id) "->" (<ra> :get-instrument-name to-id)
            ;;           ". has_audio_connection:" (<ra> :has-audio-connection from-id to-id)
            ;;           ". is_recursive:" (would-this-create-a-recursive-connection? from-id to-id))
            (or ;;(<ra> :has-audio-connection from-id to-id)
                (would-this-create-a-recursive-connection? from-id to-id)))
          (get-all-audio-instruments)))

(define (duplicate-connections id-old-instrument id-new-instrument)
  ;; in audio
  (for-each (lambda (from-instrument)
              (<ra> :create-audio-connection
                    from-instrument
                    id-new-instrument
                    (<ra> :get-audio-connection-gain from-instrument id-old-instrument)))
            (get-instruments-connecting-to-instrument id-old-instrument))
  ;; out audio
  (for-each (lambda (to-instrument)
              (<ra> :create-audio-connection
                    id-new-instrument
                    to-instrument
                    (<ra> :get-audio-connection-gain id-old-instrument to-instrument)))
            (get-instruments-connecting-from-instrument id-old-instrument))
  ;; in event
  (for-each (lambda (from-instrument)
              (<ra> :create-event-connection
                    from-instrument
                    id-new-instrument))
            (get-instruments-econnecting-to-instrument id-old-instrument))
  ;; out event
  (for-each (lambda (to-instrument)
              (<ra> :create-event-connection
                    id-new-instrument
                    to-instrument))
            (get-instruments-econnecting-from-instrument id-old-instrument)))

(define (replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
  (for-all-tracks
   (lambda (blocknum tracknum)
     (if (= id-old-instrument (<ra> :get-instrument-for-track tracknum blocknum))
         (<ra> :set-instrument-for-track id-new-instrument tracknum blocknum)))))

(define (replace-instrument-in-mixer id-old-instrument id-new-instrument)
  (define x (+ 0 (<ra> :get-instrument-x id-old-instrument)))
  (define y (+ 0 (<ra> :get-instrument-y id-old-instrument)))
  (<ra> :delete-instrument id-old-instrument)
  (<ra> :set-instrument-position x y id-new-instrument)
  )

;; Called from the outside. 'instrument-description' can be false or empty string.
(define (replace-instrument id-old-instrument description must-have-inputs? must-have-outputs?)
  
  (define (replace description)
    (undo-block
     (lambda ()
       (define patch-name (if (<ra> :instrument-name-was-autogenerated id-old-instrument)
                              ""
                              (<ra> :get-instrument-name id-old-instrument)))
       (define id-new-instrument (<ra> :create-audio-instrument-from-description description patch-name))
       (when (not (= -1 id-new-instrument))
         (<ra> :set-instrument-effect
               id-new-instrument "System Dry/Wet"
               (<ra> :get-instrument-effect id-old-instrument "System Dry/Wet"))
         (<ra> :replace-all-seq-automation id-old-instrument id-new-instrument)
         (duplicate-connections id-old-instrument id-new-instrument)
         (replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
         (replace-instrument-in-mixer id-old-instrument id-new-instrument)
         ))))
    
  (if (<ra> :instrument-is-permanent id-old-instrument)
      (<ra> :show-message "Can not be replaced")
      (if (or (not description)
              (string=? description ""))
          (start-instrument-popup-menu (<ra> :create-new-instrument-conf 0 0 #f #f #t must-have-inputs? must-have-outputs?)
                                       replace))))

;; Called from the outside. 'instrument-description' can be false or empty string.
(define (load-instrument-preset id-instrument instrument-description)
  (if (<ra> :instrument-is-permanent id-instrument)
      (<ra> :show-message "Can not load preset for this instrument")
      (let ((instrument-description (if (or (not instrument-description)
                                            (string=? instrument-description ""))
                                        (<ra> :request-load-preset-instrument-description)
                                        instrument-description)))
        (if (not (string=? instrument-description ""))
            (replace-instrument id-instrument instrument-description #f #f)))))
        

#!!
(<ra> :get-num-audio-instruments)
(define id (<ra> :get-audio-instrument-id 7))
(<ra> :get-instrument-x 17)
(<ra> :get-instrument-y 17)
 
(<ra> :set-instrument-position -80 106 17)
(<ra> :connect-audio-instrument-to-main-pipe 17)
(<ra> :delete-instrument 22)

(let ((descr (<ra> :instrument-description-popup-menu)))
  (define id (<ra> :create-audio-instrument-from-description descr))
  ;;(<ra> :delete-instrument id)
  id
  )

(<ra> :delete-instrument 27)

(<ra> :get-instrument-for-track)

;; delete instrument in track 0:
(begin
  (<ra> :delete-instrument (<ra> :get-instrument-for-track)))

(<ra> :set-instrument-name "heprqereqp3" 25)

(let ((id-old-instrument (<ra> :get-instrument-for-track))
      (descr (<ra> :instrument-description-popup-menu)))
  (replace-instrument id-old-instrument descr)
  ;;(define id-new-instrument (<ra> :create-audio-instrument-from-description descr))
  ;;(<ra> :set-instrument-for-track id-new-instrument)
  id-old-instrument)

(<ra> :get-undo-history)


!!#

;; instrument-id2 can also be list of instrument-ids.
(define (insert-new-instrument-between instrument-id1 instrument-id2 position-at-instrument-1?)
  (assert (or instrument-id1 instrument-id2))
  (if (not instrument-id2)
      (assert position-at-instrument-1?))
  (if (not instrument-id1)
      (assert (not position-at-instrument-1?)))
  (if (list? instrument-id2)
      (assert position-at-instrument-1?))

  (define out-list (cond ((not instrument-id2)
                          '())
                         ((list? instrument-id2)
                          instrument-id2)
                         (else
                          (list instrument-id2))))                       
  
  (define gain-list (map (lambda (out-id)
                           (<ra> :get-audio-connection-gain instrument-id1 out-id))
                         out-list))

  (define has-instrument2 (not (null? out-list)))

  (define instrument-description (<ra> :instrument-description-popup-menu (get-bool instrument-id1) has-instrument2))
  (if (not (string=? "" instrument-description))
      (begin
        (define position-instrument (or (if position-at-instrument-1?
                                            instrument-id1
                                            instrument-id2)
                                        instrument-id2
                                        instrument-id1))
        (define x (+ (<ra> :get-instrument-x position-instrument) 0))
        (define y (+ (<ra> :get-instrument-y position-instrument) 0))
        
        (define do-undo #f)

        (define result
          (undo-block
           (lambda ()
             (define new-instrument (<ra> :create-audio-instrument-from-description instrument-description "" x y))
             (if (not (= -1 new-instrument))
                 (begin
                   
                   (define num-inputs (<ra> :get-num-input-channels new-instrument))
                   (define num-outputs (<ra> :get-num-output-channels new-instrument))
                   
                   (cond ((and instrument-id1
                               (= 0 num-inputs))
                          (<ra> :show-message (<-> "Can not insert instrument named \n\"" (<ra> :get-instrument-name new-instrument) "\"\nsince it has no input channels"))
                          (set! do-undo #t)
                          #f)

                         ((and (= 0 num-outputs)
                               has-instrument2)
                          (<ra> :show-message (<-> "Can not insert instrument named \n\"" (<ra> :get-instrument-name new-instrument) "\"\nsince it has no output channels"))
                          (set! do-undo #t)
                          #f)

                         (else
                          
                          (<ra> :set-instrument-position x y new-instrument #t)
                          
                          (<ra> :undo-mixer-connections)
                          
                          (when instrument-id1
                            (for-each (lambda (to)
                                        (<ra> :delete-audio-connection instrument-id1 to))
                                      out-list)
                            (<ra> :create-audio-connection instrument-id1 new-instrument))
                          
                          (for-each (lambda (out-id gain)
                                      (<ra> :create-audio-connection new-instrument out-id gain))
                                    out-list
                                    gain-list)
                          
                          new-instrument)))
                 
                 #f))))

        (if do-undo
            (<ra> :undo))

        result)
      
      #f))



(define (set-random-sample-for-all-selected-sampler-instruments)
  (undo-block (lambda ()
                (for-each (lambda (id-instrument)
                            (if (string=? "Sample Player" (<ra> :get-instrument-type-name id-instrument))
                                (<ra> :set-random-instrument-sample id-instrument)))
                          (<ra> :get-selected-instruments)))))


#||
;; Use ra:create-new-instrument-conf instead.
(define-struct new-instrument-conf
  :x 0
  :y 0
  :connect-to-main-pipe #t
  :do-autoconnect #f
  :include-load-preset #t
  :must-have-inputs #f
  :must-have-outputs #f)
||#


;; The callback takes an instrument description as argument
(define (spr-entry->instrument-description entry instrconf callback)
  (define type (entry :type))
  (cond ((string=? type "NORMAL")
         (callback (<-> "1"
                        (<ra> :to-base64 (entry :type-name))
                        ":"
                        (<ra> :to-base64 (entry :name)))))
        ((string=? type "CONTAINER")
         (define new-entries (<ra> :populate-plugin-container entry))
         ;;(c-display "new-entries:" (pp new-entries))
         (cond ((= 1 (length new-entries))
                (spr-entry->instrument-description (first new-entries) instrconf callback))
               ((> (length new-entries) 1)
                (popup-menu (spr-entries->menu-entries new-entries instrconf
                                                       (lambda (entry)
                                                         (spr-entry->instrument-description entry instrconf callback))
                                                       #f)))))
        ;;(else
        ;;        (<ra> :show-message (<-> "The \"" (entry :name) "\" plugin container didn't contain any plugins"))))) ;; The populate function shows error message.
        ((string=? type "LOAD_PRESET")
         (define (use-file-requester)
           (<ra> :request-load-preset-instrument-description callback))
         (define all-presets (to-list (<ra> :get-all-presets-in-path)))
         ;;(c-display "all-presets" all-presets)
         (if (null? all-presets)
             (use-file-requester)
             (popup-menu (list "Select preset from a different directory"
                               use-file-requester)
                         "------------"
                         (map (lambda (base64-name)
                                (list (<ra> :from-base64 base64-name)
                                      (lambda ()
                                        (callback (<-> "2" base64-name)))))
                              all-presets))))
        ((string=? type "PASTE_PRESET")         
         (callback "3"))
        ((string=? type "NUM_USED_PLUGIN")
         (callback (<-> "4"
                        (<ra> :to-base64 (entry :container-name))
                        ":"
                        (<ra> :to-base64 (entry :type-name))
                        ":"
                        (<ra> :to-base64 (entry :name)))))
        (else
         ;;(c-display "entry:" entry)
         ;;(assert #f)
         )))


;; The callback is a function that takes an spr entry as argument
(define (spr-entries->menu-entries entries instrconf callback level-down-func)

  (let loop ((entries (to-list entries)))
    (if (null? entries)
        '()
        (begin
          (define entry (car entries))
          (define type (entry :type))
          (define mcallback (lambda ()
                              (callback entry)))
          ;;(c-display entry)
          (cond ((string=? type "NORMAL")
                 (cons (list (entry :name)
                             :enabled (and (or (not (instrconf :must-have-inputs))
                                               (> (entry :num-inputs) 0))
                                           (or (not (instrconf :must-have-outputs))
                                               (> (entry :num-outputs) 0)))
                             mcallback)
                       (loop (cdr entries))))
                ((string=? type "CONTAINER")
                 (cons (list (entry :name)
                             mcallback)
                       (loop (cdr entries))))
                ((string=? type "SEPARATOR")
                 (cons "-------"
                       (loop (cdr entries))))
                ((string=? type "LEVEL_UP")                 
                 (define rest #f)
                 (cons (list (entry :name)
                             (spr-entries->menu-entries (cdr entries) instrconf callback
                                                         (lambda (dasrest)
                                                           (set! rest dasrest))))
                       (loop rest)))
                ((string=? type "LEVEL_DOWN")
                 (assert level-down-func)
                 (level-down-func (cdr entries))
                 '())
                ((string=? type "LOAD_PRESET")
                 (cons (list "Load Preset(s)"
                             :enabled (instrconf :include-load-preset)
                             mcallback)
                       (loop (cdr entries))))
                ((string=? type "PASTE_PRESET")
                 (cons (list "Paste sound objects(s)"
                             :enabled (and (instrconf :include-load-preset)
                                           (<ra> :instrument-preset-in-clipboard))
                             mcallback)
                       (loop (cdr entries))))
                ((string=? type "NUM_USED_PLUGIN")
                 (cons (list (<-> (if (string-starts-with? (entry :name) "STK")
                                      (<-> "STK:"
                                           (substring (entry :name) 3))
                                      (<-> (entry :type-name) ": " (entry :name)))
                                  " (" (entry :num-uses) ")")
                             mcallback)
                       (loop (cdr entries))))
                (else
                 (<ra> :show-warning (<-> "spr-entries->menu-entries: UNHANDLED sound plugin registry entry: " (pp entry)))
                 (loop (cdr entries))))))))


;; async
(define (start-instrument-popup-menu instrconf callback)
  (popup-menu (append (list "Plugin Manager"
                            (lambda ()
                              (pmg-start instrconf callback))
                            "--------------")
                      (spr-entries->menu-entries (<ra> :get-sound-plugin-registry)
                                                 instrconf
                                                 (lambda (entry)
                                                   (spr-entry->instrument-description entry instrconf callback))
                                                 #f))))

(define (create-instrument instrconf description)
  (undo-block
   (lambda ()
     (let ((instrument-id (<ra> :create-audio-instrument-from-description description "" (instrconf :x) (instrconf :y))))
       (when (and (integer? instrument-id)
                  (not (= -1 instrument-id)))  
         (c-display (pp instrconf))
         (if (and (instrconf :connect-to-main-pipe)
                  (> (<ra> :get-num-output-channels instrument-id) 0))
             (<ra> :connect-audio-instrument-to-main-pipe instrument-id))
         (if (and (instrconf :do-autoconnect)
                  (> (<ra> :get-num-output-channels instrument-id) 0)
                  (> (<ra> :get-num-input-channels instrument-id) 0))
             (<ra> :autoconnect-instrument instrument-id
                   (instrconf :x)
                   (instrconf :y))))))))

(define (create-instrument-popup-menu instrconf)
  (start-instrument-popup-menu instrconf (lambda (description)
                                           (create-instrument instrconf description))))

                                 
#!!
(create-instrument-popup-menu (<ra> :create-new-instrument-conf))
!!#

                                  

;; async
(define (select-track-instrument tracknum)
  (undo-block
   (lambda ()
     (define midi-instruments (get-all-midi-instruments))
     (define instruments-before (get-all-audio-instruments))

     (define (is-new-instrument? id-instrument)
       (and (not (member id-instrument instruments-before))
            (member id-instrument (get-all-audio-instruments))))
     
     (define (num-new-instruments)
       (- (length (get-all-audio-instruments))
          (length instruments-before)))
     
     (define (load id-instrument)       
       (when (and (integer? id-instrument) (not (= -1 id-instrument)))
         (<ra> :set-instrument-for-track id-instrument tracknum)
         (when (and (is-new-instrument? id-instrument)
                    (= 1 (num-new-instruments)))
           (<ra> :autoposition-instrument id-instrument)
           (<ra> :connect-audio-instrument-to-main-pipe id-instrument))))
       
     (popup-menu
      "<New MIDI Instrument>" (lambda ()
                                (load (<ra> :create-midi-instrument "Unnamed")))
      "<New Sample Player>" (lambda ()
                              (load (<ra> :create-audio-instrument "Sample Player" "Sample Player")))
      "<New FluidSynth>" (lambda ()
                           (load (<ra> :create-audio-instrument "FluidSynth" "FluidSynth")))
      (if (<ra> :has-pure-data)
          (list "<New Pd Instrument>" (lambda ()
                                        (load (<ra> :create-audio-instrument "Pd" "Simple Midi Synth"))))
          #f)
      "<New Audio Instrument>" (lambda ()
                                 (start-instrument-popup-menu #t #f #f
                                                              (lambda (descr)
                                                                (load (<ra> :create-audio-instrument-from-description descr)))))
      "<Load New Preset>" (lambda ()
                            (load (<ra> :create-audio-instrument-from-description
                                        (<ra> :request-load-preset-instrument-description))))
      "----------"
      "Clone Audio Instrument" (map (lambda (num instrument-id)
                                      (if (<ra> :instrument-is-permanent instrument-id)
                                          #f
                                          (list (<-> num ". " (<ra> :get-instrument-name instrument-id))
                                                (lambda ()
                                                  (load (<ra> :clone-audio-instrument instrument-id))))))
                                    (iota (length instruments-before))
                                    instruments-before)
      (and (> (length midi-instruments) 0)
           (list "----------"
                 (map (lambda (num instrument-id)
                        (list (<-> num ". " (<ra> :get-instrument-name instrument-id))                     
                              (lambda ()
                                (load instrument-id))))
                      (iota (length midi-instruments))
                      midi-instruments)))
      "----------"
      (map (lambda (num instrument-id)
             (list (<-> num ". " (<ra> :get-instrument-name instrument-id))                     
                   (lambda ()
                     (load instrument-id))))
           (iota (length instruments-before))
           instruments-before)))))
     
#||
(select-track-instrument 0)
||#


(define (show/hide-instrument-gui)
  (let ((id (ra:get-current-instrument)))
    (when (and (not (= -1 id)) (ra:has-native-instrument-gui id))
      (if (ra:instrument-gui-is-visible id)
          (ra:hide-instrument-gui id)
          (ra:show-instrument-gui id)))))

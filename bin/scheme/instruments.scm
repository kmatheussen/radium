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

(define (for-all-tracks func)
  (for-each (lambda (blocknum)
              (for-each (lambda (tracknum)
                          (func blocknum tracknum))
                        (iota (<ra> :get-num-tracks blocknum))))
            (iota (<ra> :get-num-blocks))))

(define (get-all-midi-instruments)
  (map (lambda (instrument-num)
         (<ra> :get-midi-instrument-id instrument-num))
       (iota (<ra> :get-num-midi-instruments))))
         
(define (get-all-audio-instruments)
  (map (lambda (instrument-num)
         (<ra> :get-audio-instrument-id instrument-num))
       (iota (<ra> :get-num-audio-instruments))))

(define (get-instrument-from-name name)
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

(define (sort-instruments-by-mixer-position instruments)
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

(define (instrument-eventually-connects-to i1 i2)
  ;;(c-display "  instrument-eventually" (<ra> :get-instrument-name i1) "->" (<ra> :get-instrument-name i2))
  (any? (lambda (to)
          (if (= to i2)
              #t
              (instrument-eventually-connects-to to i2)))
        (get-instruments-and-buses-connecting-from-instrument i1)))

(define (sort-instruments-by-mixer-position-and-connections instruments)
  (sort instruments
        (lambda (i1 i2)
          (cond ((instrument-eventually-connects-to i1 i2)
                 #t)
                ((instrument-eventually-connects-to i2 i1)
                 #f)
                (else
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
                        #f)))))))
  
  
(define (get-buses-connecting-from-instrument id-instrument include-0db-buses?)
  (if (= 0 (<ra> :get-num-output-channels id-instrument))
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

(define (get-buses)
  (map (lambda (bus-num)
         (<ra> :get-audio-bus-id bus-num))
       (iota (length *bus-effect-names*))))

(define (get-instruments-connecting-to-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-source-instrument in-connection id-instrument))
       (iota (<ra> :get-num-in-audio-connections id-instrument))))

(define (get-instruments-connecting-from-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-dest-instrument in-connection id-instrument))
       (iota (<ra> :get-num-out-audio-connections id-instrument))))

(define (get-instruments-and-buses-connecting-from-instrument id-instrument)
  (append (map (lambda (in-connection)
                 (<ra> :get-audio-connection-dest-instrument in-connection id-instrument))
               (iota (<ra> :get-num-out-audio-connections id-instrument)))
          (map ra:get-audio-bus-id (get-buses-connecting-from-instrument id-instrument #f))))
  
(define (get-instruments-econnecting-to-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-event-connection-source-instrument in-connection id-instrument))
       (iota (<ra> :get-num-in-event-connections id-instrument))))

(define (get-instruments-econnecting-from-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-event-connection-dest-instrument in-connection id-instrument))
       (iota (<ra> :get-num-out-event-connections id-instrument))))

(define (get-all-instruments-with-no-input-connections)
  (define buses (get-buses))
  (keep (lambda (id-instrument)
          (and (not (member id-instrument buses))
               (= 0 (<ra> :get-num-in-audio-connections id-instrument))))
        (get-all-audio-instruments)))

(define (get-all-instruments-with-at-least-two-input-connections)
  (define buses (get-buses))
  (keep (lambda (id-instrument)
          (and (not (member id-instrument buses))
               (>= (<ra> :get-num-in-audio-connections id-instrument)
                   2)))
        (get-all-audio-instruments)))


(define (would-this-create-a-recursive-connection? goal-id id)
  (if (= goal-id id)
      #t
      (any? (lambda (id)
              (would-this-create-a-recursive-connection? goal-id id))
            (get-instruments-and-buses-connecting-from-instrument id))))
        

(define (get-all-instruments-that-we-can-send-to from-id)
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
(define (replace-instrument id-old-instrument instrument-description must-have-inputs? must-have-outputs?)
  (if (<ra> :instrument-is-permanent id-old-instrument)
      (<ra> :show-message "Can not be replaced")
      (let ((instrument-description (if (or (not instrument-description)
                                            (string=? instrument-description ""))
                                        (<ra> :instrument-description-popup-menu must-have-inputs? must-have-outputs?)
                                        instrument-description)))
        (when (not (string=? "" instrument-description))
          (undo-block
           (lambda ()
             (define patch-name (if (<ra> :instrument-name-was-autogenerated id-old-instrument)
                                    ""
                                    (<ra> :get-instrument-name id-old-instrument)))
             (define id-new-instrument (<ra> :create-audio-instrument-from-description instrument-description patch-name))
             (when (not (= -1 id-new-instrument))
               (<ra> :set-instrument-effect
                     id-new-instrument "System Dry/Wet"
                     (<ra> :get-instrument-effect id-old-instrument "System Dry/Wet"))
               (<ra> :replace-all-seq-automation id-old-instrument id-new-instrument)
               (duplicate-connections id-old-instrument id-new-instrument)
               (replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
               (replace-instrument-in-mixer id-old-instrument id-new-instrument)
               )))))))

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






;; Called from the outside. if 'do-autoconnect' is true, instruments sending sound to id-instrument and instruments getting sound from id-instrument will be connected.
;;(define (delete-instrument id-instrument do-autoconnect)
;;  ...)


(define (select-track-instrument tracknum)
  (undo-block
   (lambda ()
     (define midi-instruments (get-all-midi-instruments))
     (define instruments-before (get-all-audio-instruments))
     (define id-instrument
       (popup-menu-sync
        "<New MIDI Instrument>" (lambda ()
                                  (<ra> :create-midi-instrument "Unnamed"))
        "<New Sample Player>" (lambda ()
                                (<ra> :create-audio-instrument "Sample Player" "Sample Player"))
        "<New FluidSynth>" (lambda ()
                             (<ra> :create-audio-instrument "FluidSynth" "FluidSynth"))
        (if (<ra> :has-pure-data)
            (list "<New Pd Instrument>" (lambda ()
                                          (<ra> :create-audio-instrument "Pd" "Simple Midi Synth")))
            #f)
        "<New Audio Instrument>" (lambda ()
                                   (<ra> :create-audio-instrument-from-description 
                                         (<ra> :instrument-description-popup-menu)))
        "<Load New Preset>" (lambda ()
                              (<ra> :create-audio-instrument-from-description
                                    (<ra> :request-load-preset-instrument-description)))
        "----------"
        "Clone Audio Instrument" (map (lambda (num instrument-id)
                                        (if (<ra> :instrument-is-permanent instrument-id)
                                            #f
                                            (list (<-> num ". " (<ra> :get-instrument-name instrument-id))
                                                  (lambda ()
                                                    (<ra> :clone-audio-instrument instrument-id)))))
                                      (iota (length instruments-before))
                                      instruments-before)
        (and (> (length midi-instruments) 0)
             (list "----------"
                   (map (lambda (num instrument-id)
                          (list (<-> num ". " (<ra> :get-instrument-name instrument-id))                     
                                (lambda ()
                                  instrument-id)))
                        (iota (length midi-instruments))
                        midi-instruments)))
        "----------"
        (map (lambda (num instrument-id)
               (list (<-> num ". " (<ra> :get-instrument-name instrument-id))                     
                     (lambda ()
                       instrument-id)))
             (iota (length instruments-before))
             instruments-before)))
     
     (define (is-new-instrument? id-instrument)
       (and (not (member id-instrument instruments-before))
            (member id-instrument (get-all-audio-instruments))))

     (define (num-new-instruments)
       (- (length (get-all-audio-instruments))
          (length instruments-before)))
     
     (when (and (integer? id-instrument) (not (= -1 id-instrument)))
       (<ra> :set-instrument-for-track id-instrument tracknum)
       (when (and (is-new-instrument? id-instrument)
                  (= 1 (num-new-instruments)))
         (<ra> :autoposition-instrument id-instrument)
         (<ra> :connect-audio-instrument-to-main-pipe id-instrument))))))
#||
(select-track-instrument 0)
||#


(define (show/hide-instrument-gui)
  (let ((id (ra:get-current-instrument)))
    (when (and (not (= -1 id)) (ra:has-native-instrument-gui id))
      (if (ra:instrument-gui-is-visible id)
          (ra:hide-instrument-gui id)
          (ra:show-instrument-gui id)))))

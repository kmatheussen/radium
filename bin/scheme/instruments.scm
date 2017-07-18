(provide 'instruments.scm)


(define (get-instrument-volume-on/off-effect-name instrument-id)
  (if (= 0 (<ra> :get-num-output-channels instrument-id))
      "System In On/Off"
      "System Volume On/Off"))
  

(define-constant *bus-effect-names* (list "System Reverb"
                                          "System Chorus"
                                          "System Aux 1"
                                          "System Aux 2"
                                          "System Aux 3"))

(define-constant *bus-effect-onoff-names* (map (lambda (bus-effect-name)
                                                 (<-> bus-effect-name " On/Off"))
                                               *bus-effect-names*))

(define *instrument-memoized-generation* 0)
(define *use-instrument-memoization* #f)

(define (run-instrument-data-memoized func)
  (try-finally :try (lambda ()
                      (set! *use-instrument-memoization* #t)
                      (inc! *instrument-memoized-generation* 1)
                      (func))
               :finally (lambda ()
                          (set! *use-instrument-memoization* #f))))


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


(delafina (create-audio-connection-change :type
                                          :source
                                          :target
                                          :gain #f)
  (assert (or (string=? type "connect")
              (string=? type "disconnect")))
  (assert (integer? source))
  (assert (integer? target))
  (if (string=? type "disconnect")
      (assert (not gain))
      (assert (or (not gain)
                  (number? gain))))  
  (hash-table* :type type :source source :target target :gain gain))

(define-macro (push-audio-connection-change! changes rest)
  `(push-back! ,changes (create-audio-connection-change ,@(cdr rest))))

#!!
(macroexpand (push-audio-connection-change! changes (list :type "connect"
                                                          :source from-instrument
                                                          :target id-new-instrument
                                                          :gain (<ra> :get-audio-connection-gain from-instrument id-old-instrument))))

!!#
               
               
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
              ;;(c-display "  instrument-eventually" (<ra> :get-instrument-name i1) "->" (<ra> :get-instrument-name i2) ":" (any? (lambda (to)
              ;;                                                                                                                    (if (= to i2)
              ;;                                                                                                                        #t
              ;;                                                                                                                        (loop to)))
              ;;                                                                                                                  (get-instruments-and-buses-connecting-from-instrument i1)))
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
  
  (define container (<new> :container instruments))
  (define done (make-hash-table (max 1 (container :length)) =))
  
  ;; topological sort, plus keep as much as possible of the order
  ;; from 'sort-instruments-by-mixer-position'.
  (define ret (let loop ((ids (sort-instruments-by-mixer-position instruments)))
                ;;(c-display "...ids: " ids ", done:" done)
                (if (null? ids)
                    '()
                    (let ((id (car ids)))
                      (if (done id)
                          (loop (cdr ids))
                          (let ((ids-positioned-before (keep (lambda (id)
                                                               (and (container :contains id)
                                                                    (not (done id))))
                                                             (get-instruments-connecting-to-instrument id))))
                            ;;(c-display "id:" id ", pos-before:" ids-positioned-before)
                            (if (null? ids-positioned-before)
                                (begin
                                  (set! (done id) #t)
                                  (cons id (loop (cdr ids))))
                                (loop (append (sort-instruments-by-mixer-position ids-positioned-before)
                                              ids)))))))))
  (set! g-total-sort-time (+ g-total-sort-time (- (time) start)))
  ret)

#!!
;; blowfish.rad. 29 must be placed before 30.
(let* ((cat-instruments (<new> :get-cat-instruments))
       (insts (cat-instruments :instrument-instruments)));;(list (car (cat-instruments :instrument-instruments)) (last (cat-instruments :instrument-instruments)))))
  (for-each (lambda (inst)              
              (c-display (ra:get-instrument-name inst) ":" inst))
            insts)
  (c-display "bef:" insts)
  (c-display "aft:" (sort-instruments-by-mixer-position-and-connections insts)))
(instrument-eventually-connects-to 29 30)
!!#


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
          (and (not (memv id-instrument buses))
               (= 0 (<ra> :get-num-in-audio-connections id-instrument))))
        (get-all-audio-instruments)))

(define-instrument-memoized (get-all-instruments-with-at-least-two-input-connections)
  (define buses (get-buses))
  (keep (lambda (id-instrument)
          (and (not (memv id-instrument buses))
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


;; Finds the next plugin in a plugin path. 'instrument-id' is the plugin to start searching from.
(define-instrument-memoized (find-next-plugin-instrument-in-path instrument-id)
  (let loop ((out-instruments (reverse (sort-instruments-by-mixer-position
                                        (get-instruments-connecting-from-instrument instrument-id)))))
    (if (null? out-instruments)
        #f
        (let ((out-instrument (car out-instruments)))
          (if (= 1 (<ra> :get-num-in-audio-connections out-instrument))
              out-instrument
              (loop (cdr out-instruments)))))))

                                              
(define-instrument-memoized (find-all-plugins-used-in-mixer-strip instrument-id)
  (define next (find-next-plugin-instrument-in-path instrument-id))
  (if next
      (cons next
            (find-all-plugins-used-in-mixer-strip next))
      '()))

(define-instrument-memoized (find-all-nonbus-plugins-used-in-mixer-strip instrument-id)
  (define next (find-next-plugin-instrument-in-path instrument-id))
  (if (and next
           (not (memv next (get-buses))))
      (cons next
            (find-all-plugins-used-in-mixer-strip next))
      '()))

;; Returns a list of parallel plugins that needs their own mixer strip.
(define-instrument-memoized (get-returned-plugin-buses instrument-id)
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



(<define-class-with-custom-definer> define-instrument-memoized (<get-cat-instruments>)
  (define instruments '())
  (define no-inputs-or-outputs '())

  (define pure-buses (get-buses))

  (for-each (lambda (id)
              (if (or (> (<ra> :get-num-input-channels id)
                         0)
                      (> (<ra> :get-num-output-channels id)
                         0))
                  (push! instruments id)
                  (push! instruments-no-inputs-or-outputs id)))
            (get-all-instruments-with-no-input-connections))

  (define instrument-plugin-buses (apply append (map (lambda (instrument-id)
                                                       (get-returned-plugin-buses instrument-id))
                                                     instruments)))

  (define buses (append (get-all-instruments-with-at-least-two-input-connections)
                        pure-buses))
  
  (define buses-plugin-buses (apply append (map (lambda (instrument-id)
                                                  (get-returned-plugin-buses instrument-id))
                                                (append buses
                                                        instrument-plugin-buses))))
  
  (define all-buses (<new> :container
                           (append instrument-plugin-buses ;; Not sure if there could be duplicates here.
                                   buses
                                   buses-plugin-buses)
                           =))

  (define instrument-plugins (apply append (map find-all-nonbus-plugins-used-in-mixer-strip instruments)))

  (define buses-plugins (apply append (map find-all-plugins-used-in-mixer-strip (all-buses :list))))

  (define all-instrument-instruments (append no-inputs-or-outputs
                                             instruments
                                             instrument-plugins))

  (define all-bus-instruments (append (all-buses :list)
                                      buses-plugins))
  
  (set! instruments (<new> :container instruments =))
  (set! no-inputs-or-outputs (<new> :container no-inputs-or-outputs =))
  
  :no-input-or-outputs ()
  (no-inputs-or-outputs :list)

  :instruments ()
  (instruments :list)

  :instrument-plugins ()
  instrument-plugins

  :buses ()
  (all-buses :list)

  :buses-plugins
  ()
  buses-plugins

  :instrument-instruments ()
  all-instrument-instruments

  :bus-instruments ()
  all-bus-instruments

  :is-bus? (id)
  (all-buses :contains id)

  :is-instrument? (id)
  (instruments :contains id)

  :has-no-inputs-or-outputs? (id)
  (no-inputs-or-outputs :contains id))


(define (move-connections-to-new-instrument id-old-instrument id-new-instrument)
  (define changes '())
  
  ;; in audio
  (for-each (lambda (from-instrument)
              (push-audio-connection-change! changes (list :type "connect"
                                                           :source from-instrument
                                                           :target id-new-instrument
                                                           :gain (<ra> :get-audio-connection-gain from-instrument id-old-instrument)))
              (push-audio-connection-change! changes (list :type "disconnect"
                                                           :source from-instrument
                                                           :target id-old-instrument)))
            (get-instruments-connecting-to-instrument id-old-instrument))
  ;; out audio
  (for-each (lambda (to-instrument)
              (push-audio-connection-change! changes (list :type "connect"
                                                           :source id-new-instrument
                                                           :target to-instrument
                                                           :gain (<ra> :get-audio-connection-gain id-old-instrument to-instrument)))
              (push-audio-connection-change! changes (list :type "disconnect"
                                                           :source id-old-instrument
                                                           :target to-instrument)))
            (get-instruments-connecting-from-instrument id-old-instrument))

  (<ra> :change-audio-connections changes) ;; Apply all changes simultaneously
  
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

#||
(define (replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
  (for-all-tracks
   (lambda (blocknum tracknum)
     (if (= id-old-instrument (<ra> :get-instrument-for-track tracknum blocknum))
         (<ra> :set-instrument-for-track id-new-instrument tracknum blocknum)))))
||#

(define (replace-instrument-in-mixer id-old-instrument id-new-instrument)
  (define x (+ 0 (<ra> :get-instrument-x id-old-instrument)))
  (define y (+ 0 (<ra> :get-instrument-y id-old-instrument)))
  (<ra> :delete-instrument id-old-instrument)
  (<ra> :set-instrument-position x y id-new-instrument)
  )

;; Called from the outside. 'instrument-description' can be false or empty string.
;; Async. Returns immediately.
(define (async-replace-instrument id-old-instrument description instrconf)

  (define (replace description)
    (if (not (string=? "" description))
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
             ;;(<ra> :replace-all-seq-automation id-old-instrument id-new-instrument)
             (<ra> :replace-use-of-instrument id-old-instrument id-new-instrument)
             (<ra> :undo-mixer-connections)
             (move-connections-to-new-instrument id-old-instrument id-new-instrument)
             ;;(replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
             (replace-instrument-in-mixer id-old-instrument id-new-instrument)
             )))))
    
  (if (<ra> :instrument-is-permanent id-old-instrument)
      (<ra> :show-async-message (instrconf :parentgui) "Can not be replaced")
      (if (or (not description)
              (string=? description ""))
          (start-instrument-popup-menu instrconf replace)
          (<ra> :schedule 0 ;; We do this since the function is specified to return immediately. I.e. the caller expects the instrument configuration to be the same when we return.
                (lambda ()
                  (replace description)
                  #f)))))

(define (request-select-instrument-preset parentgui callback)
  (define (use-file-requester)
    (<ra> :request-load-preset-instrument-description parentgui callback))
  (define single-presets (to-list (<ra> :get-all-single-presets-in-path)))
  (define multi-presets (to-list (<ra> :get-all-multi-presets-in-path)))
  ;;(c-display "all-presets" all-presets)
  (if (and (null? single-presets)
           (null? multi-presets))
      (use-file-requester)
      (popup-menu (list "Select preset from a different directory"
                        use-file-requester)
                  "------------"
                  (let* ((create-entries (lambda (presets)
                                           (map (lambda (base64-name)
                                                  (list (<ra> :from-base64 base64-name)
                                                        (lambda ()
                                                          (callback (<-> "2" base64-name)))))
                                                presets)))
                         (single-entries (create-entries single-presets))
                         (multi-entries (create-entries multi-presets)))
                    (if (or (null? single-entries)
                            (null? multi-entries))                               
                        (append single-entries multi-entries)
                        (append single-entries (list "--------") multi-entries))))))


;; Called from the outside. 'instrument-description' can be false or empty string.
;; Async. Returns immediately.
(define (async-load-instrument-preset id-instrument instrument-description parentgui)
  (if (<ra> :instrument-is-permanent id-instrument)
      (<ra> :show-async-message parentgui "Can not load preset for this instrument")
      (let ((gotit (lambda (instrument-description)
                     (if (not (string=? instrument-description ""))
                         (async-replace-instrument id-instrument instrument-description (make-instrument-conf :must-have-inputs #f :must-have-outputs #f :parentgui parentgui))))))
        (if (or (not instrument-description)
                (string=? instrument-description ""))
            (request-select-instrument-preset parentgui gotit)
            (gotit instrument-description)))))


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
  (async-replace-instrument id-old-instrument descr)
  ;;(define id-new-instrument (<ra> :create-audio-instrument-from-description descr))
  ;;(<ra> :set-instrument-for-track id-new-instrument)
  id-old-instrument)

(<ra> :get-undo-history)


!!#

;; instrument-id2 can also be list of instrument-ids.
(define (insert-new-instrument-between instrument-id1 instrument-id2 position-at-instrument-1? parentgui callback)
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

  (start-instrument-popup-menu
   (make-instrument-conf :must-have-inputs (get-bool instrument-id1)
                         :must-have-outputs has-instrument2
                         :parentgui parentgui)
   (lambda (instrument-description)
     (define position-instrument (or (if position-at-instrument-1?
                                         instrument-id1
                                         instrument-id2)
                                     instrument-id2
                                     instrument-id1))
     (define x (+ (<ra> :get-instrument-x position-instrument) 0))
     (define y (+ (<ra> :get-instrument-y position-instrument) 0))
        
     (define do-undo #f)

     ;; Quite clumsy. The reason we don't call the callback directly is that we don't want to call the callback inside the undo block.
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
                       (<ra> :show-async-message parentgui (<-> "Can not insert instrument named \n\"" (<ra> :get-instrument-name new-instrument) "\"\nsince it has no input channels"))
                       (set! do-undo #t)
                       #f)
                      
                      ((and (= 0 num-outputs)
                            has-instrument2)
                       (<ra> :show-async-message parentgui (<-> "Can not insert instrument named \n\"" (<ra> :get-instrument-name new-instrument) "\"\nsince it has no output channels"))
                       (set! do-undo #t)
                       #f)
                      
                      (else
                       
                       (<ra> :set-instrument-position x y new-instrument #t)

                       (define changes '())
                       
                       (<ra> :undo-mixer-connections)
                       
                       (when instrument-id1
                         (for-each (lambda (to)
                                     (push-audio-connection-change! changes (list :type "disconnect"
                                                                                  :source instrument-id1
                                                                                  :target to)))
                                   out-list)
                         (push-audio-connection-change! changes (list :type "connect"
                                                                      :source instrument-id1
                                                                      :target new-instrument)))
                       
                       (for-each (lambda (out-id gain)
                                   (push-audio-connection-change! changes (list :type "connect"
                                                                                :source new-instrument
                                                                                :target out-id
                                                                                :gain gain)))
                                 out-list
                                 gain-list)

                       (<ra> :change-audio-connections changes) ;; Apply all changes simultaneously
                       
                       new-instrument)))
              #f))))

     (if do-undo
         (<ra> :undo))

     (if result
         (callback result)))))

     



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

;; Always use this one, and not ra:create-new-instrument-conf, to improve caching, and avoid too many places to fix code if changing the arguments for ra:create-new-instrument-conf.
(delafina (make-instrument-conf :x 0
                                :y 0
                                :connect-to-main-pipe #f
                                :do-autoconnect #f
                                :include-load-preset #t
                                :must-have-inputs #f
                                :must-have-outputs #f
                                :parentgui -2)
          (<ra> :create-new-instrument-conf
                x y
                connect-to-main-pipe
                do-autoconnect include-load-preset must-have-inputs must-have-outputs
                (<gui> :get-parent-window parentgui) ;; Improves plugin menu caching performance.
                ))


(define (same-instrconf-with-regards-to-filtering? instrconf1 instrconf2)
  (and (eq? (instrconf1 :include-load-preset)
            (instrconf2 :include-load-preset))
       (eq? (instrconf1 :must-have-outputs)
            (instrconf2 :must-have-outputs))
       (eq? (instrconf1 :must-have-inputs)
            (instrconf2 :must-have-inputs))))
       
            


;; The callback takes an instrument description as argument
(define (spr-entry->instrument-description entry instrconf callback)
  (define type (entry :type))
  (cond ((or (string=? type "NORMAL")
             (string=? type "NUM_USED_PLUGIN"))
         (callback (<-> "1"
                        (<ra> :to-base64 (entry :container-name))
                        ":"
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
            ;;;(else
            ;;; (<ra> :show-message (<-> "The \"" (entry :name) "\" plugin container didn't contain any plugins"))))) ;; The populate function shows error message for this.
        ((string=? type "LOAD_PRESET")
         (request-select-instrument-preset (instrconf :parentgui) callback))
        ((string=? type "PASTE_PRESET")         
         (callback "3"))
        (else
         ;;(c-display "entry:" entry)
         ;;(assert #f)
         )))

(define (can-spr-entry-be-used? entry instrconf)
  (assert (string=? (entry :type) "NORMAL"))
  (and (or (not (instrconf :must-have-inputs))
           (> (entry :num-inputs) 0))
       (or (not (instrconf :must-have-outputs))
           (> (entry :num-outputs) 0))))

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
                             :enabled (can-spr-entry-be-used? entry instrconf)
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


(define *popup-menu-args-cache-instrconf* #f)
(define *popup-menu-args-cache-generation* -1)
(define *popup-menu-args-cache-args* #f)
(define *popup-menu-curr-callback* #f)

#||
(pp (length (<ra> :get-sound-plugin-registry #t)))
(pp (<ra> :get-sound-plugin-registry #t))
(<ra> :get-sound-plugin-registry #t)
||#



(define (get-instrument-popup-menu-args instrconf callback)
  
  (define (my-callback entry)
    (*popup-menu-curr-callback* entry))
  
  (set! *popup-menu-curr-callback* callback) ;; Since there should never be more than one popup open at the same time, this should work, hopefully.
  
  (let ((curr-generation (<ra> :get-sound-plugin-registry-generation)))
    (when (or (not (= curr-generation *popup-menu-args-cache-generation*))
              (not (same-instrconf-with-regards-to-filtering? *popup-menu-args-cache-instrconf*
                                                              instrconf))
              (not (eq? (*popup-menu-args-cache-instrconf* :parentgui) ;; parentgui is used when openening new popup menues, plugin manager, file selector, etc.
                        (instrconf :parentgui))))
      
      ;;(c-display "REGENERATING CACHE")
      (set! *popup-menu-args-cache-instrconf* instrconf)
      (set! *popup-menu-args-cache-generation* curr-generation)
      (set! *popup-menu-args-cache-args*
            (get-popup-menu-args (append (list "Plugin Manager"
                                               (lambda ()
                                                 (pmg-start instrconf my-callback))
                                               "--------------")
                                         (spr-entries->menu-entries (<ra> :get-sound-plugin-registry)
                                                                    instrconf
                                                                    (lambda (entry)
                                                                      (spr-entry->instrument-description entry instrconf my-callback))
                                                                    #f)))))
    *popup-menu-args-cache-args*))


;; async
(define (start-instrument-popup-menu instrconf callback)
  (popup-menu-from-args (get-instrument-popup-menu-args instrconf callback)))

(define (create-instrument instrconf description)
  (undo-block
   (lambda ()
     (let ((instrument-id (<ra> :create-audio-instrument-from-description description "" (instrconf :x) (instrconf :y))))
       (when (and (integer? instrument-id)
                  (not (= -1 instrument-id)))  
         ;;(c-display (pp instrconf))
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
(create-instrument-popup-menu (make-instrument-conf))
!!#

                                  

;; async
(define (select-track-instrument tracknum)
  (define midi-instruments (get-all-midi-instruments))
  (define instruments-before (get-all-audio-instruments))
  
  (define (is-new-instrument? id-instrument)
    (and (not (memv id-instrument instruments-before))
         (memv id-instrument (get-all-audio-instruments))))
  
  (define (num-new-instruments)
    (- (length (get-all-audio-instruments))
       (length instruments-before)))
  
  (define-macro (load . code)
    `(undo-block
      (lambda ()
        (define id-instrument (begin ,@code))
        (when (and (integer? id-instrument) (not (= -1 id-instrument)))
          (<ra> :set-instrument-for-track id-instrument tracknum)
          (when (and (is-new-instrument? id-instrument)
                     (= 1 (num-new-instruments)))
            (<ra> :autoposition-instrument id-instrument)
            (<ra> :connect-audio-instrument-to-main-pipe id-instrument))))))
    
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
                              (start-instrument-popup-menu (make-instrument-conf :connect-to-main-pipe #t
                                                                                 :parentgui -1)
                                                           (lambda (descr)
                                                             (load (<ra> :create-audio-instrument-from-description descr)))))
   "<Load New Preset>" (lambda ()
                         (request-select-instrument-preset -1
                                                           (lambda (instrument-description)
                                                             (load (<ra> :create-audio-instrument-from-description instrument-description)))))
   
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
        instruments-before)))
     
#||
(select-track-instrument 0)
||#


(define (show/hide-instrument-gui)
  (let ((id (ra:get-current-instrument)))
    (when (not (= -1 id))
      (if (ra:has-native-instrument-gui id)
          (if (ra:instrument-gui-is-visible id)
              (ra:hide-instrument-gui id)
              (ra:show-instrument-gui id))
          (<ra> :show-async-message -2 (<-> "Instrument #" id " (" (<ra> :get-instrument-name id) ") doesn't have a GUI"))))))

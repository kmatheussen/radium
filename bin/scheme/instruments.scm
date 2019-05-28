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

(define-constant *send-connection-type* 0)
(define-constant *plugin-connection-type* 1)
(define-constant *auto-connection-type* 2)

(define *instrument-memoized-generation* 0)
(define *using-instrument-memoization* #f)

(define (run-instrument-data-memoized func)
  (if *using-instrument-memoization*
      (func)
      (try-finally :try (lambda ()
                          ;;(c-display "                     INSTRUMENTS MEMOIZED--->" *instrument-memoized-generation*)
                          (set! *using-instrument-memoization* #t)
                          (inc! *instrument-memoized-generation* 1)
                          (func))
                   :finally (lambda ()
                              ;;(c-display "                     <--- FINISHED INSTRUMENTS MEMOIZED--->" *instrument-memoized-generation*)
                              (set! *using-instrument-memoization* #f)))))


(define-macro (define-instrument-memoized name&arg . body)
  (let ((args (cdr name&arg))
        (func (gensym "func"))
        (args-name (gensym "args"))
	(memo (gensym "memo"))
        (false (gensym "false"))
        (last-generation (gensym "last-generation")))
    `(define ,(car name&arg)
       (let ((,memo (make-hash-table 16 equal?))
             (,last-generation -1))
	 (lambda ,args-name
           (define (,func ,@args)
             ,@body)
           ;;(if (and (equal? ,args-name '(1))
           ;;         (eq? (quote ,(car name&arg)) 'find-next-plugin-instrument-in-path))
           ;;    (c-display "Args:" ,args-name *using-instrument-memoization* ,last-generation *instrument-memoized-generation* (,memo ,args-name)))
           (if *using-instrument-memoization*
               (begin
                 (when (not (= ,last-generation
                               *instrument-memoized-generation*))
                   ;;(c-display "Recreating hash for" (quote ,name&arg) ":" ,last-generation *instrument-memoized-generation*)
                   (set! ,last-generation *instrument-memoized-generation*)
                   (set! ,memo (make-hash-table 16 equal?)))
                 (let ((hashed (,memo ,args-name)))
                   (cond ((not hashed)
                          (let ((result (apply ,func ,args-name)))
                            (set! (,memo ,args-name) (if (not result)
                                                         ',false ;; s7 hash tables have a very unfortunate logic.
                                                         result))
                            result))
                         ((eq? ',false hashed)
                          #f)
                         (else
                          hashed))))
               (apply ,func ,args-name)))))))


#!!
(define-instrument-memoized (testmemo a b)
  (c-display "Evaluating (+" a b ")")
  (+ a b)
  #f)

(begin
  (c-display "\nAAAAAAAAAAAA")
  (run-instrument-data-memoized
   (lambda ()
     (testmemo 2 3)
     (testmemo 2 3)
     (testmemo 2 3)
     ))
  (c-display "BBBBBBBBBBBB\n"))
!!#

(delafina (create-audio-connection-change :type
                                          :source
                                          :target
                                          :connection-type *auto-connection-type*
                                          :gain #f)
  (assert (or (string=? type "connect")
              (string=? type "disconnect")))
  (assert (integer? source))
  (assert (integer? target))
  (if (string=? type "disconnect")
      (assert (not gain))
      (assert (or (not gain)
                  (number? gain))))  
  (hash-table :type type :source source :target target :gain gain :connection-type connection-type))

(define-macro (push-audio-connection-change! changes rest)
  `(push-back! ,changes (create-audio-connection-change ,@(cdr rest))))


#!!
(macroexpand (push-audio-connection-change! changes (list :type "connect"
                                                          :source from-instrument
                                                          :target id-new-instrument
                                                          :gain (<ra> :get-audio-connection-gain from-instrument id-old-instrument))))

!!#

(delafina (create-audio-connection-implicitly-enabled-change :source
                                                             :target
                                                             :implicitly-enabled)
  (assert (integer? source))
  (assert (integer? target))
  (hash-table :type "connect" :source source :target target :implicitly-enabled (if implicitly-enabled 1 0)))

(define-macro (push-audio-connection-implicitly-enabled-change! changes rest)
  `(push-back! ,changes (create-audio-connection-implicitly-enabled-change ,@(cdr rest))))

#!!
(macroexpand (push-audio-connection-implicitly-enabled-change! changes (list :source from-instrument
                                                                             :target id-new-instrument
                                                                             :implicitly-enabled #t)))

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
              (any? (lambda (to)
                      (if (= to i2)
                          #t
                          (loop to)))
                    (get-instruments-connecting-from-instrument i1)))))))
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


(define-instrument-memoized (get-pure-buses)
  (map (lambda (bus-num)
         (<ra> :get-audio-bus-id bus-num))
       (iota (length *bus-effect-names*))))

(define-instrument-memoized (get-seqtrack-buses)
  (keep ra:instrument-is-seqtrack-bus
        (get-all-audio-instruments)))

(define-instrument-memoized (get-buses)
  (append (get-seqtrack-buses)
          (get-pure-buses)))

(define-instrument-memoized (get-instruments-connecting-to-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-source-instrument in-connection id-instrument))
       (iota (<ra> :get-num-in-audio-connections id-instrument))))

(define-instrument-memoized (get-instruments-connecting-from-instrument id-instrument)
  (map (lambda (in-connection)
         (<ra> :get-audio-connection-dest-instrument in-connection id-instrument))
       (iota (<ra> :get-num-out-audio-connections id-instrument))))

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
                       (get-instruments-connecting-from-instrument id))))))
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
  (define target-instruments (get-instruments-connecting-from-instrument instrument-id))

  (define plugin-targets (let ((sorted (sort (keep (lambda (target-id)
                                                     (= *plugin-connection-type*
                                                        (<ra> :get-audio-connection-type instrument-id target-id)))
                                                   target-instruments)                                    
                                             (lambda (a b)
                                               (< (<ra> :get-num-in-audio-connections a)
                                                  (<ra> :get-num-in-audio-connections b))))))
                           
                           ;;(c-display "SORTED:" instrument-id (<ra> :get-instrument-name instrument-id) " ==> " (map ra:get-instrument-name sorted))
                           
                           (cond ((null? sorted)
                                  '())
                                 ((null? (cdr sorted))
                                  sorted)
                                 (else
                                  (if (not (<ra> :release-mode))
                                      (assert #f))
                                  (let ((num-inputs (<ra> :get-num-in-audio-connections (car sorted))))
                                    (take-while sorted ;; only keep plugins that have the least number of input connections. I.e. '(1 1 5 5 7) -> '(1 1)
                                                (lambda (instrument-id)
                                                  (= num-inputs (<ra> :get-num-in-audio-connections instrument-id)))))))))

  (if (not (null? plugin-targets))
      
      (last (sort-instruments-by-mixer-position plugin-targets))
      
      (let loop ((out-instruments (reverse (sort-instruments-by-mixer-position
                                            (remove (lambda (target-id)
                                                      (or (<ra> :instrument-is-permanent target-id)
                                                          (= *send-connection-type*
                                                             (<ra> :get-audio-connection-type instrument-id target-id))))
                                                    target-instruments)))))
        ;;(c-display "  out-instruments for" (<ra> :get-instrument-name instrument-id) ": " (map ra:get-instrument-name out-instruments))
        (if (null? out-instruments)
            (begin
              ;;(c-display "    result: #f")
              #f)
            (let ((out-instrument (car out-instruments)))
              (if (= 1 (<ra> :get-num-in-audio-connections out-instrument))
                  (begin
                    ;;(c-display "    result:" (<ra> :get-instrument-name out-instrument))
                    out-instrument)
                  (loop (cdr out-instruments))))))))

                                              
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
                  (push! no-inputs-or-outputs id)))
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

  (define instrument-plugins (keep (lambda (id)
                                     (> (<ra> :get-num-in-audio-connections id) 0))
                                   (apply append (map find-all-nonbus-plugins-used-in-mixer-strip instruments))))

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
                                                           :connection-type (<ra> :get-audio-connection-type from-instrument id-old-instrument)
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
                                                           :connection-type (<ra> :get-audio-connection-type id-old-instrument to-instrument)
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
  (if (= 0 id-old-instrument)
      (begin
        (<ra> :internal-replace-main-pipe id-new-instrument)
        (set! id-new-instrument 0))
      (<ra> :delete-instrument id-old-instrument))      
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
                   (<ra> :get-stored-instrument-effect id-old-instrument "System Dry/Wet"))
             ;;(<ra> :replace-all-seq-automation id-old-instrument id-new-instrument)
             (<ra> :replace-use-of-instrument id-old-instrument id-new-instrument)
             (<ra> :undo-mixer-connections)
             (move-connections-to-new-instrument id-old-instrument id-new-instrument)
             ;;(replace-instrument-in-all-tracks! id-old-instrument id-new-instrument)
             (replace-instrument-in-mixer id-old-instrument id-new-instrument)
             )))))

  (cond ((= 0 id-old-instrument)
         (<ra> :schedule 0 ;; We do this since the function is specified to return immediately. I.e. the caller expects the instrument configuration to be the same when we return.
               (lambda ()
                 (ignore-undo-block
                  (lambda ()
                    (replace description)
                    #f)))))
        ((<ra> :instrument-is-permanent id-old-instrument)
         (show-async-message (instrconf :parentgui) "Can not be replaced"))
        ((or (not description)
             (string=? description ""))
         (start-instrument-popup-menu instrconf replace))
        (else
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
      (show-async-message parentgui "Can not load preset for this instrument")
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
                       (show-async-message parentgui (<-> "Can not insert instrument named \n\"" (<ra> :get-instrument-name new-instrument) "\"\nsince it has no input channels"))
                       (set! do-undo #t)
                       #f)
                      
                      ((and (= 0 num-outputs)
                            has-instrument2)
                       (show-async-message parentgui (<-> "Can not insert instrument named \n\"" (<ra> :get-instrument-name new-instrument) "\"\nsince it has no output channels"))
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
                                                                      :target new-instrument
                                                                      :connection-type *plugin-connection-type*
                                                                      )))
                       
                       (for-each (lambda (out-id gain)
                                   (push-audio-connection-change! changes (list :type "connect"
                                                                                :source new-instrument
                                                                                :target out-id
                                                                                :gain gain
                                                                                :connection-type (if (not instrument-id1)
                                                                                                     *auto-connection-type*
                                                                                                     (<ra> :get-audio-connection-type instrument-id1 out-id)))))
                                 out-list
                                 gain-list)

                       (<ra> :change-audio-connections changes) ;; Apply all changes simultaneously
                       
                       new-instrument)))
              #f))))

     (if do-undo
         (<ra> :undo))

     (if result
         (callback result)))))

(define (FROM_C-set-solo-for-instruments instruments doit)
  (undo-block
   (lambda ()           
     (for-each (lambda (instrument-id)
                 (<ra> :set-instrument-solo instrument-id doit))
               instruments))))

(define (FROM_C-switch-solo-for-selected-instruments)
  (let ((instruments (to-list (<ra> :get-selected-instruments))))
    (if (not (null? instruments))
        (let ((doit (not (> (<ra> :get-instrument-effect (car instruments) "System Solo On/Off") 0.5))))
          (FROM_C-set-solo-for-instruments instruments doit)))))



(define (FROM_C-set-mute-for-instruments instruments doit)
  (undo-block
   (lambda ()           
     (for-each (lambda (instrument-id)
                 (<ra> :set-instrument-mute instrument-id doit))
               instruments))))

(define (FROM_C-switch-mute-for-selected-instruments)
  (let ((instruments (to-list (<ra> :get-selected-instruments))))
    (if (not (null? instruments))
        (let ((doit (not (< (<ra> :get-instrument-effect (car instruments) "System Volume On/Off") 0.5))))
          (FROM_C-set-mute-for-instruments instruments doit)))))



(define (FROM_C-set-bypass-for-instruments instruments doit)
  (undo-block
   (lambda ()           
     (for-each (lambda (instrument-id)
                 (<ra> :set-instrument-bypass instrument-id doit))
               instruments))))

(define (FROM_C-switch-bypass-for-selected-instruments)
  (let ((instruments (to-list (<ra> :get-selected-instruments))))
    (if (not (null? instruments))
        (let ((doit (not (< (<ra> :get-instrument-effect (car instruments) "System Effects On/Off") 0.5))))
          (FROM_C-set-bypass-for-instruments instruments doit)))))


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
         (callback (<ra> :get-audio-instrument-description (<ra> :to-base64 (entry :container-name)) (<ra> :to-base64 (entry :type-name)) (<ra> :to-base64 (entry :name) ))))
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
          ;;(c-display "ENTRY" entry)
          (cond ((string=? type "NORMAL")
                 (cons (list (entry :name)
                             :enabled (can-spr-entry-be-used? entry instrconf)
                             mcallback)
                       (loop (cdr entries))))
                ((string=? type "CONTAINER")
                 (let ((in-container (loop (cdr entries))))
                   (if (null? in-container)
                       '()
                       (cons (list (entry :name)
                                   mcallback)
                             in-container))))
                ((string=? type "SEPARATOR")
                 (cons "----------------"
                       (loop (cdr entries))))
                ((string=? type "LEVEL_UP")                 
                 (define rest #f)
                 (define in-level-up (spr-entries->menu-entries (cdr entries) instrconf callback
                                                                (lambda (dasrest)
                                                                  (set! rest dasrest))))
                 ;;(c-display "  LEVEL_UP:" (entry :name) ".  CONTENTS:" in-level-up)
                 (if (null? in-level-up)
                     (loop rest)
                     (cons (list (entry :name)
                                 in-level-up)
                           (loop rest))))
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
                 (cons (list "Paste" ;; sound objects(s)"
                             :enabled (and (instrconf :include-load-preset)
                                           (<ra> :instrument-preset-in-clipboard))
                             :shortcut ra:paste-mixer-objects
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
(define *popup-menu-args-cache-preset-in-clipboard* #f)
(define *popup-menu-curr-callback* #f)


#||
(pp (length (<ra> :get-sound-plugin-registry #t)))
(pp (<ra> :get-sound-plugin-registry #t))
(<ra> :get-sound-plugin-registry #t)
||#

(define (FROM_C-request-rename-instrument instrument-id)
  (define old-name (<ra> :get-instrument-name instrument-id))
  (define new-name (<ra> :request-string "New name:" #t old-name))
  (c-display "NEWNAME" (<-> "-" new-name "-"))
  (if (and (not (string=? new-name ""))
           (not (string=? new-name old-name)))
      (<ra> :set-instrument-name new-name instrument-id)))


(delafina (get-instrument-popup-entries :instrument-id
                                        :parentgui
                                        :include-delete-and-replace #t
                                        :must-have-inputs #f :must-have-outputs #f)

  (list
   (<-> "----------Instrument: \"" (<ra> :get-instrument-name instrument-id) "\"");; " instrument")
   ;;"-----------Instrument"
   
   (and #f ;; Seems like instrument-id is always current instrument.
        (let ((is-current (= (<ra> :get-current-instrument)
                             instrument-id)))
          (and (not is-current)
               (list
                (list "Set as current instrument"
                      :enabled (not is-current)
                      (lambda ()
                        (<ra> :set-current-instrument instrument-id #f)))
                "------------------"))))
   
   (list "Delete"
         :enabled (and include-delete-and-replace
                       (not (<ra> :instrument-is-permanent instrument-id)))
         (lambda ()
           (<ra> :delete-instrument instrument-id)))
   (list "Replace"
         :enabled (and include-delete-and-replace
                       (not (<ra> :instrument-is-permanent instrument-id)))
         (lambda ()           
           (async-replace-instrument instrument-id "" (make-instrument-conf :must-have-inputs must-have-inputs :must-have-outputs must-have-outputs :parentgui parentgui)))
         )
   
   "------------------"
   
   "Rename" (lambda ()
              (FROM_C-request-rename-instrument instrument-id))
   
   "-----------"
   
   (list "Load Preset (.rec)" :enabled instrument-id
         :enabled (and include-delete-and-replace
                       (not (<ra> :instrument-is-permanent instrument-id)))
         (lambda ()
           (<ra> :request-load-instrument-preset instrument-id "" parentgui)))
   (list "Save Preset (.rec)" :enabled instrument-id
         :enabled (and include-delete-and-replace
                       (not (<ra> :instrument-is-permanent instrument-id)))
         (lambda ()
           (<ra> :save-instrument-preset (list instrument-id) parentgui)))
   
   "------------------"
   
   "Configure color" (lambda ()
                       (show-instrument-color-dialog parentgui instrument-id))
   "Generate new color" (lambda ()
                          (<ra> :set-instrument-color (<ra> :generate-new-color 0.9) instrument-id))

   "--------"
   
   (list "Show GUI"
         :enabled (<ra> :has-native-instrument-gui instrument-id)
         :check (<ra> :instrument-gui-is-visible instrument-id parentgui)
         (lambda (enabled)
           (if enabled
               (<ra> :show-instrument-gui instrument-id parentgui #f)
               (<ra> :hide-instrument-gui instrument-id))))
   (list "Recv. external MIDI"
         :check (<ra> :instrument-always-receive-midi-input instrument-id)
         (lambda (onoff)
           (<ra> :set-instrument-always-receive-midi-input instrument-id onoff)))
   
   "--------"
   
   "Show Info" (lambda ()
                 (<ra> :show-instrument-info instrument-id parentgui))

   ))
              



(define (get-instrument-popup-menu-args instrconf callback)
  
  (define (my-callback entry)
    (*popup-menu-curr-callback* entry))
  
  (set! *popup-menu-curr-callback* callback) ;; Since there should never be more than one popup open at the same time, this should work, hopefully.
  
  (let ((curr-generation (<ra> :get-sound-plugin-registry-generation)))
    (when (or (not (eq? *popup-menu-args-cache-preset-in-clipboard* (<ra> :instrument-preset-in-clipboard)))
              (not (= curr-generation *popup-menu-args-cache-generation*))
              (not (same-instrconf-with-regards-to-filtering? *popup-menu-args-cache-instrconf*
                                                              instrconf))
              (not (eq? (*popup-menu-args-cache-instrconf* :parentgui) ;; parentgui is used when openening new popup menues, plugin manager, file selector, etc.
                        (instrconf :parentgui))))
      
      ;;(c-display "REGENERATING CACHE")
      (set! *popup-menu-args-cache-instrconf* instrconf)
      (set! *popup-menu-args-cache-generation* curr-generation)
      (set! *popup-menu-args-cache-preset-in-clipboard* (<ra> :instrument-preset-in-clipboard))
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

#!!
(ra:show-message (number->string (length (ra:get-sound-plugin-registry))))
(for-each (lambda (i)
            (<ra> :add-message (pp ((<ra> :get-sound-plugin-registry) i))))
          (iota 50))
!!#

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
ra.evalScheme "(pmg-start (ra:create-new-instrument-conf) (lambda (descr) (create-instrument (ra:create-new-instrument-conf) descr)))"
(<ra> :get-instrument-name 29)
!!#


(define (get-midi-learn-menu-elements instrument-id effect-name)
  ;;(c-display "inst/eff:" instrument-id effect-name)
  (if (and effect-name
           (<ra> :instrument-effect-has-midi-learn instrument-id effect-name))
      (list
       (list "Remove MIDI Learn"
             (lambda ()
               (<ra> :remove-instrument-effect-midi-learn instrument-id effect-name)))
       (list "MIDI relearn"
             (lambda ()
               (<ra> :remove-instrument-effect-midi-learn instrument-id effect-name)
               (<ra> :add-instrument-effect-midi-learn instrument-id effect-name))))
      (list "MIDI Learn"
            :enabled effect-name
            (lambda ()
              (<ra> :add-instrument-effect-midi-learn instrument-id effect-name)))))

(define (create-select-modulator-popup-menu callback)
  (popup-menu "Create new modulator"
              (lambda ()
                (callback 'create-new))
              "-----------------"
              (map (lambda (modulator-instrument-id)
                     (list
                      (<ra> :get-modulator-description3 modulator-instrument-id)
                      (lambda ()
                        (callback modulator-instrument-id))))
                   (<ra> :get-modulator-instruments))))

#!!
(create-select-modulator-popup-menu 1 2 c-display)
!!#

(delafina (get-effect-popup-entries :instrument-id
                                    :effect-name
                                    :automation-error-message #f ;; If set to a string, the entries will be disabled, and this will be the text
                                    :modulation-error-message #f ;; If set to a string, the entries will be disabled, and this will be the text
                                    :pre-undo-block-callback (lambda () #f)
                                    :post-undo-block-callback (lambda () #f)
                                    )
  (define is-solo (and effect-name
                       (string=? effect-name "System Solo On/Off")))
  
  (list
   (<-> "------------Effect: \"" effect-name "\"")

   (and (string=? "Pd" (<ra> :get-instrument-type-name instrument-id))
        (list "Delete"
              :enabled effect-name
              (lambda ()
                (<ra> :delete-pd-controller instrument-id effect-name))))
                        
   (list "Reset"
         :enabled effect-name
         (lambda ()
           (if is-solo
               (<ra> :set-instrument-solo instrument-id #f)
               (<ra> :reset-instrument-effect instrument-id effect-name))))
         
   "-------------" ;;Midi Learn"
   (get-midi-learn-menu-elements instrument-id effect-name)

      "------------" ;;Modulators"
   (let ((has-modulator (and (not modulation-error-message)
                             (<ra> :has-modulator instrument-id effect-name))))
     (if has-modulator
         (list (list (<-> "Remove modulator (" (<ra> :get-modulator-description instrument-id effect-name) ")")
                     (lambda ()
                       (undo-block (lambda ()
                                     (pre-undo-block-callback)
                                     (<ra> :remove-modulator instrument-id effect-name)
                                     (post-undo-block-callback)))))
               (list (<-> "Replace modulator (" (<ra> :get-modulator-description instrument-id effect-name) ")")
                     (lambda ()
                       (create-select-modulator-popup-menu
                        (lambda (modulator-id)
                          (undo-block (lambda ()
                                        (pre-undo-block-callback)
                                        (if (and (symbol? modulator-id)
                                                 (eq? 'create-new modulator-id))
                                            (set! modulator-id (<ra> :create-modulator)))
                                        (<ra> :replace-modulator instrument-id effect-name modulator-id)
                                        (post-undo-block-callback))))))))
         (list (list (or modulation-error-message
                         "Assign modulator")
                     :enabled (not automation-error-message)
                     (lambda ()
                       (create-select-modulator-popup-menu
                        (lambda (modulator-id)
                          ;;(<ra> :undo-instrument-effect instrument-id effect-name) ;; store value before assigning modulator.
                          (undo-block (lambda ()
                                        ;;(c-display "adding modulator for" instrument-id effect-name)
                                        (pre-undo-block-callback)
                                        (if (and (symbol? modulator-id)
                                                 (eq? 'create-new modulator-id))
                                            (set! modulator-id (<ra> :create-modulator)))
                                        (<ra> :add-modulator instrument-id effect-name modulator-id)
                                        (post-undo-block-callback))))))))))

   "------------------------"
   (list "Start recording automation in editor"
         :enabled effect-name
         (lambda ()
           (<ra> :start-recording-instrument-automation-in-editor instrument-id effect-name)))

   "------------------------"
   ;;"-------------Automation"
   (list (or automation-error-message
             "Add automation to current editor track")
         :enabled (and (not automation-error-message)
                       effect-name)
         (lambda ()
           (undo-block (lambda ()
                         (pre-undo-block-callback)
                         (<ra> :add-automation-to-current-editor-track instrument-id effect-name)
                         (post-undo-block-callback)))))
   (list (or automation-error-message
             "Add automation to current sequencer track")
         :enabled (and (not automation-error-message)
                       effect-name)
         (lambda ()
           (undo-block (lambda ()
                         (pre-undo-block-callback)
                         (<ra> :add-automation-to-current-sequencer-track instrument-id effect-name)
                         (post-undo-block-callback)))))

   "--------------------------"
   (list "Change value when pressing \"Random\""
         :check (and effect-name
                     (<ra> :get-instrument-effect-changes-value-when-pressing-random instrument-id effect-name))
         :enabled (and effect-name)
;;                       (not (string-starts-with? effect-name "System ")))
         (lambda (ison)
           (<ra> :set-instrument-effect-changes-value-when-pressing-random instrument-id effect-name ison)))

   ;; Disabled transpose and volume since "Set new value when starting to play note" doesn't work when there's pitch glide or velocity glide in the editor.
   ;;
   (and effect-name
        (or ;;(string-starts-with? effect-name "System Transpose Voice ")
            ;;(string-starts-with? effect-name "System Volume Voice ")
            (string-starts-with? effect-name "System Pan Voice "))
        (list
         "--------------Note duplicator pan"
         (list
          :radio-buttons
          (list "Set new value immediately"
                :check (<ra> :get-note-duplicator-set-new-value-immediately instrument-id effect-name)
                (lambda (ison)
                  (if ison
                      (<ra> :set-note-duplicator-set-new-value-immediately instrument-id effect-name #t))))
          (list "Set new value when starting to play note"
                :check (not (<ra> :get-note-duplicator-set-new-value-immediately instrument-id effect-name))
                (lambda (ison)
                  (if ison
                      (<ra> :set-note-duplicator-set-new-value-immediately instrument-id effect-name #f)))))))

   "-----------------"
   (list "Help"
         (lambda ()
           (FROM-C-show-help-window "help/instrument_effect_popup_menu_framed.html")))
   
   ))
  

(define (FROM_C-show-effect-popup-menu instrument-id effect-name)
  (if (<ra> :shift-pressed)
      (<ra> :reset-instrument-effect instrument-id effect-name)
      (popup-menu (get-effect-popup-entries instrument-id effect-name
                                            ;;:pre-undo-block-callback (lambda ()
                                            ;;                           (<ra> :undo-instrument-effect instrument-id effect-name) ;; store value before assigning modulator.
                                            ;;                           #f
                                            ;;                           )
                                            )
                  )
      )
  )

;;  Sjekk ut hva dette var godt for. Kanskje det har noe med at feil verdi blir lagra i undo om ikke denne blir kalt først.
;                                        :pre-undo-block-callback (lambda ()
;                                                                   (<ra> :undo-instrument-effect instrument-id effect-name) ;; store value before assigning modulator.
;                                                                   #f
;                                                                   ))))
#!!
(show-note-duplicator-popup-menu (<ra> :get-current-instrument) "System Chance Voice 1")
!!#


(define (delete-all-unused-MIDI-instruments)
  (define used-instruments (<new> :container '() =))
  (define unused-MIDI-instruments '())
  
  (for-each (lambda (blocknum)
              (for-each (lambda (tracknum)
                          (used-instruments :add-unique! (<ra> :get-instrument-for-track tracknum blocknum)))
                        (iota (<ra> :get-num-tracks blocknum))))
            (iota (<ra> :get-num-blocks)))
  
  (for-each (lambda (midi-instrument)
              (if (not (used-instruments :contains midi-instrument))
                  (push-back! unused-MIDI-instruments midi-instrument)))
            (get-all-midi-instruments))

  (if (null? unused-MIDI-instruments)
      (show-async-message :text "There are no unused MIDI instruments in this song")
      (show-async-message :text "Are you sure? All undo data will be deleted."
                          :buttons (list "Yes" "No")
                          :is-modal #f
                          :callback (lambda (res)
                                      (when (string=? "Yes" res)
                                        (ignore-undo-block
                                         (lambda ()
                                           (for-each ra:delete-instrument unused-MIDI-instruments)))
                                        (<ra> :reset-undo))))))

#!
(delete-all-unused-MIDI-instruments)
!#

(define (get-select-track-instrument-popup-entries tracknum)
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

  (define (get-instrument-entries only-if-used)
    (list
     (and (> (length midi-instruments) 0)
          (list "----------"
                (map (lambda (num instrument-id)
                       (and (or (not only-if-used)
                                (<ra> :instrument-has-been-used instrument-id))
                            (list (<-> num ". " (<ra> :get-instrument-name instrument-id))                     
                                  (lambda ()
                                    (load instrument-id)))))
                     (iota (length midi-instruments))
                     midi-instruments)))
     "----------"
     (map (lambda (num instrument-id)
            (and (or (not only-if-used)
                     (<ra> :instrument-has-been-used instrument-id))
                 (list (<-> num ". " (<ra> :get-instrument-name instrument-id))                     
                       (lambda ()
                         (load instrument-id)))))
          (iota (length instruments-before))
          instruments-before)))
    
  (list
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
   "All instruments" (get-instrument-entries #f)
   (get-instrument-entries #t))
  )

;; async
(define (select-track-instrument tracknum)
  (popup-menu (get-select-track-instrument-popup-entries tracknum)))
     
#||
(select-track-instrument 0)
||#


(define (show/hide-instrument-gui)
  (define parentgui -2) ;; current window
  (let ((id (ra:get-current-instrument)))
    (when (not (= -1 id))
      (if (ra:has-native-instrument-gui id)
          (if (ra:instrument-gui-is-visible id parentgui)
              (ra:hide-instrument-gui id)
              (ra:show-instrument-gui id parentgui))
          (show-async-message -2 (<-> "Instrument #" id " (" (<ra> :get-instrument-name id) ") doesn't have a GUI"))))))


(define (pan-enabled? instrument-id)
  (>= (<ra> :get-instrument-effect instrument-id "System Pan On/Off") 0.5))

(define (pan-enable! instrument-id onoff)
  (when (not (eq? onoff (pan-enabled? instrument-id)))
    (<ra> :undo-instrument-effect instrument-id "System Pan On/Off")
    (<ra> :set-instrument-effect instrument-id "System Pan On/Off" (if onoff 1.0 0.0))))


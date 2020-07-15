(provide 'instruments.scm)

(my-require 'gui.scm)
(my-require 'keybindings.scm)


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


(c-define-macro (*define-instrument-memoized* name&arg . body)
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
  (assert (instrument? source))
  (assert (instrument? target))
  (if (string=? type "disconnect")
      (assert (not gain))
      (assert (or (not gain)
                  (number? gain))))  
  (hash-table :type type :source source :target target :gain gain :connection-type connection-type))

(c-define-macro (*push-audio-connection-change!* changes rest)
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
  (assert (instrument? source))
  (assert (instrument? target))
  (hash-table :type "connect" :source source :target target :implicitly-enabled (if implicitly-enabled 1 0)))

(c-define-macro (*push-audio-connection-implicitly-enabled-change!* changes rest)
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

(define (is-legal-audio-instrument? instrument-id)
  (and (<ra> :is-legal-instrument instrument-id)
       (<ra> :instrument-is-audio instrument-id)))

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
      (define visited (make-hash-table 16 equal?))        
      (let loop ((i1 i1))
        (if (visited i1)
            #f
            (begin
              (hash-table-set! visited i1 #t)
              (any? (lambda (to)
                      (if (equal? to i2)
                          #t
                          (loop to)))
                    (get-instruments-connecting-from-instrument i1)))))))
  (set! g-total-time (+ g-total-time (- (time) start)))
  ret)

(define g-total-sort-time 0)

(define-instrument-memoized (sort-instruments-by-mixer-position-and-connections instruments)
  (define start (time))
  
  (define container (<new> :container instruments))
  (define done (make-hash-table (i-max 1 (container :length)) equal?))
  
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

(define-instrument-memoized (get-bus-effect-name-from-target-instrument target-id)
  (let loop ((bus-num 0)
             (bus-effect-names *bus-effect-names*))
    (if (null? bus-effect-names)
        #f
        (if (equal? target-id (<ra> :get-audio-bus-id bus-num))
            (car bus-effect-names)
            (loop (+ 1 bus-num)
                  (cdr bus-effect-names))))))
  

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


(define-instrument-memoized (is-connected-somehow? id goal-id)
  (define visited (make-hash-table 16 equal?))
  (call-with-exit
   (lambda (return)
     (let loop ((id id))
       ;;(c-display "  REC TEST:" (<ra> :get-instrument-name id) "->" (<ra> :get-instrument-name goal-id)
       ;;           ". From:" (map ra:get-instrument-name (get-instruments-connecting-from-instrument id))
       ;;           ". Efrom:" (map ra:get-instrument-name (get-instruments-econnecting-from-instrument id))
       ;;           ". Num audio from:" (<ra> :get-num-out-audio-connections id))
       (cond ((visited id)
              #f)
             ((equal? goal-id id)
              (return #t))
             (else
              (hash-table-set! visited id #t)
              (for-each loop (get-instruments-connecting-from-instrument id))
              (for-each loop (get-instruments-econnecting-from-instrument id))
              )))
     #f)))


;; Returns all instruments that is natural to list up as send targets.
;; Note that not all of these are necessarily possible to connect to. (use ra:can-audio-connect for that)
(define-instrument-memoized (get-all-instruments-that-we-might-send-to from-id)
  (remove (lambda (to-id)
            ;;(c-display "      " (<ra> :get-instrument-name from-id) "->" (<ra> :get-instrument-name to-id)
            ;;           ". has_audio_connection:" (<ra> :has-audio-connection from-id to-id)
            ;;           ". is_recursive:" (would-this-create-a-recursive-connection? from-id to-id))
            (or ;;(<ra> :has-audio-connection from-id to-id)
                (<ra> :has-audio-connection to-id from-id)
                (equal? from-id to-id)
                ;;(is-connected-somehow? to-id from-id)
                (= (<ra> :get-num-input-channels to-id) 0)
                ))
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

                                              
;; Returns the last plugin.
(define (find-meter-instrument-id instrument-id)
  ;;(c-display "FINDMETER1" instrument-id)
  (define next-plugin-instrument (find-next-plugin-instrument-in-path instrument-id))
  ;;(c-display "FINDMETER2" instrument-id next-plugin-instrument (and next-plugin-instrument (<ra> :get-instrument-name next-plugin-instrument)))
  (if next-plugin-instrument
      (find-meter-instrument-id next-plugin-instrument)
      instrument-id))


(define-instrument-memoized (find-all-plugins-used-in-mixer-strip instrument-id)
  (define next (find-next-plugin-instrument-in-path instrument-id))
  (if next
      (cons next
            (find-all-plugins-used-in-mixer-strip next))
      '()))

(define-instrument-memoized (find-all-nonbus-plugins-used-in-mixer-strip instrument-id)
  (define next (find-next-plugin-instrument-in-path instrument-id))
  (if (and next
           (not (member next (get-buses))))
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
                              (if (equal? next-plugin-instrument out-instrument)
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
                           equal?))

  (define buses-plugins (apply append (map find-all-plugins-used-in-mixer-strip (all-buses :list))))

  (define instrument-plugins (keep (lambda (id)
                                     (and (> (<ra> :get-num-in-audio-connections id) 0)
                                          (not (all-buses :contains id)))) ;; Can happen if a bus is set to be displayed as a plugin in mixer strip.
                                   (apply append (map find-all-nonbus-plugins-used-in-mixer-strip instruments))))

  (define all-instrument-instruments (append no-inputs-or-outputs
                                             instruments
                                             instrument-plugins))

  (define all-bus-instruments (append (all-buses :list)
                                      buses-plugins))
  
  (set! instruments (<new> :container instruments equal?))
  (set! no-inputs-or-outputs (<new> :container no-inputs-or-outputs equal?))
  
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
     (if (equal? id-old-instrument (<ra> :get-instrument-for-track tracknum blocknum))
         (<ra> :set-instrument-for-track id-new-instrument tracknum blocknum)))))
||#

(define (replace-instrument-in-mixer id-old-instrument id-new-instrument)
  (define x (+ 0 (<ra> :get-instrument-x id-old-instrument)))
  (define y (+ 0 (<ra> :get-instrument-y id-old-instrument)))
  (if (equal? (<ra> :get-main-pipe-instrument) id-old-instrument)
      (begin
        (<ra> :internal-replace-main-pipe id-new-instrument)
        (set! id-new-instrument (<ra> :get-main-pipe-instrument)))
      (<ra> :delete-instrument id-old-instrument))
  (<ra> :set-instrument-position x y id-new-instrument)
  )

;; Called from the outside. 'instrument-description' can be false or empty string.
;; Async. Returns immediately.
(define (async-replace-instrument id-old-instrument description instrconf)

  (assert (instrument? id-old-instrument))
  
  (define (replace description)
    (if (not (string=? "" description))
        (undo-block
         (lambda ()
           (define patch-name (if (<ra> :instrument-name-was-autogenerated id-old-instrument)
                                  ""
                                  (<ra> :get-instrument-name id-old-instrument)))
           (define id-new-instrument (<ra> :create-audio-instrument-from-description description patch-name))
           (when (<ra> :is-legal-instrument id-new-instrument)
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

  (cond ((equal? (<ra> :get-main-pipe-instrument) id-old-instrument)
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

;; Note: Used for shortcut
(delafina (replace-instrument :instrument-id (<ra> :get-current-instrument)
                              :must-have-inputs #f
                              :must-have-outputs #f
                              :gui -2)
  (async-replace-instrument instrument-id "" (make-instrument-conf :must-have-inputs must-have-inputs :must-have-outputs must-have-outputs :parentgui gui)))

  
  
(define (create-load-instrument-preset-description filename)
  (<-> "2" (<ra> :get-base64-from-filepath filename)))

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
                                           (map (lambda (filename)
                                                  (list (<ra> :get-path-string filename)
                                                        (lambda ()
                                                          (callback (create-load-instrument-preset-description filename)))))
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
          (define new-instrument (<ra> :create-audio-instrument-from-description instrument-description "" x y #f))
          (if (<ra> :is-legal-instrument new-instrument)
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
                                                                                :gain gain)))
                                 out-list
                                 gain-list)

                       (<ra> :change-audio-connections changes) ;; Apply all changes simultaneously
                       
                       new-instrument)))
              #f))))

     (if do-undo
         (<ra> :undo))

     (if (and result
              callback)
         (callback result)))))

(define (FROM_C-remove-instrument-from-connection-path parent-instrument-id instrument-id)
  (define child-ids (get-instruments-connecting-from-instrument instrument-id))
  (define child-gains (map (lambda (to)
                             (<ra> :get-audio-connection-gain instrument-id to))
                           child-ids))
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
  
  (<ra> :change-audio-connections changes)) ;; Apply all changes simultaneously



(define (FROM_C-set-solo-for-instruments instruments doit)
  (undo-block
   (lambda ()           
     (for-each (lambda (instrument-id)
                 (<ra> :set-instrument-solo doit instrument-id))
               instruments))))

         
(define (FROM_C-switch-solo-for-selected-instruments)
  (let ((instruments (to-list (<ra> :get-extended-selected-instruments))))
    (if (not (null? instruments))
        (let ((doit (not (<ra> :get-instrument-solo (car instruments))))) ;; use get-instrument-solo instead of get-instrument-solo-from-storage here to avoid confusion.
          (FROM_C-set-solo-for-instruments instruments doit)))))



(define (FROM_C-set-mute-for-instruments instruments doit)
  (undo-block
   (lambda ()           
     (for-each (lambda (instrument-id)
                 (<ra> :set-instrument-mute doit instrument-id))
               instruments))))

(define (FROM_C-switch-mute-for-selected-instruments)
  (let ((instruments (to-list (<ra> :get-extended-selected-instruments))))
    (if (not (null? instruments))
        (let ((doit (not (< (<ra> :get-instrument-effect (car instruments) "System Volume On/Off") 0.5))))
          (FROM_C-set-mute-for-instruments instruments doit)))))



(define (FROM_C-set-bypass-for-instruments instruments doit)
  (undo-block
   (lambda ()           
     (for-each (lambda (instrument-id)
                 (<ra> :set-instrument-bypass doit instrument-id))
               instruments))))

(define (FROM_C-switch-bypass-for-selected-instruments)
  (let ((instruments (to-list (<ra> :get-extended-selected-instruments))))
    (if (not (null? instruments))
        (let ((doit (not (< (<ra> :get-instrument-effect (car instruments) "System Effects On/Off") 0.5))))
          (FROM_C-set-bypass-for-instruments instruments doit)))))

;; Note: Must return status bar id.
(define (FROM_C-display-mute-status-in-statusbar instrument-id)  
  (define in-storage (<ra> :get-instrument-solo-from-storage instrument-id))
  (define in-plugin (<ra> :get-instrument-solo instrument-id))
  (<ra> :set-statusbar-text (<-> (<ra> :get-instrument-name instrument-id) ": "
                                 (cond ((and in-storage in-plugin)                                        
                                        "Mute On")
                                       ((and in-plugin (not in-storage))
                                        "Mute On by automation")
                                       ((<ra> :instrument-is-implicitly-muted instrument-id)
                                        "Mute Implicitly on")
                                       ((and (not in-plugin) in-storage)
                                        "Mute Off by automation")
                                       (else
                                        "Mute Off"))
                                 (let ((keybinding (or (get-displayable-keybinding-from-shortcut ra:switch-instrument-mute)
                                                       (get-displayable-keybinding-from-shortcut ra:switch-mute-for-selected-instruments))))
                                   (if keybinding
                                       (<-> " (" keybinding ")")
                                       "")))))

;; Note: Must return status bar id.
(define (FROM_C-display-solo-status-in-statusbar instrument-id)
  (define in-storage (<ra> :get-instrument-solo-from-storage instrument-id))
  (define in-plugin (<ra> :get-instrument-solo instrument-id))
  
  (<ra> :set-statusbar-text (<-> (<ra> :get-instrument-name instrument-id) ": "
                                 (cond ((and in-plugin in-storage)
                                        "Solo On")
                                       ((and in-plugin (not in-storage))
                                        "Solo On by automation")
                                       ((<ra> :instrument-is-implicitly-soloed instrument-id)
                                        "Solo Implicitly on")
                                       ((and (not in-plugin) in-storage)
                                        "Solo Off by automation")
                                       (else
                                        "Solo Off"))
                                 (let ((keybinding (or (get-displayable-keybinding-from-shortcut ra:switch-instrument-solo)
                                                       (get-displayable-keybinding-from-shortcut ra:switch-solo-for-selected-instruments))))
                                   (if keybinding
                                       (<-> " (" keybinding ")")
                                       "")))))

;; Note: Must return status bar id.
(define (FROM_C-display-bypass-status-in-statusbar instrument-id)  
  (define in-storage (<ra> :get-instrument-solo-from-storage instrument-id))
  (define in-plugin (<ra> :get-instrument-solo instrument-id))
  
  (<ra> :set-statusbar-text (<-> (<ra> :get-instrument-name instrument-id) ": "
                                 (cond ((and in-plugin in-storage)
                                        "Bypass On")
                                       ((and in-plugin (not in-storage))
                                        "Bypass On by automation")
                                       ((and (not in-plugin) in-storage)
                                        "Bypass Off by automation")
                                       (else
                                        "Bypass Off"))
                                 (let ((keybinding (or (get-displayable-keybinding-from-shortcut ra:switch-instrument-bypass)
                                                       (get-displayable-keybinding-from-shortcut ra:switch-bypass-for-selected-instruments))))
                                   (if keybinding
                                       (<-> " (" keybinding ")")
                                       "")))))


;(define (delete-all-selected-instruments)
;  (undo-block (lambda ()
;                (for-each (lambda (id-instrument)
;                            
;                            (if (string=? "Sample Player" (<ra> :get-instrument-type-name id-instrument))
;                                (<ra> :set-random-instrument-sample id-instrument)))
;                          (<ra> :get-extended-selected-instruments)))))
                
(define (set-random-sample-for-all-selected-sampler-instruments)
  (undo-block (lambda ()
                (for-each (lambda (id-instrument)
                            (if (string=? "Sample Player" (<ra> :get-instrument-type-name id-instrument))
                                (<ra> :set-random-instrument-sample id-instrument)))
                          (<ra> :get-extended-selected-instruments)))))

(define (FROM-C-generate-new-color-for-all-selected-instruments mix-background)
  (define new-color (<ra> :generate-new-color mix-background))
  (undo-block (lambda ()
                (for-each (lambda (id-instrument)
                            (<ra> :set-instrument-color new-color id-instrument))
                          (<ra> :get-extended-selected-instruments)))))


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
        (to-boolean connect-to-main-pipe)
        (to-boolean do-autoconnect) (to-boolean include-load-preset) (to-boolean must-have-inputs) (to-boolean must-have-outputs)
        (<ra> :gui_get-parent-window parentgui) ;; Improves plugin menu caching performance.
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
  ;;(c-display "INSTRCONF:" instrconf)
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
         (let ((filename (entry :preset-filename)))
           (if (<ra> :is-illegal-filepath filename)
               (request-select-instrument-preset (instrconf :parentgui) callback)
               (callback (create-load-instrument-preset-description filename)))))
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
                 (cons (list (entry :name)
                             mcallback)
                       (loop (cdr entries))))
;                 (let ((in-container (loop (cdr entries)))) ;; ??
;                   (if (and #f (null? in-container))
;                       (list (entry :name)
;                             mcallback)
;                       (append (list "------------aaaa")
;                               (list (list (entry :name)
;                                           mcallback))
;                               (list "------------bbbb")
;                               in-container
;                               (list "------------cccc")))))
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
                 (cons (if (<ra> :is-illegal-filepath (entry :preset-filename))
                           (list "Load Preset(s)"
                                 :enabled (instrconf :include-load-preset)
                                 mcallback)
                           (list (entry :name)
                                 mcallback))
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
(let ((my-callback (lambda x
                     (c-display "CALLBACK:" x)))
      (instrconf ""))
  (spr-entries->menu-entries (<ra> :get-sound-plugin-registry)
                             instrconf
                             (lambda (entry)
                               (spr-entry->instrument-description entry instrconf my-callback))
                             #t))

(pp (length (<ra> :get-sound-plugin-registry #t)))
(for-each (lambda (x) (display x)(newline)) (<ra> :get-sound-plugin-registry))
(<ra> :get-sound-plugin-registry #t)
||#

;; Note: Used for shortcut
(delafina (FROM_C-request-rename-instrument :instrument-id (<ra> :get-current-instrument))
  (when (<ra> :is-legal-instrument instrument-id)
    (define old-name (<ra> :get-instrument-name instrument-id))
    (define new-name (<ra> :request-string "" #t old-name))
    (if (and (not (string=? new-name ""))
             (not (string=? old-name new-name)))
        (<ra> :set-instrument-name new-name instrument-id))))

  
(define (is-connected-to-main-pipe id)
  (define main-pipe (<ra> :get-main-pipe-instrument))
  (if (equal? id main-pipe)
      #f
      (let loop ((id id))
        (or (equal? id main-pipe)
            (any? loop
                  (get-instruments-connecting-from-instrument id))))))

(define (is-directly-connected-to-main-pipe id)
  (<ra> :has-audio-connection id (<ra> :get-main-pipe-instrument)))


;; Note: Used for shortcut
(delafina (switch-connect-instrument-to-main-pipe :instrument-id (<ra> :get-current-instrument))
  (when (and (<ra> :is-legal-instrument instrument-id)
             (> (<ra> :get-num-output-channels instrument-id) 0))
    (if (is-connected-to-main-pipe instrument-id)
        (begin
          (<ra> :undo-mixer-connections)
          (<ra> :delete-audio-connection instrument-id (<ra> :get-main-pipe-instrument)))
        (<ra> :connect-audio-instrument-to-main-pipe instrument-id))))


(define (get-sample-player-mixer-popup-menu-entries instruments)
  (if (not (any? (lambda (id)
                   (string=? "Sample Player" (<ra> :get-instrument-type-name id)))
                 instruments))
      #f
      (list ;;"-----------Sample player"
            (list "Load random sample" ;;s from folders"
                  ra:set-random-sample-for-all-selected-instruments))))

;; Note: Used for shortcut
(delafina (insert-plugin-for-instrument :instrument-id (<ra> :get-current-instrument)
                                        :gui -2)
  (insert-new-instrument-between instrument-id
                                 (get-instruments-connecting-from-instrument instrument-id)
                                 #t
                                 -2
                                 #f))

(define (request-send-instrument instrument-id callback)
  ;;(define is-bus-descendant (<ra> :instrument-is-bus-descendant instrument-id))
  (define pure-buses (get-pure-buses))
  (define seqtrack-buses (get-seqtrack-buses))
  (define buses (append pure-buses seqtrack-buses))
  (define (create-entry-text instrument-id)
    (<-> *arrow-text* " " (<ra> :get-instrument-name instrument-id)))

  (define (apply-changes changes)
    (<ra> :undo-mixer-connections)
    (<ra> :change-audio-connections changes))

  (define (create-bus-entries instrument-ids)
    (map (lambda (send-id)
           (list (create-entry-text send-id)
                 :enabled (<ra> :can-audio-connect instrument-id send-id)
                 (lambda ()
                   (callback (lambda (gain changes)
                               (push-audio-connection-change! changes (list :type "connect"
                                                                            :source instrument-id
                                                                            :target send-id
                                                                            :connection-type *send-connection-type*
                                                                            :gain (if (= 0 (<ra> :get-num-output-channels send-id))
                                                                                      1.0 ;; Sink-links must always have gain 1.0 since you can't change sink-link volume.
                                                                                      gain)))
                               (apply-changes changes))))))
         instrument-ids))
    
  (define args
    (run-instrument-data-memoized
     (lambda()
       (list
        (create-bus-entries buses)
        
        "------------"
        
        ;; audio connections
        (create-bus-entries (sort-instruments-by-mixer-position-and-connections
                             (keep (lambda (id)
                                     (not (member id buses)))
                                   (begin
                                     (define ret (get-all-instruments-that-we-might-send-to instrument-id))
                                     ret))))))))
  
  (apply popup-menu args))


;; Note: Used for shortcut
(delafina (insert-send-for-instrument :instrument-id (<ra> :get-current-instrument))
  (request-send-instrument instrument-id
                           (lambda (create-send-func)
                             (create-send-func 0 '()))))


;; Note: used for shortcut
(delafina (switch-force-as-current-instrument :instrument-id (<ra> :get-current-instrument))
  
  (define is-forced (and (<ra> :is-current-instrument-locked)
                         (equal? instrument-id (<ra> :get-current-instrument))))
  (if is-forced
      (<ra> :set-current-instrument-locked #f)
      (begin
        (<ra> :set-current-instrument-locked #t)
        (<ra> :set-current-instrument instrument-id))))

(define (get-forced-as-current-instrument-menu-entry instrument-id)
  (list "Forced as current instrument"
        :check (and (<ra> :is-current-instrument-locked)
                    (equal? instrument-id (<ra> :get-current-instrument)))
        :shortcut switch-force-as-current-instrument
        (lambda (setit)
          (if setit
              (begin
                (<ra> :set-current-instrument-locked #t)
                (<ra> :set-current-instrument instrument-id))
              (<ra> :set-current-instrument-locked #f)))))

(delafina (get-instrument-popup-entries :instrument-id
                                        :parentgui
                                        :include-replace #t
                                        :must-have-inputs #f :must-have-outputs #f
                                        :include-insert-plugin #t
                                        :put-in-submenu #f
                                        )

  (define plain
    (list
     (and #f ;; Seems like instrument-id is always current instrument.
          (let ((is-current (equal? (<ra> :get-current-instrument)
                                    instrument-id)))
            (and (not is-current)
                 (list
                  (list "Set as current instrument"
                        :enabled (not is-current)
                        (lambda ()
                          (<ra> :set-current-instrument instrument-id #f)))
                  "------------------"))))
     
     (get-sample-player-mixer-popup-menu-entries (list instrument-id))
     (list "Connected to main pipe"
           :enabled (and (> (<ra> :get-num-output-channels instrument-id) 0)
                         (if (is-connected-to-main-pipe instrument-id)
                             (is-directly-connected-to-main-pipe instrument-id) ;; Connected, but only enable entry if connected directly.
                             (not (is-connected-somehow? (<ra> :get-main-pipe-instrument) instrument-id)))) ;; Don't enable if connecting would create a recursive graph.
           :check (is-connected-to-main-pipe instrument-id)
           :shortcut switch-connect-instrument-to-main-pipe
           (lambda (doit)
             (switch-connect-instrument-to-main-pipe instrument-id)))

     (get-forced-as-current-instrument-menu-entry instrument-id)     
                         
     (and include-insert-plugin
          "------------------")
     (and include-insert-plugin
          (list "Insert plugin"
                :enabled (> (<ra> :get-num-output-channels instrument-id) 0)
                :shortcut insert-plugin-for-instrument
                (lambda ()
                  (insert-plugin-for-instrument instrument-id))))

     (and include-insert-plugin
          (list "Insert send"
                :enabled (> (<ra> :get-num-output-channels instrument-id) 0)
                :shortcut insert-send-for-instrument
                (lambda ()
                  (insert-send-for-instrument instrument-id))))
     "---------------"
     
     (list "Delete"
           :enabled (not (<ra> :instrument-is-permanent instrument-id))
           :shortcut ra:delete-instrument
           (lambda ()
             (<ra> :delete-instrument instrument-id)))
     (list "Replace"
           :enabled (and include-replace
                         (not (<ra> :instrument-is-permanent instrument-id)))
           :shortcut replace-instrument
           (lambda ()
             (replace-instrument instrument-id must-have-inputs must-have-outputs parentgui)))
     (list "Rename"
           :shortcut FROM_C-request-rename-instrument
           (lambda ()
             (FROM_C-request-rename-instrument instrument-id)))
     
     "-----------"
     
     (list "Mute "
           :check (<ra> :get-instrument-mute instrument-id)
           :shortcut ra:switch-mute-for-selected-instruments
           (lambda (doit)
             (<ra> :set-instrument-mute doit instrument-id)))   
     (list "Solo"
           :check (<ra> :get-instrument-solo instrument-id)
           :shortcut ra:switch-solo-for-selected-instruments
           (lambda (doit)
             (<ra> :set-instrument-solo doit instrument-id)))
     (list "Bypass"
           :check (<ra> :get-instrument-bypass instrument-id)
           :shortcut ra:switch-bypass-for-selected-instruments
           (lambda (doit)
             (<ra> :set-instrument-bypass doit instrument-id)))
     
     "-----------"
     
     (list "Load Preset (.rec)" :enabled instrument-id
           :enabled (and include-replace
                         (not (<ra> :instrument-is-permanent instrument-id)))
           :shortcut ra:request-load-instrument-preset
           (lambda ()
             (<ra> :request-load-instrument-preset instrument-id "" parentgui)))
     (list "Save Preset (.rec)" :enabled instrument-id
           :enabled (and include-replace
                         (not (<ra> :instrument-is-permanent instrument-id)))
           :shortcut (list ra:eval-scheme "(ra:save-instrument-preset)") ;; ra:save-instrument-preset is not available from python since it has a dynvec_t argument
           (lambda ()
             (<ra> :save-instrument-preset (list instrument-id) parentgui)))
     
     "------------------"
     
     (list "Configure color"
           :shortcut FROM_C-show-instrument-color-dialog
           (lambda ()
             (FROM_C-show-instrument-color-dialog parentgui instrument-id)))
     (list "Generate new color"
           :shortcut ra:generate-new-instrument-color
           (lambda ()
             (<ra> :generate-new-instrument-color instrument-id)))
     
     "--------"
     
     (list "Show GUI"
           :enabled (<ra> :has-native-instrument-gui instrument-id)
           :check (<ra> :instrument-gui-is-visible instrument-id parentgui)
           :shortcut ra:show-hide-instrument-gui
           (lambda (enabled)
             (if enabled
                 (<ra> :show-instrument-gui instrument-id parentgui #f)
                 (<ra> :hide-instrument-gui instrument-id))))
     (list "Recv. external MIDI"
           :check (<ra> :instrument-always-receive-midi-input instrument-id)
           :shortcut ra:switch-set-instrument-always-receive-midi-input
           (lambda (onoff)
             (<ra> :set-instrument-always-receive-midi-input instrument-id onoff)))
     
     "--------"
   
     (list "Show Info"
           :shortcut (list ra:eval-scheme "(ra:show-instrument-info)")
           (lambda ()
             (<ra> :show-instrument-info instrument-id parentgui))
           
           )
     ))

  (define instrument-name (<-> "\"" (<ra> :get-instrument-name instrument-id) "\""))
  
  (define header (<-> "Instrument: " instrument-name))

  (if put-in-submenu
      (list instrument-name
            plain)
      (cons (<-> "--------------" header)
            plain)))


              



(define (get-instrument-popup-menu-args instrconf callback)
  
  (define (my-callback entry)
    (*popup-menu-curr-callback* entry))
  
  (set! *popup-menu-curr-callback* callback) ;; Since there should never be more than one popup open at the same time, this should work, hopefully.
  
  (let ((curr-generation (<ra> :get-sound-plugin-registry-generation)))
    (when (or ;;#t
              (not (eq? *popup-menu-args-cache-preset-in-clipboard* (<ra> :instrument-preset-in-clipboard)))
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
(pretty-print (ra:get-sound-plugin-registry))

(ra:show-message (number->string (length (ra:get-sound-plugin-registry))))
(for-each (lambda (i)
            (<ra> :add-message (pp ((<ra> :get-sound-plugin-registry) i))))
          (iota 50))
!!#

;; async
(define (start-instrument-popup-menu instrconf callback)
  (popup-menu-from-args (get-instrument-popup-menu-args instrconf callback)))

#!!
(for-each c-display (get-instrument-popup-menu-args (make-instrument-conf :connect-to-main-pipe #t
                                                      :parentgui -1)
                                (lambda (descr)
                                  (c-display "selected:" descr))))
!!#

(define (create-instrument instrconf description)
  (undo-block
   (lambda ()
     (let ((instrument-id (<ra> :create-audio-instrument-from-description description "" (instrconf :x) (instrconf :y))))
       (when (<ra> :is-legal-instrument instrument-id)
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
           (<ra> :reset-instrument-effect instrument-id effect-name)))
         
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
           (FROM-C-show-help-window "help/effect_popup_menu_instrument_framed.html")))
   
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
  #f
  )

;;  Sjekk ut hva dette var godt for. Kanskje det har noe med at feil verdi blir lagra i undo om ikke denne blir kalt først.
;                                        :pre-undo-block-callback (lambda ()
;                                                                   (<ra> :undo-instrument-effect instrument-id effect-name) ;; store value before assigning modulator.
;                                                                   #f
;                                                                   ))))
#!!
(show-note-duplicator-popup-menu (<ra> :get-current-instrument) "System Chance Voice 1")
!!#



(define (show-set-current-instrument-popup-menu)
  (popup-menu (map (lambda (instrument-id)
                     (list (<ra> :get-instrument-name instrument-id)
                           :enabled (not (equal? instrument-id (<ra> :get-current-instrument)))
                           (lambda ()
                             (<ra> :set-current-instrument instrument-id #f)
                             )))
                   (sort-instruments-by-mixer-position-and-connections 
                    (get-all-audio-instruments)))))


(define (delete-all-unused-MIDI-instruments)
  (define used-instruments (<new> :container '() equal?))
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

(define (find-instrument-in-modular-mixer-coordinate-relation-to goal-instrument-id better?)
  (define x (<ra> :get-instrument-x goal-instrument-id))
  (define y (<ra> :get-instrument-y goal-instrument-id))
  (let loop ((instruments (get-all-audio-instruments))
             (best-instrument-id #f)
             (best-dx 1000000000000.0)
             (best-dy 1000000000000.0))
    (define instrument-id (cl-car instruments))
    (if (not instrument-id)
        best-instrument-id
        (let* ((x2 (<ra> :get-instrument-x instrument-id))
               (y2 (<ra> :get-instrument-y instrument-id))
               (dx (abs (- x x2)))
               (dy (abs (- y y2))))
          (if (better? x y x2 y2 dx dy best-dx best-dy)
              (loop (cdr instruments)
                    instrument-id
                    dx
                    dy)
              (loop (cdr instruments)
                    best-instrument-id
                    best-dx
                    best-dy))))))

(define (find-instrument-in-mixer-strips-coordinate-relation-to goal-instrument-id better?)
  (<declare-variable> mixer-strips-get-configuration)
  (define gui (<gui> :get-mixer-strips-gui-in-active-window))
  (if (< gui 0)
      #f
      (let ()
        (define instrument-settings (keep (lambda (s)
                                            (s :is-enabled))
                                          ((mixer-strips-get-configuration gui) :instrument-settings)))
        (define instrument-guis (apply hash-table
                                       (flatten (map (lambda (s)
                                                       (list (s :instrument-id) (s :gui-id)))
                                                     instrument-settings))))
        (define (get-box instrument-id)
          (define x1 (<gui> :get-global-x (instrument-guis instrument-id)))
          (define y1 (<gui> :get-global-y (instrument-guis instrument-id)))
          (make-box2 x1
                     y1
                     (+ x1 (<gui> :width (instrument-guis instrument-id)))
                     (+ y1 (<gui> :height (instrument-guis instrument-id)))))
        
        (define (make-instrument-data instrument-id)
          (hash-table :box (get-box instrument-id)
                      :instrument-id instrument-id))
        
        (if (not (instrument-guis goal-instrument-id))
            #f
            (let ()
              (define goal-box (get-box goal-instrument-id))
              (let loop ((datas (map (lambda (s)
                                       (make-instrument-data (s :instrument-id)))
                                     instrument-settings))
                         (best-data (<optional-hash-table>)))
                (define data (cl-car datas))
                (cond ((not data)
                       (and best-data
                            (best-data :instrument-id)))
                      ((and (not (equal? (data :instrument-id)
                                         goal-instrument-id))
                            (better? goal-box
                                     best-data
                                     data))
                       (loop (cdr datas)
                             data))
                      (else
                       (loop (cdr datas)
                             best-data)))))))))

(define* (find-instrument-in-mixer-strips-to-the-up-of (instrument-id (ra:get-current-instrument)))
  (find-instrument-in-mixer-strips-coordinate-relation-to
   instrument-id
   (lambda (box best-data data)
     (define box2 (data :box))
     (define y (box :y))
     (define maybe-y (box2 :y))
     (define best-box (and best-data
                           (best-data :box)))
     (define best-y (and best-box
                         (best-box :y)))
     (and (< maybe-y y)
          (inside-box? box2 (box :x) (box2 :y))
          (or (not best-y)
              (> maybe-y best-y))))))
                                                                        
(define* (find-instrument-in-mixer-strips-to-the-down-of (instrument-id (ra:get-current-instrument)))
  (find-instrument-in-mixer-strips-coordinate-relation-to
   instrument-id
   (lambda (box best-data data)
     (define box2 (data :box))
     (define y (box :y))
     (define maybe-y (box2 :y))
     (define best-box (and best-data
                           (best-data :box)))
     (define best-y (and best-box
                         (best-box :y)))
     (and (> maybe-y y)
          (inside-box? box2 (box :x) (box2 :y))
          (or (not best-y)
              (< maybe-y best-y))))))
                                                                        
(define* (find-instrument-in-mixer-strips-to-the-left-of (instrument-id (ra:get-current-instrument)))
  (find-instrument-in-mixer-strips-coordinate-relation-to
   instrument-id
   (lambda (box best-data data)
     (define box2 (data :box))
     (define x (box :x))
     (define maybe-x (box2 :x))
     (define y (box :y))
     (define maybe-y (box2 :y))
     (define best-box (and best-data
                           (best-data :box)))
     (define best-x (and best-box
                         (best-box :x)))
     (define best-y (and best-box
                         (best-box :y)))
     
     (define in-same-row (inside-box? box2 (box2 :x) (box :y)))
     (define best-in-same-row-as-box (and best-box
                                          (inside-box? best-box (best-box :x) (box :y))))
     (define best-in-same-row-as-box2 (and best-box
                                           (inside-box? best-box (best-box :x) (box2 :y))))

     ;;(c-display (<ra> :get-instrument-name (data :instrument-id)) ". in-same:" in-same-row ". best-in-same-as-box:" best-in-same-row-as-box ". best-in-same-as-box2:" best-in-same-row-as-box2)
     (cond (in-same-row
            (and (< maybe-x x)
                 (or (not best-x)
                     (not best-in-same-row-as-box)
                     (> maybe-x best-x))))
           ((< maybe-y y)
            (cond ((not best-box)
                   #t)
                  (best-in-same-row-as-box2
                   (> maybe-x best-x))
                  (else
                   (> maybe-y best-y))))
           (else
            #f)))))

(define* (find-instrument-in-mixer-strips-to-the-right-of (instrument-id (ra:get-current-instrument)))
  (find-instrument-in-mixer-strips-coordinate-relation-to
   instrument-id
   (lambda (box best-data data)
     (define box2 (data :box))
     (define x (box :x))
     (define maybe-x (box2 :x))
     (define y (box :y))
     (define maybe-y (box2 :y))
     (define best-box (and best-data
                           (best-data :box)))
     (define best-x (and best-box
                         (best-box :x)))
     (define best-y (and best-box
                         (best-box :y)))
     
     (define in-same-row (inside-box? box2 (box2 :x) (box :y)))
     (define best-in-same-row-as-box (and best-box
                                          (inside-box? best-box (best-box :x) (box :y))))
     (define best-in-same-row-as-box2 (and best-box
                                           (inside-box? best-box (best-box :x) (box2 :y))))

     ;;(c-display (<ra> :get-instrument-name (data :instrument-id)) ". in-same:" in-same-row ". best-in-same-as-box:" best-in-same-row-as-box ". best-in-same-as-box2:" best-in-same-row-as-box2)
     (cond (in-same-row
            (and (> maybe-x x)
                 (or (not best-x)
                     (not best-in-same-row-as-box)
                     (< maybe-x best-x))))
           ((> maybe-y y)
            (cond ((not best-box)
                   #t)
                  (best-in-same-row-as-box2
                   (< maybe-x best-x))
                  (else
                   (< maybe-y best-y))))
           (else
            #f)))))
                                                                        
#!!
(and-let* ((res (find-instrument-in-mixer-strips-to-the-up-of)))
          (<ra> :get-instrument-name res))
(and-let* ((res (find-instrument-in-mixer-strips-to-the-down-of)))
          (<ra> :get-instrument-name res))
(and-let* ((res (find-instrument-in-mixer-strips-to-the-left-of)))
          (<ra> :get-instrument-name res))
(and-let* ((res (find-instrument-in-mixer-strips-to-the-right-of)))
          (<ra> :get-instrument-name res))

(<ra> :schedule 1000
      (lambda ()
        (c-display "curr:" (<gui> :get-mixer-strips-gui-with-mouse-pointer-above-it))
        #f))
(<ra> :schedule 1000
      (lambda ()
        (c-display "curr:" (<gui> :get-mixer-strips-gui-in-active-window))
        #f))
!!#


(define* (find-instrument-in-modular-mixer-to-the-left-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-in-modular-mixer-coordinate-relation-to goal-instrument-id
                                                                      (lambda (x y x2 y2 dx dy best-dx best-dy)
                                                                        (and (< x2 x)
                                                                             (or (< dy best-dy)
                                                                                 (and (= dy best-dy)
                                                                                      (< dx best-dx)))))))
                      
(define* (find-instrument-in-modular-mixer-to-the-right-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-in-modular-mixer-coordinate-relation-to goal-instrument-id
                                                                      (lambda (x y x2 y2 dx dy best-dx best-dy)
                                                                        (and (> x2 x)
                                                                             (or (< dy best-dy)
                                                                                 (and (= dy best-dy)
                                                                                      (< dx best-dx)))))))
                      
(define* (find-instrument-in-modular-mixer-to-the-up-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-in-modular-mixer-coordinate-relation-to goal-instrument-id
                                                                      (lambda (x y x2 y2 dx dy best-dx best-dy)
                                                                        (and (< y2 y)
                                                                             (or (< dx best-dx)
                                                                                 (and (= dx best-dx)
                                                                                      (< dy best-dy)))))))
                      
(define* (find-instrument-in-modular-mixer-to-the-down-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-in-modular-mixer-coordinate-relation-to goal-instrument-id
                                                                      (lambda (x y x2 y2 dx dy best-dx best-dy)
                                                                        (and (> y2 y)
                                                                             (or (< dx best-dx)
                                                                                 (and (= dx best-dx)
                                                                                      (< dy best-dy)))))))
                      

(define* (find-instrument-to-the-X-of modular-func strips-func (goal-instrument-id (ra:get-current-instrument)))
  (cond ;;((a-mixer-strip-window-is-active?)
        ;; (strips-func goal-instrument-id))
        ((and (<gui> :is-active-window  (<gui> :get-main-mixer-gui))
              (<ra> :main-mixer-is-modular))
         (modular-func goal-instrument-id))
        (else
         (strips-func goal-instrument-id))))

(define* (find-instrument-to-the-left-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-to-the-X-of find-instrument-in-modular-mixer-to-the-left-of
                               find-instrument-in-mixer-strips-to-the-left-of
                               goal-instrument-id))

(define* (find-instrument-to-the-right-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-to-the-X-of find-instrument-in-modular-mixer-to-the-right-of
                               find-instrument-in-mixer-strips-to-the-right-of
                               goal-instrument-id))

(define* (find-instrument-to-the-up-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-to-the-X-of find-instrument-in-modular-mixer-to-the-up-of
                               find-instrument-in-mixer-strips-to-the-up-of
                               goal-instrument-id))

(define* (find-instrument-to-the-down-of (goal-instrument-id (ra:get-current-instrument)))
  (find-instrument-to-the-X-of find-instrument-in-modular-mixer-to-the-down-of
                               find-instrument-in-mixer-strips-to-the-down-of
                               goal-instrument-id))

                      

#!!
(and-let* ((res (<ra> :get-current-instrument)))
          (<ra> :get-instrument-name res))
(and-let* ((res (find-instrument-to-the-left-of)))
          (<ra> :get-instrument-name res))
(and-let* ((res (find-instrument-to-the-right-of)))
          (<ra> :get-instrument-name res))
(and-let* ((res (find-instrument-to-the-up-of)))
          (<ra> :get-instrument-name res))
(and-let* ((res (find-instrument-to-the-down-of)))
          (<ra> :get-instrument-name res))

(<ra> :switch-main-mixer-is-modular)
!!#

(define (FROM_C-move-current-instrument-left)
  (and-let* ((res (find-instrument-to-the-left-of)))
            (<ra> :set-current-instrument res #f #t)))
  
(define (FROM_C-move-current-instrument-right)
  (and-let* ((res (find-instrument-to-the-right-of)))
            (<ra> :set-current-instrument res #f #t)))
  
(define (FROM_C-move-current-instrument-up)
  (and-let* ((res (find-instrument-to-the-up-of)))
            (<ra> :set-current-instrument res #f #t)))
  
(define (FROM_C-move-current-instrument-down)
  (and-let* ((res (find-instrument-to-the-down-of)))
            (<ra> :set-current-instrument res #f #t)))

(define (get-select-track-instrument-popup-entries tracknum)
  (define midi-instruments (get-all-midi-instruments))
  (define instruments-before (get-all-audio-instruments))
  
  (define (is-new-instrument? id-instrument)
    (and (not (member id-instrument instruments-before))
         (member id-instrument (get-all-audio-instruments))))
  
  (define (num-new-instruments)
    (- (length (get-all-audio-instruments))
       (length instruments-before)))
  
  (define-macro (load . code)
    `(undo-block
      (lambda ()
        (define id-instrument (begin ,@code))
        (when (and (instrument? id-instrument)
                   (<ra> :is-legal-instrument id-instrument))
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

  (define instr-conf (make-instrument-conf :connect-to-main-pipe #t
                                           :parentgui -1))
  (define (callback descr)
    (load (<ra> :create-audio-instrument-from-description descr)))
  
  (list
   "----------Create a new instrument"
   "New Sample Player" (lambda ()
                         (load (<ra> :create-audio-instrument "Sample Player" "Sample Player")))
  ;;"<New FluidSynth>" (lambda ()
   ;;                     (load (<ra> :create-audio-instrument "FluidSynth" "FluidSynth")))
   ;;(if (<ra> :has-pure-data)
   ;;    (list "<New Pd Instrument>" (lambda ()
   ;;                                  (load (<ra> :create-audio-instrument "Pd" "Simple Midi Synth"))))
   ;;    #f)
   "----------------"
   "New Audio Instrument" (lambda ()
                            (start-instrument-popup-menu instr-conf callback))
   "New MIDI Instrument" (lambda ()
                             (load (<ra> :create-midi-instrument "Unnamed")))
   "----------------"
   "Load Preset" (lambda ()
                   (request-select-instrument-preset -1 callback))
   "----------------"
   "Show plugin manager" (lambda ()
                           (pmg-start instr-conf callback))

   "----------Clone an existing instrument"
   "All" (map (lambda (num instrument-id)
                (if (<ra> :instrument-is-permanent instrument-id)
                    #f
                    (list (<-> num ". " (<ra> :get-instrument-name instrument-id))
                          (lambda ()
                            (load (<ra> :clone-audio-instrument instrument-id))))))
              (iota (length instruments-before))
              instruments-before)
   "----------Use an existing instrument"
   "All" (get-instrument-entries #f)
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
    (when (<ra> :is-legal-instrument id)
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


(define (get-instrument-background-color gui instrument-id)
  (if (<ra> :is-legal-instrument instrument-id)
      (<gui> :mix-colors
             (<ra> :get-instrument-color instrument-id)
             (<gui> :get-background-color gui)
             0.3)
      "#666660"))

(define (get-instrument-border-color instrument-id)
  (define color #f)
  (if (equal? (<ra> :get-current-instrument) instrument-id)
      (set! color *current-mixer-strip-border-color*))
  (if (<ra> :instrument-is-selected instrument-id)
      ;;(let ((selcolor "#8888ee"))
      (let ((selcolor "#999999"))
        (if color
            (set! color (<gui> :mix-colors color selcolor 0.5))
            (set! color selcolor))))
  color)



(define (paint-instrument-background-color gui x1 y1 x2 y2 instrument-id)
  (define background-color (get-instrument-background-color gui instrument-id))
  (<gui> :filled-box gui background-color x1 y1 x2 y2))

(delafina (draw-mutesolo :gui
                         :type
                         :instrument-id
                         :x1 :y1 :x2 :y2
                         :is-selected 'undefined
                         :use-single-letters
                         :is-hovering #f
                         :background-color #f
                         :border 0
                         :implicit-border 1
                         :seqtracknum #f) ;; needs to be set if type is 'height

  (define (get-muted)
    (<ra> :get-instrument-mute instrument-id))
  (define (get-soloed)
    (<ra> :get-instrument-solo instrument-id))
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
                          (<-> "H" ((vector -1 1 2 3 0) (<ra> :get-seqtrack-min-height-type seqtracknum)))
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

  (define is-implicitly (cond ((and (eq? type 'mute)
                                    instrument-id
                                    (<ra> :is-legal-instrument instrument-id))
                               (<ra> :instrument-is-implicitly-muted instrument-id))
                              ((and (eq? type 'solo)
                                    instrument-id
                                    (<ra> :is-legal-instrument instrument-id))
                               (<ra> :instrument-is-implicitly-soloed instrument-id))
                              (else
                               #f)))

  ;;(c-display "background-color:" background-color)
  (draw-button gui text is-selected x1 y1 x2 y2 color
               :background-color background-color
               :is-hovering is-hovering
               :y-border border
               :x-border border
               :paint-implicit-border is-implicitly
               :implicit-border-width implicit-border
               :box-rounding 2)
  )

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
                           :border-color (get-instrument-border-color instrument-id)
                           ))
  

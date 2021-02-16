(provide 'import_mod.scm)


;; milkytracker fx descriptions: http://milkytracker.org/docs/MilkyTracker.html#effects
;; openMpt fx description: http://wiki.openmpt.org/Manual:_Effect_Reference
;; The mod format document: http://www.aes.id.au/modformat.html
;;
;; http://demozoo.org/music/?page=2
;; https://files.scene.org




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct event
  :patternnum
  :channel
  :linenum
  :type
  :value
  :value2 0
  :instrumentnum 0
  :tick 0
  :is-pattern-delay-line #f)

(delafina (m-e-0 :type type
                 :pattern 0
                 :channel 0
                 :instrumentnum 0
                 :line 0
                 :value 0
                 :value2 0
                 :tick 0
                 :is-pattern-delay-line #f                 
                 )
  ;;(c-display "gakk gakk" pattern channel line type value value2 instrumentnum tick is-pattern-delay-line)
  (make-event-nokeywords pattern
                         channel              
                         line
                         type
                         value
                         value2
                         instrumentnum
                         tick
                         is-pattern-delay-line
                         ))

(define (m-e type . rest)
  (if (not (memq type '(:note :velocity :break :loop :stop :pitch-slide :slide-to-note :fine-pitch-slide :vibrato :tremolo :sample-offset :velocity-slide :fine-velocity-slide :pattern-delay :tpd :bpm :retrigger-note :delay-note :arpeggio :position-jump :finetune)))
      (error 'unknown-event-type (<-> "Unknwon event type " type)))
  (apply m-e-0
         (append (list :type type)
                 rest)))
#||
(make-event 0 0 29 :note 0 0 0 0 #f)
(m-e :note :line 29 :pattern 0)
(print-event (m-e :break
                  :line 8))
(pp (procedure-source 'make-event))
||#

(define (event-to-string event)
  (<-> "[" (event :type) (make-string (- 20 (string-length (<-> (event :type)))) #\space)
       ":value " (event :value)
       (if (> (event :value2) 0)
           (<-> ", :value2 " (event :value2))
           "")
       ", :p " (event :patternnum)
       ", :c " (event :channel)
       ", :line " (if (integer? (event :linenum))
                      (event :linenum)
                      (* 1.0 (event :linenum)))
       (if (> (event :tick) 0)
           (<-> ", :tick " (event :tick))
           "")
       (if (not (= (event :instrumentnum) 0))
           (<-> ", :instrument " (event :instrumentnum))
           "")
       (if (event :is-pattern-delay-line)
           ", :is-pattern-delay-line #t"
           "")
       "]"))


(define (print-event event)
  (c-display (event-to-string event)))

(define (print-events events)
  (newline)
  (for-each print-event events))
                  
                                                 
(define-constant *default-tpd* 6)
(define-constant *default-bpm* 125)

;; Note: add-event doesn't sort channels
(define (add-event events new-event)
  (if (null? events)
      (list new-event)
      (begin
        (define event (car events))
        (if (and (<= (new-event :patternnum)
                     (event :patternnum))
                 (<= (new-event :linenum)
                     (event :linenum)))
            (cons new-event
                  events)
            (cons event
                  (add-event (cdr events) new-event))))))


(define (place-line place)
  (floor place))
(define (place-counter place)
  (numerator (- place (place-line place))))
(define (place-dividor place)
  (denominator (- place (place-line place))))

(define (place-list place)
  (let* ((line (place-line place))
         (rest (- place line)))
    (list line
          (numerator rest)
          (denominator rest))))
          
(define (event->place-list event tpds)
  (let ((linenum (event :linenum)))
    (list linenum
          (event :tick)
          (tpds linenum))))

(define (place-list-equal? place-list1 place-list2)
  (= (+ (car place-list1)
        (/ (cadr place-list1)
           (caddr place-list1)))
     (+ (car place-list2)
        (/ (cadr place-list2)
           (caddr place-list2)))))


(define (get-pattern events patternnum)
  (keep (lambda (event)
          (= (event :patternnum)
             patternnum))
        events))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion between note number and period ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant *note-to-period-table* (vector
  1712 1616 1524 1440 1356 1280 1208 1140 1076 1016  960  906  ;; c0 ->
  856  808  762  720  678  640  604  570  538  508  480  453   ;; c1 ->
  428  404  381  360  339  320  302  285  269  254  240  226   ;; c2 ->
  214  202  190  180  170  160  151  143  135  127  120  113   ;; c3 ->
  107  101   95   90   85   80   75   71   67   63   60   56   ;; c4 ->

  53  50  47  45  42  40  37  35  33  31  30  28               ;; c5 ->
  27  25  24  22  21  20  19  18  17  16  15  14               ;; c6 ->
  ))

(define-constant *max-period* (*note-to-period-table* 0))
(define-constant *min-period* (last (vector->list *note-to-period-table*)))
(define-constant *max-slide-period* 856)
(define-constant *min-slide-period* 113)

(define (get-period-to-note-table period period1 period2 note1)
  ;;(c-display period period1 period2 note1)
  (cond ((< period 0)
         '())
        ((< period period2)
         (get-period-to-note-table period
                                   period2
                                   (if (>= (+ 2 note1)
                                           (vector-length *note-to-period-table*))
                                       0
                                       (*note-to-period-table* (+ 2 note1)))
                                   (1+ note1)))
        (else
         (cons (scale period
                      period1 period2
                      note1 (1+ note1))
               (get-period-to-note-table (1- period)
                                         period1
                                         period2
                                         note1)))))

;; Note that period values between 14 and 0 are not correct (the values are scaled between 83 and 84 instead of 83 and +inf),
;; but it shouldn't matter since these periods are not allowed/used in modules.
(define-constant *period-to-note-table* (list->vector
                                (reverse
                                 (get-period-to-note-table *max-period*
                                                           (*note-to-period-table* 0)
                                                           (*note-to-period-table* 1)
                                                           0))))

(define (note->period note)
  (if (integer? note)
      (*note-to-period-table* note)
      (scale note
             (floor note) (ceiling note)
             (*note-to-period-table* (floor note))
             (*note-to-period-table* (ceiling note)))))

(define (period->note period)
  (*period-to-note-table* (round period)))


(***assert*** (period->note 1712)
              0)
(***assert*** (period->note 1616)
              1)
(***assert*** (period->note (/ (+ 1712 1616) 2))
              1/2)
(***assert*** (period->note 14)
              83)
(***assert*** (period->note 0)
              84)
(***assert*** #t
              (apply and (map (lambda (note)
                                (= note
                                   (period->note (note->period note))))
                              (iota 84))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; convert between events and patterns ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (events-to-patterns-0 events pattern)
  (if (null? events)
      (list (reverse pattern))
      (let ((event (car events))
            (patternnum ((car pattern) :patternnum)))
        (if (= (event :patternnum) patternnum)
            (events-to-patterns-0 (cdr events) (cons event pattern))
            (cons (reverse pattern)
                  (events-to-patterns-0 (cdr events) (list event)))))))
                
(define (events-to-patterns events)
  (if (null? events)
      '()
      (events-to-patterns-0 (cdr events)
                            (list (car events)))))


(***assert*** (events-to-patterns (list (m-e :note :line 29 :pattern 0)
                                        (m-e :break :line 30 :pattern 0)                                        
                                        (m-e :break :line 31 :pattern 1)))
              (list (list (m-e :note :line 29 :pattern 0)
                          (m-e :break :line 30 :pattern 0))
                    (list (m-e :break :line 31 :pattern 1))))
              

(define (patterns-to-events patterns)
  (apply append patterns))


(let ((events (list (m-e :note :line 29 :pattern 0)
                    (m-e :note :line 30 :pattern 0)
                    (m-e :break :line 30 :pattern 0)
                    (m-e :break :line 31 :pattern 1)
                    (m-e :break :line 31 :pattern 3))))
  (***assert*** (patterns-to-events (events-to-patterns events))
                events))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; convert between events and patterns ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'events' is just a flat sorted list of all events in all patterns;
;; 'patterns' is a vector of patterns, where a 'pattern' is a list of events for that pattern.

(define (group-events-in-patterns events)
  (define patterns (group-by (lambda (event) ;; TODO: It would be quicker to do this manually. group-by is doesn't assume that patternnum is always an increasing number. Well, group-by uses hash tables, so it's probably not very much slower.
                               (event :patternnum))
                             =
                             events))
  (list->vector (let loop ((patterns patterns) ;; Create empty list for empty patterns. (don't think that can happen actually, at least not when writing this comment, but this function doesn't know that)
                           (expected-patternnum 0))
                  (if (null? patterns)
                      '()
                      (let* ((pattern (car patterns))
                             (event (car pattern)))
                        (if (not (= (event :patternnum) expected-patternnum))
                            (cons '()
                                  (loop patterns
                                        (1+ expected-patternnum)))
                            (cons pattern
                                  (loop (cdr patterns)
                                        (1+ expected-patternnum)))))))))
                  

(define (ungroup-patterns-into-events patterns)
  (apply append
         (vector->list patterns)))

(let ((events (list (m-e :note :line 29 :pattern 0)
                    (m-e :note :line 30 :pattern 0)
                    (m-e :break :line 30 :pattern 0)
                    (m-e :break :line 31 :pattern 1)
                    (m-e :break :line 31 :pattern 3))))
  (***assert*** 4 (vector-length (group-events-in-patterns events)))
  (***assert*** (vector-ref (group-events-in-patterns events) 0)
                (get-pattern events 0))
  (***assert*** (ungroup-patterns-into-events (group-events-in-patterns events))
                events))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; convert between pattern and channels ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; can be optimized
(define (pattern-events-to-channels pattern-events break-event num-channels)
  (map (lambda (channelnum)
         (append (keep (lambda (event)
                         (and (= (event :channel) channelnum)
                              (not (eq? (event :type) :break))))
                       pattern-events)
                 (list (<copy-event> break-event  ;; add the break event to every channel.
                                     :channel channelnum))))
       (iota num-channels)))

;; Can be optimized a lot. Likely that this function is not needed though.
(define (channels-to-pattern channels)
  (c-display "warning, events are not sorted by channel when converting channels->pattern")
  (let loop ((result '())
             (events (apply append channels)))
    (if (null? events)
        result
        (loop (add-event result (car events)) ;; Not quite correct. add-event doesn't insert into channel-correctly.
              (cdr events)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Various pattern and playlist functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (replace-merged-patterns-in-playlist playlist from1 from2 to)
  (cond ((null? playlist)
         playlist)
        ((null? (cdr playlist))
         playlist)
        ((and (= (car playlist) from1)
              (= (cadr playlist) from2))
         (cons to
               (replace-merged-patterns-in-playlist (cddr playlist) from1 from2 to)))
        (else
         (cons (car playlist)
               (replace-merged-patterns-in-playlist (cdr playlist) from1 from2 to)))))

(define (replace-splitted-patterns-in-playlist playlist from to1 to2)
  (cond ((null? playlist)
         '())
        ((= (car playlist) from)
         (append (list to1 to2)
                 (replace-splitted-patterns-in-playlist (cdr playlist) from to1 to2)))
        (else
         (cons (car playlist)
               (replace-splitted-patterns-in-playlist (cdr playlist) from to1 to2)))))

  
(define (find-num-patterns events)
  (if (null? events)
      0
      (1+ ((last events) :patternnum))))
        

(define (get-num-lines-in-pattern events)
  (define event (car events))
  (if (eq? (event :type) :break)
      (event :linenum)
      (get-num-lines-in-pattern (cdr events))))

(***assert*** (get-num-lines-in-pattern (list (m-e :note)
                                              (m-e :break :line 64)))
              64)
#||
(define (remove-everything-after-break events end-linenum patternnum)
  (let ((event (car events)))
    (cond ((>= (event :linenum) end-linenum)
           (list (m-e :break :linenum end-linenum :patternnum patternnum))
          ((eq? (event :type) :break)
           (remove-everything-after-break (cdr events) (1+ (event :linenum)) patternnum))
          (else
           (cons event
                 (remove-everything-after-break (cdr events) end-linenum patternnum)))))))
||#

(define (replace-pattern-in-patterns patterns patternnum new-pattern)
  (list->vector (list-replace-element (vector->list patterns)
                                      patternnum
                                      new-pattern)))

(define (find-first-break-in-pattern events patternnum)
  (if (null? events)
      #f
      (let ((event (car events)))
        (cond ((< (event :patternnum) patternnum)
               (find-first-break-in-pattern (cdr events) patternnum))
              ((> (event :patternnum) patternnum)
               #f)
              ((eq? (event :type) :break)
               event)
              (else
               (find-first-break-in-pattern (cdr events) patternnum))))))

(***assert*** (find-first-break-in-pattern (list (m-e :note)) 0)
              #f)

(define (remove-break-events-and-everything-after events)
  (let ((event (car events)))
    (if (eq? (event :type) :break)
        '()
        (cons event
              (remove-break-events-and-everything-after (cdr events))))))

(define (append-two-patterns pattern1 pattern2 patternnum)
  (define num-lines-in-pattern1 (get-num-lines-in-pattern pattern1))
  (append (map (lambda (event)
                 (<copy-event> event
                               :patternnum patternnum))
               (remove-break-events-and-everything-after pattern1))
          (map (lambda (event)
                 (<copy-event> event
                               :linenum (+ num-lines-in-pattern1 (event :linenum))
                               :patternnum patternnum))
               pattern2)))
  
(define (append-patterns events from-patternnum1 from-patternnum2)
  (define to-patternnum (find-num-patterns events))
  (define new-pattern1 (get-pattern events from-patternnum1))
  (define new-pattern2 (get-pattern events from-patternnum2))

  (append-two-patterns new-pattern1 new-pattern2 to-patternnum))

(***assert*** (append-patterns (list (m-e :note :pattern 0)
                                     (m-e :break :line 64)
                                     (m-e :note :pattern 1)
                                     (m-e :break :line 64 :pattern 1))
                               0 1)
              (list (m-e :note :line 0 :pattern 2)
                    (m-e :note :line 64 :pattern 2)
                    (m-e :break :line 128 :pattern 2)))



(define (add-new-merged-pattern-to-events playlist events from-patternnum1 from-patternnum2)
  (define to-patternnum (find-num-patterns events))
  (define new-pattern (append-patterns events from-patternnum1 from-patternnum2))
  (define new-playlist (replace-merged-patterns-in-playlist playlist from-patternnum1 from-patternnum2 to-patternnum))
  
  (list new-playlist
        (append events
                new-pattern)))

(define (add-new-merged-pattern-to-patterns playlist patterns from-patternnum1 from-patternnum2)
  (define to-patternnum (vector-length patterns))
  (define new-pattern (append-two-patterns (vector-ref patterns from-patternnum1)
                                           (vector-ref patterns from-patternnum2)
                                           to-patternnum))
  (define new-playlist (replace-merged-patterns-in-playlist playlist from-patternnum1 from-patternnum2 to-patternnum))
  
  (list new-playlist
        (vector-append patterns
                       (vector new-pattern))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-unused-patterns                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (remove-unused-patterns-0 playlist patterns num-original-patterns)

  (define (pattern-has-correct-patternnum? pattern correct-patternnum)
    (if (null? pattern)
        #t
        (= ((car pattern) :patternnum)
           correct-patternnum)))
        
  (define (set-patternnum-in-pattern pattern new-patternnum)
    (if (pattern-has-correct-patternnum? pattern new-patternnum)
        pattern
        (map (lambda (event)
               (<copy-event> event
                             :patternnum new-patternnum))
             pattern)))
  
  (define (remove-pattern patterns patternnum-to-remove)
    (list->vector (list-remove (vector->list patterns)
                               patternnum-to-remove)))
                        
  (define (remove-from-playlist playlist patternnum)
    (if (null? playlist)
        '()
        (let ((p (car playlist)))
          (if (> p patternnum)
              (cons (1- p)
                    (remove-from-playlist (cdr playlist) patternnum))
              (cons p
                    (remove-from-playlist (cdr playlist) patternnum))))))

  (define (fix-patternnums patterns)
    (list->vector
     (let loop ((patterns (vector->list patterns))
                (patternnum 0))
       (if (null? patterns)
           '()
           (cons (set-patternnum-in-pattern (car patterns) patternnum)
                 (loop (cdr patterns)
                       (1+ patternnum)))))))
          
  (let loop ((playlist playlist)
             (patterns patterns)
             (patternnum (1- (vector-length patterns))))
    ;;(c-display patternnum ": " playlist)
    ;;(print-events events)
    (if (< patternnum num-original-patterns)
        (list playlist
              (fix-patternnums patterns))
        (if (memv patternnum playlist)
            (loop playlist
                  patterns
                  (1- patternnum))
            (begin
              (<ra> :show-progress-window-message-if-open (<-> "Removing temporarily created pattern " patternnum) #f)
              (loop (remove-from-playlist playlist patternnum)
                    (remove-pattern patterns patternnum)
                    (1- patternnum)))))))


    
(define (remove-unused-patterns playlist events num-original-patterns)
  (define ab (remove-unused-patterns-0 playlist
                                       (group-events-in-patterns events)
                                       num-original-patterns))
  (list (car ab)
        (ungroup-patterns-into-events (cadr ab))))


(***assert*** (remove-unused-patterns '() '() 5)
              (list '() '()))

(let ((hepp (remove-unused-patterns '(5 6 7)
                                      (list (m-e :note :pattern 0 :value 13)
                                            (m-e :note :pattern 1)
                                            (m-e :note :pattern 2)
                                            (m-e :note :pattern 3)
                                            (m-e :note :pattern 4)
                                            (m-e :note :pattern 5 :value 14))
                                      1)))
  (print-events (cadr hepp))
  (***assert*** (car hepp) '(1 2 3))
  (***assert*** (cadr hepp) (list (m-e :note :pattern 0 :value 13)
                                  (m-e :note :pattern 1 :value 14))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to split very long patterns (which can be created after merging) ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define min-num-lines-in-pattern 64)
(define max-num-lines-in-pattern 128)

(define (find-splittable-lines-for-channel events num-lines)
  (remove-duplicates-in-sorted-list
   =
   (let loop ((events events)
              (last-line 0)
              (is-playing #f))
     (if (null? events)         
         (if (or (= last-line num-lines)
                 is-playing)
             '()
             (cons last-line
                   (loop events (1+ last-line) is-playing)))
         (let* ((event (car events))
                (linenum (event :linenum)))
           (cond ((not (= linenum last-line))
                  (if is-playing
                      (loop events
                            (1+ last-line)
                            #t)
                      (cons last-line
                            (loop events
                                  (1+ last-line)
                                  #f))))
                 ((eq? (event :type) :note)
                  (cons linenum
                        (loop (cdr events)
                              linenum
                              #t)))
                 ((eq? (event :type) :stop)
                  (cons linenum
                        (loop (cdr events)
                              linenum
                              #f)))
                 (else
                 (loop (cdr events)
                       linenum
                       is-playing))))))))
  
               
(***assert*** (find-splittable-lines-for-channel (list (m-e :note :line 2)
                                                       (m-e :note :line 4)
                                                       (m-e :stop :line 6)
                                                       (m-e :note :line 6)
                                                       (m-e :stop :line 8))
                                                 10)
              '(0 1 2 4 6 8 9))
              

(***assert*** (find-splittable-lines-for-channel (list (m-e :note :line 0)
                                                       (m-e :note :line 4)
                                                       (m-e :stop :line 6)
                                                       (m-e :note :line 6)
                                                       (m-e :stop :line 8))
                                                 10)
              '(0 4 6 8 9))


(define (find-splittable-line events num-channels)
  (define  break-event (find-first events
                                   (lambda (event)
                                     (eq? (event :type) :break))))
  
  (define num-lines (break-event :linenum))
  
  (define splittable-liness (map (lambda (channel)
                                   (find-splittable-lines-for-channel channel num-lines))       
                                 (pattern-events-to-channels events break-event num-channels)))

  (define (is-line-splittable? linenum)
    (true-for-all? (lambda (splittable-lines)
                     (find-first splittable-lines (lambda (l)
                                                    (= l linenum))))
                   splittable-liness))
    
  (let loop ((linenum min-num-lines-in-pattern))
    (cond ((> linenum (- num-lines 32))
           #f)
          ((is-line-splittable? linenum)
           linenum)
          (else
           (loop (1+ linenum))))))
                 
                 
(define (get-ending-tempos-for-pattern pattern)
  ;;(print-events pattern)
  (let loop ((events (reverse pattern))
             (bpm #f)
             (tpd #f))
    (if (and bpm tpd)
        (list bpm tpd)
        (let ((event (car events)))
          (cond ((and (not tpd)
                      (eq? (event :type) :tpd))
                 (loop (cdr events)
                       bpm
                       (event :value)))
                ((and (not bpm)
                      (eq? (event :type) :bpm))
                 (loop (cdr events)
                       (event :value)
                       tpd))
                (else
                 (loop (cdr events)
                       bpm
                       tpd)))))))

(define (split-pattern playlist patterns pattern patternnum1 num-channels)

  (define split-line (find-splittable-line pattern num-channels))

  (if (not split-line)
      #f
      (let* ((patternnum2 (vector-length patterns))
             (ba (split-list pattern (lambda (event)
                                       (>= (event :linenum) split-line))))
             (pattern1 (append (car ba)
                               (list (m-e :break
                                          :line split-line
                                          :pattern patternnum1
                                          :instrumentnum -1)
                                     )))
             (pattern2-tempos (get-ending-tempos-for-pattern pattern1))
             (pattern2 (append (list (m-e :bpm
                                          :pattern patternnum2
                                          :value (car pattern2-tempos)
                                          :instrumentnum -1
                                          )
                                     (m-e :tpd
                                          :pattern patternnum2
                                          :value (cadr pattern2-tempos)
                                          :instrumentnum -1
                                          )
                                     )
                               (map (lambda (event)
                                      (<copy-event> event
                                                    :linenum (- (event :linenum) split-line)
                                                    :patternnum patternnum2))
                                    (cadr ba))))
             (new-patterns (vector-append (replace-pattern-in-patterns patterns patternnum1 pattern1)
                                          (vector pattern2)))
             (new-playlist (replace-splitted-patterns-in-playlist playlist patternnum1 patternnum1 patternnum2)))
        (<ra> :show-progress-window-message-if-open (<-> "Splitting long pattern " patternnum1) #f)
        (list new-playlist
              new-patterns))))
              
               

(define (split-long-patterns-0 playlist patterns num-channels)
  (let loop ((playlist playlist)
             (patterns patterns)
             (patternnum 0))
    (if (= patternnum (length patterns))
        (list playlist
              patterns)
        (let ((pattern (vector-ref patterns patternnum)))
          ;;(print-events pattern)
          ;;(c-display "hepp" pattern patternnum (length patterns))
          (if  (> (get-num-lines-in-pattern pattern)
                  max-num-lines-in-pattern)
               (let ((ab (split-pattern playlist
                                        patterns
                                        pattern
                                        patternnum
                                        num-channels)))
                 ;;(c-display "new: " (cadr ab))
                 (if ab
                     (loop (car ab)
                           (cadr ab)
                           (1+ patternnum))
                     (loop playlist
                           patterns
                           (1+ patternnum))))
               (loop playlist
                     patterns
                     (1+ patternnum)))))))


(define (split-long-patterns playlist events num-channels)
  (define ab (split-long-patterns-0 playlist
                                    (group-events-in-patterns events)
                                    num-channels))
  (list (car ab)
        (ungroup-patterns-into-events (cadr ab))))



(let ((hepp (split-long-patterns '(0)
                                 (list (m-e :tpd :value *default-tpd* :instrumentnum -1)
                                       (m-e :bpm :value *default-bpm* :instrumentnum -1)
                                       (m-e :note :line 0)
                                       (m-e :note :line 4)
                                       (m-e :stop :line 6)
                                       (m-e :note :line 6)
                                       (m-e :stop :line 8)
                                       (m-e :note :line 66)
                                       (m-e :stop :line 67)
                                       (m-e :break :line 256 :instrumentnum -1))
                                 4)))
  (print-events (cadr hepp))
  (***assert*** (car hepp) '(0 1 2))
  (***assert*** (cadr hepp ) (list (m-e :tpd :value *default-tpd* :instrumentnum -1)
                                   (m-e :bpm :value *default-bpm* :instrumentnum -1)
                                   (m-e :note :line 0)
                                   (m-e :note :line 4)
                                   (m-e :stop :line 6)
                                   (m-e :note :line 6)
                                   (m-e :stop :line 8)
                                   (m-e :break :line 64 :instrumentnum -1)
                                   
                                   (m-e :bpm :value *default-bpm* :pattern 1 :instrumentnum -1)
                                   (m-e :tpd :value *default-tpd* :pattern 1 :instrumentnum -1)
                                   (m-e :note :line 2 :pattern 1)
                                   (m-e :stop :line 3 :pattern 1)
                                   (m-e :break :line 64 :pattern 1 :instrumentnum -1)

                                   (m-e :bpm :value *default-bpm* :pattern 2 :instrumentnum -1)
                                   (m-e :tpd :value *default-tpd* :pattern 2 :instrumentnum -1)
                                   (m-e :break :line 128 :pattern 2 :instrumentnum -1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create new patterns for hanging notes         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (has-track-hanging-note?-0 track-events result)
  (if (null? track-events)
      result
      (let ((event (car track-events)))
        (cond ((eq? (event :type) :stop)
               (has-track-hanging-note?-0 (cdr track-events) #f))
              ((eq? (event :type) :note)
               (has-track-hanging-note?-0 (cdr track-events) #t))
              (else
               (has-track-hanging-note?-0 (cdr track-events) result))))))
  
;;(define (has-track-hanging-note?-0 track-events result)
;;  #f)

(define (has-track-hanging-note? track-events)
  (has-track-hanging-note?-0 track-events #f))
      

(define (is-hanging-note-stopped-at-beginning? track-events)
  (if (null? track-events)
      #f
      (let ((event (car track-events)))
        (cond ((> (event :linenum) 0)
               #f)
              ((eq? (event :type) :stop)
               #t)
              ((eq? (event :type) :note)
               #t)
              (else
               (is-hanging-note-stopped-at-beginning? (cdr track-events)))))))
      


(define (get-track patterns patternnum channel)
  (keep (lambda (event)
          (= (event :channel) channel))
        (vector-ref patterns patternnum)))

(define (must-create-new-pattern? playlist patterns playlistpos1 playlistpos2 channel)
  (let* ((patternnum2 (list-ref playlist playlistpos2))
         (track2 (get-track patterns patternnum2 channel)))
    (and (not (is-hanging-note-stopped-at-beginning? track2))
         (let* ((patternnum1 (list-ref playlist playlistpos1))
                (track1 (get-track patterns patternnum1 channel)))
           (has-track-hanging-note? track1)))))

(define (merge-patterns-1 playlist patterns channel playlistpos) ;; iterate over playlistpos
  (if (= (1- (length playlist)) playlistpos)
      (list playlist patterns)
      (let ((playlistpos1 playlistpos)
            (playlistpos2 (1+ playlistpos)))
        (if (must-create-new-pattern? playlist patterns playlistpos1 playlistpos2 channel)
            (let* ((patternnum1 (list-ref playlist playlistpos1))
                   (patternnum2 (list-ref playlist playlistpos2))
                   (displaying-result (<ra> :show-progress-window-message-if-open (<-> "  Merging patterns " patternnum1 " and " patternnum2 " into pattern " (vector-length patterns)) #f))
                   (new-playlist-and-patterns (add-new-merged-pattern-to-patterns playlist
                                                                                  patterns
                                                                                  patternnum1 patternnum2))
                   (new-playlist (car new-playlist-and-patterns))
                   (new-patterns (cadr new-playlist-and-patterns)))
              ;;(if (and (= patternnum1 6) (= patternnum2 21))
              ;;    (print-events (get-pattern events 21)))
              ;;(print-events (get-pattern new-events 23)))
              (merge-patterns-1 new-playlist
                                new-patterns
                                channel
                                playlistpos))
            (merge-patterns-1 playlist
                              patterns
                              channel
                              (1+ playlistpos))))))


(define (merge-patterns-0 playlist patterns channel) ;; iterate over channel
  (<ra> :show-progress-window-message-if-open (<-> "Merge pattern for channel " channel) #f)
  ;;(c-display "merge-patterns-0" playlist)
  (if (= channel -1)
      (list playlist
            patterns)
      (let* ((new-playlist-and-patterns (merge-patterns-1 playlist patterns channel 0))
             (new-playlist (car new-playlist-and-patterns))
             (new-patterns   (cadr new-playlist-and-patterns)))
        (merge-patterns-0 new-playlist new-patterns (1- channel)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace-c00-with-stops ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *last-c00-patternnum* -1)
(define (remove-c00-at-same-line patternnum channel linenum c00s)
  (when (not (= patternnum *last-c00-patternnum*))
    (<ra> :show-progress-window-message-if-open (<-> "Removing c00s from " patternnum) #f)
    (set! *last-c00-patternnum* patternnum))
  (keep (lambda (c00)
          (or (not (= patternnum (c00 :patternnum)))
              (not (= channel (c00 :channel)))
              (not (= linenum (c00 :linenum)))))
        c00s))

(define (find-c00s-at c00s patternnum channel)
  (if (null? c00s)
      '()
      (let ((c00 (car c00s)))
        (if (and (= patternnum (c00 :patternnum))
                 (= channel (c00 :channel)))
            (cons c00 
                  (find-c00s-at (cdr c00s) patternnum channel))
            (find-c00s-at (cdr c00s) patternnum channel)))))

(define (find-all-c00-stops events current-c00s)
  (define length-events (length events))
  (if (= 0 (modulo length-events 128))
      (<ra> :show-progress-window-message-if-open (<-> "Find-all-c00-stops. Left: " length-events (length current-c00s))))
  ;;(c-display "events" events)
  ;;(c-display "find-all-c00-stops" current-c00s)
  (if (null? events)
      current-c00s ;; We also convert all hanging c00s into stops.
      (let* ((event (car events))
             (patternnum (event :patternnum))
             (channel (event :channel))
             (linenum (event :linenum)))
        (cond ((eq? :note (event :type))
               (let ((c00s (find-c00s-at current-c00s patternnum channel)))
                 (remove-c00-at-same-line patternnum channel linenum
                                          (append c00s
                                                  (find-all-c00-stops (cdr events) (delete-list-from current-c00s c00s))))))
              
              ((eq? :velocity (event :type))
               (if (and (number? (event :value))
                        (= 0 (event :value)))
                   (find-all-c00-stops (cdr events) (cons event current-c00s))
                   (let ((c00s (find-c00s-at current-c00s patternnum channel))) ;; these are the only c00s we don't want to convert into stops.
                     (find-all-c00-stops (cdr events) (delete-list-from current-c00s c00s)))))
              
              (else
               (find-all-c00-stops (cdr events) current-c00s))))))

(define (convert-c00-event-to-stop events c00-event)
  (cond ((eq? (car events) c00-event)
         (cons (<copy-event> (car events)
                           :type :stop)
               (cdr events)))
        (else
         (cons (car events)
               (convert-c00-event-to-stop (cdr events) c00-event)))))

(define (replace-c00-with-stops-1 events c00s)
  (<ra> :show-progress-window-message-if-open (<-> "Replace c00 with stops-1: " (length c00s)) #f)
  (if (null? c00s)
      events
      (replace-c00-with-stops-1 (convert-c00-event-to-stop events (car c00s))
                                (cdr c00s))))

(define (replace-c00-with-stops events)
  (replace-c00-with-stops-1 events
                            (find-all-c00-stops events '())))


;;TODO: Add tests to make sure c00s which are actually just parameters for slide-to-note are not made into :stops.
;; plus fix replace-c00-with-stops and move prepare-slide-to-note after merge-patterns.


;; We don't add :stop if it's on the same line as a note
(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :line 3   :value 0)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :velocity :line 3  :value 0)))


(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :line 10  :value 0)
                     (m-e :note     :line 11  :value 1)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :stop     :line 10  :value 0)
                    (m-e :note     :line 11  :value 1)))

(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :line 10  :value 3)
                     (m-e :note     :line 11  :value 1)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :velocity :line 10  :value 3)
                    (m-e :note     :line 11  :value 1)))


(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :line 10  :value 3)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :velocity :line 10  :value 3)))

(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :line 10  :value 0)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :stop     :line 10)))

(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :channel 2 :line 10  :value 0)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :stop     :channel 2 :line 10)))

(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :line 10  :value 0)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :stop     :line 10)))

;; We don't add :stop if a new velocity turns on the volume again later
(***assert*** (replace-c00-with-stops
               (list (m-e :note     :line 3   :value 2)
                     (m-e :velocity :line 10  :value 0)
                     (m-e :velocity :line 11  :value 5)))
              (list (m-e :note     :line 3   :value 2)
                    (m-e :velocity :line 10  :value 0)
                    (m-e :velocity :line 11  :value 5)))




#||
(define events (list
                (make-event :type :note
                            :patternnum 1
                            :channel 0
                            :linenum 3
                            :value 2)
                (make-event :type :note
                            :patternnum 1
                            :channel 1
                            :linenum 8
                            :value 5)
                (make-event :type :velocity
                            :patternnum 1
                            :channel 1
                            :linenum 10
                            :value 0)
                (make-event :type :note
                            :patternnum 1
                            :channel 1
                            :linenum 11
                            :value 1)
                ))

(print-events (replace-c00-with-stops events))
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Note cut effect (ECx) effect            ;;;
;;;;  works by transforming into c00 effects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||

Done during input reading instead.

(define (remove-note-cut-effects-0 events)
  (if (null? events)
      '()
      (let ((event (car events)))
        (if (eq? (event :type) :cut-note)
            (cons (<copy-event>
                   (<copy-event>
                    (<copy-event>
                     event
                     :value 0)
                    :tick (event :value))
                   :type :velocity)
                  (remove-note-cut-effects-0 (cdr events)))
            (cons event
                  (remove-note-cut-effects-0 (cdr events)))))))
        
;; This can probably be done during input instead. It's just creating a c00 effect with a custom tick value.
(define (remove-note-cut-effects events)
  (remove-note-cut-effects-0 events))

(***assert*** (remove-note-cut-effects (list (m-e :cut-note :value 5)))
              (list (m-e :velocity :tick 5)))
||#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Merge patterns (main function)          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-patterns playlist events num-channels)
  (c-display "merge 1 " (length events))
  (define new-events (replace-c00-with-stops events))
  (c-display "merge 2")
  (define patterns (group-events-in-patterns new-events))
  (c-display "merge 3")
  (let* ((new-playlist-and-patterns (merge-patterns-0 playlist patterns (1- num-channels)))
         (new-playlist (car new-playlist-and-patterns))
         (new-patterns   (cadr new-playlist-and-patterns)))
    (c-display "merge 4")
    (list new-playlist
          (ungroup-patterns-into-events new-patterns))))

#||
(let* ((stuff (process-events '(0 0)
                              #(#("for game music:" "/home/kjetil/.radium/mod_samples/GrooveLoop.wav" 11282 0 64 0 0 24))
                             (list (m-e :note        :line 1   :value 5  :instrumentnum 1 :pattern 0 :channel 2)
                                   ;(m-e :velocity    :line 2   :value 32  :pattern 0)
                                   ;;(m-e :loop        :line 3   :value 0)
                                   ;;(m-e :loop        :line 4   :value 10)
                                   (m-e :velocity    :line 5   :value 9 :channel 2)
                                   (m-e :break       :line 5   :channel 2 :pattern 0 :value 10 :instrumentnum -1)
                                   (m-e :loop        :line 6   :value 1   :pattern 0 :instrumentnum -1)
                                   (m-e :note        :line 10  :value 6 :instrumentnum 1)
                                   (m-e :break       :line 64  :channel 2 :pattern 0 :instrumentnum -1)
                                   )))
       (playlist (car stuff))
       (patterns (caddr stuff))
       (pattern (car patterns)))
  (c-display "pattern" pattern)
  (c-display "playlist: " playlist)
  (c-display "num-patterns: " (length patterns))
  (print-events (car (pattern 0))))

(print-events (cadr (merge-patterns '(0 1)
                                    (list (m-e :note  :pattern 0 :line 5 :value 1 :instrumentnum 2)
                                          (m-e :break :pattern 0 :line 62 :channel 2)
                                          (m-e :note  :pattern 1 :line 2 :value 2 :instrumentnum 2)
                                          (m-e :break :pattern 1 :line 64))
                                    4)))
||#                                       

;; test continuing note
(let* ((events (list (m-e :note  :pattern 0 :line 5 :value 1 :instrumentnum 2)
                     (m-e :break :pattern 0 :line 64)
                     (m-e :note  :pattern 1 :line 2 :value 2 :instrumentnum 2)
                     (m-e :break :pattern 1 :line 64)))
       (hepp (merge-patterns '(0 1) events 3)))

  (***assert*** (car hepp) '(2))
  (***assert*** (cadr hepp)
                (append events
                        (list (m-e :note  :pattern 2 :line 5  :value 1 :instrumentnum 2)
                              (m-e :note  :pattern 2 :line 66 :value 2 :instrumentnum 2)
                              (m-e :break :pattern 2 :line 128)))))


(let* ((events (list (m-e :break :pattern 0 :line 64)
                     (m-e :note  :pattern 1 :line 2 :value 2 :instrumentnum 2)
                     (m-e :break :pattern 1 :line 64)))
       (hepp (merge-patterns '(0 1) events 3)))

  ;;(print-events (cadr hepp))

  (***assert*** (car hepp) '(0 1))
  (***assert*** (cadr hepp) events))



#||
;; test continuing instrument num
(let* ((events (list (m-e :note  :pattern 0 :line 5 :value 1 :instrumentnum 2)
                     (m-e :break :pattern 0 :line 64)
                     (m-e :note  :pattern 1 :line 0 :value 2 :instrumentnum 0)
                     (m-e :break :pattern 1 :line 64)))
       (hepp (merge-patterns '(0 1) events 3)))

  (begin
    (c-display "playlist: " (car hepp))
    (print-events (cadr hepp)))
  
  (***assert*** (car hepp) '(2))
  (***assert*** (cadr hepp)
                (append events
                        (list (m-e :note  :pattern 2 :line 5  :value 1 :instrumentnum 2)
                              (m-e :note  :pattern 2 :line 64 :value 2 :instrumentnum 0)
                              (m-e :break :pattern 2 :line 128)))))
||#
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Extend offset                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (have-note-event-in-this-cell? linenum channel patternnum events)
  (let ((event (car events)))
    (cond ((not (= (event :linenum)
                   linenum))
           #f)
          ((not (= (event :channel)
                   channel))
           #f)
          ((not (= (event :patternnum)
                   patternnum))
           #f)
          ((eq? :note (event :type))
           #t)
          (else
           (have-note-event-in-this-cell? linenum channel patternnum (cdr events))))))

(define (have-sample-offset-event-in-this-cell? linenum channel patternnum events)
  (let ((event (car events)))
    (cond ((not (= (event :linenum)
                   linenum))
           #f)
          ((not (= (event :channel)
                   channel))
           #f)
          ((not (= (event :patternnum)
                   patternnum))
           #f)
          ((eq? :sample-offset (event :type))
           #t)
          (else
           (have-sample-offset-event-in-this-cell? linenum channel patternnum (cdr events))))))

(***assert*** (have-sample-offset-event-in-this-cell? 0 0 0 (list (m-e :note  :pattern 0 :line 0 :value 10)
                                                               (m-e :sample-offset :value 10)))
              #t)

(***assert*** (have-sample-offset-event-in-this-cell? 0 0 0 (list (m-e :note  :pattern 0 :line 0 :value 10)
                                                               (m-e :sample-offset :line 1 :value 10)))
              #f)

(***assert*** (have-sample-offset-event-in-this-cell? 0 0 0 (list (m-e :note  :pattern 0 :line 0 :value 10)
                                                               (m-e :break :line 1)))
              #f)

(define (add-sample-offset-events offset-event events)
  (define patternnum (offset-event :patternnum))
  (define channel (offset-event :channel))

  (let loop ((events events)
             (last-added-line (offset-event :linenum)))
    (let* ((event (car events))
           (linenum (event :linenum)))

      (define (add-offset-event linenum)
        (append (list (<copy-event> offset-event
                                  :linenum linenum)
                      event)
                (loop (cdr events)
                      linenum)))

      (cond ((and (eq? :break (event :type))
                  (= linenum (1+ last-added-line)))
             events)

            ((= linenum
                last-added-line)
             (cons event
                   (loop (cdr events)
                         last-added-line)))
            
            ((> linenum ;; i.e there was a line without events
                (1+ last-added-line))
             (cons (<copy-event> offset-event
                               :linenum (1+ last-added-line))
                   (loop events
                         (1+ last-added-line))))

            ((< (event :channel)
                channel)
             (cons event
                   (loop (cdr events)
                         last-added-line)))

            ((> (event :channel)
                channel)
             (add-offset-event linenum))
            
            ((or (have-note-event-in-this-cell? linenum channel patternnum events)
                 ;;(eq? :stop (event :type))
                 (have-sample-offset-event-in-this-cell? linenum channel patternnum events))
             events)
            
            (else
             (add-offset-event linenum))))))

(define (expand-sample-offsets events)
  (if (null? events)
      '()
      (let ((event (car events)))
        (cond ((eq? :sample-offset (event :type))
               (cons event
                     (expand-sample-offsets (add-sample-offset-events event
                                                                      (cdr events)))))
              (else
               (cons event
                     (expand-sample-offsets (cdr events))))))))


(let* ((events (list (m-e :note  :pattern 0 :line 0 :value 10)
                     (m-e :sample-offset :value 10)
                     (m-e :break :pattern 0 :line 1)
                     (m-e :note  :pattern 1 :line 0 :value 11)))
       (hepp (expand-sample-offsets events)))
  ;;(print-events hepp)
  (***assert*** hepp events))

(let* ((events (list (m-e :note  :pattern 0 :line 0 :value 10)
                     (m-e :sample-offset :value 10)
                     (m-e :break :pattern 0 :line 3)
                     (m-e :note  :pattern 1 :line 0 :value 11)))
       (hepp (expand-sample-offsets events)))
  (print-events hepp)
  (***assert*** hepp
                (list (m-e :note  :pattern 0 :line 0 :value 10)
                      (m-e :sample-offset :value 10)
                      (m-e :sample-offset :value 10 :line 1)
                      (m-e :sample-offset :value 10 :line 2)
                      (m-e :break :pattern 0 :line 3)
                      (m-e :note  :pattern 1 :line 0 :value 11)))
  )

(let* ((events (list (m-e :sample-offset :value 10 :line 0)
                     (m-e :sample-offset :value 11 :line 2)
                     (m-e :break :pattern 0        :line 4)
                     ))
       (hepp (expand-sample-offsets events)))
  (print-events hepp)
  (***assert*** hepp
                (list (m-e :sample-offset :value 10)
                      (m-e :sample-offset :value 10 :line 1)
                      (m-e :sample-offset :value 11 :line 2)
                      (m-e :sample-offset :value 11 :line 3)
                      (m-e :break :pattern 0 :line 4)))
  )

(let* ((events (list (m-e :note  :pattern 0 :value 50 :line 3)
                     (m-e :sample-offset    :value 10 :line 3)
                     (m-e :note  :pattern 0 :value 60 :line 4)
                     (m-e :sample-offset    :value 11 :line 4)
                     (m-e :note  :pattern 0 :value 70 :line 5)
                     (m-e :break :pattern 0        :line 4)
                     ))
       (hepp (expand-sample-offsets events)))
  (print-events hepp)
  (***assert*** hepp events)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Add break events                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-break-events-0 events
                             must-add-break
                             max-num-lines
                             patternnum
                             num-patterns)
  (define (next-pattern)
    (let ((rest (add-break-events-0 events
                                     #t
                                     max-num-lines
                                     (1+ patternnum)
                                     num-patterns)))
      (if must-add-break
          (cons (m-e :break
                     :pattern patternnum
                     :channel 0
                     :instrumentnum -1
                     :line max-num-lines) ;;(1- max-num-lines))
                rest)
          rest)))

  (cond ((= patternnum num-patterns)
         '())
        ((null? events)
         (next-pattern))
        (else
         (let ((event (car events)))
           (if (> (event :patternnum) patternnum)
               (next-pattern)
               (cons event
                     (add-break-events-0 (cdr events)
                                          (if (eq? (event :type) :break)
                                              #f
                                              must-add-break)
                                          max-num-lines
                                          patternnum
                                          num-patterns)))))))
          
(define (add-break-events events max-num-lines num-patterns)
  (add-break-events-0 events
                       #t
                       max-num-lines
                       0
                       num-patterns))



;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-events-after-breaks-0 events remove-patternnum)
  (if (null? events)
      '()
      (let ((event (car events)))
        (cond ((= (event :patternnum) remove-patternnum)
               (remove-events-after-breaks-0 (cdr events) remove-patternnum))
              ((eq? (event :type) :break)
               (cons event
                     (remove-events-after-breaks-0 events (event :patternnum))))
              (else
               (cons event
                     (remove-events-after-breaks-0 (cdr events) remove-patternnum)))))))

;; simplify-break-events and fix-break-events must be called first.
;; It does not remove events placed on the same line as a break if it is placed before the break in the events list. 'fix-break-events' takes care of that.
(define (remove-events-after-breaks events)
  (remove-events-after-breaks-0 events -1))


(***assert*** (remove-events-after-breaks (list (m-e :break :line 8)
                                                (m-e :note :line 8)
                                                (m-e :break :line 16)
                                                (m-e :note :line 16)))
              (list (m-e :break :line 8)))



#||
(define events (list
                (make-event :type :note
                            :patternnum 1
                            :channel 2
                            :linenum 8
                            :value 5)
                ))

(print-events (add-break-events events 64))

(define hepp (create-new-patterns-based-on-continuing-notes '(0 1 2 1)
                                                            (add-break-events events 64 '(0 1 2 1))
                                                            3))

(print-events (cadr hepp))

(length (cadr hepp))

||#


;; This function is called right after reading in the events. It makes sure the events are sorted properly since they are placed one line earlier than they start working in the MOD format.
;; <strike>It also makes sure that a break-lin starts with the break event.</strike> Not needed, break events are always inserted in the previous line.
(define (fix-break-events events)
  (let loop ((events events)
             (break-event (<optional-hash-table>)))
    ;;(c-display "events: " (map event-to-string events))
    ;;(c-display "break-event: " (and break-event (event-to-string break-event)))
    (if (null? events)
        (if break-event
            (list break-event)
            '())
        (let ((event (car events)))
          (cond ((and break-event
                      (or (not (= (event :patternnum)
                                  (break-event :patternnum)))
                          (>= (event :linenum)
                              (break-event :linenum))))
                 (cons break-event
                       (loop events
                             #f)))
                
                ((and break-event
                      (eq? :break (event :type))) ;; Double break on the same line and pattern. Throw away the first break. (think it's the last one that counts)
                 (loop (cdr events) 
                       event))
                
                ((eq? :break (event :type))
                 (loop (cdr events)
                       event))

                (else
                 (cons event
                       (loop (cdr events)
                             break-event))))))))


(***assert*** (fix-break-events (list (m-e :break :line 8)
                                      (m-e :note :line 7)
                                      (m-e :note :line 7)))
              (list (m-e :note :line 7)
                    (m-e :note :line 7)
                    (m-e :break :line 8)))

#||
(print-events (fix-break-events (list (m-e :break :line 8)
                                      (m-e :note :line 8)
                                      (m-e :note :line 8))))
||#

(***assert*** (fix-break-events (list (m-e :break :line 8)
                                      (m-e :note :line 8)
                                      (m-e :note :line 8)))
              (list (m-e :break :line 8)
                    (m-e :note :line 8)
                    (m-e :note :line 8)))

#||
The behavior for these three tests are not needed since break events are always inserted on the previous line.
(***assert*** (fix-break-events (list (m-e :note :line 8)
                                      (m-e :break :line 8)))
              (list (m-e :break :line 8)
                    (m-e :note :line 8))))

(***assert*** (fix-break-events (list (m-e :note :line 8)
                                      (m-e :break :line 8)
                                      (m-e :note :line 8)))
              (list (m-e :break :line 8)
                    (m-e :note :line 8)
                    (m-e :note :line 8)))

(***assert*** (fix-break-events (list (m-e :note :line 8)
                                      (m-e :note :line 8)
                                      (m-e :break :line 8)))
              (list (m-e :break :line 8)
                    (m-e :note :line 8)
                    (m-e :note :line 8)))
||#

(***assert*** (fix-break-events (list (m-e :break :line 8)
                                      (m-e :note :line 0 :pattern 1)))
              (list (m-e :break :line 8)
                    (m-e :note :line 0 :pattern 1)))

(***assert*** (fix-break-events (list (m-e :break :line 8)
                                      (m-e :break :line 9)
                                      (m-e :note :line 8)))
              (list (m-e :break :line 8)
                    (m-e :note :line 8)
                    (m-e :break :line 9)))

#||
(set! (*s7* 'history-size) 8)
||#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (find-loop-events-0 events loopstart)
  (if (null? events)
      '()
      (let ((event (car events)))
        (if (eq? (event :type) :loop)
            (if (= (event :value) 0)
                (find-loop-events-0 (cdr events) event)
                (let ((value2 (if (= (event :patternnum)
                                     (loopstart :patternnum))
                                  (loopstart :linenum)
                                  0)))
                  (cons (<copy-event> event :value2 value2)
                        (find-loop-events-0 (cdr events) loopstart))))
            (find-loop-events-0 (cdr events) loopstart)))))

(define (find-loop-events events)
  (find-loop-events-0 events (m-e :loop :pattern -1)))

(define (find-a-loop-event-0 events loopstart)
  (if (null? events)
      #f
      (let ((event (car events)))
        (if (eq? (event :type) :loop)
            (if (= (event :value) 0)
                (find-a-loop-event-0 (cdr events) event)
                (let ((use-loopstart (and (= (event :patternnum)
                                             (loopstart :patternnum))
                                          (= (event :channel)
                                             (loopstart :channel)))))
                  (if use-loopstart
                      (list loopstart event)
                      (list #f event))))
            (find-a-loop-event-0 (cdr events) loopstart)))))

(define (find-a-loop-event events)
  (find-a-loop-event-0 events (m-e :loop :pattern -1)))

(define (expand-loop-split before during events patternnum startline endline)
  (define (next-before event)
    (expand-loop-split (cons event before) during (cdr events) patternnum startline endline))
  (define (next-during event)
    (expand-loop-split before (cons event during) (cdr events) patternnum startline endline))
  (define (got-it)
    (list (reverse before)
          (reverse during)
          events))
  (if (null? events)
      (got-it)
      (let ((event (car events)))
        (cond ((< (event :patternnum) patternnum)
               (next-before event))
              ((> (event :patternnum) patternnum)
               (got-it))
              ((< (event :linenum) startline)
               (next-before event))
              ((>= (event :linenum) endline)
               (got-it))
              (else
               (next-during event))))))
              

(define (expand-loop-looped-events-0 events startline endline num)
  (if (= num 0)
      events
      (map (lambda (event)
             (<copy-event> event :linenum (+ (event :linenum)
                                           (* num (- endline startline)))))
           events)))

(define (expand-loop-looped-events events startline endline num num-repeats)
  (if (= num num-repeats)
      '()
      (append (expand-loop-looped-events-0 events startline endline num)
              (expand-loop-looped-events events startline endline (1+ num) num-repeats))))

(define (expand-loop-after-events events patternnum startline endline num-repeats)
  (if (null? events)
      '()
      (let ((event (car events)))
        (if (= (event :patternnum) patternnum)
            (cons (<copy-event> event :linenum (+ (event :linenum)
                                                (* num-repeats (- endline startline))))
                  (expand-loop-after-events (cdr events) patternnum startline endline num-repeats))
            events))))
              
;; Must check whether the endline is played. Now it is not played.
(define (expand-loop events patternnum startline endline num-repeats)
  (define splitted (expand-loop-split '() '() events patternnum startline endline))

  (define before-events (car splitted))
  (define loop-events (cadr splitted))
  (define after-events (caddr splitted))

  (append before-events
          (expand-loop-looped-events loop-events startline endline 0 num-repeats)
          (expand-loop-after-events  after-events patternnum startline endline num-repeats)))


(define (expand-loops-old events)
  ;(define org-events events)
  ;(define (print events)
  ;  (print-events (keep (lambda (event)
  ;                        (and (= 0 (event :patternnum))
  ;                             (or (= 0 (event :channel))
  ;                                 (= 2 (event :channel)))))
  ;                      events)))
  (let loop ((loops (reverse (find-loop-events events))) ;; Must start to expand loop from end of the patterns (hence the call to 'reverse'), if not start and end might not correspond to the original data.
             (events events))
    (if (null? loops)
        (begin
          ;(c-display "          BEFORE")
          ;(print org-events)
          ;(c-display "          AFTER")
          ;(print (remove (lambda (event)
          ;                 (eq? (event :type) :loop))
          ;               events))
          ;(assert #f)
          events
          )
        (let* ((loop-event (car loops))
               (startline (loop-event :value2))
               (endline (loop-event :linenum))
               (num-repeats (1+ (loop-event :value))))
          (loop (cdr loops)
                (expand-loop events
                             (loop-event :patternnum)
                             startline        
                             endline
                             num-repeats))))))

;; This is not the right way to do it, but the right way to do it probably required a rewrite
;; of 'run-through-patterns' (into a lot more complicated version of it). This one, however,
;; should work for 99.999% of all patterns. I.e. all patterns except pattern 0 in black queen,
;; and perhaps one or two more. I.e. some of those where you jump into the middle of a loop
;; from somewhere else using the :break event.
(define (expand-loops events)
  (let ((loop-events (find-a-loop-event events)))
    (if (not loop-events)
        events
        (let* ((event1 (car loop-events))
               (event2 (cadr loop-events))
               (startline (if event1 (event1 :linenum) 0))
               (endline (event2 :linenum))
               (num-repeats (1+ (event2 :value)))
               (patternnum (event2 :patternnum))
               (filtered-events (delete-list-from events (keep (lambda (e) e) loop-events))))
          (expand-loops (expand-loop filtered-events
                                     patternnum
                                     startline        
                                     endline
                                     num-repeats))))))
;;(assert #f)

#||
(print-events (expand-loops (list (m-e :note :line 3)
                                  (m-e :loop :line 61 :value 2)
                                  (m-e :break :channel 2 :line 64))))
||#

(***assert*** (expand-loops (list (m-e :note :line 3)
                                  (m-e :loop :line 5 :value 0)
                                  (m-e :note :line 8 :value 1)
                                  (m-e :loop :line 10 :value 3)
                                  (m-e :note :line 12 :value 2)
                                  (m-e :note :pattern 1 :line 9 :value 3)
                                  (m-e :loop :pattern 1 :line 10 :value 1)))
              (list (m-e :note :line 3)
                    (m-e :note :line 8 :value 1)
                    (m-e :note :line 13 :value 1)
                    (m-e :note :line 18 :value 1)
                    (m-e :note :line 23 :value 1)
                    (m-e :note :line 32 :value 2)
                    (m-e :note :pattern 1 :line 9 :value 3)
                    (m-e :note :pattern 1 :line 19 :value 3)))
              
#||
(print-events (expand-loops (list (m-e :note :line 3)
                                  (m-e :loop :line 5 :value 0)
                                  (m-e :note :line 8 :value 1)
                                  (m-e :loop :line 10 :value 3)
                                  (m-e :note :line 12 :value 2)
                                  ;;(m-e :loop :pattern 1 :line 0 :value 0) ;; default loop start, unnecessary event
                                  (m-e :note :pattern 1 :line 9 :value 3)
                                  (m-e :loop :pattern 1 :line 10 :value 1))))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (legalize-pattern-lengths-0 playlist playlistpos events)
  (if (>= playlistpos (1- (length playlist))) ;; We stop if it's the last played pattern
      (list playlist events)
      (let* ((patternnum1 (list-ref playlist playlistpos))
             (first-break-in-pattern1 (find-first-break-in-pattern events patternnum1)) ;; actually, there should be only one break now
             (patternlength1 (first-break-in-pattern1 :linenum))
             (patternnum2 (list-ref playlist (1+ playlistpos)))
             (first-break-in-pattern2 (find-first-break-in-pattern events patternnum2))
             (patternlength2 (first-break-in-pattern2 :linenum)))
        (if (or (= 1 patternlength1)
                (= 1 patternlength2))
            (let* ((new-playlist-and-events (add-new-merged-pattern-to-events playlist events patternnum1 patternnum2))
                   (new-playlist (car new-playlist-and-events))
                   (new-events (cadr new-playlist-and-events)))
              ;;(print-events new-events)
              (list new-playlist new-events)
              (legalize-pattern-lengths-0 new-playlist playlistpos new-events))
            (legalize-pattern-lengths-0 playlist (1+ playlistpos) events)))))


(define (find-one-lined-patterns events)
  (if (null? events)
      '()
      (let ((event (car events)))
        (if (and (eq? (event :type) :break)
                 (= (event :linenum) 1))
            (cons (event :patternnum)
                  (find-one-lined-patterns (cdr events)))
            (find-one-lined-patterns (cdr events))))))

(define (erase-patterns events patterns)
  (if (or (null? events)
          (null? patterns))
      events
      (let ((event (car events))
            (patternnum (car patterns)))
        (cond ((< (event :patternnum) patternnum)
               (cons event
                     (erase-patterns (cdr events) patterns)))
              ((> (event :patternnum) patternnum)
               (erase-patterns events (cdr patterns)))
              ((eq? (event :type) :break)
               (cons (<copy-event> event :linenum 64)
                     (erase-patterns (cdr events) patterns)))
              (else
               (erase-patterns (cdr events) patterns))))))
              
(define (erase-one-lined-patterns events)
  (erase-patterns events
                  (find-one-lined-patterns events)))
      
;; Radium blocks must have at least two lines. Ensure that.
;; Only makes sense to call this function after calling remove-events-after-breaks.
(define (legalize-pattern-lengths playlist events)
  (let* ((new-playlist-and-events (legalize-pattern-lengths-0 playlist 0 events))
         (new-playlist (car new-playlist-and-events))
         (new-events (cadr new-playlist-and-events)))
    (list new-playlist
          (erase-one-lined-patterns new-events))))



(let* ((playlist '(0 1 2))
       (hepp (legalize-pattern-lengths playlist
                                       (list (m-e :note  :pattern 0 :line 0 :value 10)
                                             (m-e :break :pattern 0 :line 1)
                                             (m-e :note  :pattern 1 :line 0 :value 11)
                                             (m-e :break :pattern 1 :line 1)
                                             (m-e :note  :pattern 2 :line 0 :value 12)
                                             (m-e :break :pattern 2 :line 2)))))

  ;;(c-display (car hepp))
  ;;(print-events (cadr hepp))

  (***assert*** (car hepp) '(3 2))
  (***assert*** (cadr hepp)
                (list (m-e :break :pattern 0 :line 64)
                      (m-e :break :pattern 1 :line 64)
                      (m-e :note  :pattern 2 :line 0 :value 12)
                      (m-e :break :pattern 2 :line 2)
                      (m-e :note  :pattern 3 :line 0 :value 10)
                      (m-e :note  :pattern 3 :line 1 :value 11)
                      (m-e :break :pattern 3 :line 2)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pattern delay effect (EEx effect)    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-pattern-delay pattern-events pattern-delay-event)
  (if (null? pattern-events)
      '()
      (let ((event (car pattern-events))
            (linenum (pattern-delay-event :linenum)))
        (cond ((structs-equal? event pattern-delay-event)
               (apply-pattern-delay (cdr pattern-events) pattern-delay-event))  ;; remove the pattern-delay-event event itself from pattern-events
              
              ((and (= (event :linenum) linenum)
                    (memq (event :type) '(:pitch-slide :slide-to-note :vibrato :tremolo :velocity-slide)))
               (append (cons event
                             (map (lambda (lines-to-add) ;; Note that the events are not ordered by channel anymore by doing this.
                                    (<copy-event> event
                                                :linenum (+ 1 (event :linenum) lines-to-add)
                                                :is-pattern-delay-line #t))
                                  (iota (pattern-delay-event :value))))
                       (apply-pattern-delay (cdr pattern-events) pattern-delay-event)))
              
              ((<= (event :linenum) linenum)
               (cons event
                     (apply-pattern-delay (cdr pattern-events) pattern-delay-event)))

              (else
               (cons (<copy-event> event :linenum (+ (pattern-delay-event :value)
                                                   (event :linenum)))
                     (apply-pattern-delay (cdr pattern-events) pattern-delay-event)))))))

;; TODO: Make process events instead of pattern.
(define (remove-pattern-delay-effects pattern-events)
  (let ((pattern-delay-event (find-first pattern-events (lambda (event)
                                                          (eq? (event :type) :pattern-delay)))))
    (if (not pattern-delay-event)
        pattern-events
        (remove-pattern-delay-effects (apply-pattern-delay pattern-events
                                                           pattern-delay-event)))))



(***assert*** (remove-pattern-delay-effects (list (m-e :pattern-delay :line 3 :value 1)
                                                  (m-e :pitch-slide   :line 3 :value 5)))
              (list (m-e :pitch-slide   :line 3 :value 5)
                    (m-e :pitch-slide   :line 4 :value 5 :is-pattern-delay-line #t)))
                    

(***assert*** (remove-pattern-delay-effects (list (m-e :note          :line 3 :value 1)
                                                  (m-e :pattern-delay :line 3 :value 0)
                                                  (m-e :note          :line 3 :value 2)
                                                  (m-e :note          :line 4 :value 4)))
              (list (m-e :note          :line 3 :value 1)
                    (m-e :note          :line 3 :value 2)
                    (m-e :note          :line 4 :value 4)))

(let ((hepp (remove-pattern-delay-effects (list (m-e :note          :line 3 :value 1)                                                
                                                (m-e :pattern-delay :line 3 :value 3)
                                                (m-e :note          :line 3 :value 2)
                                                
                                                (m-e :pattern-delay :line 4 :value 1)
                                                (m-e :note          :line 4 :value 4)
                                                
                                                (m-e :note          :line 5 :value 5)))))
  (print-events hepp)
  (***assert*** hepp
                (list (m-e :note          :line 3 :value 1)
                      (m-e :note          :line 3 :value 2)
                      (m-e :note          :line (+ 4 3) :value 4)
                      (m-e :note          :line (+ 5 3 1) :value 5))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tpd registry                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-tpds-0 pattern-events last-line last-tpd)
  ;;(c-display "pat:" (length pattern))
  (let ((event (car pattern-events)))
    ;;(c-display "event" event);;(event-to-string event))
    (cond

     ((> (event :linenum) last-line)
      (cons last-tpd
            (create-tpds-0 pattern-events
                           (1+ last-line)
                           last-tpd)))
     
     ((eq? (event :type) :break)
      (list last-tpd)) ;; Needed for the :break events
     
     ((eq? (event :type) :tpd)
      (create-tpds-0 (cdr pattern-events)
                     last-line
                     (event :value)))
     
     (else
      (create-tpds-0 (cdr pattern-events)
                     last-line
                     last-tpd)))))

;; simplify-break-events must be called on the pattern before calling
(define (create-tpds pattern-events)
  (list->vector (create-tpds-0 pattern-events 0 *default-tpd*)))

(let ((hepp (create-tpds (list (m-e :tpd :line 1 :value 2)
                               (m-e :tpd :line 1 :value 3)
                               (m-e :tpd :line 1 :value 4)
                               (m-e :note :line 2)
                               (m-e :tpd :line 3 :value 8)
                               (m-e :tpd :line 3 :value 9)
                               (m-e :break :line 5)))))
  ;;(c-display "hepp: " hepp)
  ;;(c-display "aslist: " (vector->list hepp))
  (***assert*** hepp
                (vector *default-tpd* ; line 0
                        4             ; line 1
                        4             ; line 2
                        9             ; line 3
                        9             ; line 4
                        9)))           ; line 5

(define (get-tpd tpds linenum)
  (vector-ref tpds linenum))


(define (get-num-lines-from-tpds tpds)
  (1- (vector-length tpds)))

(***assert*** (get-num-lines-from-tpds (create-tpds (list (m-e :tpd :line 1 :value 2)
                                                          (m-e :tpd :line 1 :value 3)
                                                          (m-e :tpd :line 1 :value 4)
                                                          (m-e :note :line 2)
                                                          (m-e :tpd :line 3 :value 9)                               
                                                          (m-e :break :line 5))))
              5)
                             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; note retrigger effect (E9x) effect  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
  tpd = 6
  retrigger = 1
  -> 0/6 1/6 2/6 3/6 4/6 5/6

  tpd = 3
  retrigger = 1
  -> 0/3 1/3 2/3

  tpd = 6
  retrigger = 2
  -> 0/6 2/6 4/6
||#

;; TODO: What happens if we retrigger after volume or pitch has changed?
(define (get-retrigger-notes note retrigger-event tpds)
  (define interval (retrigger-event :value))

  (if (= interval 0)
      '()
      (begin
        
        (define linenum (retrigger-event :linenum))
  
        (define tpd (get-tpd tpds linenum))

        ;;(c-display "note: " note ", linenum: " linenum)
        (define firsttick (if (= (note :linenum)
                                 linenum)
                              interval ;; don't retrigger at tick 0 if the note started playing on the same line
                              0))
        
        (let loop ((tick firsttick))
          (if (>= tick tpd)
              '()
              (cons (<copy-event> note
                                :linenum linenum
                                :tick tick)                    
                    (loop (+ tick interval))))))))
               

(define (remove-note-retrigger-effects-0 channel tpds curr-note)
  (if (null? channel)
      '()
      (let ((event (car channel)))
        (cond ((eq? (event :type) :retrigger-note)
               (if curr-note
                   (append (get-retrigger-notes curr-note
                                                event
                                                tpds)
                           (remove-note-retrigger-effects-0 (cdr channel)
                                                            tpds
                                                            curr-note))
                   (remove-note-retrigger-effects-0 (cdr channel)
                                                    tpds
                                                    curr-note)))
              ((eq? (event :type) :stop)
               (cons event 
                     (remove-note-retrigger-effects-0 (cdr channel)
                                                      tpds
                                                      #f)))
              ((eq? (event :type) :note)
               (cons event
                     (remove-note-retrigger-effects-0 (cdr channel)
                                                      tpds
                                                      event)))
              (else
               (cons event
                     (remove-note-retrigger-effects-0 (cdr channel)
                                                      tpds
                                                      curr-note)))))))
        
(define (remove-note-retrigger-effects channel tpds)
  (remove-note-retrigger-effects-0 channel tpds #f))

(let ((hepp (remove-note-retrigger-effects (list (m-e :note :line 3)
                                                 (m-e :retrigger-note :line 4 :value 2)
                                                 (m-e :note :line 5)
                                                 (m-e :retrigger-note :line 5 :value 2)
                                                 (m-e :stop :line 6)
                                                 (m-e :retrigger-note :line 6 :value 2))
                                           (create-tpds (list (m-e :tpd :line 4 :value 4)
                                                              (m-e :break :line 10))))))
  (print-events hepp)
  
  (***assert*** hepp
                (list (m-e :note :line 3)
                      (m-e :note :line 4)
                      (m-e :note :line 4 :tick 2)
                      (m-e :note :line 5)
                      (m-e :note :line 5 :tick 2)
                      (m-e :stop :line 6)))
  )
                


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; note delay effect (EDx) effect      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-note-delay-effects events)
  (if (null? events)
      '()
      (let ((event1 (car events)))
        (cond ((eq? (event1 :type) :delay-note)
               (remove-note-delay-effects (cdr events))) ;; remove stray delay-note events
              ((not (eq? (event1 :type) :note))
               (cons event1
                     (remove-note-delay-effects (cdr events))))
              ((null? (cdr events))
               (list event1))
              (else
               (let ((event2 (cadr events)))              
                 (if (and (eq? (event2 :type) :delay-note)
                          (= (event1 :channel) (event2 :channel))
                          (= (event1 :linenum) (event2 :linenum))
                          (= (event1 :patternnum) (event2 :patternnum)))
                     (cons (<copy-event> event1 :tick (event2 :value))
                           (remove-note-delay-effects (cddr events)))
                     (cons event1
                           (remove-note-delay-effects (cdr events))))))))))

(let ((hepp (remove-note-delay-effects (list (m-e :note :line 2)
                                             (m-e :delay-note :line 2 :value 5)
                                             (m-e :delay-note :line 3 :value 6)))))
  (print-events hepp)
  (***assert*** hepp (list (m-e :note :line 2 :tick 5))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Clone instruments                                                      ;;;
;;;;  Necessary if tremolo, vibrato, panning or sample offset is changed simultaneously on the same instrument  ;;;
;;        actually, don't know if panning follows the channel. Probably not, but should check out.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-struct cloned-instrument
  :old-instrumentnum
  :new-instrumentnum
  )

(define (cloned-instrument-to-string event)
  (<-> "[old-instrument: " (event :old-instrumentnum)
       ", new-instrument: " (event :new-instrumentnum)
       "]"))

(define-constant *clashing-effect-types* (list :vibrato   ;; vibrato-with-volume-slide effects have been converted to two separate events earlier
                                               :tremolo   ;; tremolo-with-volume-slide effects have been converted to two separate events earlier
                                               :sample-offset
                                               :finetune))

(define (is-mayclashing-effect? event)
  (memq (event :type) *clashing-effect-types*))
;  (let ((type (event :type)))
;    (or (eq? type :vibrato)
;        (eq? type :tremolo)
;        (eq? type :sample-offset)))


(define (is-mayclashing-effect-clashing? event playing-mayclashing-effects)
  (memq (event :type) (map (lambda (effect)
                             (event :type))
                           playing-mayclashing-effects)))

(define (is-effect-clashing? playing-mayclashing-effects event) ;; note that 'event' must be a mayclashing effect event
  (let ((instrumentnum (event :instrumentnum))
        (type          (event :type))
        (channel       (event :channel)))
    (find-first playing-mayclashing-effects
                (lambda (playing-mayclashing-effect)
                  (and (= (playing-mayclashing-effect :instrumentnum) instrumentnum)
                       (not (= (playing-mayclashing-effect :channel)
                               channel))
                       (eq? (playing-mayclashing-effect :type) type))))))

(define (is-effect-clashing-with-playing-note? playing-notes event) ;; note that 'event' must be a mayclashing effect event
  (let ((instrumentnum (event :instrumentnum))
        (type          (event :type))
        (channel       (event :channel)))
    (find-first playing-notes
                (lambda (playing-note)
                  (and (not (= (playing-note :channel)
                               channel))
                       (= (playing-note :instrumentnum)
                          instrumentnum))))))

(define (instrument-used? events instrumentnum)
  (if (null? events)
      #f
      (let ((event (car events)))
        (cond ((= instrumentnum (event :instrumentnum))
               #t)
              (else
               (instrument-used? (cdr events) instrumentnum))))))

(define (find-already-cloned-instrument cloned-instruments instrumentnum result)
  (find-first cloned-instruments
              (lambda (cloned-instrument)
                (and (= instrumentnum (cloned-instrument :old-instrumentnum))
                     (not (instrument-used? result
                                            (cloned-instrument :new-instrumentnum)))))))

(define (find-new-instrumentnum cloned-instruments instruments)
  (if (null? cloned-instruments)
      (1+ (length instruments))
      (let ((cloned-instrument (last cloned-instruments))) ;; the last cloned instrument is the one with the highest instrument number
        (1+ (cloned-instrument :new-instrumentnum)))))

(define (clone-instrument-if-necessary cloned-instruments event instruments result)
  (let ((already-cloned-instrument (find-already-cloned-instrument cloned-instruments (event :instrumentnum) result)))
    (if already-cloned-instrument
        (list (already-cloned-instrument :new-instrumentnum)
              cloned-instruments)
        (let ((new-instrumentnum (find-new-instrumentnum cloned-instruments instruments)))
          (list new-instrumentnum
                (append cloned-instruments
                        (list (make-cloned-instrument (event :instrumentnum)
                                                      new-instrumentnum))))))))

(define (replace-clashed-events events channel old-instrumentnum new-instrumentnum)
  (if (null? events)
      '()
      (let ((event (car events)))
        (cond ((and (= (event :channel) channel)
                    (= (event :instrumentnum) old-instrumentnum))
               (cons (<copy-event> event :instrumentnum new-instrumentnum)
                     (replace-clashed-events (cdr events)
                                             channel
                                             old-instrumentnum
                                             new-instrumentnum)))
              (else
               (cons event
                     (replace-clashed-events (cdr events)
                                             channel
                                             old-instrumentnum
                                             new-instrumentnum)))))))

;; both 'events' and 'result' are list of events. 'events' is forward in time, 'result' is backwards in time.
(define (replace-instrument-numbers-for-clashing-effect cloned-instruments event events instruments result)
  (let ((old-instrumentnum (event :instrumentnum))
        (channel (event :channel)))
    (let* ((stuff (clone-instrument-if-necessary cloned-instruments event instruments result))
           (new-instrumentnum (car stuff))
           (new-cloned-instruments (cadr stuff)))
      (list new-cloned-instruments
            (replace-clashed-events events channel old-instrumentnum new-instrumentnum)
            (replace-clashed-events result channel old-instrumentnum new-instrumentnum)))))


;; Must remove unaffiliated effect events before calling this function, plus add instrument number to all effect events.
;;
(define (get-cloned-instruments-0 events
                                  result
                                  cloned-instruments
                                  instruments
                                  playing-mayclashing-effects ;; we don't really need to test against these...
                                  playing-notes
                                  last-linenum)

  (define (is-event-clashing? event)
    (and (is-mayclashing-effect? event)
         (or (is-effect-clashing-with-playing-note? playing-notes event)
             (is-effect-clashing? playing-mayclashing-effects event))))

  (define (remove-playing-note playing-notes event)
    (let loop ((playing-notes playing-notes))      
      (if (null? playing-notes)
          (begin
            ;;(c-display "**** WARNING: Stop note event without a corresponding play note event detected:" event)
            '())
          (let ((playing-note (car playing-notes)))
            (if (and (= (playing-note :channel)
                        (event :channel)))
                (cdr playing-notes)
                (cons playing-note
                      (loop (cdr playing-notes))))))))

  ;;(c-display "len: " (length events))
  ;;(c-display "cloned: " cloned-instruments)
  (if (null? events)
      (list cloned-instruments (reverse result))
      (let* ((event (car events))
             (linenum (event :linenum)))

        ;;(c-display "_event: " (event-to-string event) ", last/now: " last-linenum (event :linenum))
        
        (cond (#f #f)

              ((not (= linenum last-linenum))
               (get-cloned-instruments-0 events
                                         result                                      
                                         cloned-instruments
                                         instruments
                                         '()
                                         playing-notes
                                         linenum))
              
              ((is-event-clashing? event)
               (let* ((stuff (replace-instrument-numbers-for-clashing-effect cloned-instruments
                                                                             event
                                                                             events
                                                                             instruments
                                                                             result))
                      (new-cloned-instruments (car stuff))
                      (new-events (cadr stuff))
                      (new-result (caddr stuff)))
                 (get-cloned-instruments-0 (append (reverse new-result)
                                                   new-events)
                                           '()
                                           new-cloned-instruments
                                           instruments
                                           '()
                                           '()
                                           -1)))

              ((eq? (event :type) :stop)
               (get-cloned-instruments-0 (cdr events)
                                         (cons event result)
                                         cloned-instruments
                                         instruments
                                         playing-mayclashing-effects
                                         (remove-playing-note playing-notes event)
                                         linenum))
              
              (else
               (get-cloned-instruments-0 (cdr events)
                                         (cons event result)
                                         cloned-instruments
                                         instruments
                                         (if (is-mayclashing-effect? event)
                                             (cons event playing-mayclashing-effects)
                                             playing-mayclashing-effects)
                                         (if (eq? (event :type) :note)
                                             (cons event
                                                   (remove-playing-note playing-notes event))
                                             playing-notes)
                                         linenum))))))
  

;; Returns (list a b) where a = list of 'new-instrument' structs, b = transformed events
;; (Note that the instruments are not created here. The function only returns a list of 'cloned-instrument' structs.)
;;
(define (get-cloned-instruments events instruments)
  (let loop ((patterns (vector->list (group-events-in-patterns events)))
             (result '())
             (cloned-instruments '()))
                                 
    (if (null? patterns)
        (list cloned-instruments
              (ungroup-patterns-into-events (list->vector (reverse result))))
        (let ((pattern (car patterns)))
          (if (not (null? pattern))
              (c-display "  checking pattern" ((car pattern) :patternnum)))
          (define ab (get-cloned-instruments-0 pattern
                                               '() ;; result
                                               cloned-instruments
                                               instruments
                                               '() ;; playing mayclashing
                                               '() ;; playing notes
                                               -1))  ;; last linenum
          (loop (cdr patterns)
                (cons (cadr ab)
                      result)
                (car ab))))))

  


(define *test-instruments* (make-vector 100 #f))


;; 1. Tests to make sure instruments are not cloned unless needed.
;;
;;
(let ((hepp (get-cloned-instruments (list) *test-instruments*)))
  (***assert*** hepp '(() 
                       ())))

(let* ((events (list (m-e :note :line 5 :instrumentnum 9)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  ;;(c-display "hepp: " hepp)
  (***assert*** hepp (list '()
                           events)))


(let* ((events (list (m-e :note :line 5)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  ;;(c-display "hepp: " hepp)
  (***assert*** hepp (list '()
                           events)))

(let* ((events (list (m-e :note :line 5)
                     (m-e :vibrato :line 9)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  ;;(c-display "hepp: " hepp)
  (***assert*** hepp (list '()
                           events)))

;; different instrument number
(let* ((events (list (m-e :note :line 5)
                     (m-e :vibrato :line 9 :instrumentnum 0)
                     (m-e :vibrato :line 9 :instrumentnum 1)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (***assert*** hepp (list '()
                           events)))

;; different line numbers
(let* ((events (list (m-e :note :line 5)
                     (m-e :vibrato :line 9)
                     (m-e :vibrato :line 10)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (***assert*** hepp (list '()
                           events)))

;; different patterns
(let* ((events (list (m-e :vibrato :channel 0 :line 9 :pattern 1)
                     (m-e :vibrato :channel 1 :line 9 :pattern 2)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (***assert*** hepp (list '()
                           events)))


;; different effect types
(let* ((events (list (m-e :vibrato :channel 0 :line 9 :pattern 1)
                     (m-e :sample-offset :channel 1 :line 9 :pattern 2)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  ;;(print-events events)
  ;;(print-events (cadr hepp))
  (***assert*** hepp (list '()
                           events)))


;; 2. Other tests
;;
;;

;; 2.1 basic tests for vibrato, tremolo and sample-offset
(let* ((events (list (m-e :vibrato :channel 0 :line 9 :pattern 1 :value2 1)
                     (m-e :vibrato :channel 1 :line 9 :pattern 1 :value2 2)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (***assert*** (cadr hepp)
                (list (car events)
                      (<copy-event> (cadr events) :instrumentnum 101)))

  (***assert*** (car hepp)
                (list (make-cloned-instrument :old-instrumentnum 0
                                              :new-instrumentnum 101)))
  )

(let* ((events (list (m-e :tremolo :channel 0 :line 9 :pattern 1 :value2 1)
                     (m-e :tremolo :channel 1 :line 9 :pattern 1 :value2 2)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (***assert*** (cadr hepp)
                (list (car events)
                      (<copy-event> (cadr events) :instrumentnum 101)))

  (***assert*** (car hepp)
                (list (make-cloned-instrument :old-instrumentnum 0
                                              :new-instrumentnum 101)))
  )

(let* ((events (list (m-e :sample-offset :channel 0 :line 9 :pattern 1 :value2 1)
                     (m-e :sample-offset :channel 1 :line 9 :pattern 1 :value2 2)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (***assert*** (cadr hepp)
                (list (car events)
                      (<copy-event> (cadr events) :instrumentnum 101)))

  (***assert*** (car hepp)
                (list (make-cloned-instrument :old-instrumentnum 0
                                              :new-instrumentnum 101)))
  )

;; 2.2 Check that other events in the pattern change instrument number too, not the just the clashing effects
(let* ((events (list (m-e :note    :channel 1 :line 3  :pattern 1 :value2 1)
                     (m-e :note    :channel 2 :line 4  :pattern 1 :value2 1)
                     (m-e :note    :channel 2 :line 5  :pattern 1 :value2 1 :instrumentnum 5) ;;check that unrelated instrument numbers are not changed
                     
                     (m-e :vibrato :channel 1 :line 9  :pattern 1 :value2 2)                     
                     (m-e :vibrato :channel 2 :line 9  :pattern 1 :value2 2)

                     (m-e :note    :channel 1 :line 12 :pattern 1 :value2 1)
                     (m-e :note    :channel 2 :line 16 :pattern 1 :value2 1)
                     (m-e :note    :channel 2 :line 17 :pattern 1 :value2 1 :instrumentnum 5) ;;check that unrelated instrument numbers are not changed
                     ))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (print-events (cadr hepp))
  ;;(c-display (car hepp)))
  (***assert*** (cadr hepp)
                 (map (lambda (event)
                        (if (and (= 3 (event :channel))
                                 (= 0 (event :instrumentnum)))
                            (<copy-event> event :instrumentnum 101)
                            event))
                      (cadr hepp)))

  (***assert*** (car hepp)
                (list (make-cloned-instrument :old-instrumentnum 0
                                              :new-instrumentnum 101)))
  )

;; 2.3 test three simultaneously channels
(let* ((events (list (m-e :vibrato :channel 0 :line 9 :pattern 1 :value2 1)
                     (m-e :vibrato :channel 1 :line 9 :pattern 1 :value2 2)
                     (m-e :vibrato :channel 2 :line 9 :pattern 1 :value2 2)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (print-events (cadr hepp))
  (c-display (car hepp))
  (***assert*** (cadr hepp)
                (list (car events)
                      (<copy-event> (cadr events)  :instrumentnum 101)
                      (<copy-event> (caddr events) :instrumentnum 102)))

  (***assert*** (car hepp)
                 (list (make-cloned-instrument :old-instrumentnum 0
                                               :new-instrumentnum 101)
                       (make-cloned-instrument :old-instrumentnum 0
                                               :new-instrumentnum 102)))
  
  )

;; 2.35 test two clones of two different instruments
(let* ((events (list (m-e :vibrato :channel 0 :line 9 :pattern 1 :value2 1)
                     (m-e :vibrato :channel 1 :line 9 :pattern 1 :value2 2)
                     (m-e :vibrato :channel 2 :line 9 :pattern 1 :value2 3 :instrumentnum 1)
                     (m-e :vibrato :channel 3 :line 9 :pattern 1 :value2 4 :instrumentnum 1)))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (print-events (cadr hepp))
  (c-display (car hepp))
  (***assert*** (cadr hepp)
                (list (car events)
                      (<copy-event> (cadr events)  :instrumentnum 101)
                      (caddr events)
                      (<copy-event> (cadddr events) :instrumentnum 102)))

  (***assert*** (car hepp)
                 (list (make-cloned-instrument :old-instrumentnum 0
                                               :new-instrumentnum 101)
                       (make-cloned-instrument :old-instrumentnum 1
                                               :new-instrumentnum 102)))
  
  )

;; 2.4 reuse cloned instrument in other patterns
(let* ((events (list (m-e :vibrato :channel 0 :line 9 :pattern 1 :value2 1)
                     (m-e :vibrato :channel 1 :line 9 :pattern 1 :value2 2)
                     (m-e :vibrato :channel 0 :line 10 :pattern 2 :value2 3)
                     (m-e :vibrato :channel 1 :line 10 :pattern 2 :value2 4)
                     ))
       (hepp (get-cloned-instruments events *test-instruments*)))
  (***assert*** (cadr hepp)
                 (map (lambda (event)
                        (if (= (event :channel) 1)
                            (<copy-event> event :instrumentnum 101)
                            event))
                      (cadr hepp)))

  (***assert*** (car hepp)
                (list  (make-cloned-instrument :old-instrumentnum 0
                                               :new-instrumentnum 101)))
  
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  assign instrument number to events ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assign-instrument-numbers-0 events instrument-numbers last-patternnum) ;; Careful! 'instrument-numbers' is mutable.
  (define (next)
    (assign-instrument-numbers-0 (cdr events)
                                 instrument-numbers
                                 last-patternnum))
  (if (null? events)
      '()
      (let ((event (car events)))
        (if (not (= (event :patternnum) last-patternnum))
            (assign-instrument-numbers-0 events
                                         (make-hash-table 39 =)
                                         (event :patternnum))
            (let ((channel (event :channel))
                  (instrumentnum (event :instrumentnum)))
              (assert (>= instrumentnum -1))
              (cond ((= instrumentnum -1) ;; tempos and such
                     (cons event (next)))
                    ((= instrumentnum 0)                           
                     (let ((stored-instrumentnum (instrument-numbers channel)))
                       (if stored-instrumentnum
                           (cons (<copy-event> event :instrumentnum stored-instrumentnum)
                                 (next))
                           (next)))) ;; throw away events without instrument number assigned (what are we supposed to do with them?)
                    (else
                     (hash-table-set! instrument-numbers channel instrumentnum)
                     (cons event
                           (next)))))))))
               
;; Beware that all control events must have instrumentnum = -1 here. If not, they will be removed.
(define (assign-instrument-numbers events)
  (assign-instrument-numbers-0 events (make-hash-table 39 =) -1))


(let ((events (list (m-e :vibrato :channel 0 :line 9 :pattern 1))))
  (***assert*** (assign-instrument-numbers events)
                '()))
  
(let ((events (list (m-e :note :channel 0 :line 9 :pattern 1 :instrumentnum 1))))
  (***assert*** (assign-instrument-numbers events)
                events))
  
(let ((events (list (m-e :note :channel 0 :line 9 :pattern 1 :instrumentnum 1)
                    (m-e :vibrato :channel 0 :line 9 :pattern 1 :instrumentnum 0))))
  (***assert*** (assign-instrument-numbers events)
                (list (car events)
                      (<copy-event> (cadr events) :instrumentnum 1))))
  
 
(let ((events (list (m-e :note    :channel 0 :line 9 :pattern 1 :instrumentnum 1)
                    (m-e :vibrato :channel 1 :line 9 :pattern 1 :instrumentnum 0))))
  (***assert*** (assign-instrument-numbers events)
                (list (car events))))
 
(let ((events (list (m-e :note    :channel 0 :line 9 :pattern 1 :instrumentnum 1)
                    (m-e :vibrato :channel 0 :line 9 :pattern 2 :instrumentnum 0))))
  (***assert*** (assign-instrument-numbers events)
                (list (car events))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Add :stop events to all notes    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We don't add :stop events for notes that stops at :break though.
;; We only add :stop events for notes that are stopped because of a subsequent note on the same channel.

(define (add-stop-events-0 channel last-note)
  (if (null? channel)
      '()
      (let* ((event (car channel))
             (type (event :type)))
        (cond ((or (eq? type :note))
               ;;(eq? type :break)) ;; Note that we put the :stop event at "line 64" here. That is not legal in Radium, so we need to handle this situation specifically when sending notes over. (no, simpler to interpret :break events as stops instead of checking if the :stop event is placed on the same line as the :break event.)
               (if last-note
                   (append (list (<copy-event> last-note
                                             :linenum (event :linenum)
                                             :tick (event :tick)
                                             :type :stop)
                                 event)
                           (add-stop-events-0 (cdr channel)
                                              event))
                                              ;;;(and (eq? type :note) event)))
                   (cons event
                         (add-stop-events-0 (cdr channel)
                                            event))))
                                            ;;;(and (eq? type :note) event)))))
              ((eq? type :stop)
               (cons event
                     (add-stop-events-0 (cdr channel) #f)))
              (else
               (cons event
                     (add-stop-events-0 (cdr channel) last-note)))))))
             
      
(define (add-stop-events channel)
  (add-stop-events-0 channel #f))


(***assert*** (add-stop-events (list (m-e :note :line 4 :instrumentnum 4)
                                     (m-e :note :line 6 :tick 3 :instrumentnum 5)
                                     (m-e :break :line 8)))
              (list (m-e :note :line 4 :instrumentnum 4)
                    (m-e :stop :line 6 :tick 3 :instrumentnum 4)
                    (m-e :note :line 6 :tick 3 :instrumentnum 5)
                    (m-e :break :line 8)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       remove effect memory          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct pattern-change
  :patternnum
  :org-patternnum
  :pattern-data
  )

(define (find-compatible-change pattern-changes org-patternnum pattern-data)
  (let loop ((changes pattern-changes))
    (if (null? changes)
        #f
        (let ((change (car changes)))
          (if (and (= org-patternnum
                      (change :org-patternnum))
                   (my-equal? pattern-data
                              (change :pattern-data)))
              change
              (loop (cdr changes)))))))

(***assert*** (find-compatible-change (list (make-pattern-change :patternnum 4
                                                                 :org-patternnum 2
                                                                 :pattern-data '(1 2 3)))
                                      2
                                      '(1 2 3))
              (make-pattern-change :patternnum 4
                                   :org-patternnum 2
                                   :pattern-data '(1 2 3)))

(***assert*** (find-compatible-change (list (make-pattern-change :patternnum 4
                                                                 :org-patternnum 2
                                                                 :pattern-data '(1 2 3)))
                                      3
                                      '(1 2 3))
              #f)

(***assert*** (find-compatible-change (list (make-pattern-change :patternnum 4
                                                                 :org-patternnum 2
                                                                 :pattern-data '(1 2 3)))
                                      2
                                      '(2 3))
              #f)


(define (pattern-in-changes? pattern-changes org-patternnum)
  (true-for-at-least-one? (lambda (change)
                            (= (change :org-patternnum)
                               org-patternnum))
                          pattern-changes))




;; I.e. playlist[playlistpos] = new-patternnum
(define (set-in-playlist playlist playlistpos new-patternnum)
  (let loop ((playlist playlist)
             (n 0))
    (if (= n playlistpos)
        (cons new-patternnum (cdr playlist))
        (cons (car playlist)
              (loop (cdr playlist)
                    (1+ n))))))

(***assert*** (set-in-playlist '(0) 0 5)
              '(5))
(***assert*** (set-in-playlist '(0 1) 0 5)
              '(5 1))
(***assert*** (set-in-playlist '(0 1) 1 5)
              '(0 5))

(define (set-memory! memory channelnum key value)
  (hash-table-set! memory (<_> channelnum "__" key) value))

(define (get-memory memory channelnum key)
  (memory (<_> channelnum "__" key)))

(define (create-clean-memory num-channels)
  (define memory (make-hash-table (1- (* 39 num-channels)) eq?))
  (hash-table-set! memory :tpd *default-tpd*)
  (hash-table-set! memory :bpm *default-bpm*)
  (for-each (lambda (channelnum)
              (set-memory! memory channelnum :instrumentnum 0)
              (set-memory! memory channelnum :volume 0))
            (iota num-channels))
  memory)

;; helper function for run-through-patterns-0
(define (run-through-pattern events memory instruments)

  (define (has-next-event?)
    (not (null? (cdr events))))
  
  (define (get-next-event)
    (cadr events))

  (define (next-event-is event)
    (cond ((not event)
           (run-through-pattern (cdr events) memory instruments))
          ((list? event)
           (append event
                   (run-through-pattern (cdr events) memory instruments)))
          (else
           (cons event
                 (run-through-pattern (cdr events) memory instruments)))))

  (if (null? events)
      '()
      (let* ((event (car events))
             (type (event :type))
             (channel (event :channel))
             (instrumentnum (event :instrumentnum)))

        (define (get-mem key)
          (get-memory memory channel key))
        
        (define (set-mem! key value)
          (set-memory! memory channel key value))

        (define (legal-volume volume)
          (between 0 volume 64))

        (define old-instrumentnum (get-mem :instrumentnum)) ;; necessary for a hack below.
        
        ;; 1. Update channel instrument num
        ;;
        (if (> instrumentnum 0)
            (set-mem! :instrumentnum (event :instrumentnum)))
;        (if (and (eq? type :note)
;                 (> instrumentnum 0))
;            (set-mem! :instrumentnum instrumentnum))

        ;;(c-display "event: " (event-to-string event))
        
        ;; 2. Update channel volume and sample-offset
        ;;
        (cond (#f #f)

              ((and (eq? type :note)
                    (> instrumentnum 0))
               (set-mem! :sample-offset 0)
               (set-mem! :volume (instrument-volume (instruments (1- instrumentnum)))))

              ((eq? type :velocity)
               (set-mem! :volume (event :value)))

              ((eq? type :sample-offset)
               (set-mem! :sample-offset (event :value)))

              ((eq? type :stop)
               (set-mem! :volume 0))
              
              ((eq? type :velocity-slide)
               (set-mem! :volume (legal-volume (+ (get-mem :volume)
                                                  (* (1- (memory :tpd)) ;; This will not be correct if there is velocity-slide on a patterh with pattern-delay-line, since we expand pattern-delay afterwards.
                                                     (event :value))))))
              
              ((eq? type :fine-velocity-slide)
               (set-mem! :volume (legal-volume (+ (get-mem :volume)
                                                  (event :value))))))
               
        ;;(c-display "event: " (event-to-string event))
        ;;(c-display "volume: " (get-mem :volume))
        ;;(c-display (instruments 0))
        ;;(print-event event)
        ;;(c-display "old: " old-instrumentnum)
        
        ;; 3. Update event instrument number
        ;;
        (if (= 0 instrumentnum)
            (let ((stored-instrumentnum (get-mem :instrumentnum)))
              (if stored-instrumentnum
                  (set! event (<copy-event> event
                                          :instrumentnum stored-instrumentnum)))))

        ;; 4. Update event
        ;;
        (cond (#f #f)
              
              ((memq type '(:vibrato :tremolo))
               (define value1 (event :value))
               (define value2 (event :value2))
               (if (or (= value1 0)
                       (= value2 0))
                   (let ((stored-value (get-mem type)))
                     (if stored-value
                         (begin
                           (if (= 0 value1)
                               (set! value1 (car stored-value)))
                           (if (= 0 value2)
                               (set! value2 (cadr stored-value)))))))
               (set-mem! type (list value1 value2))
               
               (if (or (= value1 0)
                       (= value2 0))
                   (next-event-is #f)
                   (next-event-is (<copy-event> event
                                              :value value1
                                              :value2 value2))))
              
              ((and (eq? type :note)
                    (has-next-event?)               
                    (eq? ((get-next-event) :type) :slide-to-note)  ;; throw away note which is only functioning as a parameter value for slide-to-note.
                    (= (event :linenum)
                       ((get-next-event) :linenum))
                    (= (event :channel)
                       ((get-next-event) :channel)))
               (if (> (event :value) 0)
                   (set-mem! :note (event :value)))
               (if (= 0 instrumentnum)
                   (next-event-is #f)
                   (begin
                     (cond ((and (not (= 0 old-instrumentnum))
                                 (not (= old-instrumentnum instrumentnum)))
                            (set! instrumentnum old-instrumentnum)
                            (set! event (<copy-event> event
                                                    :instrumentnum old-instrumentnum))
                            (set! events (cons event
                                               (cons (<copy-event> (get-next-event)
                                                                 :instrumentnum old-instrumentnum)
                                                     (cddr events)))))
                           ((not (= (instrument-volume (instruments (1- instrumentnum)))
                                    (get-mem :volume)))
                            (c-display "event:" (event-to-string event))
                            (c-display "next-event:" (event-to-string (get-next-event)))
                            (c-display "vol: " (instrument-volume (instruments (1- instrumentnum))))
                            (c-display "memvol: " (get-mem :volume))
                            (assert #f)))
                     (next-event-is (<copy-event> event ;; If the removed note has a defined instrument, we need to add a velocity event here.
                                                :type :velocity
                                                :value (get-mem :volume))))))

              ;; This one should work for both 4-mat's madness and hoffman's freerunner:
              ((and (eq? type :note)
                    (= 0 (event :value)))
               (let* ((velocity-event (<copy-event> event
                                                  :type :velocity
                                                  :value (get-mem :volume)))
                      (offset-value (get-mem :sample-offset))
                      (offset-event (if (or (= 0 offset-value)
                                            (have-sample-offset-event-in-this-cell? (event :linenum) (event :channel) (event :patternnum) events))
                                        #f
                                        (<copy-event> event
                                                    :type :sample-offset
                                                    :value offset-value)))
                      (extra-events (append (list velocity-event)
                                            (if offset-event
                                                (list offset-event)
                                                '()))))
                 
                 (if (= instrumentnum old-instrumentnum)                   
                     (next-event-is extra-events)
                     (let ((stored-value (get-mem :note)))
                       (if (and (= 0 offset-value)
                                stored-value)
                           (next-event-is (cons (<copy-event> event :value stored-value)
                                                extra-events))
                           (next-event-is extra-events))))))

              ((eq? type :note)
               (set-mem! :note (event :value))
               (if (= 0 instrumentnum)
                   (let* ((velocity-event (<copy-event> event
                                                      :type :velocity
                                                      :value (get-mem :volume)))
                          (offset-value (get-mem :sample-offset))
                          (offset-event (if (or (not offset-value)
                                                (= 0 offset-value)
                                                (have-sample-offset-event-in-this-cell? (event :linenum) (event :channel) (event :patternnum) events))
                                            #f
                                            (<copy-event> event
                                                        :type :sample-offset
                                                        :value offset-value)))
                          (extra-events (append (list velocity-event)
                                                (if offset-event
                                                    (list offset-event)
                                                    '()))))
                     (next-event-is (cons event extra-events)))
                   (next-event-is event)))
               

              ((eq? type :slide-to-note)
               (define value1 (event :value))
               (define value2 (get-mem :note))

               (if (= 0 value1)
                   (set! value1 (get-mem type))
                   (set-mem! type value1))

               ;;(print-event event)
               ;;(c-display "value: " value1)
               ;;(c-display "note: " value2)
               
               (if (or (not value1)
                       (not value2))
                   (next-event-is #f)
                   (next-event-is (<copy-event> event
                                              :value value1
                                              :value2 (note->period value2)))))

              ((memq type '(:pitch-slide :velocity-slide :fine-pitch-slide :fine-velocity-slide))
               (if (= (event :value) 0)
                   (let ((stored-value (get-mem type)))
                     ;;(c-display "stored value: " stored-value)
                     (if stored-value
                         (next-event-is (<copy-event> event :value stored-value))
                         (next-event-is #f))) ;; No value. Just throw the event away
                   (begin
                     (set-mem! type (event :value))
                     (next-event-is event))))

              ;;((memq type '(:stop))
              ;; (hash-table-set! memory (get-key :note) #f) ;; is this right? c00 can create a :stop event, but should a c00 make us forget last note value? (answer: no. Don't know why this was done)
              ;; (next-event-is event))
              
              ((memq type '(:tpd))
               (hash-table-set! memory :tpd (event :value))
               (next-event-is event))
              
              ((memq type '(:bpm))
               (hash-table-set! memory :bpm (event :value))
               (next-event-is event))
              
              (else
               (next-event-is event))))))


;;velocities [:note               :value 83, :p 19, :c 0, :line 30, :instrument 4] (30 0 1 ) (29 65533 65534 ) ((30 48 #f ) ) 
;;[:break              :value 55, :p 19, :c 0, :line 30, :instrument -1] 

(define (get-pattern-data-for-next-run-through-0 result
                                                 events
                                                 memory
                                                 patternnum
                                                 curr-playlistpos
                                                 next-playlistpos
                                                 linenum-in-next-pattern
                                                 end-linenum)

  ;;(c-display "        " patternnum curr-playlistpos next-playlistpos linenum-in-next-pattern end-linenum)
  
  (define (finished next-linenum)
    (let ((next-playlistpos (or next-playlistpos (1+ curr-playlistpos))))
      (c-display "  ***** playlistpos: " curr-playlistpos ", next playlistpos:" next-playlistpos ", patternnum: " patternnum ", next-linenum:" linenum-in-next-pattern next-linenum ", end-linenum:" end-linenum ", null-events?" (null? events) "\n\n")
      (list (reverse (cons (m-e :break :line end-linenum :pattern patternnum :instrumentnum -1) ;; we need a :break event to know how many lines there are in the pattern
                           result))
            next-playlistpos
            next-linenum)))
  
  (if (null? events)
      (finished linenum-in-next-pattern)
      (let* ((event (car events))
             (type (event :type))
             (linenum (event :linenum)))
        
        (cond ((and end-linenum (>= linenum end-linenum))
               (finished linenum-in-next-pattern))

              ((eq? :loop type) ;; loop logic copied from the protracker 2.3d clone source code made by Olav "8bitbubsy" Sorensen.
               (let* ((channelnum (event :channel))
                      (value (event :value))
                      (loop-counter (or (get-memory memory channelnum :loop-counter) 0))
                      (loop-counter-before loop-counter)
                      (do-looping? (cond ((= value 0)
                                          (set-memory! memory channelnum :loop-start-line linenum)
                                          #f)
                                         ((= loop-counter 0)
                                          (set-memory! memory channelnum :loop-counter value)
                                          #t)
                                         (else
                                          (set! loop-counter (1- loop-counter))
                                          (set-memory! memory channelnum :loop-counter loop-counter)
                                          (> loop-counter 0)))))

                 ;;(c-display "   " linenum ": do-looping:" do-looping?
                 ;;           ", value:" value
                 ;;           ", counter:" loop-counter-before loop-counter
                 ;;           ", event:" (event-to-string event)
                 ;;           ", memstartline:" (get-memory memory channelnum :loop-start-line))
                 ;;(when (= linenum 63)
                 ;;  (print-events events)
                 ;;  (c-display "  << loop finished"))
                 (if do-looping?
                     (get-pattern-data-for-next-run-through-0 result
                                                              (cdr events)
                                                              memory
                                                              patternnum
                                                              curr-playlistpos
                                                              curr-playlistpos
                                                              (or (get-memory memory channelnum :loop-start-line) 0)
                                                              (1+ linenum))
                     (get-pattern-data-for-next-run-through-0 (cons event result)
                                                              (cdr events)
                                                              memory
                                                              patternnum
                                                              curr-playlistpos
                                                              next-playlistpos
                                                              linenum-in-next-pattern
                                                              end-linenum))))
              ((eq? :position-jump type)
               ;;(c-display "  posjump:" (event-to-string event))
               (get-pattern-data-for-next-run-through-0 result
                                                        (cdr events)
                                                        memory
                                                        patternnum
                                                        curr-playlistpos
                                                        (event :value)
                                                        linenum-in-next-pattern
                                                        (1+ linenum)))

              ((eq? :break type)
               ;;(c-display "  break. curr-playlistpos:" curr-playlistpos ", next-playlistpos: " next-playlistpos ", event:" (event-to-string event))
               ;;(print-events events)
               ;;(c-display "  << break finished")
               (get-pattern-data-for-next-run-through-0 result
                                                        (cdr events)
                                                        memory
                                                        patternnum
                                                        curr-playlistpos
                                                        (if #f
                                                            (if next-playlistpos
                                                                (1+ next-playlistpos)
                                                                (1+ curr-playlistpos))
                                                            next-playlistpos
                                                            )
                                                        (event :value)
                                                        (1+ linenum)))

              (else
               (get-pattern-data-for-next-run-through-0 (cons event result)
                                                        (cdr events)
                                                        memory
                                                        patternnum
                                                        curr-playlistpos
                                                        next-playlistpos
                                                        linenum-in-next-pattern
                                                        end-linenum))))))

(define (get-pattern-data-for-next-run-trough patterns memory patternnum playlistpos start-linenum max-num-lines)
  (define stuff (get-pattern-data-for-next-run-through-0 '()
                                                         (remove-while (vector-ref patterns patternnum)
                                                                       (lambda (event)
                                                                         (< (event :linenum)
                                                                            (or start-linenum 0))))
                                                         memory
                                                         patternnum
                                                         playlistpos
                                                         #f ;; next-playlistpos
                                                         #f ;;linenum-in-next-pattern
                                                         max-num-lines ;;end-linenum
                                                         ))
  ;;(c-display " RESULT:" start-linenum ((car (reverse (car stuff))) :linenum))
  (list (if start-linenum
            (map (lambda (event)
                   (<copy-event> event
                               :linenum (- (event :linenum) start-linenum)))
                 (car stuff))
            (car stuff))
        (cadr stuff)
        (caddr stuff)))

  
(define (run-through-patterns-not-in-playlist-0 playlist patterns max-num-lines num-channels instruments)
  (let loop ((patternnum (1- (vector-length patterns)))
             (patterns patterns))
    (cond ((= -1 patternnum)
           patterns)
          ((not (memv patternnum playlist))
           (c-display "Running through pattern" patternnum)
           (let* ((memory (create-clean-memory num-channels))
                  (stuff (get-pattern-data-for-next-run-trough patterns memory patternnum 0 0 max-num-lines))
                  (org-pattern (car stuff))
                  (org-pattern-with-tempos (append (list (m-e :tpd :value *default-tpd* :pattern patternnum)
                                                         (m-e :bpm :value *default-bpm* :pattern patternnum))
                                                   org-pattern))
                  (new-pattern (run-through-pattern org-pattern-with-tempos
                                                    memory
                                                    instruments)))
             (loop (1- patternnum)
                   (replace-pattern-in-patterns patterns patternnum new-pattern))))
          (else
           (loop (1- patternnum)
                 patterns)))))


(define (run-through-patterns-not-in-playlist playlist events max-num-lines num-channels instruments)
  (ungroup-patterns-into-events (run-through-patterns-not-in-playlist-0 playlist
                                                                        (group-events-in-patterns events)
                                                                        max-num-lines
                                                                        num-channels
                                                                        instruments)))
                                          

;; This function "plays through" the song (using the playlist) and creates a new pattern for each positition in the playlist.
;;
;;   The new patterns always contains tempo events at line 0 plus that effect memory is removed and so forth.
;;   I.e. it fills in all information that you could only obtain if playing through the song.
 ;;
;;   When a new pattern is created, it checks the previously created patterns to see if we can use one of those.
;;   If not, we add the new pattern to the song.
;;
(define (run-through-patterns-0 playlistpos
                                playlist
                                linenum
                                max-num-lines
                                org-playlist
                                org-patterns ;; All original pattern (input)
                                patterns     ;; the result (output)
                                memory
                                instruments
                                changes) ;; 'changes' is just an optimization. However, I don't think it makes a very big difference. Probably best just to remove it.

  (define last-playlistpos-key 'playlistpos-key)
  (define last-playlistpos (or (memory last-playlistpos-key) 0))
  (hash-table-set! memory last-playlistpos-key playlistpos)

  (define playlistpos-key (<_> "playlistpos-key " playlistpos "_" linenum))
  
  (define num-visits-before (or (memory playlistpos-key) 0))

  (if (not (= last-playlistpos playlistpos)) ;; Don't update num visits when looping.
      (hash-table-set! memory playlistpos-key (1+ num-visits-before)))
      
  ;;(c-display "pos: " playlistpos "___playlist " playlist " linenum: " linenum (length (get-pattern events 21)) ", num_patterns:" (find-num-patterns events))
  (c-display "   playlistpos-key:" playlistpos-key ", num-visits-before:" num-visits-before)
      
  (cond ((= (length org-playlist)
            playlistpos)
         
         ;; Outcome 1a: Finished.
         (begin
           ;;(c-display "finiifnished")
           ;;(print-events (get-pattern events 21))
           (list playlist
                 patterns)))
         
        ;; Outcome 1b: We've been here two times before. Time to stop. Finished.
        ((>= num-visits-before 2)
         (list playlist
               patterns))
        
        (else
         
         (let* ((patternnum (list-ref org-playlist playlistpos))
                (org-pattern-and-next-playlistpos-and-linenum (get-pattern-data-for-next-run-trough org-patterns memory patternnum playlistpos linenum max-num-lines))
                (org-pattern (car org-pattern-and-next-playlistpos-and-linenum) )
                (next-playlistpos (cadr org-pattern-and-next-playlistpos-and-linenum))
                (next-linenum (caddr org-pattern-and-next-playlistpos-and-linenum))
                (org-pattern-with-tempos (append (list (m-e :tpd :value (or (memory :tpd) *default-tpd*) :pattern patternnum :instrumentnum -1)
                                                       (m-e :bpm :value (or (memory :bpm) *default-bpm*) :pattern patternnum :instrumentnum -1))
                                                 org-pattern))
                (new-pattern (run-through-pattern org-pattern-with-tempos
                                                  memory
                                                  instruments)))

           ;;(c-display "  ai:" (event-to-string (car new-pattern)))
           (if (not (pattern-in-changes? changes patternnum)) ;; TODO: Check if changing this to "(and #f (not (pattern-in-changes? changes patternnum))" makes any difference in computation speed. (the code would be somewhat simpler then)
                
               ;; Outcome 2: Pattern used in the playlist for the first time.
               (let* ((new-change (make-pattern-change :patternnum patternnum
                                                       :org-patternnum patternnum
                                                       :pattern-data new-pattern)))
                 (c-display " used for the first time:" patternnum (map (lambda (c) (c :org-patternnum)) changes) ", org-playlist:" org-playlist)
                 (run-through-patterns-0 next-playlistpos
                                         (append playlist (list patternnum))
                                         next-linenum
                                         max-num-lines
                                         org-playlist
                                         org-patterns
                                         (replace-pattern-in-patterns patterns patternnum new-pattern)
                                         memory
                                         instruments
                                         (cons new-change
                                               changes)))
               
               (let ((compatible-change (find-compatible-change changes patternnum new-pattern))) ;; TODO: Do the thing above, and rewrite "find-compatible-change" to not use 'changes'. Just check all patterns instead.
                 
                 (if compatible-change
                     
                     ;; Outcome 3: Pattern was used in the playlist before, and one of those patterns was equal to the current produced events. Just reuse that pattern.
                     (run-through-patterns-0 (or next-playlistpos (1+ playlistpos))
                                             (append playlist (list (compatible-change :patternnum)))
                                             next-linenum
                                             max-num-lines
                                             org-playlist
                                             org-patterns
                                             patterns
                                             memory
                                             instruments
                                             changes)
                     
                     ;; Outcome 4: Pattern was used in the playlist before, but there were no compatible versions. Create a new pattern and modify the playlist.
                     (let* ((new-patternnum (vector-length patterns))
                            (new-change (make-pattern-change :patternnum new-patternnum
                                                             :org-patternnum patternnum
                                                             :pattern-data new-pattern))
                            (new-pattern-with-new-patternnums (map (lambda (event)
                                                                     (<copy-event> event
                                                                                 :patternnum new-patternnum))
                                                                   new-pattern)))
                       ;;(c-display "CHANGES: " changes)
                       (c-display "   Copy pattern" patternnum "into" new-patternnum (length org-pattern-with-tempos) (length new-pattern) (length new-pattern-with-new-patternnums))
                       ;;(print-events new-pattern-with-new-patternnums)
                       ;;(print-events (get-pattern (append events
                       ;;                                   new-pattern-with-new-patternnums)
                       ;;                           21))
                       (run-through-patterns-0 (or next-playlistpos (1+ playlistpos))
                                               (append playlist (list new-patternnum))
                                               next-linenum
                                               max-num-lines
                                               org-playlist
                                               org-patterns
                                               (vector-append patterns
                                                              (vector new-pattern-with-new-patternnums))
                                               memory
                                               instruments
                                               (cons new-change
                                                     changes))))))))))
                                          
;; returns (list playlist events)
(define (run-through-patterns playlist events max-num-lines num-channels instruments)
  (define patterns (group-events-in-patterns events))
  (define ab (run-through-patterns-0 0
                                     '()
                                     0
                                     max-num-lines
                                     playlist
                                     patterns
                                     patterns
                                     (create-clean-memory num-channels)
                                     instruments
                                     '()))
  (list (car ab)
        (ungroup-patterns-into-events (cadr ab))))

(set! *test-instruments*
      (list->vector (map (lambda (i)
                           (vector "aa" "adf" 14213 0 40 0 0 0))
                         (iota 20))))
#||
;; Test break and position-jump
(let ((hepp (run-through-patterns '(0)
                                  (list (m-e :note :line 0 :value 20 :instrumentnum 1)
                                        (m-e :note :line 1 :value 21)
                                        (m-e :note :line 2 :value 22)
                                        (m-e :position-jump :line 2 :value 0 :instrumentnum -1)
                                        (m-e :break :line 2 :value 3 :instrumentnum -1)
                                        (m-e :note :line 3 :value 23))
                                  64 4 *test-instruments*)))
  (c-display "playlist" (car hepp))
  (print-events (cadr hepp))

  (***assert*** (car hepp) '(0 1))

  (***assert*** (cadr hepp)
                (list (m-e :tpd :value *default-tpd*)
                      (m-e :bpm :value *default-bpm*)
                      (m-e :note :line 0 :value 20 :instrumentnum 1)
                      (m-e :note :line 1 :value 21 :instrumentnum 1)
                      (m-e :note :line 2 :value 22 :instrumentnum 1)
                      (m-e :break :line 3 :instrumentnum -1)
                      
                      (m-e :tpd :value *default-tpd* :pattern 1 :instrumentnum 1)
                      (m-e :bpm :value *default-bpm* :pattern 1 :instrumentnum 1)
                      (m-e :note :line 0 :value 23 :instrumentnum 1 :pattern 1)
                      (m-e :break :line 61 :instrumentnum -1 :pattern 1))))
  

;;(assert #f)


(define (test-run-through-patterns1 effect-type)
  
  (***assert*** (run-through-patterns '(0)
                                      (list (m-e effect-type :value 0))
                                      64 4
                                      )
                (list '(0)
                      (list (m-e :tpd :value *default-tpd*)
                            (m-e :bpm :value *default-bpm*)
                            (m-e :break :line 64 :instrumentnum -1))))
  
  (***assert*** (run-through-patterns '(0)
                                      (list (m-e effect-type :value 0))
                                      64 4 *test-instruments*)
                (list '(0)
                      (list (m-e :tpd :value *default-tpd*)
                            (m-e :bpm :value *default-bpm*)
                            (m-e :break :line 64 :instrumentnum -1))))
  
  (let ((hepp (run-through-patterns '(0)
                                    (list (m-e effect-type :value 0)
                                          (m-e effect-type :value 10)
                                          (m-e effect-type :value 0))
                                    64 4 *test-instruments*)))
    (***assert*** hepp
                  (list '(0)
                        (list (m-e :tpd :value *default-tpd*)
                              (m-e :bpm :value *default-bpm*)
                              (m-e effect-type :pattern 0 :value 10)
                              (m-e effect-type :pattern 0 :value 10)
                              (m-e :break :line 64 :instrumentnum -1)))))
  
  (let ((hepp (run-through-patterns '(0)
                                    (list (m-e effect-type :value 0)
                                          (m-e effect-type :channel 1 :value 10)
                                          (m-e effect-type :value 0))
                                    64 4 *test-instruments*)))
    (***assert*** hepp
                  (list '(0)
                        (list (m-e :tpd :value *default-tpd*)
                              (m-e :bpm :value *default-bpm*)
                              (m-e effect-type :pattern 0 :channel 1 :value 10)
                              (m-e :break :line 64 :instrumentnum -1)))))
  
  (let ((hepp (run-through-patterns '(0 0)
                                    (list (m-e effect-type :value 0)
                                          (m-e effect-type :value 10)
                                          (m-e effect-type :value 0))
                                    64 4 *test-instruments*)))
    (c-display "playlist" (car hepp))
    (print-events (cadr hepp))
    (***assert*** hepp
                  (list '(0 1)
                        (list (m-e :tpd :value *default-tpd*)
                              (m-e :bpm :value *default-bpm*)
                              (m-e effect-type :pattern 0 :value 10)
                              (m-e effect-type :pattern 0 :value 10)
                              (m-e :break :line 64 :pattern 0 :instrumentnum -1)
                              
                              (m-e :tpd :value *default-tpd* :pattern 1)
                              (m-e :bpm :value *default-bpm* :pattern 1)
                              (m-e effect-type :pattern 1 :value 10)
                              (m-e effect-type :pattern 1 :value 10)
                              (m-e effect-type :pattern 1 :value 10)
                              (m-e :break :line 64 :pattern 1 :instrumentnum -1)))))

  )

(let ((hepp (run-through-patterns '(0 1)
                                  (list (m-e :break :line 10 :value 11 :instrumentnum -1)
                                        (m-e :position-jump :line 10 :value 0 :instrumentnum -1)
                                        (m-e :note :pattern 1 :line 21 :value 50 :instrumentnum 5 ))
                                  64 4 *test-instruments*)))
  ;;(m-e :pitch-slide :value 0)
                                        
  (c-display "hepp: " hepp)
  (c-display "playlist" (car hepp))
  (print-events (cadr hepp)))


;;(assert #f)


(for-each test-run-through-patterns1
          (list :pitch-slide :velocity-slide :fine-pitch-slide :fine-velocity-slide))


(let ((hepp (run-through-patterns '(0 0)
                                  (list (m-e :pitch-slide :value 0)
                                        (m-e :pitch-slide :value 10)
                                        (m-e :pitch-slide :value 0))
                                  64 4 *test-instruments*)))
  (c-display "hepp: " hepp)
  (c-display "playlist" (car hepp))
  (print-events (cadr hepp)))

  
;; tempo testing
(let ((hepp (run-through-patterns '(0 0)
                                  (list (m-e :tpd :value 2 :line 3))
                                  64 4 *test-instruments*)))
  (c-display "playlist" (car hepp))
  (print-events (cadr hepp))
  (***assert*** hepp
                (list '(0 1)
                      (list (m-e :tpd :value *default-tpd*)
                            (m-e :bpm :value *default-bpm*)
                            (m-e :tpd :value 2 :line 3)
                            (m-e :break :line 64 :instrumentnum -1)
                            (m-e :tpd :value 2 :pattern 1)
                            (m-e :bpm :value *default-bpm* :pattern 1)
                            (m-e :tpd :value 2 :line 3 :pattern 1)
                            (m-e :break :line 64 :pattern 1 :instrumentnum -1)                            
                            ))))


;; slide-to-note testing
(define (test-slide-to-note events expected-result)
  (define result (run-through-patterns '(0) events 64 4 *test-instruments*))
  (print-events (cadr result))
  (***assert*** result
                (list '(0)
                      (append (list (m-e :tpd :value *default-tpd*)
                                    (m-e :bpm :value *default-bpm*))
                              expected-result
                              (list (m-e :break :line 64 :instrumentnum -1))
                              ))))

(test-slide-to-note '()
                    '())

(test-slide-to-note (list (m-e :note))
                    '())

(test-slide-to-note (list (m-e :slide-to-note :value 90))
                    '())

||#


#||
(test-slide-to-note (list (m-e :note           :value 5 :line 0)
                          (m-e :slide-to-note  :value 0 :line 1))
                    (list (m-e :note           :value 5 :line 0)))

(test-slide-to-note (list (m-e :note           :value 5 :line 0)
                          (m-e :note           :value 6 :line 1)
                          (m-e :slide-to-note :value 0 :line 1))
                    (list (m-e :note           :value 5  :line 0)))


(test-slide-to-note (list (m-e :note           :value 5 :line 0)
                          (m-e :note           :value 6 :line 1)
                          (m-e :slide-to-note :value 30 :line 1))
                    (list (m-e :note           :value 5  :line 0)
                          (m-e :slide-to-note  :value 30 :line 1 :value2 (note->period 6))))

(test-slide-to-note (list (m-e :note           :value 5 :line 0)
                          (m-e :note           :value 6 :line 1)
                          (m-e :slide-to-note :value 30 :line 1)
                          (m-e :slide-to-note :value 0 :line 2))
                    (list (m-e :note           :value 5  :line 0)
                          (m-e :slide-to-note  :value 30 :line 1 :value2 (note->period 6))
                          (m-e :slide-to-note  :value 30 :line 2 :value2 (note->period 6))))


(test-slide-to-note (list (m-e :note           :value 5 :line 0 :instrumentnum 1)
                          (m-e :velocity       :value 0 :line 0)
                          (m-e :note           :value 10 :line 1 :instrumentnum 1)
                          (m-e :slide-to-note  :value 30 :line 1)
                          (m-e :velocity       :value 0  :line 5))
                    (list (m-e :note           :value 5 :line 0 :instrumentnum 1)
                          (m-e :velocity       :value 0 :line 0 :instrumentnum 1)
                          (m-e :slide-to-note  :value 30 :line 1 :value2 (note->period 10) :instrumentnum 1)
                          (m-e :velocity       :value 0  :line 5 :instrumentnum 1)))

||#

;;(assert #f)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  helper functions for velocity and pitch gliding ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (tickify y num-ticks-in-line)
  y) ;; nah, don't bother. 'y' is the correct value, the tickified value only exists in protracker.

;; Keep correct graph direction even if x2 goes outside legal value
(define (clamp-glide y1 x1
                     y2 x2
                     min-x2 max-x2
                     num-ticks-in-line)
  ;;(c-display "clamp-glide" y1 x1 y2 x2 min-x2 max-x2 num-ticks-in-line)
  (cond ((= x1 x2)
         (list y1 x1 y2 x2))
        ((> x2 x1)
         ;;(c-display "glide a" y1 x1 y2 x2 (<= x2 max-x2))
         (if (<= x2 max-x2)
             (list y1 x1 y2 x2)
             (let ((new-y2 (scale max-x2
                                  x1 x2
                                  y1 y2)))
               (list y1 x1
                     (tickify new-y2 num-ticks-in-line) max-x2))))
        (else
         ;;(c-display "glide b" y1 x1 y2 x2)
         (if (>= x2 min-x2)
             (list y1 x1 y2 x2)
             (let ((new-y2 (scale min-x2
                                  x1 x2
                                  y1 y2)))
               (list y1 x1
                     (tickify new-y2 num-ticks-in-line) min-x2))))))
         
(clamp-glide 45 29
             45.16666666666666 28
             113 856
             6)

(***assert*** (clamp-glide 5 0
                           6 2
                           0 2
                           4)
              '(5 0 6 2))
                  
(***assert*** (clamp-glide 5 0
                           6 4
                           0 1
                           4)
              `(5 0 ,(+ 5 1/4) 1))
                  
(***assert*** (clamp-glide 5 4
                           6 0
                           1 4
                           4)
              `(5 4 ,(+ 5 3/4) 1))
                  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   create pitch changes for a note   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-constant *test-tpds* (list->vector (iota 64))) ;; contains (vector 0 1 2 3 ... 84)
                                  
(define (create-arpeggio-pitches linenum curr-period val1 val2 tpds)
  (let* ((note1 (period->note curr-period))
         (note2 (+ note1 val1))
         (note3 (+ note1 val2))
         (notes (vector note1 note2 note3))
         (num-ticks (get-tpd tpds linenum)))
    (let loop ((tick 0))
      (if (= tick num-ticks)
          (list (list (1+ linenum) note1 #f))
          (cons (list (+ linenum (/ tick num-ticks)) (notes (modulo tick 3)) #f)
                (loop (1+ tick)))))))

(***assert*** (create-arpeggio-pitches 1 20 4 8 (vector 5 6))
              (let* ((note1 (period->note 20))
                     (note2 (+ note1 4))
                     (note3 (+ note1 8)))
                (list (list (+ 1 0/6) note1 #f)
                      (list (+ 1 1/6) note2 #f)
                      (list (+ 1 2/6) note3 #f)
                      (list (+ 1 3/6) note1 #f)
                      (list (+ 1 4/6) note2 #f)
                      (list (+ 1 5/6) note3 #f)
                      (list (+ 1 6/6) note1 #f))))


(define (clamp-period period)
  (between *min-slide-period* period *max-slide-period*))

(define (period-glide-to-note-glide period-glide)
  ;;(c-display "period-glide: " period-glide)
  (list (list (car period-glide)
              (period->note (cadr period-glide))
              #t)
        (list (caddr period-glide)
              (period->note (cadddr period-glide))
              #f)))

(define (get-pitches-0 note curr-period events tpds)
  (let* ((event (car events))
         (linenum (event :linenum))
         (place (+ linenum (/ (event :tick) (tpds linenum))))
         (type (event :type)))
    (cond ((or (eq? type :stop)
               (eq? type :note))
           (list (list place
                       (period->note curr-period)
                       #f)))

          ((eq? type :break)
           (assert (= place linenum))
           (list (list place
                       (period->note curr-period)
                       #f)))

          ((eq? type :pitch-slide)
           ;; The mod v4. document clearly says that final period is period-value*(tpd-1), and not period-value*tpd.
           ;; The source of soundtracker and openMTP indicates that it might be correct too although it sounds like the sound might sound a bit strange sometimes.
           (let* ((num-ticks-in-line (get-tpd tpds linenum))
                  (new-period        (- curr-period
                                        (* (event :value)
                                           (if (event :is-pattern-delay-line)
                                               num-ticks-in-line
                                               (1- num-ticks-in-line)))))
                  (glide             (clamp-glide place       curr-period
                                                  (1+ linenum) new-period
                                                  *min-slide-period* *max-slide-period*
                                                  num-ticks-in-line)))
             (append (period-glide-to-note-glide glide)
                     (get-pitches-0 note (last glide) (cdr events) tpds))))
          
          ((eq? type :fine-pitch-slide)
           (let ((new-period (clamp-period (- curr-period (event :value)))))
             (cons  (list place
                          (period->note new-period)
                          #f)
                    (get-pitches-0 note new-period (cdr events) tpds))))
          
          ((eq? type :arpeggio)
           (assert (= place linenum))
           (append (create-arpeggio-pitches linenum
                                            curr-period
                                            (event :value)
                                            (event :value2)
                                            tpds)
                   (get-pitches-0 note curr-period (cdr events) tpds)))


          ((and (eq? type :slide-to-note)
                (not (= (event :value2)
                        curr-period)))
           (assert (= place linenum))
           (let* ((num-ticks-in-line (tpds linenum))
                  (value (event :value))
                  (goal-period (event :value2))
                  (up? (> goal-period curr-period))
                  (period-next-line (if up?
                                        (+ curr-period (* (1- num-ticks-in-line) value)) ;; think (1- num-ticks-in-line) is how protracker does it...
                                        (- curr-period (* (1- num-ticks-in-line) value)))) ;; think (1- num-ticks-in-line) is how protracker does it...
                  (span-whole-line (if up?
                                       (>= goal-period period-next-line)
                                       (<= goal-period period-next-line))))

             ;;(c-display "hepp" curr-period period-next-line goal-period num-ticks-in-line)
             ;;(c-display "span-whole: " span-whole-line)
             (if span-whole-line
                 (let ((glide (clamp-glide place         curr-period
                                           (1+ linenum) period-next-line
                                           *min-slide-period* *max-slide-period*
                                           num-ticks-in-line)))
                   (append (period-glide-to-note-glide glide)
                           (get-pitches-0 note
                                          (last glide)
                                          (cdr events)
                                          tpds)))
                 (let ((glide (clamp-glide linenum
                                           curr-period
                                           (+ linenum
                                              (/ (min num-ticks-in-line
                                                      (ceiling (abs (scale goal-period
                                                                           curr-period period-next-line ;; probably not entirely correct due to the num-ticks-in-line-1 weirdness.
                                                                           0 num-ticks-in-line))))
                                                 num-ticks-in-line))
                                           goal-period
                                           *min-slide-period* *max-slide-period*
                                           num-ticks-in-line)))
                   ;;(c-display "glide: " glide)
                   ;;(c-display "period-glide: " (period-glide-to-note-glide glide))
                   (append (period-glide-to-note-glide glide)
                           (get-pitches-0 note
                                          (last glide)
                                          (cdr events)
                                          tpds))))))
                  
          (else
           (get-pitches-0 note
                          curr-period
                          (cdr events)
                          tpds)))))

(define (validate-gliding2 glidings)
  (define org-glidings glidings)
  (let loop ((glidings glidings))
    (cond ((not (null? (cdr glidings)))
           (let ((p1 (car glidings))
                 (p2 (cadr glidings)))
             (if (not (> (car p2) (car p1)))
                 (begin
                   ;;(for-each c-display org-glidings)
                   ;;(c-display "p1: " p1 (* 1.0 (car p1)) ", p2: " p2 (* 1.0 (car p2)))
                   ;;(assert #f)
                   #f
                   )
                 (loop (cdr glidings)))))
          (else
           #t))))

;; #t = linear
;; #f = holding
(define (remove-unnecessary-gliding-points glidings)
  (let loop ((glidings glidings)
             (prev-gliding-value #f)
             (prev-was-holding #f))
    (if (or (null? glidings)
            (null? (cdr glidings)))
        glidings
        (let* ((a (car glidings))
               (b (cadr glidings))
               (holding-a (not (caddr a)))
               (value-a (cadr a))
               (value-b (cadr b)))
          (if (and prev-gliding-value
                   (= prev-gliding-value value-a)
                   prev-was-holding
                   holding-a)
              (loop (cdr glidings)
                    prev-gliding-value
                    prev-was-holding)
              (cons (car glidings)
                    (loop (cdr glidings)
                          value-a
                          holding-a)))))))


         
;; #t = linear
;; #f = holding
(define (fix-gliding glidings)
  (remove-unnecessary-gliding-points
   (let loop ((glidings glidings)
              (is-first #t)
              (prev-was-holding #f))
     (if (or (null? glidings)
             (null? (cdr glidings)))
         glidings
         (let* ((a (car glidings))
                (b (cadr glidings))
                (holding-a (not (caddr a)))
                (time-a (car a))
                (time-b (car b)))
           (if (= time-a time-b)
               (let ((value-a (cadr a))
                     (value-b (cadr b)))
                 (cond ((null? (cddr glidings))
                        (list a))
                       ((= value-a value-b)
                        (loop (cdr glidings) is-first holding-a))
                       (is-first
                        (loop (cdr glidings) is-first holding-a))
                       (prev-was-holding ;; If previous glide was holding, then 'a' has no practical function and can be thrown away.
                        (loop (cdr glidings) is-first holding-a))
                       (else
                        (cons (list (-line time-a) value-a #f) ;; must include both glides
                              (loop (cdr glidings) #f #t)))))
               (cons a
                     (loop (cdr glidings) #f holding-a))))))))



;; Before calling:
;; * note-arguments for ':slide-to-note' must be moved into :value2.
;;
(define (get-pitches note channel-events tpds) ;; 'events' is here all the rest of the events in the channel right after the note.
  (let* ((linenum (note :linenum))
         (place1 (+ (note :linenum) (/ (note :tick) (tpds linenum))))
         (value1 (note :value)))
    (fix-gliding
     (cons (list place1 value1 #f)
           (get-pitches-0 note
                          (note->period (note :value))
                          channel-events
                          tpds)))))

#||
pitches:  ((30 0 #t ) (1736/65 12 #f ) (31 12 #f ) ) 
velocities:  ((30 31 #f ) (31 31 #f ) ) 
[:note               :value 0, :p 19, :c 1, :line 30, :instrument 10] 

[:pitch-slide        :value -65, :p 19, :c 1, :line 30, :instrument 10] 
[:break              :value 0, :p 19, :c 0, :line 31, :instrument -1] 

(get-pitches (m-e :note :value 0
||#


(***assert*** (get-pitches (m-e :note :line 44 :value 29)
                           (list
                            (m-e :slide-to-note :line 45 :value 48 :value2 339)
                            (m-e :break :line 64))
                           (make-vector 100 6))
              '((44 29 #f)
                (45 29 #t)
                (271/6 28 #f) ;; Note: Haven't calculated and checked manually that 271/6 is correct. Might be wrong.
                (64 28 #f)))



;; Testing 'slide'
;;
(***assert*** (get-pitches (m-e :note :line 5)
                           (list
                            (m-e :stop :line 9))
                           *test-tpds*)
              '((5 0 #f)
                (9 0 #f)))

(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :pitch-slide :line 5 :value 7)
                            (m-e :stop :line 6))
                           *test-tpds*)
              (let ((period1 (- (note->period 20)
                                (* 4 7))))
                `((5 20 #t)
                  (6 ,(period->note period1) #f))))

(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :pitch-slide :line 5 :value 7)
                            (m-e :stop :line 9))
                           *test-tpds*)
              (let ((period1 (- (note->period 20)
                                (* 4 7))))
                `((5 20 #t)
                  (6 ,(period->note period1) #f)
                  (9 ,(period->note period1) #f))))

(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :pitch-slide :line 6 :value 7)
                            (m-e :stop :line 9))
                           *test-tpds*)
              (let ((period1 (- (note->period 20)
                                (* 5 7))))
                `((5 20 #f)
                  (6 20 #t)
                  (7 ,(period->note period1) #f)
                  (9 ,(period->note period1) #f))))

(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :pitch-slide :line 6 :value 7)
                            (m-e :break :line 9))
                           *test-tpds*)
              (let ((period1 (- (note->period 20)
                                (* 5 7))))
              `((5 20 #f)
                (6 20 #t)
                (7 ,(period->note period1) #f)
                (9 ,(period->note period1) #f)))) ;; TODO: Fix outside-block line positions when sending over to radium instead.

(***assert*** (get-pitches (m-e :note :line 5 :value 20)
                           (list 
                            (m-e :pitch-slide :line 6 :value 7)
                            (m-e :pitch-slide :line 7 :value 8)
                            (m-e :stop :line 9))
                           *test-tpds*)
              (let* ((period7 (- (note->period 20)
                                 (* 5 7)))
                     (period8  (- period7
                                  (* 6 8))))
                `((5 20 #f)
                  (6 20 #t)
                  (7 ,(period->note period7) #t)
                  (8 ,(period->note period8) #f)
                  (9 ,(period->note period8) #f))))

#||
(get-pitches (m-e :note :line 5 :value 20)
             (list 
              (m-e :pitch-slide :line 6 :value 7)
              (m-e :pitch-slide :line 7 :value 8)
              (m-e :stop :line 9))
             *test-tpds*)
(* 1.0 593/28)
(* 1.0 619/27)
((5 20) (6 20 7 593/28) (7 593/28 8 619/27) (9 619/27))
||#


;; testing trying to slide outside min and max period
;;
;; min period
(let* ((note1 46)
       (period1 (note->period note1))
       (note2 (period->note *min-slide-period*)))
  (***assert*** (get-pitches (m-e :note :value note1 :line 0)
                             (list 
                              (m-e :pitch-slide :line 0 :value 7)
                              (m-e :stop :line 1))
                             (vector 4 4 4))
                `((0   ,note1 #t)
                  (1/3 ,note2 #f)
                  (1   ,note2 #f))))
;;
;; max period
(let* ((note1 13)
       (period1 (note->period note1))
       (note2 (period->note *max-slide-period*)))
  ;;(c-display "period1" period1)
  (***assert*** (get-pitches (m-e :note :value note1 :line 0)
                             (list 
                              (m-e :pitch-slide :line 0 :value -48)
                              (m-e :stop :line 1))
                             (vector 4 4 4))
                `((0   ,note1 #t)
                  (1/3 ,note2 #f)
                  (1   ,note2 #f))))


;; Testing 'fine-slide'
;;
(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :fine-pitch-slide :line 5 :value 7)
                            (m-e :stop :line 6))
                           *test-tpds*)
              (let ((period1 (- (note->period 20)
                                7)))
                `((5 ,(period->note period1) #f)
                  (6 ,(period->note period1) #f))))

(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :fine-pitch-slide :line 6 :value 7)
                            (m-e :stop :line 8))
                           *test-tpds*)
              (let ((period1 (- (note->period 20)
                                7)))
                `((5 20 #f)
                  (6 ,(period->note period1) #f)
                  (8 ,(period->note period1) #f))))

(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :fine-pitch-slide :line 5 :value 7)
                            (m-e :fine-pitch-slide :line 7 :value 8)
                            (m-e :stop :line 8))
                           *test-tpds*)
              (let* ((period1 (- (note->period 20)
                                 7))
                     (period2 (- period1 8)))
                `((5 ,(period->note period1) #f)
                  (7 ,(period->note period2) #f)
                  (8 ,(period->note period2) #f))))

(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list 
                            (m-e :pitch-slide :line 5 :value 7)
                            (m-e :fine-pitch-slide :line 6 :value 8)
                            (m-e :stop :line 8))
                           *test-tpds*)
              (let* ((period1 (note->period 20))
                     (period2 (- (note->period 20)
                                 (* 4 7)))
                     (period3 (- period2 8)))
                (list (list 5          (period->note period1)  #t)
                      (list (-line 6)  (period->note period2)  #f)
                      (list 6          (period->note period3)  #f)
                      (list 8          (period->note period3)  #f))))


;; Testing 'arpeggio'
;;
(***assert*** (get-pitches (m-e :note :value 20 :line 5)
                           (list
                            (m-e :pitch-slide :line 5 :value 7)
                            (m-e :arpeggio :line 6 :value 3 :value2 8)
                            (m-e :stop :line 7))
                           *test-tpds*)
              (let* ((period1 (- (note->period 20)
                                 (* 7 4)))
                     (note1 (period->note period1))
                     (note2 (+ note1 3))
                     (note3 (+ note1 8)))
                
                ;;(c-display "hepp" note1 note2 note3)
                (list (list 5 20 #t)

                      (list (+ 6 0/6) note1 #f)

                      (list (+ 6 1/6) note2 #f)

                      (list (+ 6 2/6) note3 #f)

                      (list (+ 6 3/6) note1 #f)

                      (list (+ 6 4/6) note2 #f)

                      (list (+ 6 5/6) note3 #f)

                      (list (+ 6 6/6) note1 #f))))
              

;; Testing 'slide-to-note
;;
;; simple down 
(let* ((note1 20)
       (note2 18)
       (period1 (note->period note1))  ;; = 604
       (period2 (note->period note2))) ;; = 538,  604-538 = 66
  ;;(c-display "p12" period1 period2)
  (***assert*** (get-pitches (m-e :note :line 1 :value note1)
                             (list
                              (m-e :slide-to-note :line 1 :value 6 :value2 period2)
                              (m-e :stop :line 2))
                             (vector 12 12 12 12))
                (list (list 1 note1 #t)
                      (list 2 note2 #f))))

;; simple up
(let* ((note1 18)
       (note2 20)
       (period1 (note->period note1))
       (period2 (note->period note2)))
  (***assert*** (get-pitches (m-e :note :line 1 :value note1)
                             (list
                              (m-e :slide-to-note :line 1 :value 6 :value2 period2)
                              (m-e :stop :line 2))
                             (vector 12 12 12 12))
                (list (list 1 note1 #t)
                      (list 2 note2 #f))))

;; down
(let* ((note1 20)
       (note2 18)
       (period1 (note->period note1))  ;; = 604
       (period2 (note->period note2))) ;; = 538,  604-538 = 66
  ;;(c-display "p12" period1 period2)
  (***assert*** (get-pitches (m-e :note :line 1 :value note1)
                             (list
                              (m-e :slide-to-note :line 2 :value 3 :value2 period2)
                              (m-e :slide-to-note :line 3 :value 3 :value2 period2)
                              (m-e :stop :line 4))
                             (vector 12 12 12 12 12 12))
                (list (list 1 note1 #f)
                      (list 2 note1 #t)
                      (list 3 (period->note (+ period1 (* 3 11))) #t)
                      (list 4 note2 #f))))
;; up
(let* ((note1 18)
       (note2 20)
       (period1 (note->period note1))
       (period2 (note->period note2)))
  (***assert*** (get-pitches (m-e :note :line 1 :value note1)
                             (list
                              (m-e :slide-to-note :line 2 :value 3 :value2 period2)
                              (m-e :slide-to-note :line 3 :value 3 :value2 period2)
                              (m-e :stop :line 4))
                             (vector 12 12 12 12 12 12))
                (list (list 1 note1 #f)
                      (list 2 note1 #t)
                      (list 3 (period->note (- period1 (* 3 11))) #t)
                      (list 4 note2 #f))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          get-velocities           ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (velocity-glide-to-velocity-glide glide)
  (list (list (car glide)
              (cadr glide)
              #t)
        (list (caddr glide)
              (cadddr glide)
              #f)))

(define (get-velocities-0 note default-value curr-velocity events tpds)
  (let* ((event (car events))
         (linenum (event :linenum))
         (place (+ linenum (/ (event :tick) (tpds linenum))))
         (type (event :type)))
    (cond ((or (eq? type :stop)
               (eq? type :note))
           (list (list place
                       curr-velocity
                       #f)))
          
          ((eq? type :break)
           (assert (= place linenum))
           (list (list linenum
                       curr-velocity
                       #f)))

          ((eq? type :velocity)
           (let ((value (event :value)))
             (cons (list place
                         value
                         #f)
                   (get-velocities-0 note default-value value (cdr events) tpds))))

          ((eq? type :velocity-slide)
           (assert (= place linenum))
           (let* ((num-ticks-in-line (get-tpd tpds linenum))
                  (new-velocity (+ curr-velocity
                                   (* (event :value)
                                      (if (event :is-pattern-delay-line)
                                          num-ticks-in-line
                                          (1- num-ticks-in-line)))))
                  (glide (clamp-glide linenum      curr-velocity
                                      (1+ linenum) new-velocity
                                      0 64
                                      num-ticks-in-line)))
             (append (velocity-glide-to-velocity-glide glide)
                     (get-velocities-0 note default-value (last glide) (cdr events) tpds))))

          ((eq? type :fine-velocity-slide)
           (assert (= place linenum))
           (let ((new-velocity (between 0 (+ curr-velocity (event :value)) 64)))
             ;;(c-display "curr: " curr-velocity)
             ;;(c-display "new: " new-velocity (event :value))
             (cons  (list linenum
                          new-velocity
                          #f)
                    (get-velocities-0 note default-value new-velocity (cdr events) tpds))))
          
          (else
           (get-velocities-0 note
                             default-value
                             curr-velocity
                             (cdr events)
                             tpds)))))

(define (get-velocities note instrument-velocity channel-events tpds)
  (let* ((linenum (note :linenum))
         (place1 (+ linenum (/ (note :tick) (tpds linenum)))))
    (fix-gliding
     (cons (list place1 instrument-velocity #f)
           (get-velocities-0 note
                             instrument-velocity
                             instrument-velocity
                             channel-events
                             tpds)))))


;; test :fine-velocity-slide
;;

(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              32
                              (list 
                               (m-e :fine-velocity-slide :line 5 :value 64)
                               (m-e :stop :line 6))
                              *test-tpds*)
              '((5 64 #f)
                (6 64 #f)))

(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              64
                              (list 
                               (m-e :fine-velocity-slide :line 5 :value 5)
                               (m-e :stop :line 6))
                              *test-tpds*)
              '((5 64 #f)
                (6 64 #f)))

(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              1
                              (list 
                               (m-e :fine-velocity-slide :line 5 :value -5)
                               (m-e :stop :line 6))
                              *test-tpds*)
              '((5 0 #f)
                (6 0 #f)))


;; test :velocity
;;
(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              32
                              (list 
                               (m-e :velocity :line 5 :value 5)
                               (m-e :stop :line 6))
                              *test-tpds*)
              '((5 5 #f)
                (6 5 #f)))

(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              32
                              (list 
                               (m-e :velocity :line 6 :value 5)
                               (m-e :stop :line 7))
                              *test-tpds*)
              `((5 32 #f)
                (6 5 #f)
                (7 5 #f)))

;; test :velocity-slide
;;
(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              32
                              (list 
                               (m-e :velocity-slide :line 5 :value 5)
                               (m-e :stop :line 6))
                              (vector 5 5 5 5 5 5 5 5 5))
              '((5 32 #t)
                (6 52 #f)))
                
(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              32
                              (list 
                               (m-e :velocity-slide :line 6 :value -5)
                               (m-e :stop :line 8))
                              (vector 5 5 5 5 5 5 5 5 5 5))
              '((5 32 #f)
                (6 32 #t)
                (7 12 #f)
                (8 12 #f)))
                
(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              2
                              (list 
                               (m-e :velocity-slide :line 5 :value -8)
                               (m-e :stop :line 6))
                              (vector 2 2 2 2 2 2 2 2))
              `((5 2 #t)
                (,(+ 5 1/4) 0 #f)
                (6 0 #f)))
                

(***assert*** (get-velocities (m-e :note :value 20 :line 5)
                              62
                              (list 
                               (m-e :velocity-slide :line 5 :value 8)
                               (m-e :stop :line 6))
                              (vector 2 2 2 2 2 2 2 2))
              `((5 62 #t)
                (,(+ 5 1/4) 64 #f)
                (6 64 #f)))
                



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         split-channels            ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Make sure each channel only contains events belonging to one instrument
;; Returns a list of channels
(define (split-channel channel)
  (group-by (lambda (event)
              (event :instrumentnum))
            =
            channel))


(let* ((channel (list (m-e :note :instrumentnum 5 :line 3)
                      (m-e :note :instrumentnum 5 :line 4)
                      (m-e :note :instrumentnum 2 :line 5)
                      (m-e :note :instrumentnum 9 :line 7)
                      (m-e :note :instrumentnum 2 :line 8)))
       (channels (split-channel channel)))
  ;;(for-each print-events channels))
  (***assert*** channels (list (list (channel 0)
                                     (channel 1))
                               (list (channel 2)
                                     (channel 4))
                               (list (channel 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     * add :line value to :tick,   ;;;;;;
;;;;     * validate tick values        ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
(define (add-line-to-tick-value channel tpds)
  (let loop ((events channel))
    (if (null? events)
        '()
        (let* ((event (car events))
               (tick (event :tick))
               (line (event :line))
               (tpd (tpds line)))
          (if (>= tick tpd)
              (loop (cdr events)) ;; throw the event away (this might not be correct behavior for note cut, and possible other effects, since there has been a non-existing stop event during processing)
              (cons (<copy-event> event :tick (+ (/ tick (tpds line))
                                               line))
                    (loop cdr events)))))))

(***assert*** (add-line-to-tick-value (list (m-e :note :line 0 :tick 0)
                                            (m-e :note :line 1 :tick 2)
                                            (m-e :velocity :line 1 :tick 0)
                                            (m-e :note :line 2 :tick 1))
                                      (vector 2 2))
              (list (m-e :note :line 0 :tick 0)
                    (m-e :velocity :line 1 :tick 1)
                    (m-e :note :line 2 :tick (+ 2 1/2)))
              ||#


              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Get tempo events            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-tempo-events pattern-events)
  (keep (lambda (event)
          (let ((type (event :type)))
            (or (eq? :tpd type)
                (eq? :bpm type))))
        pattern-events))

              


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            Pattern                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-pattern channels tempos tpds)
  (assert (list? channels))
  (assert (list? tempos))
  (assert (vector? tpds))
  (vector channels tempos tpds))


(define (pattern-get-channels pattern)
  (pattern 0))

(define (pattern-get-tempos pattern)
  (pattern 1))

(define (pattern-get-tpds pattern)
  (pattern 2))

(define (pattern-get-num-lines pattern)
  (get-num-lines-from-tpds (pattern-get-tpds pattern)))

(define (pattern-get-num-channels pattern)
  (length (pattern-get-channels pattern)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         interface                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *events* '())

(define (start-adding-protracker-events!)
  (set! *events* '()))



(define *playlist* '())

(define (set-protracker-playlist! daslist)
  (set! *playlist* daslist))

(define *num-channels* 4)
(define *num-lines* 64)

(define (set-protracker-pattern-format num-channels num-lines)
  ;;(c-display "numc:" num-channels) (assert #f)
  (set! *num-channels* num-channels)
  (set! *num-lines* num-lines))


;; Each element in the *instrumentlist* vector is another vector with the following six elements:
;; 0. Sample name
;; 1. Sample filename
;; 2. Num samples
;; 3. Finetune (between -8 and 7, spanning between -0.25 and 0.25 notes)
;; 4. Sample default volume (0-64). Needed for default velocity of notes
;; 5. Sample loop start. Ignored, if loop length = 0
;; 6. Sample loop length. Ignored if 0.
;; 7. Radium instrument number (filled in when calling send-instruments-to-radium).
;;
(define *instrumentlist* (vector))

(define (set-protracker-instrumentlist! daslist)
  (set! *instrumentlist* daslist))

(define (instrument-sample-name instr)
  (<ra> :from-base64 (instr 0)))
(define (instrument-sample-filename instr)
  (define home-path (<ra> :get-home-path))
  (define samples-path (<ra> :append-file-paths
                             (<ra> :append-file-paths
                                   home-path                             
                                   (<ra> :get-path ".radium"))
                             (<ra> :get-path "mod_samples")))
  (define basename (<ra> :get-path (instr 1)))
  (define full-path (<ra> :append-file-paths
                          samples-path
                          basename))
  full-path)
(define (instrument-num-samples instr)
  (instr 2))
(define (instrument-finetune instr)
  (instr 3))
(define (instrument-volume instr)
  ;;(c-display "instr: " (to-displayable-string instr))
  (instr 4))
(define (instrument-loop-start instr)
  (instr 5))
(define (instrument-loop-length instr)
  (instr 6))
(define (instrument-radium-instrument-num instr)
  (instr 7))
(define (set-instrument-radium-instrument-num! instr num)
  (vector-set! instr 7 num))

#||
(instrument-radium-instrument-num (*instrumentlist* 9))

(print-events (keep (lambda (event)
                      (= (event :channel) 3))
                    (get-pattern (reverse *events*) 22)))

(print-events ((pattern-get-channels *temp*) 12))

(keep (lambda (event)
        (= (event :instrumentnum) 31))
      (reverse *events*))







(begin (length (*temp* 0)))

       



||#



;;(map instrument-volume (vector->list *instrumentlist*))
;;(instrument-volume (*instrumentlist* 9))


;; add events to *events*
(define (add-protracker-trackline patternnum channelnum linenum period instrumentnum effectnum effectvalue)
  
  (define (value1)
    (ash effectvalue -4))
  
  (define (value2)
    (logand effectvalue #xf))

  (define (push-volume-slide-event!)
    (let ((volumechange (- (value1) (value2))))
      (if (not (= 0 volumechange))
          (push! *events* (m-e :velocity-slide
                               :line linenum
                               :pattern patternnum
                               :channel channelnum
                               :value volumechange)))))


  (when (> instrumentnum 31)
    (c-display "import_mod.scm/error: instrument number outside legal range:" instrumentnum ". Can not be higher than 32")
    (set! instrumentnum 0))

  ;; add note event
  (when (or (> period 0)
            (> instrumentnum 0))

    (when (> period 0)
      (when (> period *max-period*)
        (c-display "import_mod.scm/error: note period outside legal range:" period ". Period can not be higher than" *max-period*)
        (set! period *max-period*))
      (when (< period *min-period*)
        (c-display "import_mod.scm/error: note period outside legal range:" period ". Period can not be lower than" *min-period*)
        (set! period *min-period*)))
    
    (push! *events* (m-e :note
                         :instrumentnum instrumentnum
                         :line linenum
                         :pattern patternnum
                         :channel channelnum
                         :value (if (= period 0)
                                    0
                                    (period->note period)))))

  
  ;; add effect/tempo/etc. events
  (cond ((and (= effectnum 0)
              (> effectvalue 0))
         (push! *events* (m-e :arpeggio
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value (value1)
                              :value2 (value2))))
        
        ((and (or (= effectnum 1)
                  (= effectnum 2))
              (> effectvalue 0)) ;; no portamento effect memory in protracker
         (push! *events* (m-e :pitch-slide
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value (if (= effectnum 1)
                                         effectvalue
                                         (- effectvalue)))))

        ((= effectnum 3)
         (push! *events* (m-e :slide-to-note
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value effectvalue)))

        ((= effectnum 4)
         (push! *events* (m-e :vibrato
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value (value1)
                              :value2 (value2))))

        ((= effectnum 5)
         (push! *events* (m-e :slide-to-note
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value 0))
         (push-volume-slide-event!))
        
        ((= effectnum 6)
         (push! *events* (m-e :vibrato
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value 0))
         (push-volume-slide-event!))
        
        ((= effectnum 7)
         (push! *events* (m-e :tremolo
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value (value1)
                              :value2 (value2))))

        ((= effectnum 8)
         ;;(assert #f)
          (c-display "import_mod.scm/error: 8xx Panning/etc is not supported (MOD)" effectvalue))
        
        ((= effectnum 9)
         (push! *events* (m-e :sample-offset
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value (+ (* 4096 (value1)) (* 256 (value2))))))


        ((= effectnum 10)
         (push-volume-slide-event!))

        ((= effectnum 11)
         (push! *events* (m-e :position-jump
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :instrumentnum -1
                              :value effectvalue
                              )))

        ((= effectnum 12)
         (push! *events* (m-e :velocity
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value (between 0 effectvalue 64)))) ;; Massacre by Esau has at least one volume with value 65. Wonder what it means.
         
        ((= effectnum 13)
         (push! *events* (m-e :break
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :instrumentnum -1
                              :value (+ (* (value1) 10) (value2)))))

        ((= effectnum 15)
         (let ((value (if (= effectvalue 0)
                          1
                          effectvalue)))
           (push! *events* (m-e (if (<= effectvalue 32) ;; some trackers use "<=", some "<". OpenMTP and protracker use "<" though.
                                    :tpd
                                    :bpm)
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :instrumentnum -1
                                :value value))))
        
        ((= effectnum 14)
         (let ((effectnum (value1))
               (effectvalue (value2)))
           (cond ((= effectnum 0)
                  (c-display "import_mod.scm/error: E0x Set filter On/Off is not supported (MOD)" effectvalue))
                 
                 ((or (= effectnum 1)
                      (= effectnum 2))
                  (push! *events* (m-e :fine-pitch-slide
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :value (if (= effectnum 1)
                                                  (- effectvalue)
                                                  effectvalue))))
                 
                 ((= effectnum 3)
                  (c-display "import_mod.scm/error: E3x Glissando control is not supported (MOD)" effectvalue ", p:" patternnum ", c:" channelnum ", line:" linenum))

                 ((= effectnum 4)
                  (c-display "import_mod.scm/error: E4x Vibrato control is not supported (MOD)" effectvalue))

                 ((= effectnum 5)
                  (push! *events* (m-e :finetune
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :value (if (< effectvalue 8)
                                                  effectvalue
                                                  (- effectvalue 16)))))

                 ((= effectnum 6)
                  (push! *events* (m-e :loop
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :instrumentnum -1
                                       :value effectvalue)))
                  
                 ((= effectnum 7)
                  (c-display "import_mod.scm/error: E5x Tremolo control is not supported (MOD)" effectvalue))

                 ((= effectnum 8)
                  (c-display "import_mod.scm/error: E8x Set panning position or karplus strong is not supported (MOD)" effectvalue))

                 ((= effectnum 9)
                  (push! *events* (m-e :retrigger-note
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :value effectvalue)))

                 ((and (or (= effectnum 10)
                           (= effectnum 11))
                       (> effectvalue 0))
                  (push! *events* (m-e :fine-velocity-slide
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :value (if (= effectnum 10)
                                                  effectvalue
                                                  (- effectvalue)))))

                 ((= effectnum 12) ;; note-cut
                  (push! *events* (m-e :velocity
                                       :tick effectvalue
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :value 0)))

                 ((and (= effectnum 13)
                       (> effectvalue 0))
                  (push! *events* (m-e :delay-note
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :value effectvalue)))

                 ((and (= effectnum 14)
                       (> effectvalue 0))
                  (push! *events* (m-e :pattern-delay
                                       :line linenum
                                       :pattern patternnum
                                       :channel channelnum
                                       :instrumentnum -1
                                       :value effectvalue)))

                 ((= effectnum 15)
                  (c-display "import_mod.scm/error: Efx Invert loop is not supported (MOD)" effectvalue)))))))

;; Returns an event that will stop the note. (i.e. not necessarily a :stop event)
(define (get-next-stop-event events)
  (find-first (append events (list (m-e :break :line 64))) ;; TODO: Add, etc.
              (lambda (event)
                (memq (event :type) '(:note :stop :break)))))

(get-next-stop-event (list (m-e :note :line 5)
                           (m-e :break :line 6)))

(define *pitch-transpose* 24)

(define (send-note-event-to-radium note channelnum events instrument tpds num-lines)

  (define (legal-pos pos)
    (min (-line num-lines)
         pos))
  
  ;;(c-display "instruments: " instruments)
  (let* ((pitches (get-pitches note events tpds))
         (velocities (get-velocities note (instrument-volume instrument) events tpds))
         
         (first-pitch (car pitches))
         (first-pitch-pos (car first-pitch))
         (first-pitch-value (cadr first-pitch))
         
         (last-pitch (last pitches))
         (last-pitch-pos (legal-pos (car last-pitch)))
         (last-pitch-value (cadr last-pitch))
         
         (first-velocity (car velocities))
         (first-velocity-pos (car first-velocity))
         (first-velocity-value (cadr first-velocity))
         
         (last-velocity (last velocities))
         (last-velocity-pos (legal-pos (car last-velocity)))
         (last-velocity-value (cadr last-velocity))
         
         (start-place first-velocity-pos) ;;(place-list first-velocity-pos))
         (stop-place last-velocity-pos)) ;;(place-list last-velocity-pos)))
    

    (when (null? (cdr velocities))
      (c-display "velocities" (event-to-string note) start-place stop-place velocities)
      (print-events events))
    
    (assert (= last-pitch-pos last-velocity-pos))
    (assert (= first-pitch-pos first-velocity-pos))

    (assert (not (null? (cdr velocities)))) ;; could fail if not all events after :break have been removed
    (assert (not (null? (cdr pitches))))
    
    ;;(c-display "note start: " (event->place-list note tpds))
    ;;(c-display "vel start:  " (place-list first-velocity-pos))
    (assert (place-list-equal? (event->place-list note tpds) (place-list first-velocity-pos)))

    ;;(c-display "pitches" (event-to-string note) start-place stop-place pitches)

    '(when (and (= (note :patternnum) 4)
               (= (note :channel) 3)
               (= (note :linenum) 44))
          (c-display "gakk")
          (print-events events))

    
    (if (or (not (validate-gliding2 pitches))
            (not (validate-gliding2 velocities)))
        (begin
          (c-display "FAILED:"
                     (validate-gliding2 pitches)
                     (validate-gliding2 velocities))
          (c-display "pitches: " pitches)
          (c-display "velocities: " velocities)
          (c-display note)
          (print-events events)
          (assert-non-release #f)
          )
        (let ((radium-notenum (<ra> :add-note (+ *pitch-transpose* first-pitch-value)
                                          (/ first-velocity-value 64)
                                          start-place
                                          stop-place
                                          channelnum -1 -1)))
          (when (not (= -1 radium-notenum))
            
            ;; set end velocity value
            (<ra> :set-velocity (/ last-velocity-value 64)
                  'same-place
                  1
                  radium-notenum
                  channelnum)
            
            ;; add velocity nodes
            (for-each (lambda (velocity)
                        ;;(c-display velocities)
                        (when (>= (car velocity) last-velocity-pos)
                          (c-display "note" (event-to-string note))
                          (c-display "channelnum" channelnum)
                          (c-display "instrument" instrument)
                          (c-display "pitches" pitches)
                          (c-display "velocities" velocities)
                          (c-display "num-lines" num-lines)
                          (print-events events)
                          (assert #f))
                        (<ra> :add-velocity
                              (/ (cadr velocity) 64) ;; value
                              (car velocity) ;; place
                              radium-notenum
                              channelnum))
                      (butlast (cdr velocities)))
            
            ;; set velocity logtypes
            (for-each (lambda (velocity velocitynum)
                        (<ra> :set-velocity-logtype (if (caddr velocity) *logtype-linear* *logtype-hold*) velocitynum  radium-notenum channelnum))
                      (butlast velocities)
                      (iota (1- (length velocities))))
            
            ;; whether to add pitches or not
            (when (or (not (= first-pitch-value last-pitch-value))
                      (> (length pitches) 2))
              
              (<ra> :set-note-end-pitch
                    (+ *pitch-transpose* last-pitch-value)
                    radium-notenum
                    channelnum)
              
              (for-each (lambda (pitch)  ;; TODO: Fix.
                          (assert (< (car pitch) last-pitch-pos))
                          (<ra> :add-pitchnum
                                (+ *pitch-transpose* (cadr pitch))  ;; value
                                (car pitch) ;; place
                                ;; ra:add-pitchnum doesn't need a 'radium-notenum' argument. It finds note automatically, which works fine as long as the track is monophonic.
                                channelnum))
                        (butlast (cdr pitches)))
              
              ;; set pitch logtypes
              (for-each (lambda (pitch pianonotenum)
                          (<ra> :set-pianonote-logtype (if (caddr pitch) *logtype-linear* *logtype-hold*) pianonotenum  radium-notenum channelnum))
                        (butlast pitches)
                        (iota (1- (length pitches))))
              
              ))))))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; helper function for effects ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Must send events of the right type. E.g it doesn't filter on (event :type)
(define (get-effect-glidings events tpds reset-value get-value)
  (fix-gliding
   (let loop ((events events))
     (if (null? events)
         '()
         (let ((event (car events)))
           (let* ((linenum (event :linenum))
                  (place (+ linenum (/ (event :tick) (tpds linenum)))) ;; Can tick be anything other than 0?
                  (value (get-value event)))
             
             (append (list (list place value #f)
                           (list (1+ linenum) reset-value #f))
                     (loop (cdr events)))))))))
                    

(define (send-fx-to-radium glidings fx-name tracknum num-lines)
  (define (legal-pos pos)
    (min (-line num-lines)
         pos))

  (when (not (validate-gliding2 glidings))        
    (c-display "FAILED " fx-name "tracknum")
    (c-display glidings)
    (assert #f))

  (define first-gliding (car glidings))
  (define first-pos (car first-gliding))
  (define first-value (cadr first-gliding))

  (c-display "   __FX-NUM for" fx-name ", tracknum:" tracknum ", line:" first-pos ", value1:" first-value)
  
  (assert (<= first-value 1))
  (assert (>= first-value 0))

  (define fx-num (<ra> :add-fx first-value first-pos fx-name tracknum))
  
  (<ra> :set-fxnode-logtype *logtype-hold* 0 fx-num tracknum)

  (define second-gliding (cadr glidings))  
  (define second-pos (legal-pos (car second-gliding)))
  (define second-value (cadr second-gliding))

  (assert (<= second-value 1))
  (assert (>= second-value 0))

  (<ra> :set-fxnode 1 second-value second-pos fx-num tracknum)
  (<ra> :set-fxnode-logtype *logtype-hold* 1 fx-num tracknum)

  (if (and (= tracknum 1)
           (> first-value 1))
      (c-display glidings))
  
  ;;(c-display "   FX-NUM for" fx-name ":" fx-num ", tracknum:" tracknum ", line:" first-pos ", value1:" first-value ", line2:" second-pos ", value2:" second-value)
  
  (for-each (lambda (gliding)
              (let ((place (legal-pos (car gliding)))
                    (value (cadr gliding)))
                (assert (not (caddr gliding))) ;; Always HOLD glide mode. Can't do it any other way in protracker. (volume and pitch can glide linearly, but not vibrato/tremolo/finetune/startoffset)
                (assert (<= value 1))
                (assert (>= value 0))
                (define fxnode-num (<ra> :add-fxnode value place fx-num tracknum))
                (<ra> :set-fxnode-logtype *logtype-hold* fxnode-num fx-num tracknum)
                )
              )
            (cddr glidings)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; vibrato ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; amplitude y/16 semitones
(define (get-vibrato-depths vibrato-events tpds)
  (get-effect-glidings vibrato-events tpds 0 (lambda (event)
                                               (scale (event :value2)
                                                      0 16
                                                      0 2/5)))) ;; Vibrato range in protracker is between -1 and 1 semitones, while in radium it is between -2.5 and 2.5 semitones


(for-each c-display
 (get-vibrato-depths (list (m-e :vibrato :line 5 :value 10 :value2 5)
                           (m-e :vibrato :line 6 :value 10 :value2 9)
                           (m-e :vibrato :line 8 :value 11 :value2 8)
                           (m-e :vibrato :line 9 :value 12 :value2 2))
                     (make-vector 10 6)))

;;(assert #f)


#||
(***assert*** (get-vibrato-depths (list (m-e :vibrato :line 5 :value 10 :value2 20)
                                        (m-e :vibrato :line 6 :value 10 :value2 21)
                                        (m-e :vibrato :line 8 :value 11 :value2 22)
                                        (m-e :break :line 9))
                                  (make-vector 10 6))
              (list `((5 10)
                      (,(-line 7) 10)
                      
                      (7 0)
                      (,(-line 8) 0)
                      
                      (8 11)
                      (,(-line (-line 9)) 11)
                      
                      (,(-line 9) 0))))
||#

;; protracker: "(x * ticks)/64 cycles occur in the division"
;; radium: 0 -> 1 means 0 = 0 hz, 1 = 20 Hz.
;;
;; ld = line duration in seconds
;; hz = vibrato speed
;; nc = number of cycles
;;  5 nc = 5 hz, if ld = 1
;; 10 nc = 10 hz, if ld = 1
;; 10 nc = 5 hz, if ld = 2
;; -----------------
;; nc    = hz * ld
;; -----------------
;; hz    = nc / ld
;; hz    = number of cycles / line duration
;; -------------------
;; number of cycles = (/ (* (event :value) tpd) 64)
;; -------------------
;; vibrato speed = scale(hz 0 20 0 1) ;; <-- Vibrato speed range in radium goes between 0 and 20 Hz
;;
(define (get-vibrato-speeds vibrato-events tracknum tpds)  
  (get-effect-glidings vibrato-events tpds 0 (lambda (event)
                                               (define linenum (event :linenum))
                                               (define tpd (tpds linenum))
                                               (define number-of-cycles-per-line (/ (* (event :value)
                                                                                       (if (event :is-pattern-delay-line)
                                                                                           tpd
                                                                                           (1- tpd))) ;; It's crazy, but the doc (and protracker 2.3d clone implementation) says tpd-1
                                                                                    64))
                                               (define hz (/ number-of-cycles-per-line
                                                             (<ra> :line-duration linenum tracknum)))
                                               (scale hz
                                                      0 20
                                                      0 1))))

#||
(***assert*** (get-radium-vibrato-speed-events (list (m-e :vibrato :tick 5 :value 10 :value2 20)
                                                     (m-e :vibrato :tick 6 :value 10 :value2 21)
                                                     (m-e :vibrato :tick 8 :value 11 :value2 22)))
              (list `((5 20)
                      (,(-line 6) 20)
                      
                      (6 21)
                      (,(-line 7) 21)
                      
                      (7 0)
                      (,(-line 8) 0)
                      
                      (8 12)
                      (,(-line 9) 12)
                      (9 0))))
||#

#||
(define id_depth (<ra> :add-fx 0.1 1 "Vibrato Depth" 0))
(<ra> :set-fxnode 1
               0.3
               4
               id_depth
               0)


(define id_speed (<ra> :add-fx 0.5 5 "Vibrato Speed" 0))
(<ra> :set-fxnode 1
               0.8
               9
               id_speed
               0)

(define id_speed (<ra> :add-fx 0.5 53 "Vibrato Speed" 0))
(<ra> :set-fxnode 1
               0
               54
               id_speed
               0)
||#



(define (send-vibrato-events-to-radium tracknum events instrument tpds)
  (define num-lines (get-num-lines-from-tpds tpds))
  (let* ((vibrato-events (keep (lambda (event)
                                 (eq? (event :type) :vibrato))
                               events))
         (depths (get-vibrato-depths vibrato-events tpds))
         (speeds (get-vibrato-speeds vibrato-events tracknum tpds)))

    ;;(assert (= (length depths) (length speeds))) ;; <-- Might not be true if some of the gliding points have been removed because they were unnecessary
    
    (when (not (null? vibrato-events))
        (assert (>= (length depths) 2))
        (assert (>= (length speeds) 2))

        (send-fx-to-radium depths "Vibrato Depth" tracknum num-lines)
        (send-fx-to-radium speeds "Vibrato Speed" tracknum num-lines))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; tremolo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Doc says: "waveform with amplitude y*(ticks - 1)", which I don't understand.
;; I interpret it as meaning that the amplitude goes between 1 and 1-(y/16).
(define (get-tremolo-depths tremolo-events tpds)
  (get-effect-glidings tremolo-events tpds 0 (lambda (event)
                                               (scale (event :value2)
                                                      0 16
                                                      0 1))))

(for-each c-display
 (get-tremolo-depths (list (m-e :tremolo :line 5 :value 10 :value2 5)
                           (m-e :tremolo :line 6 :value 10 :value2 9)
                           (m-e :tremolo :line 8 :value 11 :value2 8)
                           (m-e :tremolo :line 9 :value 12 :value2 2))
                     (make-vector 10 6)))

;;(assert #f)


#||
(***assert*** (get-tremolo-depths (list (m-e :tremolo :line 5 :value 10 :value2 20)
                                        (m-e :tremolo :line 6 :value 10 :value2 21)
                                        (m-e :tremolo :line 8 :value 11 :value2 22)
                                        (m-e :break :line 9))
                                  (make-vector 10 6))
              (list `((5 10)
                      (,(-line 7) 10)
                      
                      (7 0)
                      (,(-line 8) 0)
                      
                      (8 11)
                      (,(-line (-line 9)) 11)
                      
                      (,(-line 9) 0))))
||#

;; protracker: "(x * ticks)/64 cycles occur in the division"
;; radium: 0 -> 1 means 0 = 0 hz, 1 = 20 Hz.
;;
;; ld = line duration in seconds
;; hz = tremolo speed
;; nc = number of cycles
;;  5 nc = 5 hz, if ld = 1
;; 10 nc = 10 hz, if ld = 1
;; 10 nc = 5 hz, if ld = 2
;; -----------------
;; nc    = hz * ld
;; -----------------
;; hz    = nc / ld
;; hz    = number of cycles / line duration
;; -------------------
;; number of cycles = (/ (* (event :value) tpd) 64)
;; -------------------
;; tremolo speed = scale(hz 0 20 0 1) ;; <-- Tremolo speed range in radium goes between 0 and 50 Hz
;;
(define (get-tremolo-speeds tremolo-events tracknum tpds)  
  (get-effect-glidings tremolo-events tpds 0 (lambda (event)
                                               (define linenum (event :linenum))
                                               (define tpd (tpds linenum))
                                               (define number-of-cycles-per-line (/ (* (event :value)
                                                                                       (if (event :is-pattern-delay-line)
                                                                                           tpd
                                                                                           (1- tpd))) ;; It's crazy, but the doc says tpd-1
                                                                                    64))
                                               (define hz (/ number-of-cycles-per-line
                                                             (<ra> :line-duration linenum tracknum)))
                                               (scale hz
                                                      0 50
                                                      0 1))))

#||
(***assert*** (get-radium-tremolo-speed-events (list (m-e :tremolo :tick 5 :value 10 :value2 20)
                                                     (m-e :tremolo :tick 6 :value 10 :value2 21)
                                                     (m-e :tremolo :tick 8 :value 11 :value2 22)))
              (list `((5 20)
                      (,(-line 6) 20)
                      
                      (6 21)
                      (,(-line 7) 21)
                      
                      (7 0)
                      (,(-line 8) 0)
                      
                      (8 12)
                      (,(-line 9) 12)
                      (9 0))))
||#

#||
(define id_depth (<ra> :add-fx 0.1 1 "Tremolo Depth" 0))
(<ra> :set-fxnode 1
               0.3
               4
               id_depth
               0)


(define id_speed (<ra> :add-fx 0.5 5 "Tremolo Speed" 0))
(<ra> :set-fxnode 1
               0.8
               9
               id_speed
               0)

(define id_speed (<ra> :add-fx 0.5 53 "Tremolo Speed" 0))
(<ra> :set-fxnode 1
               0
               54
               id_speed
               0)
||#



(define (send-tremolo-events-to-radium tracknum events instrument tpds)
  (define num-lines (get-num-lines-from-tpds tpds))
  (let* ((tremolo-events (keep (lambda (event)
                                 (eq? (event :type) :tremolo))
                               events))
         (depths (get-tremolo-depths tremolo-events tpds))
         (speeds (get-tremolo-speeds tremolo-events tracknum tpds)))

    ;;(assert (= (length depths) (length speeds))) <-- Might not be true if some of the gliding points have been removed because they were unnecessary
    
    (when (not (null? tremolo-events))
        (assert (>= (length depths) 2))
        (assert (>= (length speeds) 2))

        (send-fx-to-radium depths "Tremolo Depth" tracknum num-lines)
        (send-fx-to-radium speeds "Tremolo Speed" tracknum num-lines))))



;;(assert #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; sampleoffset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-sampleoffsets sampleoffset-events instrument tpds)
  (define num-samples-in-instrument (instrument-num-samples instrument))
  (get-effect-glidings sampleoffset-events tpds 0 (lambda (event)
                                                    (scale (min (event :value) num-samples-in-instrument)
                                                           0 num-samples-in-instrument
                                                           0 1))))


#||
(define id_depth (<ra> :add-fx 0.1 1 "Sampleoffset Depth" 0))
(<ra> :set-fxnode 1
               0.3
               4
               id_depth
               0)


(define id_speed (<ra> :add-fx 0.5 5 "Sampleoffset Speed" 0))
(<ra> :set-fxnode 1
               0.8
               9
               id_speed
               0)

(define id_speed (<ra> :add-fx 0.5 53 "Sampleoffset Speed" 0))
(<ra> :set-fxnode 1
               0
               54
               id_speed
               0)
||#



(define (send-sampleoffset-events-to-radium tracknum events instrument tpds)
  (define num-lines (get-num-lines-from-tpds tpds))
  (let* ((sampleoffset-events (keep (lambda (event)
                                 (eq? (event :type) :sample-offset))
                               events))
         (sample-offsets (get-sampleoffsets sampleoffset-events instrument tpds)))

    ;;(c-display "instrument: " instrument)
    ;;(c-display "num-samples: " (instrument-num-samples instrument))
    ;;(c-display "values: " (map (lambda (event) (event :value)) sampleoffset-events))
    (if (not (null? sampleoffset-events))
        (send-fx-to-radium sample-offsets "Start Position" tracknum num-lines))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; finetune ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Radium finetune effect goes between 0 and 1, where 0 = -100 cents, and 1 = +100 cents.
(define (protracker-finetune-to-radium-finetune protracker-finetune)
  (if (< 0 protracker-finetune)
      (scale protracker-finetune
             -8 0
             0  0.5)
      (scale protracker-finetune
             0 8
             0.5 1)))

(define (get-finetunes finetune-events tpds default-finetune)  
  (get-effect-glidings finetune-events tpds default-finetune (lambda (event)
                                                               (protracker-finetune-to-radium-finetune (event :value)))))


(define (send-finetune-events-to-radium tracknum events instrument tpds)
  (define num-lines (get-num-lines-from-tpds tpds))
  (define default-finetune (protracker-finetune-to-radium-finetune (instrument-finetune instrument)))
  (let* ((finetune-events (keep (lambda (event)
                                 (eq? (event :type) :finetune))
                                events))
         (finetunes (get-finetunes finetune-events tpds default-finetune)))
         
    (when (not (null? finetunes))
        (assert (>= (length finetunes) 2))

        (send-fx-to-radium finetunes "Finetune" tracknum num-lines))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Tempos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-radium-tempos tempo-events)
  (let loop ((events tempo-events)
             (result '())
             (last-bpm 125)
             (last-tpd 6)
             (last-linenum -1))
    ;;(c-display "events::: " events)
    (if (null? events)
        (reverse result)
        (let* ((event (car events))
               (type (event :type))
               (linenum (event :linenum))
               (same-line? (= linenum last-linenum))
               (bpm (if (eq? type :bpm)
                        (event :value)
                        last-bpm))
               (tpd (if (eq? type :tpd)
                        (event :value)
                        last-tpd))
               (radium-bpm (round (/ (* 6 bpm)
                                     tpd)))
               (lpb (if (> radium-bpm 999)
                        8
                        4)))
          ;;(c-display "got here" linenum bpm tpd )
          (loop (cdr events)
                (cons (list linenum lpb radium-bpm) (if same-line? (cdr result) result))
                bpm
                tpd
                linenum)))))
                      
(***assert*** (get-radium-tempos (list (m-e :tpd :line 0 :value 1)
                                       (m-e :bpm :line 0 :value 400)
                                       (m-e :tpd :line 2 :value 9)
                                       (m-e :bpm :line 3 :value 80)))
              `((0 8 ,(round (/ (* 6 400) 1)))
                (2 4 ,(round (/ (* 6 400) 9)))
                (3 4 ,(round (/ (* 6 80) 9)))))

(define (send-tempos-to-radium tempo-events patternnum num-patterns)
  (define tempos (get-radium-tempos tempo-events))
  (define num-tempos (length tempos))
  (let loop ((tempos tempos)
             (last-lpb 4)
             (last-bpm 125)
             (bpms '())
             (i 0))

    ;;(<ra> :show-progress-window-message-if-open (<-> "Sending tempo " i "/" num-tempos " for pattern " patternnum "/" (1- num-patterns) " to Radium") #f)
  
    ;;(c-display "looping" last-lpb last-bpm tempos)
    ;;(c-display "hmm1" (null? tempos))
    ;;(c-display "hmm2" (not (null? tempos)))
    (if (null? tempos)
        (<ra> :set-bpms bpms)
        (let* ((tempo (car tempos))
               ;;(l1 (c-display "p1"))
               (line (car tempo))
               ;;(l2 (c-display "p2"))
               (lpb (cadr tempo))
               ;;(l1 (c-display "p3"))
               (bpm (caddr tempo)))
          ;;(c-display "l1")
          (when (not (= lpb last-lpb))
            ;;(c-display line ": LPB" lpb)
            (<ra> :add-lpb lpb line))
          ;;(c-display "l2")
          ;;(c-display "l3")
          (loop (cdr tempos)
                lpb
                bpm
                (if (= bpm last-bpm)
                    bpms
                    (cons (hash-table :bpm (if (= lpb 8)
                                               (round (/ bpm 2))
                                               bpm)
                                      :place line)
                          bpms))
                (+ i 1)                
                )
          ))))

#||
(send-tempos-to-radium (list (list (m-e :tpd :line 0 :value 6)
                                   (m-e :bpm :line 0 :value 125))
                             (list (m-e :tpd :line 1 :value 1))
                             (list (m-e :bpm :line 1 :value 400))
                             (list (m-e :tpd :line 2 :value 9)
                                   (m-e :bpm :line 3 :value 80))))
||#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Sending events to Radium ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define *temp* '())

(define (send-pattern-to-radium pattern patternnum num-patterns instruments)
  (define num-lines (pattern-get-num-lines pattern))
  (define tpds (pattern-get-tpds pattern))
  
  (<ra> :set-num-lines num-lines)
  
  (if (= 0 (pattern-get-num-channels pattern))
      (<ra> :set-num-tracks 1)
      (<ra> :set-num-tracks (pattern-get-num-channels pattern)))

  (<ra> :minimize-block-tracks)

  ;; Important that we do this first since we need to ask radium later for line duration.
  (send-tempos-to-radium (pattern-get-tempos pattern) patternnum num-patterns)

  (define num-tracks (pattern-get-num-channels pattern))
  
  (for-each (lambda (channel tracknum)
              (<ra> :show-progress-window-message-if-open (<-> "Sending pattern " patternnum "/" (1- num-patterns) " (track #" tracknum ") to Radium") #f)
              (let ((first-instrument-event (find-first channel
                                                        (lambda (event)
                                                          (> (event :instrumentnum) 0)))))
                (if first-instrument-event
                    (let* ((instrument-num (first-instrument-event :instrumentnum))
                           (instrument (instruments (1- instrument-num)))
                           (radium-instrument-num (instrument-radium-instrument-num instrument)))

                      ;;(if (= (first-instrument-event :patternnum) 22)
                      ;;    (set! *temp* pattern))
                      
                      (when radium-instrument-num
                         (<ra> :set-instrument-for-track radium-instrument-num tracknum)
                         (send-vibrato-events-to-radium tracknum channel instrument tpds)
                         (send-tremolo-events-to-radium tracknum channel instrument tpds)
                         (send-sampleoffset-events-to-radium tracknum channel instrument tpds)
                         (send-finetune-events-to-radium tracknum channel instrument tpds))

                      (<ra> :set-track-pan (get-pan-value (first-instrument-event :channel)) tracknum)
                      (<ra> :set-track-pan-on-off #t tracknum)
                                                     
                      (<ra> :set-track-volume-on-off #f tracknum)
                                                     
                      (let loop ((events channel))
                        (if (not (null? events))
                            (let ((event (car events)))
                              (if (eq? (event :type) :note)
                                  (send-note-event-to-radium event tracknum (cdr events) instrument tpds num-lines))
                              (loop (cdr events)))))))))

            (pattern-get-channels pattern)
            (iota num-tracks)))

(define (clear-radium-editor)
  (while (> (<ra> :get-num-blocks) 1)
     (<ra> :delete-block))
  (<ra> :append-block)
  (<ra> :select-prev-block)
  (<ra> :delete-block))

(define (send-playlist-to-radium playlist)
  ;;(<ra> :set-playlist-length (length playlist))
  (let loop ((seqblocknum 0)
             (time 0)
             (playlist playlist))
    (when (not (null? playlist))
      (define actual-seqblocknum (<ra> :create-seqblock 0 (car playlist) time))
      (assert (= actual-seqblocknum seqblocknum))
      ;;(<ra> :set-playlist-block seqblocknum (car playlist))
      (loop (1+ seqblocknum)
            (<ra> :get-seqblock-end-time actual-seqblocknum 0)
            (cdr playlist))))
  (<ra> :set-sequencer-visible-start-time 0)
  (<ra> :set-sequencer-visible-end-time (<ra> :get-sequencer-song-length-in-frames)))

(define (send-events-to-radium playlist instruments patterns)
  (clear-radium-editor)
  
  (<ra> :set-main-lpb 4)
  (<ra> :set-main-bpm 125) ;; Default mod value

  (define num-patterns (length patterns))
  
  (for-each (lambda (pattern patternnum)
              (<ra> :show-progress-window-message-if-open (<-> "Sending pattern " patternnum "/" (1- num-patterns) " to Radium") #f)
              (send-pattern-to-radium pattern patternnum num-patterns instruments)
              (if (< patternnum (1- num-patterns))
                  (<ra> :append-block)))
            patterns
            (iota num-patterns))
  
  (<ra> :show-progress-window-message-if-open "Sending playlist to Radium")
  (send-playlist-to-radium playlist)

  (for-each (lambda (n)
              (<ra> :select-prev-block))
            (iota 200))
  )

(define (send-instruments-to-radium instruments)
  (for-each (lambda (instrument)
              (when (> (instrument-num-samples instrument) 0)
                (define radium-instrument-num (<ra> :create-audio-instrument "Sample Player" "Sample Player" (instrument-sample-name instrument) 0 0 #f))
                (<ra> :autoposition-instrument radium-instrument-num)
                (<ra> :connect-audio-instrument-to-main-pipe radium-instrument-num)
                
                (set-instrument-radium-instrument-num! instrument radium-instrument-num)
                
                (<ra> :set-instrument-sample radium-instrument-num (instrument-sample-filename instrument))
                
                (let ((loop-length (instrument-loop-length instrument)))
                  (if (> loop-length 0)
                      (<ra> :set-instrument-loop-data radium-instrument-num
                            (instrument-loop-start instrument)
                            loop-length)))
                (let ((finetune (instrument-finetune instrument)))
                  (if (not (= 0 finetune))
                      (<ra> :set-instrument-effect radium-instrument-num "Finetune" (protracker-finetune-to-radium-finetune finetune))))
                
                ;; We don't hear clicks when modules are played in protracker, for some reason.
                ;; But in Radium the clicks are quite noticable.
                (<ra> :set-instrument-effect radium-instrument-num "Attack" (scale 1 0 1000 0 1.0)) ;; TODO: Implement set-normalized-instrument-effect
                (<ra> :set-instrument-effect radium-instrument-num "Release" (scale 1 0 2000 0 1.0))
                ))
            
            (vector->list instruments)))
              

(define (process-events playlist instruments events max-num-lines num-channels)

  (define step 0)

  (define (print-progress-message message)
    (define message (<-> step ". PREPARING MOD. " message))
    (c-display message)
    (<ra> :show-progress-window-message-if-open message))
    
  (define (set-playlist-and-events! message new-playlist-and-events)
    (print-progress-message (<-> message " finished."))
    (set! step (1+ step))
    (set! playlist (car new-playlist-and-events))
    (set! events (cadr new-playlist-and-events)))
  
  (define (set-events! message new-events)
    (set-playlist-and-events! message (list playlist new-events)))
  
  (define (add-instruments-and-events! message new-instruments-and-events)    
    (define cloned-instruments (map (lambda (cloned-instrument)
                                      (define new-instrument-num (1- (cloned-instrument :old-instrumentnum)))
                                      (assert-non-release (>= new-instrument-num 0))
                                      (let ((instrument (instruments (max 0 new-instrument-num))))
                                        (vector-copy instrument)))
                                    (car new-instruments-and-events)))
    (c-display "   cloned-instruments:" cloned-instruments)
    (set! instruments (list->vector (append (vector->list instruments)
                                            cloned-instruments)))
    ;;(c-display "instr after: " instruments)
    (set-events! message (cadr new-instruments-and-events)))
  
  (c-display "MAX:" max)
  (c-display "PLAYLIST:" playlist)
  (define num-original-patterns (1+ (apply max playlist)))

  ;;(set-events! "Fix break events"
  ;;             (fix-break-events events))

  ;; :break is used to find the number of lines in a pattern, so we need to do this
  ;; for patterns that are not in the playlist (since breaks are also added in 'run-through-patterns' for patterns that are in the playlist) and does not have a :break.
  (set-events! "Adding break events to all patterns"
               (add-break-events events max-num-lines num-original-patterns))

  ;;(set-playlist-and-events! "Simplify break events"
  ;;                          (simplify-break-events playlist events))

  ;;(set-events! "Replace c00 with stops"
  ;;             (replace-c00-with-stops events))

  ;;(c-display "playlist" playlist)
  ;;(print-events (get-pattern events 0))
  ;;(assert #f)

  ;; Must run this one before 'merge-patterns' so that all patterns have :break events.
  ;; We also run this one before 'run-through-patterns' since there are fewer patterns at this point (just an optimization).
  (set-events! "Run through patterns not in playlist"
               (run-through-patterns-not-in-playlist playlist events max-num-lines num-channels instruments))
  ;;(assert #f)
  
  (set-playlist-and-events! "Run through patterns in playlist"
                            (run-through-patterns playlist events max-num-lines num-channels instruments))

  ;;(c-display "playlist" playlist)
  ;;(print-events (get-pattern events 0))
  ;;(assert #f)

  ;;(print-events (get-pattern events 21))

  ;; We are expanding loops inside run-through-patterns instead. (impossible to do correctly afterwards unfortunately)
  ;(set-events! "Expand loops"
  ;             (expand-loops events))


  ;; Need to run this one after run-through-patterns since breaks are handled in run-through-patterns (at least that's the plan)
  (set-playlist-and-events! "Merge patterns"
                            (merge-patterns playlist events num-channels)) ;; merge-patterns adds :stop events and merges patterns if there are hanging notes from the previously played pattern.

  (set-playlist-and-events! "Remove temporarily created patterns"
                            (remove-unused-patterns playlist events num-original-patterns))

  ;;(set-events! "Convert c00 into STP"
  ;;             (replace-c00-with-stops events))

  (set-playlist-and-events! "Split long patterns"
                            (split-long-patterns playlist events num-channels))

  
  ;;(print-events (get-pattern events 68))
  ;;  (assert #f)
  
  (set-events! "Expand sample offset events"
               (expand-sample-offsets events))

  ;; Must run this one after 'run-through-patterns' since we can not be sure that all patterns
  ;; in the original playlist are played. (It's possible to jump over patterns in the playlist using :position-jump)
  ;;(set-events! "Run through patterns not in playlist (again)"
  ;;             (run-through-patterns-not-in-playlist playlist events max-num-lines num-channels instruments))

  
  #||
  (set-events! "Prepare slide-to-note"
               (prepare-slide-to-note events num-channels))

  (c-display "playlist-before" playlist)
  (set-playlist-and-events! "Merge patterns"
                            (merge-patterns playlist events num-channels))
  (c-display "playlist-after" playlist)
  ;;(assert #f)
  
  (set-playlist-and-events! "Create new patterns based on tempo"
                            (create-new-patterns-based-on-tempo playlist events))
  ||#

  (set-events! "Remove events after breaks"
               (remove-events-after-breaks events))

  (set-playlist-and-events! "Legalize pattern lengths"
                            (legalize-pattern-lengths playlist events))

  (set-events! "Remove note delay effects"
               (remove-note-delay-effects events))

  ;;(c-display "num-events1: " (length events) (events 5) (events 6) (events 7))
  ;;(print-events events)

  ;; same here.
  ;;(set-events! "Assign instrument numbers"
  ;;             (assign-instrument-numbers events))


  ;;(c-display "num-events2: " (length events) (events 5) (events 6) (events 7))

  (c-display "here")
  
  ;; works on events:
  (add-instruments-and-events! "Clone instruments"
                               (get-cloned-instruments events instruments))

  (c-display "here2")
  ;;(print-events events)
  
  (print-progress-message (<-> "Sending instruments to Radium"))
  (send-instruments-to-radium instruments)


  (define patterns
    (map (lambda (pattern-events)

           ;;(c-display "Remove pattern delay effects")
           (set! pattern-events (remove-pattern-delay-effects pattern-events))  ;; Beware that 'pattern-events-to-channels' returns events which might not be ordered by channel anymore.

           ;;(set! pattern-events (remove-everything-after-break pattern-events)) ;; Need to do this for the patterns that are not in the playlist (those are not run through 'run-through-pattern')
           
           (define  break-event (find-first pattern-events
                                            (lambda (event)
                                              (eq? (event :type) :break))))
           ;;(print-events pattern-events)
           ;;(c-display "break-event: " break-event)
           (print-progress-message (<-> "Processing pattern " (break-event :patternnum)))
           
           (define tpds (create-tpds pattern-events))

           (define tempos (get-tempo-events pattern-events))
           
           (define channels (pattern-events-to-channels pattern-events break-event num-channels))
           
           (set! channels (map add-stop-events channels))
           
           (set! channels (apply append (map split-channel channels)))

           ;; Remove all channels not containing musical data. (i.e empty channels or channels only containing events with instrumentnum = -1
           (set! channels (keep (lambda (channel)
                                  (true-for-at-least-one? (lambda (event)
                                                            (not (= (event :instrumentnum) -1)))
                                                          channel))
                                channels))

           ;; add break event to every channel.
           (set! channels (map (lambda (channel)
                                 (append channel
                                         (list break-event)))
                               channels))

           (set! channels (map (lambda (channel)
                  
                                  ;; works on channels:
                                  ;;(c-display "Remove note retrigger effects")
                                  (set! channel (remove-note-retrigger-effects channel tpds))
                                  
                                  ;; works on channels:
                                  ;;(c-display "Remove effect memory")
                                  ;;(set! channel (remove-effect-memory channel))
                                  
                                  ;; The functions which sends data to Radium expects :tick to contain both both the value of :line and :tick
                                  ;;(set! channel (add-line-to-tick-value channel tpds))
                                  
                                  channel)
                               channels))

           (make-pattern channels
                         tempos
                         tpds))
         
         (events-to-patterns events)))

  (list playlist
        instruments
        patterns)
  
  )

#||
(send-protracker-events-to-radium '(0) '())
||#

(<ra> :eval-python "print 51")
(<ra> :eval-scheme "(c-display 50)")

(define (get-pan-value channelnum)
  (if (= 1 (modulo (floor (/ (1- channelnum)
                             2))
                   2))
      -0.37
      0.37))

#||
(load-protracker-module)
||#

(define (load-protracker-module filename)
  (assert (<ra> :is-legal-filepath filename))

  (<ra> :open-progress-window
        (<ra> :append-base64-strings
              (<ra> :to-base64 "Please wait, loading ")
              (<ra> :get-base64-from-filepath filename))
        #t)
    
  (try-finally
   :try (lambda ()
          (<ra> :reset-undo)
          (<ra> :load-song (<ra> :get-path "sounds/mod_song_template.rad"))
          (try-finally :try (lambda ()
                              (<ra> :start-ignoring-undo)
                              
                              (<ra> :eval-python "import import_mod2")
                              (<ra> :eval-python "import_mod2=reload(import_mod2)")
                              
                              (set! *playlist* #f)
                              
                              (<ra> :eval-python (<-> "import_mod2.import_mod(\"" (<ra> :get-base64-from-filepath filename) "\")"))
                              
                              (let* ((stuff (process-events *playlist*
                                                            *instrumentlist*
                                                            (reverse *events*)
                                                            *num-lines*
                                                            *num-channels*
                                                            ))
                                     (playlist (car stuff))
                                     (instruments (cadr stuff))
                                     (patterns (caddr stuff))
                                     (num 0))
                                (send-events-to-radium playlist instruments patterns)
                                (c-display "playlist before: " *playlist*)
                                (c-display "playlist after: " playlist)
                                (<ra> :show-progress-window-message-if-open (<-> "Loading graphical data into memory"))
                                (<ra> :internal_update-all-block-graphics)
                                #t))
                       
                       :finally (lambda ()
                                  (<ra> :stop-ignoring-undo)                          
                                  (<ra> :reset-undo))))
   :finally (lambda ()
              (<ra> :close-progress-window)))
  
  )
  
  
(delafina (async-load-protracker-module :filename (<ra> :create-illegal-filepath))
  (<ra> :schedule 1
        (lambda ()
          (when (<ra> :ask-are-you-sure-song-has-changed)
            (if (<ra> :is-illegal-filepath filename)
                (create-file-requester "Choose MOD file" (<ra> :create-illegal-filepath) "Mod files" "*.mod *.MOD mod.* MOD.*" #t "" #f #t -1 load-protracker-module)
                (load-protracker-module filename)))
          #f)))





#||
FIX:
(let* ((stuff (process-events '(0)
                              #(#("for game music:" "/home/ksvalast/.radium/mod_samples/for32game32music58.wav" 11282 0 64 0 0 24))
                             (list (m-e :note           :value 5 :line 0 :instrumentnum 1)
                                   (m-e :velocity       :value 0 :line 0)
                                   (m-e :note           :value 10 :line 1 :instrumentnum 1)
                                   (m-e :slide-to-note  :value 30 :line 1)
                                   (m-e :velocity       :value 0  :line 5))))
       (patterns (caddr stuff))
       (pattern (car patterns)))
  (print-events (car (pattern 0))))
||#



#||

* add-break-events events default-num-lines playlist)

* simplify-break-events playlist events
  * add-break-events

* remove-events-after-breaks events
  * simplify-break-events

* expand-loops events
  * remove-events-after-breaks

;; * remove-note-cut-effects

* replace-c00-with-stops
  * remove-note-cut-effects
  * prepare-slide-to-note (some c00-notes are actually just effect parameters for prepare-slide-to-note, and must be converted to effect parameter first)

* merge-patterns
  * expand-loops
  calls:
    * replace-c00-with-stops
    * remove-note-cut-effects


* create-new-patterns-based-on-tempo playlist events ;; TODO: determine whether merge-patterns or create-new-patterns-based-on-tempo must be run first, or if it matters.
  * expand-loops 

* remove-pattern-delay-effects pattern
  * simplify-break-events

* legalize-pattern-lengths playlist events)
  - last

* remove-note-retrigger-effects channel tpds
   - needs notes with velocity and portamento information in order to retrigger correct volume and pitch (i think, should check this)

* remove-note-delay-effects events
  - Probably anywhere.

* get-cloned-instruments events
  * assign-instrument-numbers

* add-stop-events

* assign-instrument-numbers
  - Note that non-instrument events such as tempo change must have instrument number -1 assigned before calling to avoid the events to be thrown away.
    This must be done when the events are read in.

* remove-effect-memory
  * remove-glide-to-note <- This one either translates 3xx effects into 1xx or 2xx effects, or removes those effects if there are no forthcoming notes

;;* remove-glide-to-note
  * assign-instrument-numbers (no, probably not)
  * replace-c00-with-stops
  * merge-patterns (needs to merge patterns if the slide-to-note effect is not connected to a playing note in the current pattern)
  - This one probably needs to be run very early, since it uses a "note on" event as parameter for an effect. In other words, these notes are not real notes,
    and should be removed. I haven't thought through this, but these "ghost notes" may screw up other things. At least it doesn't hurt to remove them
    as soon as possible.

    From the documentation:

    " Any note in this channel's division is not played,
      but changes the "remembered" note - it can be thought of as a
      parameter to this effect. "

* prepare-slide-to-note

* get-pitches
  * prepare-slide-to-note
  * add-stop-events

* get-velocities
  * add-stop-events
 

* TODO:
  * [14][3]: Set glissando on/off ;; a lot of work, and probably for nothing. I doubt this effect is used very much. (GODZILLA actually uses this effect, but it seems more like a typo (i.e. e93 instead of e39)
  * [14][15]: Invert loop
  * Retrigger note after volume or pitch has changed. (also read above, for handling note events with value 0)

||#

#||
(for-each (lambda (i)
            (print-events *events*)
            (gc)
            (print-events *events*)
            (gc)
            )
          (iota 100))
(load "import_mod.scm")
(gc)


(<ra> :set-playlist-length 27)
(<ra> :set-playlist-block 15 3)

  (let loop ((pos 0)
             (playlist playlist))
    (when (not (null? playlist))
      (<ra> :set-playlist-block pos (car playlist))
      (loop (1+ pos)
            (cdr playlist)))))

||#

      

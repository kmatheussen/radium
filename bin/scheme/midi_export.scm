(provide 'midi_export.scm)


(define *midi-pulses-per-quarter-note* 480)
(define *midi-export-current-block* #f)

(define-struct midi-event
  :stime
  :type
  :data)

(define-struct note-track
  :tracknum
  :instrument-id
  :events)

(define-struct tick-map-entry
  :stime
  :tick
  :bpm)

  
;; ============================================================
;; MIDI channel clash detection
;; ============================================================

;; Check interval overlap.
(define (intervals-overlap? interval-a interval-b)
  (and (< (interval-a :start) (interval-b :end))
       (< (interval-b :start) (interval-a :end))))


;; Check if any interval pair between two instruments overlaps.
(define (instruments-overlap? intervals-a intervals-b)
  (find-cross-pair intervals-overlap? intervals-a intervals-b))


;; Build a clash map from note-track lists (song-absolute stimes).
;; Returns a hash table mapping instrument_t → list of clashing instrument_t.
(define (get-MIDI-channel-clash-map-from-note-tracks note-track-lists)

  (define-struct instrument-interval
    :start
    :end)
  
  (define instrument-intervals (make-hash-table))

  (define (build-instrument-intervals-for-note-track note-track)
    (define instr (note-track :instrument-id))
    (when (<ra> :is-legal-instrument instr)
      (define track-events (note-track :events))
      (define pending-ons (make-hash-table)) ;; key is pitch, value is list of stime values (stack).
      (if (not (hash-table-ref instrument-intervals instr))
          (hash-table-set! instrument-intervals instr '()))
      (for-each (lambda (event)
                  (cond ((eq? (event :type) 'note-on)
                         ;; note-on.
                         (let* ((pitch (car (event :data)))
                                (lst (hash-table-ref pending-ons pitch)))
                           (hash-table-set! pending-ons pitch
                                            (cons (event :stime) (or lst '())))))
                        ((eq? (event :type) 'note-off)
                         ;; note-off: pair with matching note-on
                         (let* ((pitch (car (event :data)))
                                (lst (hash-table-ref pending-ons pitch)))
                           (assert (and lst (not (null? lst))))
                           (when (and lst
                                      (not (null? lst)))
                             (hash-table-add-to-set! instrument-intervals instr
                                                     (make-instrument-interval :start (car lst)
                                                                               :end (event :stime)))
                             (hash-table-set! pending-ons pitch (cdr lst)))))
                        (else
                         #f)))
                track-events)))
  
  ;; Phase 1: Build instrument intervals from note-tracks
  (for-each (lambda (note-track-list)
              (for-each build-instrument-intervals-for-note-track
                        note-track-list))
            note-track-lists)
  
  ;; Phase 2: Pairwise overlap check
  (define clash-map (make-hash-table))
  (let ((instruments (hash-table-keys instrument-intervals)))
    (for-each (lambda (instr)
                (hash-table-set! clash-map instr '()))
              instruments)
    (for-each-pair (lambda (i j)
                     (when (instruments-overlap? (hash-table-ref instrument-intervals i)
                                                 (hash-table-ref instrument-intervals j))
                       (hash-table-add-to-set! clash-map i j)
                       (hash-table-add-to-set! clash-map j i)))
                   instruments))

  clash-map)


;; Assign MIDI channels (0-15) to block instruments using greedy graph coloring.
;; Returns a hash table mapping instrument_t → channel (int).
;; Note: We skip channel 9, since that's the drum channel.
(define (create-MIDI-channel-map note-track-lists)
  (define clash-map (get-MIDI-channel-clash-map-from-note-tracks note-track-lists))
  (define instruments (hash-table-keys clash-map))
  (define channel-map (make-hash-table))

  ;; Phase 1: Pre-assign channels for MIDI instruments
  (define midi-channels (make-hash-table)) ;; track which channels are taken by MIDI instruments
  (for-each (lambda (instrument)
              (if (string=? (<ra> :get-instrument-type-name instrument) "MIDI")
                  (let ((ch (string->number (<ra> :get-instrument-data instrument "channel"))))
                    (hash-table-set! channel-map instrument ch)
                    (hash-table-set! midi-channels ch #t))))
            instruments)

  ;; Non-MIDI instruments only
  (define non-midi-instruments (keep (lambda (i)
                                       (not (hash-table-ref channel-map i)))
                                     instruments))

  (if (<= (length non-midi-instruments)
          (- 15 (length (hash-table-keys midi-channels))))
      ;; Enough channels left: assign unique channels, skipping 9 and MIDI-assigned channels
      (let loop ((is non-midi-instruments)
                 (ch 0))
        (if (null? is)
            '()
            (let ((ch (let loop2 ((c ch))
                        (cond ((> c 15)
                               c)
                              ((or (= c 9)
                                   (hash-table-ref midi-channels c))
                               (loop2 (+ c 1)))
                              (else
                               c)))))
              (hash-table-set! channel-map (car is) ch)
              (hash-table-set! midi-channels ch #t)
              (loop (cdr is) (+ ch 1)))))
      ;; More than 15 total instruments: greedy coloring with fallback
      (let* ((degree (lambda (instr)
                       (let ((clashes (hash-table-ref clash-map instr)))
                         (if clashes
                             (length clashes)
                             0))))
             (sorted-instruments (sort non-midi-instruments
                                       (lambda (a b)
                                         (> (degree a)
                                            (degree b))))))

        (define used-channels-table (make-hash-table)) ;; key is channel, value is num used.

        ;; Pre-populate used-channels-table with MIDI channel assignments
        (for-each (lambda (kv)
                    (hash-table-set! used-channels-table (cdr kv) 1))
                  channel-map)

        (define (get-used-channels ch)
          (or (hash-table-ref used-channels-table ch) 0))

        (define (inc-used-channels! ch)
          (hash-table-set! used-channels-table
                           ch
                           (1+ (get-used-channels ch))))

        (for-each (lambda (instrument)
                    (define clash-channels (keep (lambda (ch)
                                                   ch)
                                                 (map (lambda (other)
                                                        (hash-table-ref channel-map other))
                                                      (or (hash-table-ref clash-map instrument) '()))))
                    (define ch (let loop ((ch 0)
                                          (best-ch 0)
                                          (best-count 999))
                                 (cond ((> ch 15)
                                        best-ch)
                                       ((= ch 9) ;; Skip the drum channel.
                                        (loop (+ ch 1)
                                              best-ch
                                              best-count))
                                       ((not (member ch clash-channels))
                                        ch)
                                       ((= 0 (get-used-channels ch))
                                        ch)
                                       (else
                                        (let ((count (get-used-channels ch)))
                                          (if (< count best-count)
                                              (loop (+ ch 1) ch count)
                                              (loop (+ ch 1) best-ch best-count)))))))
                    (inc-used-channels! ch)
                    (hash-table-set! channel-map
                                     instrument
                                     ch))
                  sorted-instruments)))

  channel-map)


;; ============================================================
;; Radium data conversion
;; ============================================================

;; Build tempo tick map entries for a block.
;; Returns a list of tick-map-entry structs sorted by stime.
;; start-stime offsets all collected stime values.
(define (build-tempo-tick-map blocknum pfreq start-stime)

  (define-struct tempo-point
    :stime
    :bpm)

  (define tempo-points
    (let loop ((i 0)
               (points '()))
      (if (>= i (<ra> :num-bpms blocknum))
          (if (or (null? points)
                  (> ((car points) :stime) start-stime))
              (cons (make-tempo-point :stime start-stime  ;; Ensure a tempo point at start-stime
                                      :bpm (* (<ra> :get-main-bpm)
                                              (<ra> :get-reltempo blocknum)))
                    (reverse points))
              (reverse points))
          (let* ((place (<ra> :get-bpm-place i blocknum))
                 (bpm (<ra> :get-bpm i blocknum))
                 (reltempo (<ra> :get-reltempo blocknum))
                 (effective-bpm (* bpm reltempo))
                 (stime (+ start-stime (<ra> :get-stime-from-place place blocknum))))
            (loop (1+ i)
                  (cons (make-tempo-point :stime stime
                                          :bpm effective-bpm)
                        points))))))

  (let loop ((tempo-points tempo-points)
             (tick-map '())
             (prev-stime 0)
             (prev-tick-exact 0)
             (prev-bpm ((car tempo-points) :bpm)))
    (if (null? tempo-points)
        (reverse tick-map)
        (let* ((point (car tempo-points))
               (stime (point :stime))
               (bpm (point :bpm))
               (delta-samples (- stime prev-stime))
               (delta-ticks-exact (/ (* delta-samples prev-bpm *midi-pulses-per-quarter-note*)
                                     (* 60 pfreq)))
               (tick-exact (+ prev-tick-exact delta-ticks-exact)))
          (loop (cdr tempo-points)
                (cons (make-tick-map-entry :stime stime
                                           :tick tick-exact
                                           :bpm bpm)
                      tick-map)
                stime
                tick-exact
                bpm)))))


;; Build a song-wide tempo tick map using the blocks on seqtrack 0.
;; Returns (tempo-events . tempo-tick-map) covering the entire song timeline.
(define (build-song-tempo-tick-map seqtracknum pfreq)
  (assert (= seqtracknum 0))
  (define num-seqblocks (<ra> :get-num-seqblocks seqtracknum))

  ;; Collect all tempo points from all seqblocks with offset stimes
  (define points (flatten (map (lambda (seqblocknum)
                                 (build-tempo-tick-map (<ra> :get-seqblock-blocknum seqblocknum seqtracknum)
                                                       pfreq
                                                       (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                               (iota num-seqblocks))))
  
  ;; Sort all points by stime
  (assert (equal? points
                  (sort points
                        (lambda (a b)
                          (< (a :stime) (b :stime))))))
  
  ;; Rebuild tempo-events and tick-map from merged points
  (let loop ((points points)
             (tempo-events '())
             (tick-map '())
             (prev-stime 0)
             (prev-tick-exact 0)
             (prev-bpm ((car points) :bpm)))
    (if (null? points)
        (cons (reverse tempo-events)
              (reverse tick-map))
        (let* ((entry (car points))
               (stime (entry :stime))
               (bpm (entry :bpm))
               (delta-samples (- stime prev-stime))
               (delta-ticks-exact (/ (* delta-samples prev-bpm *midi-pulses-per-quarter-note*)
                                     (* 60 pfreq)))
               (tick-exact (+ prev-tick-exact delta-ticks-exact))
               (tick (round tick-exact)))
          (loop (cdr points)
                (cons (make-midi-event :stime stime
                                       :type 'tempo
                                       :data (list bpm))
                      tempo-events)
                (cons (make-tick-map-entry :stime stime
                                           :tick tick-exact
                                           :bpm bpm)
                      tick-map)
                stime
                tick-exact
                bpm)))))


;; Build a song-wide tempo tick map using sequencer timing.
;; Returns (tempo-events . tempo-tick-map) covering the entire song timeline.
(define (build-song-tempo-tick-map-from-sequencer-timing pfreq)
  (define tempos (<ra> :get-all-sequencer-tempos))

  (define tempo-events (map (lambda (tempo)
                              (make-midi-event :stime (tempo :time)
                                               :type 'tempo
                                               :data (list (tempo :bpm))))
                            tempos))
  
  ;;(c-display "tempo-events:" tempo-events)
  
  (define tick-map (map (lambda (tempo)
                          (define tick-exact (* (tempo :num_quarters) *midi-pulses-per-quarter-note*))
                          (make-tick-map-entry :stime (tempo :time)
                                               :tick tick-exact
                                               :bpm (tempo :bpm)))
                        tempos))

  (cons tempo-events tick-map))

#!!
(build-song-tempo-tick-map-from-sequencer-timing (<ra> :get-sample-rate))
(<ra> :is-using-sequencer-timing)
!!#

;; Convert a sample-time to MIDI ticks using the tempo tick map.
(define (stime-to-ticks stime tempo-tick-map pfreq)
  (define first (car tempo-tick-map))
  (define first-stime ((car tempo-tick-map) :stime))
  (define first-bpm ((car tempo-tick-map) :bpm))

  (if (< stime first-stime)
      ;; Before the first tempo event: extrapolate from start
      (/ (* stime first-bpm *midi-pulses-per-quarter-note*) (* 60 pfreq))
      ;; Find enclosing tick-map-entry
      (let loop ((das-map tempo-tick-map))
        (define tick-map-entry (car das-map))
        (define tick (tick-map-entry :tick))
        (define bpm (tick-map-entry :bpm))
        (define next (cdr das-map))
        
        (if (or (null? next)
                (< stime
                   ((car next) :stime)))
            (let ((delta-samples (- stime (tick-map-entry :stime))))
              (+ tick
                 (/ (* delta-samples bpm *midi-pulses-per-quarter-note*)
                    (* 60 pfreq))))
            (loop next)))))


(define (sort-by-stime events)
  (sort events (lambda (a b)
                 (or (< (a :stime) (b :stime))
                     (and (= (a :stime) (b :stime))
                          (eq? (a :type) 'note-off)
                          (not (eq? (b :type) 'note-off)))))))


;; Collect signature events with STimes-based tick positions.
(define (collect-signature-events blocknum start-stime)
  (map (lambda (signature-num)
         (define sig-place (<ra> :get-signature-place signature-num blocknum))
         (define num-sig (<ra> :get-signature-numerator signature-num blocknum))
         (define den-sig (<ra> :get-signature-denominator signature-num blocknum))
         (make-midi-event :stime (+ start-stime (<ra> :get-stime-from-place sig-place blocknum))
                          :type 'signature
                          :data (list num-sig den-sig)))
       (iota (<ra> :num-signatures blocknum))))


;; Collect note events from one track with STimes-based tick positions.
(define (collect-note-events tracknum blocknum offset-stime)
  (define track-volume (<ra> :get-track-volume tracknum blocknum))
  (define has-instrument (not (<ra> :is-illegal-instrument (<ra> :get-instrument-for-track tracknum blocknum))))

  (sort-by-stime (apply append
                       (map (lambda (note)
                              (define start-place (<ra> :get-note-start note tracknum blocknum))
                              (define end-place (<ra> :get-note-end note tracknum blocknum))
                              (define pitch-float (<ra> :get-note-value note tracknum blocknum))
                              (define vel-float (<ra> :get-velocity-value 0 note tracknum blocknum))
                              (define pitch (max 0 (min 127 (round pitch-float))))
                              (define velocity (if has-instrument
                                                   (max 1 (min 127 (round (* vel-float 127 track-volume))))
                                                   0))
                              (define start-stime (+ offset-stime (<ra> :get-stime-from-place2
                                                                        start-place
                                                                        tracknum
                                                                        blocknum)))
                              (define end-stime (+ offset-stime (<ra> :get-stime-from-place2
                                                                      end-place
                                                                      tracknum
                                                                      blocknum)))
                              ;; Ensure note-off comes after note-on even for zero-length notes
                              (when (<= end-stime start-stime)
                                (set! end-stime (+ start-stime 1/999))) ;; Increase a little bit to avoid note-off to be placed before note-on.
                              (list  (make-midi-event :stime start-stime
                                                      :type 'note-on
                                                      :data (list pitch velocity))
                                     (make-midi-event :stime end-stime
                                                      :type 'note-off
                                                      :data (list pitch 0))))
                            (vector->list (<ra> :get-all-notes tracknum blocknum))))))


;; Collect MIDI init events (program change, volume, pan) for MIDI instruments from the channel map.
;; Returns a list of note-track structs with init events at tick -1.
(define (collect-MIDI-instrument-init-events instrument->tracknum-map)
  (define (has-volume? instr)
    (string=? "1" (<ra> :get-instrument-data instr "volumeonoff")))

  (define (has-pan? instr)
    (string=? "1" (<ra> :get-instrument-data instr "panonoff")))

  (define (get-preset entry)
    (define instr (car entry))
    (let ((preset-str (and (string=? (<ra> :get-instrument-type-name instr) "MIDI")
                           (<ra> :get-instrument-data instr "preset"))))
      (and preset-str
           (not (string=? "" preset-str))
           (string->number preset-str))))

  (map (lambda (entry)
         (let ((tracknum (cdr entry))
               (instr (car entry)))
           (define preset (get-preset entry))
           (define events (list (make-midi-event :stime -1
                                                  :type 'program-change
                                                  :data (list preset))))
           (when (has-volume? instr)
             (set! events (cons (make-midi-event :stime -1
                                                  :type 'control-change
                                                  :data (list 7 (string->number (<ra> :get-instrument-data instr "volume"))))
                                 events)))
           (when (has-pan? instr)
             (set! events (cons (make-midi-event :stime -1
                                                  :type 'control-change
                                                  :data (list 10 (string->number (<ra> :get-instrument-data instr "pan"))))
                                 events)))
           ;; Add enabled CC values
           (define numcc (string->number (<ra> :get-instrument-data instr "numcc")))
           (let loop ((i 0))
             (if (< i numcc)
                 (begin
                   (if (string=? "1" (<ra> :get-instrument-data instr (string-append "ccsonoff" (number->string i))))
                       (let ((cc-number (string->number (<ra> :get-instrument-data instr (string-append "cc" (number->string i)))))
                             (cc-value (string->number (<ra> :get-instrument-data instr (string-append "ccvalues" (number->string i))))))
                         (set! events (cons (make-midi-event :stime -1
                                                              :type 'control-change
                                                              :data (list cc-number cc-value))
                                            events))))
                   (loop (+ i 1)))))
           (make-note-track :tracknum tracknum
                            :instrument-id instr
                            :events events)))
       (keep get-preset
             (map identity instrument->tracknum-map))))



;; ============================================================
;; Returns conductor events (signatures) for a single block.
;; ============================================================
(define (get-block-conductor-events blocknum start-stime)
  (define conductor-events (collect-signature-events blocknum start-stime))

  ;; Ensure initial time signature at beginning of block (use main signature)
  (if (not (find-if (lambda (e)
                      (and (eq? (e :type) 'signature)
                           (= (e :stime) start-stime)))
                    conductor-events))
      (cons (make-midi-event :stime start-stime
                             :type 'signature
                             :data (list (<ra> :get-main-signature-numerator)
                                         (<ra> :get-main-signature-denominator)))
            conductor-events)
      conductor-events))


;; ============================================================
;; Returns note tracks for a single block.
;; ============================================================
(define (get-block-note-tracks instrument->tracknum-map blocknum start-stime)
  (map (lambda (tracknum)
         (define instrument (<ra> :get-instrument-for-track tracknum blocknum))
         (define note-events (collect-note-events tracknum
                                                  blocknum
                                                  start-stime))
         (make-note-track :tracknum (hash-table-ref instrument->tracknum-map instrument)
                          :instrument-id instrument
                          :events note-events))
       (iota (<ra> :get-num-tracks blocknum))))


;; Returns note tracks for a seqblock in a seqtrack.
(define (get-seqblock-note-tracks instrument->tracknum-map seqtracknum seqblocknum)
  (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
  (define start-stime (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
  (get-block-note-tracks instrument->tracknum-map blocknum start-stime))


;; Returns conductor events for a seqblock in a seqtrack.
(define (get-seqblock-conductor-events seqtracknum seqblocknum)
  (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
  (define start-stime (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
  (get-block-conductor-events blocknum start-stime))


;; ===================================================================
;; Merge helpers
;; ====================================================================

;; Merge lists of conductor events into a single sorted list.
(define (merge-conductor-events conductor-lists)
  (sort-by-stime (apply append conductor-lists)))

;; Merge lists of note tracks by tracknum into a single list of note-track structs.
(define (merge-seqblock-note-tracks note-track-lists)
  ;; Find max track count across all blocks
  (define num-tracks
    (if (null? note-track-lists)
        0
        (apply max (map length note-track-lists))))

  ;; Merge note tracks by tracknum
  (map (lambda (tracknum)
         (define events
           (sort-by-stime
             (apply append
                    (map (lambda (trs)
                           (if (< tracknum (length trs))
                               ((list-ref trs tracknum) :events)
                               '()))
                         note-track-lists))))
         (define instrument-id (or (let find ((lists note-track-lists))
                                     (if (null? lists)
                                         (<ra> :create-illegal-instrument)
                                         (let ((tr (if (< tracknum (length (car lists)))
                                                       (list-ref (car lists) tracknum)
                                                       #f)))
                                           (if (and tr (not (<ra> :is-illegal-instrument (tr :instrument-id))))
                                               (tr :instrument-id)
                                               (find (cdr lists))))))
                                   (<ra> :create-illegal-instrument)))
         (make-note-track :tracknum tracknum
                          :instrument-id instrument-id
                          :events events))
       (iota num-tracks)))


;; ============================================================
;; MIDI binary encoding primitives
;; ============================================================

;; Encode a variable-length quantity as a byte list.
;; Collects 7-bit groups MSB-first, then sets bit 7 on all
;; bytes except the last (which is the LSB).
(define (encode-varlen val)
  (define chunks (let loop ((v val)
                            (chunks '()))
                   (let ((new-chunks (cons (logand v #x7F)
                                           chunks))
                         (next-v (ash v -7)))
                     (if (> next-v 0)
                         (loop next-v
                               new-chunks)
                         new-chunks))))
  (define num-chunks (length chunks))
  (define last (- num-chunks 1))
  
  (let iter ((cs chunks)
             (i 0)
             (result '()))
    (if (null? cs)
        (reverse result)
        (let ((b (car cs)))
          (if (< i last)
              (iter (cdr cs) (+ i 1) (cons (logior b #x80) result))
              (iter (cdr cs) (+ i 1) (cons b result)))))))

(***assert*** (encode-varlen 0) ;; 1-byte varlen
              (list 0))
(***assert*** (encode-varlen 127) ;; max single byte
              (list 127))
(***assert*** (encode-varlen 128) ;; first 2-byte value, continuation set on MSB
              (list #x81 0))
(***assert*** (encode-varlen 1000) ;; (7<<7)|104 = 896+104 = 1000
              (list #x87 #x68))
(***assert*** (encode-varlen #x3FFF) ;; max 2-byte value
              (list #xFF #x7F))
(***assert*** (encode-varlen #x4000) ;; first 3-byte value
              (list #x81 #x80 0))
(***assert*** (encode-varlen #x1FFFFF) ;; max MIDI varlen
              (list #xFF #xFF #x7F))
(***assert*** (encode-varlen (* 480 100)) ;; typical delta time
              (list #x82 #xF7 0))


(define (bytes-concat . args)
  (apply append args))


;; ============================================================
;; MIDI event encoders (all return byte lists)
;; ============================================================

;; Meta event: FF + type + varlen-length + data
(define (encode-meta-event delta type data)
  (bytes-concat (encode-varlen delta)
                (list #xFF type)
                (encode-varlen (length data))
                data))

(***assert*** (encode-meta-event 0 #x3 (list 1 2))
              (list #x00 #xFF #x03 #x02 #x01 #x02))


(define (encode-end-of-track delta)
  (bytes-concat (encode-varlen delta)
                (list #xFF #x2F 0)))

(***assert*** (encode-end-of-track 0)
              (list #x00 #xFF #x2F #x00))


;; Set Tempo: FF 51 03 tttttt   (mpqn = 60,000,000 / bpm)
(define (encode-set-tempo delta bpm)
  (define mpqn (floor (/ 60000000 bpm)))
  (define b0 (logand (ash mpqn -16) #xFF))
  (define b1 (logand (ash mpqn -8) #xFF))
  (define b2 (logand mpqn #xFF))
  
  (encode-meta-event delta #x51 (list b0 b1 b2)))

(***assert*** (encode-set-tempo 0 120)               ;; 500000 us/qn = 0x07A120
              (list #x00 #xFF #x51 #x03 #x07 #xA1 #x20))


;; Time Signature: FF 58 04 nn dd cc bb
;; dd = log2(denominator). 4->2, 8->3, 2->1, 1->0.
(define (encode-time-signature delta numerator denominator)
  (define denom-exp (cond ((= denominator 1) 0)
                          ((= denominator 2) 1)
                          ((= denominator 4) 2)
                          ((= denominator 8) 3)
                          ((= denominator 16) 4)
                          ((= denominator 32) 5)
                          (else
                           (round (/ (log denominator)
                                     (log 2))))))
  
  (encode-meta-event delta #x58 (list numerator (max 0 denom-exp) 24 8)))

(***assert*** (encode-time-signature 0 4 4)           ;; 4/4, denom-exp=2
              (list #x00 #xFF #x58 #x04 #x04 #x02 #x18 #x08))


;; Note On: 9n kk vv
(define (encode-note-on delta channel pitch velocity)
  (bytes-concat (encode-varlen delta)
                (list (logior #x90 channel) pitch velocity)))

(***assert*** (encode-note-on 0 0 60 100)             ;; C4, velocity 100
              (list #x00 #x90 60 100))


;; Note Off: 8n kk vv
(define (encode-note-off delta channel pitch)
  (bytes-concat (encode-varlen delta)
                (list (logior #x80 channel) pitch 0)))

(***assert*** (encode-note-off 0 0 60)                ;; C4 note-off
              (list #x00 #x80 60 0))


;; Program Change: Cn pp
(define (encode-program-change delta channel program)
  (bytes-concat (encode-varlen delta)
                (list (logior #xC0 channel) program)))


;; Control Change: Bn cc vv
(define (encode-control-change delta channel controller value)
  (bytes-concat (encode-varlen delta)
                (list (logior #xB0 channel) controller value)))


(define (get-midi-event-bytes delta midi-event channel)
  (let ((data (midi-event :data)))
    (cond ((eq? (midi-event :type) 'tempo)
           (encode-set-tempo delta (car data)))
          ((eq? (midi-event :type) 'signature)
           (encode-time-signature delta (car data) (cadr data)))
          ((eq? (midi-event :type) 'note-on)
           (encode-note-on delta channel (car data) (cadr data)))
          ((eq? (midi-event :type) 'note-off)
           (encode-note-off delta channel (car data)))
          ((eq? (midi-event :type) 'program-change)
           (encode-program-change delta channel (car data)))
          ((eq? (midi-event :type) 'control-change)
           (encode-control-change delta channel (car data) (cadr data)))
          (else
            '()))))


;; ============================================================
;; File I/O helpers
;; ============================================================

;; Write a chunk header: 4 ID bytes + 4-byte big-endian length
(define (midi-write-chunk-header type length file)
  ;; "MThd" = 4D 54 68 64,  "MTrk" = 4D 54 72 6B
  (if (string=? type "MThd")
      (begin
        (<ra> :write8-to-file file #x4D)  ;; M
        (<ra> :write8-to-file file #x54)  ;; T
        (<ra> :write8-to-file file #x68)  ;; h
        (<ra> :write8-to-file file #x64)) ;; d
      (begin
        (assert (string=? type "MTrk"))
        (<ra> :write8-to-file file #x4D)  ;; M
        (<ra> :write8-to-file file #x54)  ;; T
        (<ra> :write8-to-file file #x72)  ;; r
        (<ra> :write8-to-file file #x6B)));; k
  (<ra> :write-be32-to-file file length))


;; Encode events into MIDI bytes and write a track chunk.
(define (midi-write-track-chunk events end-tick tempo-tick-map pfreq channel file)
  (define prev-tick 0)
  
  ;; Encode all events with delta encoding (must do in separate step since we need length of file before writing chunk)
  (define encoded (append (apply append
                                 (map-in-order (lambda (event)
                                                 (define stime (max 0 (event :stime)))
                                                 (define tick (stime-to-ticks stime tempo-tick-map pfreq))
                                                 (define delta (round (- tick prev-tick)))
                                                 (set! prev-tick tick)
                                                 (get-midi-event-bytes delta event channel))
                                               events))))
  
  ;; Add EndOfTrack (do in separate step afterwards to ensure prev-tick is currect.)
  (set! encoded (append encoded
                        (encode-end-of-track (round (- end-tick prev-tick)))))
  
  ;; Write track chunk
  (midi-write-chunk-header "MTrk" (length encoded) file)
  (for-each (lambda (b)
              (<ra> :write8-to-file file b))
            encoded))


;; ============================================================
;; Main export function
;; ============================================================

(define (export-midi-to-file filename)
  (define pfreq (<ra> :get-sample-rate))

  (define seqtracknum 0)
  (define use-sequencer (<ra> :is-using-sequencer-timing))
  (define include-conductor (not use-sequencer))

  ;; Song-wide tempo tick map
  (define song-tempo-result (if use-sequencer
                                (build-song-tempo-tick-map-from-sequencer-timing pfreq)
                                (build-song-tempo-tick-map seqtracknum pfreq)))
  (define song-tempo-events (car song-tempo-result))
  (define song-tempo-tick-map (cdr song-tempo-result))

  ;; Song-wide signature events from sequencer timing
  (define song-signature-events
    (if use-sequencer
        (map (lambda (sig)
               (make-midi-event :stime (sig :time)
                                :type 'signature
                                :data (list (sig :numerator) (sig :denominator))))
             (<ra> :get-all-sequencer-signatures))
        '()))

  ;; Define blocknums
  (define blocknums (if *midi-export-current-block*
                        (list -1)
                        (flatten (map (lambda (seqtracknum)
                                        (map (lambda (seqblocknum)
                                               (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
                                             (iota (<ra> :get-num-seqblocks seqtracknum))))
                                      (iota (<ra> :get-num-seqtracks))))))
  
  ;; Pass 1: Build instrument → tracknum map from block instruments (first seen wins)
  (define instrument->tracknum-map (make-hash-table))
  (let ((next-tracknum 0))
    (for-each (lambda (blocknum)
                (for-each (lambda (tracknum)
                            (define instr (<ra> :get-instrument-for-track tracknum blocknum))
                            (if (and (not (<ra> :is-illegal-instrument instr))
                                     (not (hash-table-ref instrument->tracknum-map instr)))
                                (begin
                                  (hash-table-set! instrument->tracknum-map instr next-tracknum)
                                  (set! next-tracknum (+ next-tracknum 1)))))
                          (iota (<ra> :get-num-tracks blocknum))))
              blocknums))

  ;; Collect note track lists from all non-audio seqtracks (or current block)
  (define all-note-track-lists (if *midi-export-current-block*
                                   (list (get-block-note-tracks instrument->tracknum-map -1 0))
                                   (apply append
                                          (map (lambda (seqtracknum)
                                                 (if (<ra> :seqtrack-for-audiofiles seqtracknum)
                                                     '()
                                                     (map (lambda (seqblocknum)
                                                            (get-seqblock-note-tracks instrument->tracknum-map seqtracknum seqblocknum))
                                                          (iota (<ra> :get-num-seqblocks seqtracknum)))))
                                               (iota (<ra> :get-num-seqtracks))))))
  
  ;; Collect conductor event lists from seqtrack 0 (if not sequencer timing)
  (define conductor-lists
    (if include-conductor
        (if *midi-export-current-block*
            (list (get-block-conductor-events -1 0))
            (map (lambda (seqblocknum)
                   (get-seqblock-conductor-events 0 seqblocknum))
                 (iota (<ra> :get-num-seqblocks 0))))
        '()))

  ;; Merge + add init events
  (define note-tracks (merge-seqblock-note-tracks (append all-note-track-lists
                                                          (list (collect-MIDI-instrument-init-events instrument->tracknum-map)))))
  
  (define conductor-events (sort-by-stime (append (merge-conductor-events conductor-lists)
                                                  song-tempo-events
                                                  song-signature-events)))
  
  ;; Compute last tick across all tracks (plus padding)
  (define end-tick (let ((last-tick 0))
                      (for-each (lambda (event)
                                  (set! last-tick (max last-tick
                                                       (stime-to-ticks (event :stime) song-tempo-tick-map pfreq))))
                                conductor-events)
                      (for-each (lambda (note-track)
                                  (for-each (lambda (event)
                                              (set! last-tick (max last-tick
                                                                   (stime-to-ticks (event :stime) song-tempo-tick-map pfreq))))
                                            (note-track :events)))
                                note-tracks)
                      (+ last-tick *midi-pulses-per-quarter-note*)))

  (define instrument->midi-channel-map (create-MIDI-channel-map all-note-track-lists))

  ;; Open binary file
  (let ((file (<ra> :open-file-for-binary-writing filename)))
    (if (<ra> :is-illegal-file file)
        (begin
          (c-display "export-midi: Could not open file for writing")
          #f)
        (begin
          ;; SMF Header chunk
          (midi-write-chunk-header "MThd" 6 file)
          (<ra> :write-be16-to-file file 1)          ;; Format 1
          (<ra> :write-be16-to-file file (1+ (length note-tracks)))    ;; Number of tracks (+1 is the tempo/signature track.)
          (<ra> :write-be16-to-file file *midi-pulses-per-quarter-note*)  ;; PPQ
          
          ;; Conductor track
          (midi-write-track-chunk conductor-events end-tick song-tempo-tick-map pfreq -1 file)
          
          ;; Note tracks
          (for-each (lambda (note-track)
                      (define channel (or (hash-table-ref instrument->midi-channel-map (note-track :instrument-id))
                                          0))
                      (midi-write-track-chunk (note-track :events)
                                              end-tick
                                              song-tempo-tick-map
                                              pfreq
                                              channel
                                              file))
                    note-tracks)
          
          (<ra> :close-file file)
          #t))))


;; ============================================================
;; Entry point — called from menu
;; ============================================================

(define (export-midi-with-title title)
  (let ((filename (<ra> :get-save-filename
                        title
                        "*.mid *.MID *.midi *.MIDI"
                        (<ra> :create-illegal-filepath)
                        "MIDI files"
                        ".mid")))
    (if (not (<ra> :is-illegal-filepath filename))
        (if (export-midi-to-file filename)
            (<ra> :show-async-message (<-> "MIDI file exported to \"" (<ra> :get-path-string filename) "\"."))
            (<ra> :show-async-message (<-> "Failed to export MIDI file \"" (<ra> :get-path-string filename) "\"."))))))

(define (export-midi-block)
  (set! *midi-export-current-block* #t)
  (export-midi-with-title "Export MIDI File for current block"))

(define (export-midi-song)
  (set! *midi-export-current-block* #f)
  (export-midi-with-title "Export MIDI File for song"))

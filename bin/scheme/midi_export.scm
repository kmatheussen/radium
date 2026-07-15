(provide 'midi_export.scm)


(define *midi-export-auto-assign-MIDI-channel* #t)
(define *midi-pulses-per-quarter-note* 480)

(define-struct midi-event
  :tick
  :type
  :bytes)

(define-struct note-track
  :channel
  :events)

(define-struct instrument-interval
  :start
  :end)

(define-struct tick-map-entry
  :stime
  :tick
  :bpm)

(define-struct block-result
  :conductor-events
  :note-tracks)

(define-struct seqblock-result
  :stime
  :conductor-events
  :note-tracks)

;; ============================================================
;; MIDI channel clash detection
;; ============================================================

;; Helper: add val to the set stored at key in a hash table.
;; Mutates the hash table.
(define (hash-table-add-to-set! table key val)
  (let ((lst (hash-table-ref table key)))
    (if lst
        (if (not (member val lst))
            (hash-table-set! table key (cons val lst)))
        (hash-table-set! table key (list val)))))


;; Helper: check interval overlap.
(define (intervals-overlap? interval-a interval-b)
  (and (< (interval-a :start) (interval-b :end))
       (< (interval-b :start) (interval-a :end))))

;; Helper: calls (proc a b) for every unique unordered pair from lst.
(define (for-each-pair proc lst)
  (let outer ((remaining lst))
    (when (not (null? (cdr remaining)))
      (let ((first (car remaining)))
        (let inner ((rest (cdr remaining)))
          (if (null? rest)
              (outer (cdr remaining))
              (begin
                (proc first (car rest))
                (inner (cdr rest)))))))))

;; Helper: iterate all pairs (a from lst-a, b from lst-b) calling (proc a b).
;; Returns the first non-#f result from proc, or #f if proc always returns #f.
(define (find-cross-pair proc lst-a lst-b)
  (call-with-exit
    (lambda (return)
      (for-each (lambda (a)
                  (for-each (lambda (b)
                              (let ((result (proc a b)))
                                (if result
                                    (return result))))
                            lst-b))
                lst-a)
      #f)))


;; Helper: check if any interval pair between two instruments overlaps.
(define (instruments-overlap? intervals-a intervals-b)
  (find-cross-pair intervals-overlap? intervals-a intervals-b))

;; Return a hash table mapping instrument_id → list of instrument_ids
;; that have overlapping notes in the given block.
(define (get-MIDI-channel-clash-map blocknum)
  (define instrument-intervals (make-hash-table))

  ;; Phase 1: Collect (start . end) intervals per instrument  
  (for-each (lambda (tracknum)
              (let ((instr (<ra> :get-instrument-for-track tracknum blocknum -1)))
                (if (<ra> :is-legal-instrument instr)
                    (for-each (lambda (note)
                                (hash-table-set! instrument-intervals
                                                 instr
                                                 (cons (make-instrument-interval :start (<ra> :get-note-start note tracknum blocknum)
                                                                                 :end (<ra> :get-note-end note tracknum blocknum))
                                                       (or (hash-table-ref instrument-intervals instr) '()))))
                              (vector->list (<ra> :get-all-notes tracknum blocknum))))))
            (iota (<ra> :get-num-tracks blocknum)))

  (define clash-map (make-hash-table))

  ;; Phase 2: Initialize all instruments with empty clash lists
  (for-each (lambda (kv)
              (define instr (car kv))
              (if (not (hash-table-ref clash-map instr))
                  (hash-table-set! clash-map instr '())))
            instrument-intervals)

  ;; Phase 3: Pairwise overlap check
  (for-each-pair (lambda (i j)
                   (when (instruments-overlap? (hash-table-ref instrument-intervals i)
                                               (hash-table-ref instrument-intervals j))
                     (hash-table-add-to-set! clash-map i j)
                     (hash-table-add-to-set! clash-map j i)))
                 (hash-table-keys instrument-intervals))

  clash-map)


;; Helper: return a list of all keys in a hash table.
(define (hash-table-keys ht)
  (let ((keys '()))
    (for-each (lambda (kv) (set! keys (cons (car kv) keys))) ht)
    keys))

;; Assign MIDI channels (0-15) to block instruments using greedy graph coloring.
;; Returns a hash table mapping instrument_t → midi_channel.
;; Note: We skip channel 9, since that's the drum channel.
(define (create-MIDI-channel-map-for-block blocknum)
  (define clash-map (get-MIDI-channel-clash-map blocknum))
  (define instruments (hash-table-keys clash-map))
  (define degree (lambda (instr)
                   (let ((clashes (hash-table-ref clash-map instr)))
                     (if clashes
                         (length clashes)
                         0))))
  (define sorted-instruments (sort instruments
                                   (lambda (a b)
                                     (> (degree a) (degree b)))))
  (define channel-map (make-hash-table))

  (for-each
   (lambda (instr)
     (define clashes (or (hash-table-ref clash-map instr) '()))
     (define used-channels (keep (lambda (c)
                                   c)
                                 (map (lambda (other)
                                        (hash-table-ref channel-map other))
                                      clashes)))
     (define available-channel (let loop ((ch 0))
                                 (if (> ch 15)
                                     #f
                                     (if (or (member ch used-channels)
                                             (= ch 9)) ;; Skip the drum channel.
                                         (loop (+ ch 1))
                                         ch))))
     (if available-channel
         (hash-table-set! channel-map instr available-channel)
         ;; Fallback: pick channel with fewest conflicts
         (let ((best-channel (let loop ((ch 0)
                                        (best-ch 0)
                                        (best-count 999))
                               (if (> ch 15)
                                   best-ch
                                   (if (= ch 9)
                                       (loop (+ ch 1)
                                             best-ch
                                             best-count) ;; Skip the drum channel.
                                       (let ((count (length (keep (lambda (c)
                                                                    (= c ch))
                                                                  used-channels))))
                                         (if (< count best-count)
                                             (loop (+ ch 1)
                                                   ch
                                                   count)
                                             (loop (+ ch 1)
                                                   best-ch
                                                   best-count))))))))
           (hash-table-set! channel-map instr best-channel))))
   sorted-instruments)

  channel-map)


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


;; ============================================================
;; Radium data conversion
;; ============================================================

;; Build tempo tick map and collect tempo events.
;; tempo-tick-map: list of (stime tick bpm) triples sorted by stime,
;; each representing the start of a constant-tempo segment.
;; Returns (tempo-events . tempo-tick-map).
(define (build-tempo-tick-map blocknum pfreq)

  (define-struct tempo-point
    :stime
    :bpm)

  (define tempo-points
    (let loop ((i 0)
               (points '()))
      (if (>= i (<ra> :num-bpms blocknum))
          (if (or (null? points)
                  (> ((car points) :stime) 0))
              (cons (make-tempo-point :stime 0  ;; Ensure a tempo point at stime 0
                                      :bpm (* (<ra> :get-main-bpm) (<ra> :get-reltempo blocknum)))
                    (reverse points))
              (reverse points))
          (let* ((place (<ra> :get-bpm-place i blocknum))
                 (bpm (<ra> :get-bpm i blocknum))
                 (reltempo (<ra> :get-reltempo blocknum))
                 (effective-bpm (* bpm reltempo))
                 (stime (<ra> :get-stime-from-place place blocknum)))
            (loop (1+ i)
                  (cons (make-tempo-point :stime stime
                                          :bpm effective-bpm)
                        points))))))
  
  (let loop ((tempo-points tempo-points)
             (tempo-events '())
             (tick-map '())
             (prev-stime 0)
             (prev-tick-exact 0)
             (prev-bpm ((car tempo-points) :bpm)))
    (if (null? tempo-points)
        (cons (reverse tempo-events)
              (reverse tick-map))
        (let* ((point (car tempo-points))
               (stime (point :stime))
               (bpm (point :bpm))
               (delta-samples (- stime prev-stime))
               (delta-ticks-exact (/ (* delta-samples prev-bpm *midi-pulses-per-quarter-note*)
                                     (* 60 pfreq)))
               (tick-exact (+ prev-tick-exact delta-ticks-exact))
               (tick (round tick-exact)))
          (loop (cdr tempo-points)
                (cons (make-midi-event :tick tick
                                       :type 'tempo
                                       :bytes (list bpm))
                      tempo-events)
                (cons (make-tick-map-entry :stime stime
                                           :tick tick-exact
                                           :bpm bpm)
                      tick-map)
                stime
                tick-exact
                bpm)))))


;; Convert a sample-time to MIDI ticks using the tempo tick map.
(define (stime-to-ticks stime tempo-tick-map pfreq)
  (define first (car tempo-tick-map))
  (define first-stime ((car tempo-tick-map) :stime))
  (define first-bpm ((car tempo-tick-map) :bpm))

  (if (< stime first-stime)
      ;; Before the first tempo event: extrapolate from start
      (round (/ (* stime first-bpm *midi-pulses-per-quarter-note*) (* 60 pfreq)))
      ;; Find enclosing segment
      (let loop ((map tempo-tick-map))
        (define seg (car map))
        (define seg-stime (seg :stime))
        (define seg-tick (seg :tick))
        (define seg-bpm (seg :bpm))
        (define next (cdr map))
        
        (if (or (null? next) (< stime ((car next) :stime)))
            (let ((delta-samples (- stime seg-stime)))
              (round (+ (seg :tick)
                        (/ (* delta-samples (seg :bpm) *midi-pulses-per-quarter-note*)
                           (* 60 pfreq)))))
            (loop next)))))


(define (sort-by-tick events)
  (sort events (lambda (a b)
                 (< (a :tick) (b :tick)))))


;; Collect signature events with STimes-based tick positions.
(define (collect-signature-events blocknum tempo-tick-map pfreq)
  (define n (<ra> :num-signatures blocknum))
  (define events '())
  
  (let loop ((i 0))
    (if (>= i n)
        (reverse events)
        (let* ((sig-place (<ra> :get-signature-place i blocknum))
               (num-sig (<ra> :get-signature-numerator i blocknum))
               (den-sig (<ra> :get-signature-denominator i blocknum))
               (stime (<ra> :get-stime-from-place sig-place blocknum -1))
               (tick (stime-to-ticks stime tempo-tick-map pfreq)))
          (set! events (cons (make-midi-event :tick tick :type 'signature :bytes (list num-sig den-sig)) events))
          (loop (+ i 1))))))


;; Collect note events from one track with STimes-based tick positions.
(define (collect-note-events tracknum blocknum tempo-tick-map pfreq)
  (define notes-vec (<ra> :get-all-notes tracknum blocknum -1))
  (define track-volume (<ra> :get-track-volume tracknum blocknum -1))
  (define has-instrument (not (<ra> :is-illegal-instrument (<ra> :get-instrument-for-track tracknum blocknum -1))))
  
  (let loop ((notes (vector->list notes-vec))
             (events '()))
    (if (null? notes)
        (sort-by-tick events)
        (let ((note (car notes)))
          (define start-place (<ra> :get-note-start note tracknum blocknum -1))
          (define end-place (<ra> :get-note-end note tracknum blocknum -1))
          (define pitch-float (<ra> :get-note-value note tracknum blocknum -1))
          (define vel-float (<ra> :get-velocity-value 0 note tracknum blocknum -1))
          (define pitch (max 0 (min 127 (round pitch-float))))
          (define velocity (if has-instrument
                               (max 1 (min 127 (round (* vel-float 127 track-volume))))
                               0))
          (define start-stime (<ra> :get-stime-from-place2 start-place
                                    tracknum blocknum -1))
          (define end-stime (<ra> :get-stime-from-place2 end-place
                                  tracknum blocknum -1))
          (define start-tick (stime-to-ticks start-stime tempo-tick-map pfreq))
          (define end-tick (stime-to-ticks end-stime tempo-tick-map pfreq))
          
          (loop (cdr notes)
                (append (list  (make-midi-event :tick end-tick
                                                :type 'note-off
                                                :bytes (list pitch 0))
                               (make-midi-event :tick start-tick
                                                :type 'note-on
                                                :bytes (list pitch velocity)))
                        events))))))


;; Sent as "event-encoder" argument for "encode-event-sequence" for tempo/signature/etc. events.
(define (conductor-event-encoder delta midi-event)
  (if (eq? (midi-event :type) 'end-of-track)
      (encode-end-of-track delta)
      (let ((bytes (midi-event :bytes)))
        (cond ((eq? (midi-event :type) 'tempo)
               (encode-set-tempo delta (car bytes)))
              ((eq? (midi-event :type) 'signature)
               (encode-time-signature delta (car bytes) (cadr bytes)))
              (else
               '())))))

;; Creates a function that is sent "event-encoder" argument for "encode-event-sequence" for note events.
(define (make-note-event-encoder channel)
  (lambda (delta midi-event)
    (if (eq? (midi-event :type) 'end-of-track)
        (encode-end-of-track delta)
        (let ((bytes (midi-event :bytes)))
          (cond ((eq? (midi-event :type) 'note-on)
                 (encode-note-on delta channel (car bytes) (cadr bytes)))
                ((eq? (midi-event :type) 'note-off)
                 (encode-note-off delta channel (car bytes)))
                (else
                 '()))))))



;; Encode a sorted sequence of (tick tag ...) events into a list of midi-event structs,
;; using delta encoding. 'end-tick' is where the track ends.
(define (encode-event-sequence events event-encoder end-tick)
  (define prev-tick 0)
  (define result '())
  
  (for-each
   (lambda (event)
     (define tick (event :tick))
     (define delta (- tick prev-tick))
     (set! prev-tick tick)
     (set! result
           (cons (make-midi-event :tick tick
                                  :type (event :type)
                                  :bytes (event-encoder delta event))
                 result)))
   events)
  
  (append (reverse result)
          (list (make-midi-event :tick end-tick
                                 :type 'end-of-track
                                 :bytes (event-encoder (- end-tick prev-tick)
                                                       (make-midi-event :tick end-tick
                                                                        :type 'end-of-track
                                                                        :bytes '()))))))



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


;; Write an entire track chunk from a list of midi-event structs.
(define (midi-write-track-chunk midi-events file)
  (define all-bytes (apply append (map (lambda (ev)
                                         (ev :bytes))
                                       midi-events)))
  
  (midi-write-chunk-header "MTrk" (length all-bytes) file)
  
  (for-each (lambda (b)
              (<ra> :write8-to-file file b))
            all-bytes))


;; ============================================================
;; Block export — collects raw midi-events for a single block
;; ============================================================

(define (get-block-result blocknum)
  (define pfreq (<ra> :get-sample-rate))
  (define tempo-result (build-tempo-tick-map blocknum pfreq))
  (define tempo-events (car tempo-result))
  (define tempo-tick-map (cdr tempo-result))

  (define conductor-events (sort-by-tick (append tempo-events
                                                 (collect-signature-events blocknum tempo-tick-map pfreq))))

  ;; Check we have initial tempo at tick 0
  (assert (find-if (lambda (e)
                     (and (eq? (e :type) 'tempo)
                          (= (e :tick) 0)))
                   conductor-events))
  
  ;; Ensure initial time signature at tick 0 (use main signature)
  (if (not (find-if (lambda (e)
                      (and (eq? (e :type) 'signature)
                           (= (e :tick) 0)))
                    conductor-events))
      (set! conductor-events
            (cons (make-midi-event :tick 0
                                   :type 'signature
                                   :bytes (list (<ra> :get-main-signature-numerator)
                                                (<ra> :get-main-signature-denominator)))
                  conductor-events)))
  
  ;; Collect note tracks
  (define auto-channel-map (if *midi-export-auto-assign-MIDI-channel*
                               (create-MIDI-channel-map-for-block blocknum)
                               #f))
  
  (define note-tracks
    (let loop ((tracknum 0)
               (note-tracks '()))
      (if (>= tracknum (<ra> :get-num-tracks blocknum))
          (reverse note-tracks)
          (let ((channel (if auto-channel-map
                             (let ((instr (<ra> :get-instrument-for-track tracknum blocknum)))
                               (if (<ra> :is-illegal-instrument instr)
                                   -1
                                   (or (hash-table-ref auto-channel-map instr) -1)))
                             (<ra> :get-track-midi-channel tracknum blocknum)))
                (note-events (collect-note-events tracknum
                                                  blocknum
                                                  tempo-tick-map
                                                  pfreq)))
            (loop (1+ tracknum)
                  (cons (make-note-track :channel (max channel 0)
                                         :events note-events)
                        note-tracks))))))
  
  (make-block-result :conductor-events conductor-events
                     :note-tracks note-tracks))


;; Returns the block results for a seqblock in a seqtrack,
;; with the seqblock's stime as the start time.
(define (get-seqblock-result seqtracknum seqblocknum)
  (define blocknum (<ra> :get-seqblock-blocknum seqblocknum seqtracknum))
  (define block (get-block-result blocknum))
  (define stime (<ra> :get-seqblock-start-time seqblocknum seqtracknum))
  
  (make-seqblock-result :stime stime
                        :conductor-events (block :conductor-events)
                        :note-tracks (block :note-tracks)))


;; ============================================================
;; Main export function
;; ============================================================

(define (export-midi-to-file filename)
  (define block (get-block-result -1))
  (define conductor-events (block :conductor-events))
  (define note-tracks (block :note-tracks))

  ;; Compute last tick across all tracks (plus padding)
  (define last-tick 0)
  (for-each (lambda (e)
              (set! last-tick (max last-tick (e :tick))))
            conductor-events)
  (for-each (lambda (tr)
              (for-each (lambda (e)
                          (set! last-tick (max last-tick (e :tick))))
                        (tr :events)))
            note-tracks)
  (set! last-tick (+ last-tick *midi-pulses-per-quarter-note*))

  ;; Encode all track data
  (let* ((conductor-midi-events (encode-event-sequence conductor-events conductor-event-encoder last-tick))
         (note-track-midi-events (map (lambda (tr)
                                        (encode-event-sequence (tr :events)
                                                               (make-note-event-encoder (tr :channel))
                                                               last-tick))
                                      note-tracks)))

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
            (<ra> :write-be16-to-file file (1+ (length note-track-midi-events)))    ;; Number of tracks (+1 is the tempo/signature track.)
            (<ra> :write-be16-to-file file *midi-pulses-per-quarter-note*)  ;; PPQ

            ;; Conductor track
            (midi-write-track-chunk conductor-midi-events file)

            ;; Note tracks
            (for-each (lambda (evs)
                        (midi-write-track-chunk evs file))
                      note-track-midi-events)

            (<ra> :close-file file)
            #t)))))


;; ============================================================
;; Entry point — called from menu
;; ============================================================

(define (export-midi)
  (let ((filename (<ra> :get-save-filename
                        "Export MIDI File"
                        "*.mid *.MID *.midi *.MIDI"
                        (<ra> :create-illegal-filepath)
                        "MIDI files"
                        ".mid")))
    (if (not (<ra> :is-illegal-filepath filename))
        (if (export-midi-to-file filename)
            (c-display "MIDI file exported to" filename)
            (c-display "Failed to export MIDI file")))))

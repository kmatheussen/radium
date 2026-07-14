(provide 'midi_export.scm)


(define *midi-export-auto-assign-MIDI-channel* #t)


;; ============================================================
;; MIDI channel clash detection
;; ============================================================

;; Helper: add val to the set stored at key in the alist.
;; Returns the (possibly modified) alist.
(define (alist-cons-set alist key val)
  (let ((pair (assoc key alist)))
    (if pair
        (let ((lst (cdr pair)))
          (if (member val lst)
              alist
              (begin (set-cdr! pair (cons val lst)) alist)))
        (cons (cons key (list val))
              alist))))

;; Helper: check interval overlap. Intervals are (start . end) pairs of s7 ratios.
(define (intervals-overlap? start-a end-a start-b end-b)
  (and (< start-a end-b) (< start-b end-a)))

;; Helper: check if any interval pair between two instruments overlaps.
(define (instruments-overlap? intervals-a intervals-b)
  (let outer ((as intervals-a))
    (if (null? as)
        #f
        (let* ((a (car as))
               (sa (car a))
               (ea (cdr a)))
          (let inner ((bs intervals-b))
            (if (null? bs)
                (outer (cdr as))
                (let* ((b (car bs))
                       (sb (car b))
                       (eb (cdr b)))
                  (if (intervals-overlap? sa ea sb eb)
                      #t
                      (inner (cdr bs))))))))))

;; Return a hash table mapping instrument_id → list of instrument_ids
;; that have overlapping notes in the given block.
(define (get-MIDI-channel-clash-map blocknum)
  (let* ((num-tracks (<ra> :get-num-tracks blocknum))
         (instrument-intervals '()))

    ;; Phase 1: Collect (start . end) intervals per instrument
    (let loop ((tracknum 0))
      (if (< tracknum num-tracks)
          (let ((instr (<ra> :get-instrument-for-track tracknum blocknum -1)))
            (if (<ra> :is-legal-instrument instr)
                (let ((notes-vec (<ra> :get-all-notes tracknum blocknum -1)))
                  (let note-loop ((notes (vector->list notes-vec)))
                    (if (null? notes)
                        (loop (+ tracknum 1))
                        (let ((note (car notes)))
                          (let* ((pair (assoc instr instrument-intervals))
                                 (intervals (if pair (cdr pair) '()))
                                 (start (<ra> :get-note-start note tracknum blocknum -1))
                                 (end (<ra> :get-note-end note tracknum blocknum -1))
                                 (new-intervals (cons (cons start end) intervals)))
                            (if pair
                                (set-cdr! pair new-intervals)
                                (set! instrument-intervals
                                      (cons (cons instr new-intervals) instrument-intervals)))
                            (note-loop (cdr notes)))))))
                (loop (+ tracknum 1))))))
    
    (define clash-alist '())
    
    ;; Phase 2: Pairwise overlap check
    (let ((instruments (map car instrument-intervals)))
      (let outer ((is instruments) (ca clash-alist))
        (if (null? is)
            (set! clash-alist ca)
            (let* ((i (car is))
                   (intervals-i (cdr (assoc i instrument-intervals))))
              (let inner ((js (cdr is)) (ca ca))
                (if (null? js)
                    (outer (cdr is) ca)
                    (let* ((j (car js))
                           (intervals-j (cdr (assoc j instrument-intervals))))
                      (if (instruments-overlap? intervals-i intervals-j)
                          (inner (cdr js)
                                 (alist-cons-set (alist-cons-set ca i j) j i))
                          (inner (cdr js) ca)))))))))
    
    ;; Phase 3: Convert clash alist to hash table
    (let ((table (make-hash-table)))
      ;; Initialize all instruments with empty clash lists
      (for-each (lambda (pair)
                  (let ((instr (car pair)))
                    (if (not (assoc instr clash-alist))
                        (hash-table-set! table instr '()))))
                instrument-intervals)
      ;; Insert clash data
      (for-each (lambda (pair)
                  (hash-table-set! table (car pair) (cdr pair)))
                clash-alist)
      table)))


;; Helper: return a list of all keys in a hash table.
(define (hash-table-keys ht)
  (let ((keys '()))
    (for-each (lambda (kv) (set! keys (cons (car kv) keys))) ht)
    keys))

;; Assign MIDI channels (0-15) to block instruments using greedy graph coloring.
;; Returns a hash table mapping instrument_t → midi_channel.
(define (create-MIDI-channel-map-for-block blocknum)
  (let* ((clash-map (get-MIDI-channel-clash-map blocknum))
         (instruments (hash-table-keys clash-map))
         (degree (lambda (instr)
                   (let ((clashes (hash-table-ref clash-map instr)))
                     (if clashes
                         (length clashes)
                         0))))
         (sorted-instruments (sort instruments
                                   (lambda (a b)
                                     (> (degree a) (degree b)))))
         (channel-map (make-hash-table)))

    (for-each
     (lambda (instr)
       (let* ((clashes (or (hash-table-ref clash-map instr) '()))
              (used-channels (keep (lambda (c)
                                     c)
                                   (map (lambda (other)
                                          (hash-table-ref channel-map other))
                                        clashes)))
              (available-channel (let loop ((ch 0))
                                   (if (> ch 15)
                                       #f
                                       (if (member ch used-channels)
                                           (loop (+ ch 1))
                                           ch)))))
         (if available-channel
             (hash-table-set! channel-map instr available-channel)
             ;; Fallback: pick channel with fewest conflicts
             (let ((best-channel (let loop ((ch 0) (best-ch 0) (best-count 999))
                                   (if (> ch 15)
                                       best-ch
                                       (let ((count (length (keep (lambda (c)
                                                                    (= c ch))
                                                                  used-channels))))
                                         (if (< count best-count)
                                             (loop (+ ch 1) ch count)
                                             (loop (+ ch 1) best-ch best-count)))))))
               (hash-table-set! channel-map instr best-channel)))))
     sorted-instruments)

    channel-map))


;; ============================================================
;; MIDI binary encoding primitives
;; ============================================================

;; Encode a variable-length quantity as a byte list.
;; Collects 7-bit groups MSB-first, then sets bit 7 on all
;; bytes except the last (which is the LSB).
(define (encode-varlen val)
  (let ((chunks '()))
    (let loop ((v val))
      (set! chunks (cons (logand v #x7F) chunks))
      (set! v (ash v -7))
      (if (> v 0) (loop v)))
    (let* ((n (length chunks))
           (last (- n 1)))
      (let iter ((cs chunks)
                 (i 0)
                 (result '()))
        (if (null? cs)
            (reverse result)
            (let ((b (car cs)))
              (if (< i last)
                  (iter (cdr cs) (+ i 1) (cons (logior b #x80) result))
                  (iter (cdr cs) (+ i 1) (cons b result)))))))))

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
  (let* ((mpqn (floor (/ 60000000 bpm)))
         (b0 (logand (ash mpqn -16) #xFF))
         (b1 (logand (ash mpqn -8) #xFF))
         (b2 (logand mpqn #xFF)))
    (encode-meta-event delta #x51 (list b0 b1 b2))))

(***assert*** (encode-set-tempo 0 120)               ;; 500000 us/qn = 0x07A120
              (list #x00 #xFF #x51 #x03 #x07 #xA1 #x20))


;; Time Signature: FF 58 04 nn dd cc bb
;; dd = log2(denominator). 4->2, 8->3, 2->1, 1->0.
(define (encode-time-signature delta numerator denominator)
  (let* ((denom-exp (cond ((= denominator 1) 0)
                          ((= denominator 2) 1)
                          ((= denominator 4) 2)
                          ((= denominator 8) 3)
                          ((= denominator 16) 4)
                          ((= denominator 32) 5)
                          (else (round (/ (log denominator) (log 2)))))))
    (encode-meta-event delta #x58 (list numerator (max 0 denom-exp) 24 8))))

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

;; Convert a Place (represented as an s7 ratio a/b where
;; a = line*dividor + counter,  b = dividor) to MIDI ticks.
;; ticks = (position_in_lines / lpb) * resolution
(define (place-to-ticks place-ratio lpb resolution)
  (let* ((num (numerator place-ratio))
         (den (denominator place-ratio))
         (ticks (/ (* num resolution) (* den lpb))))
    (max 0 (round ticks))))

(***assert*** (place-to-ticks 0 4 480)                ;; tick 0
              0)
(***assert*** (place-to-ticks 4 4 480)                ;; 1 beat at lpb=4 → 480 ticks
              480)

;; Collect tempo change events: returns ((tick tempo bpm) ...)
(define (collect-tempo-events blocknum lpb resolution)
  (let* ((n (<ra> :num-bpms blocknum))
         (events '()))
    (let loop ((i 0))
      (if (>= i n)
          (reverse events)
          (let* ((place (<ra> :get-bpm-place i blocknum))
             (bpm (<ra> :get-bpm i blocknum))
             (reltempo (<ra> :get-reltempo blocknum))
             (effective-bpm (* bpm reltempo))
             (tick (place-to-ticks place lpb resolution)))
            (set! events (cons (list tick 'tempo effective-bpm) events))
            (loop (+ i 1)))))))


;; Collect signature events.
;; getSignature returns Place{0, numerator, denominator} => s7 ratio num/den.
(define (collect-signature-events blocknum lpb resolution)
  (let* ((n (<ra> :num-signatures blocknum))
         (events '()))
    (let loop ((i 0))
      (if (>= i n)
          (reverse events)
          (let* ((sig-place (<ra> :get-signature-place i blocknum))
                 (num-sig (<ra> :get-signature-numerator i blocknum))
                 (den-sig (<ra> :get-signature-denominator i blocknum))
                 (tick (place-to-ticks sig-place lpb resolution)))
            (set! events (cons (list tick 'signature num-sig den-sig) events))
            (loop (+ i 1)))))))


;; Collect note events from one track.
;; Returns ((tick 'note-on pitch velocity) (tick 'note-off pitch 0) ...)
(define (collect-note-events tracknum blocknum lpb resolution)
  (let* ((notes-vec (<ra> :get-all-notes tracknum blocknum -1))
         (events '()))
    (for-each
     (lambda (note)
       (let* ((start-ratio (<ra> :get-note-start note tracknum blocknum -1))
              (end-ratio (<ra> :get-note-end note tracknum blocknum -1))
              (pitch-float (<ra> :get-note-value note tracknum blocknum -1))
              (vel-float (<ra> :get-velocity-value 0 note tracknum blocknum -1))
              (pitch (max 0 (min 127 (round pitch-float))))
              (velocity (max 1 (min 127 (round (* vel-float 127)))))
              (start-tick (place-to-ticks start-ratio lpb resolution))
              (end-tick (place-to-ticks end-ratio lpb resolution)))
         (when (> end-tick start-tick)
           (set! events (cons (list start-tick 'note-on pitch velocity) events))
           (set! events (cons (list end-tick 'note-off pitch 0) events)))))
     (vector->list notes-vec))
    (reverse events)))


(define (sort-by-tick events)
  (sort events (lambda (a b) (< (car a) (car b)))))


;; Encode a sorted sequence of (tick tag ...) events into a byte list,
;; using delta encoding. 'end-tick' is where the track ends.
(define (encode-event-sequence events event-encoder end-tick)
  (let ((prev-tick 0)
        (result '()))
    (for-each
     (lambda (event)
       (let* ((tick (car event))
              (delta (- tick prev-tick)))
         (set! prev-tick tick)
         (set! result
               (append result (event-encoder delta event)))))
     events)
    (append result (event-encoder (- end-tick prev-tick) #f))))


(define (conductor-event-encoder delta event)
  (if (not event)
      (encode-end-of-track delta)
      (let ((tag (cadr event)))
        (cond
         ((eq? tag 'tempo)
          (encode-set-tempo delta (caddr event)))
         ((eq? tag 'signature)
          (encode-time-signature delta (caddr event) (cadddr event)))
         (else '())))))


(define (make-note-event-encoder channel)
  (lambda (delta event)
    (if (not event)
        (encode-end-of-track delta)
        (let ((tag (cadr event))
              (pitch (caddr event))
              (vel (cadddr event)))
          (cond
           ((eq? tag 'note-on)
            (encode-note-on delta channel pitch vel))
           ((eq? tag 'note-off)
            (encode-note-off delta channel pitch))
           (else '()))))))


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
        (<ra> :write8-to-file file #x4D)  ;; M
        (<ra> :write8-to-file file #x54)  ;; T
        (<ra> :write8-to-file file #x72)  ;; r
        (<ra> :write8-to-file file #x6B)));; k
  (<ra> :write-be32-to-file file length))


;; Write an entire track chunk (header + all bytes)
(define (midi-write-track-chunk bytes file)
  (midi-write-chunk-header "MTrk" (length bytes) file)
  (for-each (lambda (b) (<ra> :write8-to-file file b)) bytes))


;; ============================================================
;; Main export function
;; ============================================================

(define (export-midi-to-file filename)
  (let* ((blocknum -1)
         (windownum -1)
         (lpb (<ra> :get-main-lpb))
         (resolution (* lpb 120))
         (num-tracks (<ra> :get-num-tracks blocknum))

         (conductor-events
          (sort-by-tick
           (append (collect-tempo-events blocknum lpb resolution)
                   (collect-signature-events blocknum lpb resolution))))

         (note-tracks '()))

    ;; Ensure initial tempo at tick 0 (use main BPM)
    (let ((tempo-at-0 (keep (lambda (e) (and (eq? (cadr e) 'tempo) (= (car e) 0)))
                            conductor-events)))
      (if (null? tempo-at-0)
        (set! conductor-events
              (cons (list 0 'tempo
                          (* (<ra> :get-main-bpm) (<ra> :get-reltempo blocknum)))
                    conductor-events))))

    ;; Ensure initial time signature at tick 0 (use main signature)
    (let ((sig-at-0 (keep (lambda (e) (and (eq? (cadr e) 'signature) (= (car e) 0)))
                          conductor-events)))
      (if (null? sig-at-0)
        (set! conductor-events
              (cons (list 0 'signature
                          (<ra> :get-main-signature-numerator)
                          (<ra> :get-main-signature-denominator))
                    conductor-events))))
    (set! conductor-events (sort-by-tick conductor-events))

    ;; Collect note tracks (only MIDI instrument tracks with notes)
    (let ((auto-channel-map (if *midi-export-auto-assign-MIDI-channel*
                                (create-MIDI-channel-map-for-block blocknum)
                                #f)))
      (let loop ((tracknum 0))
        (if (< tracknum num-tracks)
          (let* ((notes-vec (<ra> :get-all-notes tracknum blocknum windownum))
                 (n (vector-length notes-vec))
                 (channel (if auto-channel-map
                            (let ((instr (<ra> :get-instrument-for-track tracknum blocknum windownum)))
                              (if (<ra> :is-illegal-instrument instr)
                                -1
                                (or (hash-table-ref auto-channel-map instr) -1)))
                            (<ra> :get-track-midi-channel tracknum blocknum windownum))))
            (if (and (> n 0) (>= channel 0) (<= channel 15))
              (let ((events (sort-by-tick
                              (collect-note-events tracknum blocknum lpb resolution))))
                (c-display "EVENTS:" events)
                (if (not (null? events))
                  (set! note-tracks
                        (cons (cons channel events) note-tracks)))))
            (loop (+ tracknum 1))))))
    (set! note-tracks (reverse note-tracks))

    ;; Compute last tick across all tracks (plus padding)
    (let ((last-tick 0))
      (for-each (lambda (e) (set! last-tick (max last-tick (car e)))) conductor-events)
      (for-each (lambda (tr)
                  (for-each (lambda (e) (set! last-tick (max last-tick (car e))))
                            (cdr tr)))
                note-tracks)
      (set! last-tick (+ last-tick resolution))

      ;; Encode all track data
      (let* ((conductor-bytes (encode-event-sequence conductor-events conductor-event-encoder last-tick))
             (note-track-bytes (map (lambda (tr)
                                      (let* ((channel (car tr))
                                             (events (cdr tr))
                                             (encoder (make-note-event-encoder channel)))
                                        (encode-event-sequence events encoder last-tick)))
                                    note-tracks))
             (num-mtrk (1+ (length note-track-bytes))))

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
                (<ra> :write-be16-to-file file num-mtrk)    ;; Number of tracks
                (<ra> :write-be16-to-file file resolution)  ;; PPQ

                ;; Conductor track
                (midi-write-track-chunk conductor-bytes file)

                ;; Note tracks
                (for-each (lambda (bytes)
                            (midi-write-track-chunk bytes file))
                          note-track-bytes)

                (<ra> :close-file file)
                #t)))))))


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

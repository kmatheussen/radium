(provide 'import_xm.scm)
(my-require 'import_mod.scm)


;; ================================================================
;; XM module import for Radium.
;; Reuses import_mod.scm for event handling and Radium integration.
;; ================================================================


;; Configuration.

(set! *xm-pan-values* (make-vector 32 128))

(define *xm-frequency-linear* #f)
(define (set-xm-frequency-linear! linear?)
  (set! *xm-frequency-linear* linear?))



;; --------------------------------------------------------------------
;; Global state (set by Python)
;; --------------------------------------------------------------------

(define *xm-num-channels* 0)
(define *xm-num-patterns* 256)

(define *xm-events* '())
(define (start-adding-xm-events!)
  (set! *xm-events* '()))

(define (add-xm-trackline patternnum channelnum linenum note instrumentnum volume effectnum effectvalue)
  ;; Generate note FIRST so velocity/effect events come after it.
  ;; send-note-event-to-radium in import_mod.scm passes (cdr events) to get-velocities,
  ;; meaning only events AFTER the note are seen by the velocity engine.
  (if (and (= 0 patternnum)
           (= 0 channelnum))
      (c-display "=====EVENT. :line" linenum ":note" note ":instrumentnum" instrumentnum ":volume" volume ":effectnum" effectnum ":effectvalue" effectvalue))
  
  (define (effect-x)
    (ash effectvalue -4))
  (define (effect-y)
    (logand effectvalue #x0f))

  ;; Helper for combined effects (5/6): push volume slide if there is one
  (define (push-vol-slide-if-any!)
    (let ((volchange (- (effect-x) (effect-y))))
      (push! *xm-events* (m-e :velocity-slide
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :value volchange))))

  ;; Note event
  (cond
   ((= note 97)
    ;; XM note byte 97 = stop note (key-off): release currently playing note on this channel.
    ;; Translate to :stop event, which import_mod.scm handles as note-end.
    (push! *xm-events* (m-e :stop
                            :line linenum
                            :pattern patternnum
                            :channel channelnum)))
   ((> note 0)
    (push! *xm-events* (m-e :note
                            :instrumentnum instrumentnum
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value note))))

  ;; XM volume column byte decoding (FT2 spec):
  ;; 0x00-0x0F: (no effect)
  ;; 0x10-0x50: Set volume        → volume - 0x10 (0-64), floor at 1
  ;; 0x60-0x6F: Volume slide down → -(volume - 0x60)
  ;; 0x70-0x7F: Volume slide up   → +(volume - 0x70)
  ;; 0x80-0x8F: Fine vol slide dn → -(volume - 0x80)
  ;; 0x90-0x9F: Fine vol slide up → +(volume - 0x90)
  ;; (Vibrato/panning/portamento via volume column not yet handled)
  (cond
   ((and (>= volume 16) (<= volume 80))
    ;; 0x10-0x50: Set volume (0-64). FT2 volume 0 is quietest, not silence, so floor at 1.
    (push! *xm-events* (m-e :velocity
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (max 1 (- volume 16)))))
   ((and (>= volume 96) (<= volume 111))
    ;; 0x60-0x6F: Volume slide down
    (push! *xm-events* (m-e :velocity-slide
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (- (- volume 96)))))
   ((and (>= volume 112) (<= volume 127))
    ;; 0x70-0x7F: Volume slide up
    (push! *xm-events* (m-e :velocity-slide
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (- volume 112))))
   ((and (>= volume 128) (<= volume 143))
    ;; 0x80-0x8F: Fine volume slide down
    (push! *xm-events* (m-e :fine-velocity-slide
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (- (- volume 128)))))
   ((and (>= volume 144) (<= volume 159))
    ;; 0x90-0x9F: Fine volume slide up
    (push! *xm-events* (m-e :fine-velocity-slide
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (- volume 144)))))

  ;; Effect handling — same encoding as MOD for effects 0x0-0xF
  (cond
   ;; 0x0: Arpeggio (XM 3.1 = MOD 0xy)
   ((and (= effectnum 0) (> effectvalue 0))
    (push! *xm-events* (m-e :arpeggio
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (effect-x)
                            :value2 (effect-y))))

   ;; 0x1/0x2: Portamento up/down (XM 3.2/3.3 = MOD 1xx/2xx)
   ((and (or (= effectnum 1) (= effectnum 2))
         (> effectvalue 0))
    (push! *xm-events* (m-e :pitch-slide
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (if (= effectnum 1)
                                       effectvalue
                                       (- effectvalue)))))

   ;; 0x3: Tone portamento (XM 3.4 = MOD 3xx)
   ((= effectnum 3)
    (push! *xm-events* (m-e :slide-to-note
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value effectvalue)))

   ;; 0x4: Vibrato (XM 3.5 = MOD 4xy)
   ((= effectnum 4)
    (push! *xm-events* (m-e :vibrato
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (effect-x)
                            :value2 (effect-y))))

   ;; 0x5: Portamento + Volume slide (XM 3.6 = MOD 5xy)
   ((= effectnum 5)
    (push! *xm-events* (m-e :slide-to-note
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value 0))
    (push-vol-slide-if-any!))

   ;; 0x6: Vibrato + Volume slide (XM 3.7 = MOD 6xy)
   ((= effectnum 6)
    (push! *xm-events* (m-e :vibrato
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value 0))
    (push-vol-slide-if-any!))

   ;; 0x7: Tremolo (XM 3.8 = MOD 7xy)
   ((= effectnum 7)
    (push! *xm-events* (m-e :tremolo
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (effect-x)
                            :value2 (effect-y))))

   ;; 0x8: Set panning (XM 3.9). XM-only, not in MOD.
   ;; 00=left, 80=center, FF=right. Stored per-channel, applied in get-pan-value.
   ((= effectnum 8)
    (when (< channelnum (vector-length *xm-pan-values*))
      (vector-set! *xm-pan-values* channelnum effectvalue)))

   ;; 0x9: Sample offset (XM 3.10 = MOD 9xx)
   ((= effectnum 9)
    (push! *xm-events* (m-e :sample-offset
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value effectvalue)))

   ;; 0xA: Volume slide (XM 3.11 = MOD Axy)
   ((= effectnum 10)
    (push-vol-slide-if-any!))

   ;; 0xB: Position jump (XM 3.12 = MOD Bxx)
   ((= effectnum 11)
    (push! *xm-events* (m-e :position-jump
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :instrumentnum -1
                            :value effectvalue)))

   ;; 0xC: Set volume (XM 3.13 = MOD Cxx)
   ((= effectnum 12)
    (push! *xm-events* (m-e :velocity
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :value (min effectvalue 64))))

   ;; 0xD: Pattern break (XM 3.14 = MOD Dxy)
   ((= effectnum 13)
    (push! *xm-events* (m-e :break
                            :line linenum
                            :pattern patternnum
                            :channel channelnum
                            :instrumentnum -1
                            :value (+ (* (effect-x) 10) (effect-y)))))

   ;; 0xE: E-commands (XM 3.15 = MOD Exy)
   ((= effectnum 14)
    (let ((enum (effect-x))
          (evalue (effect-y)))
      (cond
       ;; E1x/E2x: Fine portamento up/down
       ((or (= enum 1) (= enum 2))
        (push! *xm-events* (m-e :fine-pitch-slide
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :value (if (= enum 1)
                                           (- evalue)
                                           evalue))))
       ;; E5x: Set fine-tune
       ((= enum 5)
        (push! *xm-events* (m-e :finetune
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :value (if (< evalue 8)
                                           evalue
                                           (- evalue 16)))))
       ;; E6x: Jump loop
       ((= enum 6)
        (push! *xm-events* (m-e :loop
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :instrumentnum -1
                                :value evalue)))
       ;; E9x: Retrigger note
       ((= enum 9)
        (push! *xm-events* (m-e :retrigger-note
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :value evalue)))
       ;; EAx/EBx: Fine volume slide up/down
       ((or (= enum 10) (= enum 11))
        (when (> evalue 0)
          (push! *xm-events* (m-e :fine-velocity-slide
                                  :line linenum
                                  :pattern patternnum
                                  :channel channelnum
                                  :value (if (= enum 10)
                                             evalue
                                             (- evalue))))))
       ;; ECx: Note cut
       ((= enum 12)
        (push! *xm-events* (m-e :velocity
                                :tick evalue
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :value 0)))
       ;; EDx: Note delay
       ((= enum 13)
        (push! *xm-events* (m-e :delay-note
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :value evalue)))
       ;; EEx: Pattern delay
       ((= enum 14)
        (push! *xm-events* (m-e :pattern-delay
                                :line linenum
                                :pattern patternnum
                                :channel channelnum
                                :value evalue))))))

   ;; 0xF: Set speed/tempo (XM 3.16 = MOD Fxx)
   ((= effectnum 15)
    (let ((value (max effectvalue 1)))
      (push! *xm-events* (m-e (if (< effectvalue 32)
                                  :tpd
                                  :bpm)
                              :line linenum
                              :pattern patternnum
                              :channel channelnum
                              :instrumentnum -1
                              :value value))))))
(define (stop-adding-xm-events!)
  ;; Events are collected. Now reverse and process.
  (set! *xm-events* (reverse *xm-events*)))


;; --------------------------------------------------------------------
;; Playlist
;; --------------------------------------------------------------------

(define *xm-playlist* '())
(define *xm-pattern-format* "")

(define (set-xm-playlist! daslist)
  (set! *xm-playlist* daslist))


;; --------------------------------------------------------------------
;; Instrument list
;; --------------------------------------------------------------------

(define *xm-instrumentlist* (vector))

(define (set-xm-instrumentlist! daslist)
  (set! *xm-instrumentlist* daslist))

;; Instrument vector layout:
;;   0: instrument name (base64)
;;   1: XI temp file path (base64, empty if no samples)
;;   2: Radium instrument number (filled in later)
;;   3: sample volume (0-64)
;;   4: sample finetune (-128..127)
;;   5: loop start (frames)
;;   6: loop length (frames)

(define (xm-instrument-name instr)
  (<ra> :from-base64 (instr 0)))

(define (xm-instrument-xi-filepath instr)
  (define raw (<ra> :from-base64 (instr 1)))
  (if (string=? raw "")
      (<ra> :create-illegal-filepath)
      (<ra> :get-path raw)))

(define (xm-instrument-num-samples instr)
  ;; Return >0 if instrument has sample data (XI file path is non-empty)
  (if (string=? "" (<ra> :from-base64 (instr 1)))
      0
      1))

(define (xm-instrument-radium-num instr)
  (instr 2))

(define (set-xm-instrument-radium-num! instr num)
  (vector-set! instr 2 num))

(define (xm-instrument-sample-volume instr)
  (instr 3))

(define (xm-instrument-sample-finetune instr)
  (instr 4))

(define (xm-instrument-sample-loop-start instr)
  (instr 5))

(define (xm-instrument-sample-loop-length instr)
  (instr 6))


;; --------------------------------------------------------------------
;; Pattern format
;; --------------------------------------------------------------------

(define *xm-num-channels* 4)

(define (set-xm-pattern-format num-channels num-patterns)
  (set! *xm-num-channels* num-channels)
  (set! *xm-num-patterns* num-patterns))


;; --------------------------------------------------------------------
;; Default tempo/BPM
;; --------------------------------------------------------------------

(define *xm-default-tempo* 6)
(define *xm-default-bpm* 125)

(define (set-xm-default-tempo! tempo bpm)
  (set! *xm-default-tempo* tempo)
  (set! *xm-default-bpm* bpm))


(set! *pitch-transpose* 0)  ;; XM notes are already in MIDI pitch space, no period conversion needed


;; NOTE: instrument-sample-filename in import_mod.scm now handles absolute
;; paths (XM temp files starting with "/") as well as relative MOD sample paths.


#||
(define (send-note-event-to-radium note channelnum events instrument tpds num-lines)
  (let* ((velocities (get-velocities note 64 events tpds))
         (start-place (car (car velocities)))
         (stop-place (car (last velocities)))
         (pitch (note :value)))
    (<ra> :add-note pitch 0.8 start-place stop-place channelnum)))
||#

;; --------------------------------------------------------------------
;; If #t, XM instruments are created as "XI Instrument" plugins (full envelope/polyphony).
;; If #f, XM instruments are created as "Sample Player" plugins (simpler, MOD-compatible).
;; Both can load .xi sample files from setInstrumentSample.
;; --------------------------------------------------------------------

(define (send-xm-instruments-to-radium instruments)
  (if *use-xi-instrument*
      (begin
        (<ra> :show-progress-window-message-if-open "Creating XI instruments")
        (for-each (lambda (instrument)
                    (define xi-file (xm-instrument-xi-filepath instrument))
                    (define instrument-name (xm-instrument-name instrument))
                    
                    (define radium-num
                      (<ra> :create-audio-instrument
                            "XI Instrument" "XI Instrument"
                            instrument-name 0 0 #f))
                    
                    (<ra> :autoposition-instrument radium-num)
                    (<ra> :connect-audio-instrument-to-main-pipe radium-num)
                    
                    (set-xm-instrument-radium-num! instrument radium-num)
                    
                    (when (> (xm-instrument-num-samples instrument) 0)
                      (if (<ra> :is-legal-filepath xi-file)
                          (<ra> :set-instrument-sample radium-num xi-file))))
                  (vector->list instruments)))
      ;; Using Sample Player — instruments created by send-instruments-to-radium in import_mod.scm
      #f))


;; --------------------------------------------------------------------
;; send-xm-instruments-to-radium
;; --------------------------------------------------------------------


;; --------------------------------------------------------------------
;; Main: send-xm-to-radium


(define (send-xm-to-radium)
  ;; Ensure pan table covers all channels
  (when (> *xm-num-channels* (vector-length *xm-pan-values*))
    (let ((new-pan (make-vector *xm-num-channels* 128)))
      (for-each (lambda (i)
                  (vector-set! new-pan i (vector-ref *xm-pan-values* i)))
                (iota (vector-length *xm-pan-values*)))
      (set! *xm-pan-values* new-pan)))
  
  ;; Convert XM instrument vectors to MOD-compatible format (8 elements):
  ;;   0: name, 1: xi-filepath, 2: num-samples, 3: finetune, 4: volume, 5: loop-start, 6: loop-length, 7: radium-num
  ;; *use-xi-instrument* controls num-samples: 0=prevent MOD pipeline from creating duplicates, 1=let MOD pipeline create Sample Players.
  (define mod-instruments
    (list->vector
     (map (lambda (xm-instr)
            (let ((mod-instr (make-vector 8)))
              (vector-set! mod-instr 0 (<ra> :from-base64 (xm-instr 0)))  ;; name
              (vector-set! mod-instr 1 (<ra> :from-base64 (xm-instr 1)))  ;; xi filepath (for instrument-sample-filename)
              (vector-set! mod-instr 2 (if *use-xi-instrument* 0 (xm-instrument-num-samples xm-instr)))  ;; num-samples: 0=XI (prevent MOD creation), 1=Sample Player (only for instruments with samples)
              (vector-set! mod-instr 3 0)   ;; finetune: already in XI frequency table (MOD conversion is Protracker-specific)
              (vector-set! mod-instr 4 (xm-instrument-sample-volume xm-instr))    ;; volume (0-64)
              (vector-set! mod-instr 5 0)   ;; loop start: XI file handles this (byte→frame conversion done by load_xi_instrument)
              (vector-set! mod-instr 6 0)   ;; loop length: XI file handles this
              (vector-set! mod-instr 7 #f)  ;; radium-num (set after creation)
              mod-instr))
          (vector->list *xm-instrumentlist*))))
  
  ;; Create XI instruments and set radium-nums
  (send-xm-instruments-to-radium *xm-instrumentlist*)
  
  ;; Copy radium-nums from XM instruments to MOD-compatible instruments
  (for-each (lambda (xm-instr mod-instr)
              (vector-set! mod-instr 7 (xm-instr 2)))  ;; radium-num from XM vector
            (vector->list *xm-instrumentlist*)
            (vector->list mod-instruments))
  
  ;; Use the MOD pipeline for event processing, pattern building, and sending
  (define num-vel-events (length (keep (lambda (e) (eq? (e :type) :velocity)) *xm-events*)))
  (c-display "XM velocity events before process-events:" num-vel-events)

  (define stuff
    (process-events *xm-playlist*
                    mod-instruments
                    *xm-events*
                    64              ;; max-num-lines
                    *xm-num-channels*))
  
  (define playlist (car stuff))
  (define instruments (cadr stuff))
  (define patterns (caddr stuff))
  
  (<ra> :show-progress-window-message-if-open "Sending events to Radium")
  (send-events-to-radium playlist instruments patterns)
  
  ;; XM files have their own BPM from the header; override the MOD default (125).
  (<ra> :set-main-bpm *xm-default-bpm*)
  
  (<ra> :show-progress-window-message-if-open "Loading graphical data into memory")
  (<ra> :internal_update-all-block-graphics))


;; --------------------------------------------------------------------
;; Main entry point
;; --------------------------------------------------------------------

(define (load-xm-module filename)
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
          (try-finally
           :try (lambda ()
                  (<ra> :start-ignoring-undo)
                  
                  (<ra> :eval-python "import import_xm2")
                  (<ra> :eval-python "import_xm2=reload(import_xm2)")
                  
                  (<ra> :eval-python
                        (<-> "import_xm2.import_xm(\""
                             (<ra> :get-base64-from-filepath filename)
                             "\")"))
                   
                  (send-xm-to-radium))
           
           :finally (lambda ()
                      (<ra> :stop-ignoring-undo)
                      (<ra> :reset-undo))))
   :finally (lambda ()
              (<ra> :close-progress-window))))


(delafina (async-load-xm-module :filename (<ra> :create-illegal-filepath))
  (<ra> :schedule 1
        (lambda ()
          (when (<ra> :ask-are-you-sure-song-has-changed)
            (if (<ra> :is-illegal-filepath filename)
                (create-file-requester "Choose XM file" (<ra> :create-illegal-filepath) "XM files" "*.xm *.XM xm.* XM.*" #t "" #f #t -1 load-xm-module)
                (load-xm-module filename)))
          #f)))

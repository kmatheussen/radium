(provide 'seqblock_audio.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)

(define (midi-to-hz midi)
  (* 440
     (expt 2 (/ (- midi 69)
                12))))

(define (hz-to-midi hz)
  (assert (> hz 0))
  (+ 69
     (* 12
        (/ (log (/ hz
                   440))
           (log 2)))))

(define-class (<seqblock-gui-functions>)
  ;; Speed 1.0 = pitch 0
  ;; Speed 2.0 = pitch -12
  ;; Speed 4.0 = pitch -24
  ;; speed 0.5 = pitch 12
  ;; speed 0.25 = pitch 24

  :get-slider-from-pitch (pitch)
  (scale pitch
         -24 24
         0 1)
  
  :get-pitch-from-slider (slider)
  (scale slider
         0 1
         -24 24)
  
  :get-pitch-from-speed (speed)
  (- (hz-to-midi (* (/ 1.0 speed)
                    440))
     69)

  :get-speed-from-pitch (pitch)
  (/ 440
     (midi-to-hz (+ 69 pitch)))
  
  :get-slider-from-speed (speed)
  (this->get-slider-from-pitch (this->get-pitch-from-speed speed))
  
  :get-speed-from-slider (slider)
  (this->get-speed-from-pitch (this->get-pitch-from-slider slider))
  )

#!!
(define funcs (<new> :seqblock-gui-functions))
(funcs :get-pitch-from-speed (scale -3
                                    0 -12
                                    1.0 0.5))
(funcs :get-pitch-from-speed 0.5)
(funcs :get-pitch-from-speed 2.0)
(funcs :get-pitch-from-speed 1.0)
(funcs :get-pitch-from-speed 1.5)
(funcs :get-pitch-from-speed 2.0)
(funcs :get-pitch-from-speed 2.5)
(funcs :get-speed-from-pitch 0)
(funcs :get-speed-from-pitch -15.86313713864835)

(funcs :get-slider-from-speed 2.0)
(funcs :get-slider-from-speed 0.5)

(funcs :get-slider-from-speed 0.12)
(funcs :get-slider-from-speed 0.25)
(funcs :get-slider-from-speed 0.5)
(funcs :get-slider-from-speed 1.5)
(funcs :get-slider-from-speed 2.0)
(funcs :get-slider-from-speed 4.0)
(funcs :get-slider-from-speed 4.5)

(<ra> :get-seqblock-speed 0 1)

!!#

#||
* Resampler type
* Resampler factor / Pitch
* Gain + button to set normalized gain.
. Reverse? (probably easier to create new file than to modify samplereader)

* Granulation:
  * Grain frequency (ms)
  * Grain length (ms)
  * Grain ramp (fade in + fade out duration) (% of grain length)
  * Grain jitter (%). 100: Very random position of grains. 0: Same distance between each grain.
||#

(define *seqblock-guis* (make-hash-table 32 =))

(define (create-audio-seqblock-gui seqblocknum seqtracknum)
  (define funcs (<new> :seqblock-gui-functions))
  
  (define seqblockid (<ra> :get-seqblock-id seqblocknum seqtracknum))
  (define has-started #f)
  (define main-layout (<gui> :vertical-layout));flow-layout))

  (define gain-group (<gui> :group "Gain"))

  (define gain-slider (<gui> :horizontal-slider "Gain (Db): " -35 (<ra> :gain-to-db (<ra> :get-seqblock-gain seqblockid)) 35
                             (lambda (db)
                               (when has-started
                                 ;;(c-display "setting gain to db:" db ". gain:" (<ra> :db-to-gain db))
                                 (<ra> :set-seqblock-gain (<ra> :db-to-gain db) seqblockid)))))
  
  (<gui> :add gain-group (<gui> :horizontal-layout
                                gain-slider
                                (<gui> :button "Normalize!"
                                       (lambda ()
                                         (when has-started
                                           (<gui> :set-value gain-slider (<ra> :gain-to-db (get-normalized-seqblock-gain seqblockid))))))))
;                                                
;                                                <ra> :set-seqblock-gain (get-normalized-seqblock-gain seqblockid) seqblockid)
;                                         (

  (define volume-automation-checkbox (<gui> :checkbox "Volume automation" (<ra> :get-seqblock-automation-enabled 0 seqblockid) #f
                                            (lambda (ison)
                                              (when has-started
                                                (<ra> :set-seqblock-automation-enabled ison 0 seqblockid)))))

  (<gui> :add gain-group volume-automation-checkbox)

  (<gui> :add main-layout gain-group)

  (when #t
    (define ratio (<ra> :get-seqblock-resample-ratio seqblockid))
    (define resampler-group (if #t
                                main-layout
                                (<gui> :group (<-> "Resampling / pitch"))))

    (define (set type)
      (when has-started
        (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblockid))
        ;;(c-display "type:" type ". seqtracknum:" seqtracknum)
        (define seqblocks (map (lambda (seqblock)
                                 (if (= (seqblock :id)
                                        seqblockid)
                                     (copy-hash seqblock
                                                :resampler-type type)
                                     seqblock))
                               (<ra> :get-seqblocks-state seqtracknum)))
        (<ra> :create-gfx-seqblocks-from-state seqblocks seqtracknum)
        (<ra> :apply-gfx-seqblocks seqtracknum)
        )
      )

    (define resampler-type (<ra> :get-seqblock-resampler-type seqblockid))

    
    (<gui> :add resampler-group (<gui> :group (<-> "Resampler type")
                                       (<gui> :horizontal-layout
                                              ;;(<gui> :text "Resampler Type:     ")
                                              (<gui> :radiobutton "Sample and hold" (= resampler-type 0) (lambda (doit) (if doit (set 0))))
                                              (<gui> :radiobutton "Linear" (= resampler-type 1) (lambda (doit) (if doit (set 1))))
                                              (<gui> :radiobutton "Cubic" (= resampler-type 2) (lambda (doit) (if doit (set 2))))
                                              (<gui> :radiobutton "Sinc1" (= resampler-type 3) (lambda (doit) (if doit (set 3))))
                                              (<gui> :radiobutton "Sinc2" (= resampler-type 4) (lambda (doit) (if doit (set 4)))))))
    (if #f
        (<gui> :add resampler-group (<gui-number-input> "Rate: "
                                                        :input-type 'float
                                                        :direction 'horizontal
                                                    :min 0
                                                    :curr 1
                                                    :max 100
                                                    :callback (lambda (val)
                                                                (when has-started
                                                                  (c-display "got" val)))))
        )

    (begin
      
      (define pitch-group (<gui> :group "Pitch"))
      
      (define curr-pitch 0)
      
      (define pitch-slider #f)
      (define pitch-text-input #f)

      (define (pitch-is-different a b)
        (>= (abs (- a b))
            0.01))

      (define last-change-time -10000)
      
      (define (set-new-pitch! new-pitch)
        (set! seqtracknum (<ra> :get-seqblock-seqtrack-num seqblockid))
        (set! seqblocknum (<ra> :get-seqblock-seqblock-num seqblockid))
        
        ;;(c-display "new-pitch/old-pitch" new-pitch curr-pitch)
        (when (pitch-is-different new-pitch curr-pitch)

          (define time (<ra> :get-ms))
          (when (> time
                   (+ last-change-time 1000))
            (<ra> :undo-sequencer)
            (set! last-change-time time))
          
          (set! curr-pitch new-pitch)
          (define new-speed (funcs :get-speed-from-pitch new-pitch))

          (define seqblocks (<ra> :get-seqblocks-state seqtracknum))
          (define seqblock (seqblocks seqblocknum))
          (define new-seqblock (copy-hash seqblock
                                          :speed new-speed))
          ;;(pretty-print new-seqblock)
          (set! (seqblocks seqblocknum) new-seqblock)
          (<ra> :create-gfx-seqblocks-from-state seqblocks seqtracknum)
          (<ra> :apply-gfx-seqblocks seqtracknum)

          (if (pitch-is-different (<gui> :get-value pitch-slider) new-pitch)
              (<gui> :set-value pitch-slider (funcs :get-slider-from-pitch new-pitch)))
          (if (pitch-is-different (<gui> :get-value pitch-text-input) new-pitch)              
              (<gui> :set-value pitch-text-input new-pitch))
          ))

      (define-struct those-things
        :speed
        :slider-value
        :pitch)
      
      (delafina (get-those-things :speed (<ra> :get-seqblock-speed seqblockid))
        (make-those-things :speed speed
                           :slider-value (funcs :get-slider-from-speed speed)
                           :pitch (funcs :get-pitch-from-speed speed)))

      (let ((those-things (get-those-things)))
        (set! curr-pitch (those-things :pitch))
        (set! pitch-slider (<gui> :horizontal-slider "" 0 (those-things :slider-value) 1
                                  (lambda (slider)
                                    (when has-started
                                      (set-new-pitch! (funcs :get-pitch-from-slider slider))))))
        
        (<gui> :set-size-policy pitch-slider #t #t)
        
        (<gui> :add-paint-callback pitch-slider
               (lambda (width height)
                 (let ((those-things (get-those-things)))
                   (paint-horizontal-slider :widget pitch-slider
                                            :value (those-things :slider-value)
                                            :text (two-decimal-string (those-things :pitch))))))

        (set! pitch-text-input (<gui> :float-text -48 (those-things :pitch) 48
                                      (lambda (new-pitch)
                                        (when has-started
                                          (set-new-pitch! new-pitch))
                                        )))
        )

      (<ra> :schedule 100
            (lambda ()
              (and (<gui> :is-open pitch-slider)
                   (<gui> :is-open pitch-text-input)
                   (let ((those-things (get-those-things)))
                     (set-new-pitch! (those-things :pitch))
                     100))))
      
      (<gui> :add pitch-group (<gui> :horizontal-layout
                                     ;;(<gui> :text "Pitch:   ")
                                     pitch-slider
                                     pitch-text-input
                                     (<gui> :button "Reset"
                                            (lambda ()
                                              (<gui> :set-value pitch-slider 0.5)))))

      ;(define checkbox (<gui> :checkbox "Show pitch slider in seqblock" #f #f
      ;                        (lambda (ison)
      ;                          (when has-started
      ;                            (c-display ison)))))
      ;;(<gui> :add pitch-group checkbox)
      
      (<gui> :add resampler-group pitch-group)
      )


    (if (not (= resampler-group main-layout))
        (<gui> :add main-layout resampler-group))

    ;;(if (= 1.0 ratio)
    ;;    (<gui> :set-enabled resampler-group #f))
    )
  
  ;; Grain
  ;;;;;;;;;;;;;;;;;;;;;;;
  
  (define grain-length (<ra> :get-seqblock-grain-length seqblockid))
  ;;(define grain-frequency (<ra> :get-seqblock-grain-frequency seqblockid))
  ;;(define grain-overlap (/ grain-length grain-frequency))
  (define grain-overlap (<ra> :get-seqblock-grain-overlap seqblockid))

  ;;(define (get-grain-frequency overlap)
  ;;  (/ grain-length overlap))
    
  ;(<gui> :add main-layout (<gui> :horizontal-slider "Grain frequency (ms): " 0.1 grain-frequency 1000
  ;                               (lambda (val)
  ;                                 (when has-started
  ;                                   (c-display "  Grain Frequency FREQ1 (ms):" val)
  ;                                   (<ra> :set-seqblock-grain-frequency val seqblockid)))))

  (define grain-group (<gui> :group "Granular synthesis"))

  (define (add-parameter automationnum slider)
    (define is-enabled (<ra> :get-seqblock-automation-enabled automationnum seqblockid))
    (<gui> :set-enabled slider (not is-enabled))
    (define checkbox (<gui> :checkbox "" is-enabled #f
                                          (lambda (ison)
                                            (when has-started
                                              (<ra> :set-seqblock-automation-enabled ison automationnum seqblockid)
                                              (<gui> :set-enabled slider (not ison))))))
    (<gui> :set-tool-tip checkbox "Automation enabled")
    (<gui> :add grain-group (<gui> :horizontal-layout
                                   slider
                                   checkbox)))

  (add-parameter 1 (<gui> :horizontal-slider "Grain overlap (X): " 0.1 grain-overlap 50
                          (lambda (val)
                            (when has-started
                              ;;(set! grain-frequency (get-grain-frequency val))
                              ;;(c-display "  Grain OVERLAP (X):" grain- ". overlap:" val)
                              ;;(<ra> :set-seqblock-grain-frequency grain-frequency seqblockid)
                              (<ra> :set-seqblock-grain-overlap val seqblockid)
                              ))))
  
  (add-parameter 2 (<gui> :horizontal-layout
                          (<gui> :horizontal-slider "Grain length (ms): " 0.001 grain-length 1000
                                 (lambda (val)
                                   (when has-started
                                     ;;(c-display "  Grain Frequency LENGTH (ms):" val)
                                     ;;(set! grain-length val)
                                     ;;(set! grain-frequency (get-grain-frequency grain-overlap))
                                     (<ra> :set-seqblock-grain-length val seqblockid))))))

  (define (slider->jitter slider-value)
    (set! slider-value (/ slider-value 100.0))
    (set! slider-value (expt slider-value 3))
    slider-value)
  
  (define (jitter->slider jitter)
    (set! jitter (expt jitter (/ 1 3)))
    (set! jitter (* jitter 100))
    jitter)
  
  (define (get-grain-jitter-text value)
    (<-> "Grain jitter (%): " (two-decimal-string (* 100 (slider->jitter value)))))
    
  (define jitter-slider (<gui> :horizontal-slider get-grain-jitter-text 0 (jitter->slider (<ra> :get-seqblock-grain-jitter seqblockid)) 100
                               (lambda (val)
                                 (when has-started
                                   (set! val (slider->jitter val))
                                   (c-display "  Grain Frequency JITTER (%):" (* 100 val))
                                   (<ra> :set-seqblock-grain-jitter val seqblockid)))))

  (add-parameter 3 jitter-slider)

  (add-parameter 4 (<gui> :horizontal-slider "Grain ramp (%): " 0 (* 100 (<ra> :get-seqblock-grain-ramp seqblockid)) 50
                          (lambda (val)
                            (when has-started
                              (set! val (/ val 100.0))
                              (c-display "  Grain RAMP (%):" (* 100 val))
                              (<ra> :set-seqblock-grain-ramp val seqblockid)))))

  (let ()
    (define checkbox (<gui> :checkbox "Strict no jitter when jitter is 0.00%" (<ra> :get-seqblock-grain-strict-no-jitter seqblockid) #f
                            (lambda (ison)
                              (when has-started
                                (<ra> :set-seqblock-grain-strict-no-jitter ison seqblockid)))))
    (<gui> :set-tool-tip checkbox
           (<-> "If set, the distance between the start of all grains will always be the same when jitter is 0.00%.\n"
                "\n"
                "The duration of the generated sound will be slightly wrong if this mode is set,\n"
                "but the sound will contain a purer comb filter effect, if you are looking for that effect.\n"
                "\n"
                "If this mode is not set, the distances will differ in size by at most 1 frame in such a way\n"
                "that the total duration of the generated sound will be correct, at the cost of a less pure\n"
                "comb filter effect.\n"
                "\n"
                "If overlap is set high, and grain length is set low, it's easier to hear the difference."))
    (<gui> :add grain-group checkbox))

  (<gui> :add main-layout grain-group)


  (let ((stretch-checkbox (<gui> :checkbox "Stretch automation" 
                                 (<ra> :get-seqblock-automation-enabled 5 seqblockid)
                                 #f
                                 (lambda (ison)
                                   (when has-started
                                     (<ra> :set-seqblock-automation-enabled ison 5 seqblockid)))))
        (speed-checkbox (<gui> :checkbox "Speed automation" 
                               (<ra> :get-seqblock-automation-enabled 6 seqblockid)
                               #f
                               (lambda (ison)
                                 (when has-started
                                   (<ra> :set-seqblock-automation-enabled ison 6 seqblockid))))))
    (<gui> :add main-layout
           (<gui> :group "Stretch and speed automation"
                  (<gui> :horizontal-layout stretch-checkbox speed-checkbox))
           ;;(<gui> :horizontal-layout stretch-checkbox)
           ))

  (when #f
    (define apply-button (<gui> :button "Apply!"
                                (lambda ()
                                  (c-display "hepp"))))
    
    (<gui> :add main-layout apply-button))

  (<gui> :add main-layout (<gui> :button "Close"
                                 (lambda ()
                                   (<gui> :close main-layout))))
  
  (set! has-started #t)

  (define old-gui (*seqblock-guis* seqblockid))
  (if old-gui
      (<gui> :close old-gui))
  
  (set! (*seqblock-guis* seqblockid) main-layout)

  (define seqblock-deleted-callback-called #f)
  
  (define (seqblock-deleted-callback)
    (set! seqblock-deleted-callback-called #t)
    (<gui> :close main-layout))
  
  (<ra> :add-seqblock-deleted-callback seqblockid seqblock-deleted-callback)
  
  (<gui> :add-deleted-callback main-layout
         (lambda (runs-custom-exec)
           (if (not seqblock-deleted-callback-called)
               (<ra> :remove-seqblock-deleted-callback seqblockid seqblock-deleted-callback))
           (set! (*seqblock-guis* seqblockid) #f)))

  (<gui> :set-takes-keyboard-focus main-layout #f)
  (<gui> :set-parent main-layout (<gui> :get-sequencer-gui))

  #||
  (<gui> :minimize-as-much-as-possible main-layout)
  (<gui> :set-size main-layout (<gui> :width main-layout) (+ 40 (<gui> :height main-layout)))
  (<gui> :minimize-as-much-as-possible main-layout)
  ||#

  (<gui> :set-window-title main-layout (<ra> :get-seqblock-name seqblocknum seqtracknum))
  
  ;; There's a slight flicker when opening the window. I've tried to delay opening, minimize, etc. but this Qt bug is probably almost impossible to workaround.
  (<gui> :show main-layout)
  )


;;(if (not *is-initializing*)
;;    (create-audio-seqblock-gui 0 1))



  
                                 
                                 
         

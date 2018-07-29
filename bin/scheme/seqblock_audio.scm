(provide 'seqblock_audio.scm)

;;(my-require 'area.scm)
(my-require 'gui.scm)
(my-require 'instruments.scm)
(my-require 'area.scm)


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
  (define seqblockid (<ra> :get-seqblock-id seqblocknum seqtracknum))
  (define started #f)
  (define main-layout (<gui> :vertical-layout));flow-layout))

  (define gain-group (<gui> :group "Gain"))

  (define gain-slider (<gui> :horizontal-slider "Gain (Db): " -35 (<ra> :get-seqblock-gain seqblockid) 35
                             (lambda (db)
                               (when started
                                 (<ra> :set-seqblock-gain (<ra> :db-to-gain db) seqblockid)))))
  
  (<gui> :add gain-group (<gui> :horizontal-layout
                                gain-slider
                                (<gui> :button "Normalize!"
                                       (lambda ()
                                         (<gui> :set-value gain-slider (get-normalized-seqblock-gain seqblockid))))))
;                                                
;                                                <ra> :set-seqblock-gain (get-normalized-seqblock-gain seqblockid) seqblockid)
;                                         (

  (define volume-automation-checkbox (<gui> :checkbox "Volume automation" (<ra> :get-seqblock-automation-enabled 0 seqblockid) #f
                                            (lambda (ison)
                                              (when started
                                                (<ra> :set-seqblock-automation-enabled ison 0 seqblockid)))))

  (<gui> :add gain-group volume-automation-checkbox)

  (<gui> :add main-layout gain-group)

  (when #t
    (define ratio (<ra> :get-seqblock-resample-ratio seqblocknum seqtracknum))
    (define resampler-group (<gui> :group (<-> "Resampling (ratio: " (two-decimal-string ratio) ")")))

    (define (set type)
      (when started
        (define seqtracknum (<ra> :get-seqblock-seqtrack-num seqblockid))
        (c-display "type:" type ". seqtracknum:" seqtracknum)
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
    
    (<gui> :add resampler-group (<gui> :horizontal-layout
                                       (<gui> :text "Type:     ")
                                       (<gui> :radiobutton "Sample and hold" (= resampler-type 0) (lambda (doit) (if doit (set 0))))
                                       (<gui> :radiobutton "Linear" (= resampler-type 1) (lambda (doit) (if doit (set 1))))
                                       (<gui> :radiobutton "Cubic" (= resampler-type 2) (lambda (doit) (if doit (set 2))))
                                       (<gui> :radiobutton "Sinc1" (= resampler-type 3) (lambda (doit) (if doit (set 3))))
                                       (<gui> :radiobutton "Sinc2" (= resampler-type 4) (lambda (doit) (if doit (set 4))))))
    (if #f
        (<gui> :add resampler-group (<gui-number-input> "Rate: "
                                                        :input-type 'float
                                                        :direction 'horizontal
                                                    :min 0
                                                    :curr 1
                                                    :max 100
                                                    :callback (lambda (val)
                                                                (when started
                                                                  (c-display "got" val)))))
        )
    
    (<gui> :add main-layout resampler-group)

    (if (= 1.0 ratio)
        (<gui> :set-enabled resampler-group #f))
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
  ;                                 (when started
  ;                                   (c-display "  Grain Frequency FREQ1 (ms):" val)
  ;                                   (<ra> :set-seqblock-grain-frequency val seqblockid)))))

  (define grain-group (<gui> :group "Granular synthesis"))

  (define (add-parameter automationnum slider)
    (define is-enabled (<ra> :get-seqblock-automation-enabled automationnum seqblockid))
    (<gui> :set-enabled slider (not is-enabled))
    (define checkbox (<gui> :checkbox "" is-enabled #f
                                          (lambda (ison)
                                            (when started
                                              (<ra> :set-seqblock-automation-enabled ison automationnum seqblockid)
                                              (<gui> :set-enabled slider (not ison))))))
    (<gui> :set-tool-tip checkbox "Automation enabled")
    (<gui> :add grain-group (<gui> :horizontal-layout
                                   slider
                                   checkbox)))

  (add-parameter 1 (<gui> :horizontal-slider "Grain overlap (X): " 0.1 grain-overlap 50
                          (lambda (val)
                            (when started
                              ;;(set! grain-frequency (get-grain-frequency val))
                              ;;(c-display "  Grain OVERLAP (X):" grain- ". overlap:" val)
                              ;;(<ra> :set-seqblock-grain-frequency grain-frequency seqblockid)
                              (<ra> :set-seqblock-grain-overlap val seqblockid)
                              ))))
  
  (add-parameter 2 (<gui> :horizontal-layout
                          (<gui> :horizontal-slider "Grain length (ms): " 0.1 grain-length 1000
                                 (lambda (val)
                                   (when started
                                     (c-display "  Grain Frequency LENGTH (ms):" val)
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
                                 (when started
                                   (set! val (slider->jitter val))
                                   (c-display "  Grain Frequency JITTER (%):" (* 100 val))
                                   (<ra> :set-seqblock-grain-jitter val seqblockid)))))

  (add-parameter 3 jitter-slider)

  (add-parameter 4 (<gui> :horizontal-slider "Grain ramp (%): " 0 (* 100 (<ra> :get-seqblock-grain-ramp seqblockid)) 50
                          (lambda (val)
                            (when started
                              (set! val (/ val 100.0))
                              (c-display "  Grain RAMP (%):" (* 100 val))
                              (<ra> :set-seqblock-grain-ramp val seqblockid)))))
  

  (<gui> :add main-layout grain-group)

  (when #f
    (define apply-button (<gui> :button "Apply!"
                                (lambda ()
                                  (c-display "hepp"))))
    
    (<gui> :add main-layout apply-button))

  (<gui> :add main-layout (<gui> :button "Close"
                                 (lambda ()
                                   (<gui> :close main-layout))))
  
  (set! started #t)

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

  (<gui> :set-parent main-layout -1)
  (<gui> :show main-layout)
  (<gui> :set-window-title main-layout (<ra> :get-seqblock-name seqblocknum seqtracknum))
  )


(if (not *is-initializing*)
    (create-audio-seqblock-gui 0 1))



  
                                 
                                 
         

(provide 'mixer.scm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-keybinding-help-window)
  (FROM-C-show-help-window "help/index.html?page=editing_keybindings"))

(define (FROM_C-show-mixer-config-popup-menu num)
  (popup-menu
   (list
    (<-> "Reset A/B button #" (+ num 1))
    :enabled (or (= -1 num)
                 (<ra> :mixer-config-num-is-used num))
    (lambda ()
      (<ra> :reset-mixer-config-num num)))
   "-------------"
   (get-keybinding-configuration-popup-menu-entries "ra:set-curr-mixer-config-num"
                                                    (list num)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

#!!
(let ((show-keybinding-help-func (lambda () #t)))
  (assq 'show-keybinding-help-func
        (let->list (curlet))))

(get-procedure-name (lambda () 2 3))
(get-displayable-keybinding "show-keybinding-help-window" '())

(popup-menu "aiai"
            show-keybinding-help-window)

(get-procedure-name show-keybinding-help-window)
(<ra> :mixer-config-num-is-used 6)
!!#


(define (FROM_C-show-mixer-config-reset-popup-menu num)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:reset-mixer-config-num"
                                                    (list num)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-mixer-config-settings-menu)
  (popup-menu
   (map (lambda (base)
          (define base2 (cons "include" (append base (list "in" "mixer" "config"))))
          (list (string-join (cons "Include" base) " ")
                :check (eval-string (<-> "(ra:" (string-join base2 "-") ")"))
                (lambda (val)
                  (eval-string (<-> "(ra:" (string-join (cons "set" base2) "-") " " (if val "#t" "#f") ")")))))
        '(("audio" "connections")
          ("event" "connections")
          ("volume")
          ("panning")
          ("mute" "solo" "bypass")
          ("system" "effects")
          ("instrument" "effects")
          ("instrument" "states")
          ("mixer" "strips" "configuration")
          ("remember" "current" "instrument")
          ("modulator" "connections")
          ("system" "volume")
          ))
   ))
  
  

                                                    

(define (FROM_C-window-mode-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-main-mixer-is-in-window"
                                                    '()
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
 
(define (FROM_C-show-modular-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-main-mixer-is-modular"
                                                    '()
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
 
(define (FROM_C-show-instrument-in-mixer-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-instrument-in-mixer"
                                                    '()
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
 
(define (FROM_C-show-sequencer-in-mixer-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-sequencer-in-mixer"
                                                    '()
                                                    "FOCUS_MIXER FOCUS_SEQUENCER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
 
(define (FROM_C-show-cpu-usage-in-mixer-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-show-cpu-usage-in-mixer"
                                                    '()
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-mixer-connections-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-visible-mixer-connections"
                                                    '()
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-mixer-bus-connections-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-visible-mixer-bus-connections"
                                                    '()
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-mixer-zoom-reset-popup-menu)
  (popup-menu
   "--------Reset zoom"
   (get-keybinding-configuration-popup-menu-entries "ra:unzoom"
                                                    '()
                                                    "")
   "--------Zoom in"
   (get-keybinding-configuration-popup-menu-entries "ra:zoom"
                                                    '(1)
                                                    "")
   "--------Zoom out"
   (get-keybinding-configuration-popup-menu-entries "ra:zoom"
                                                    '(-1)
                                                    "")
   "--------Zoom in more"
   (get-keybinding-configuration-popup-menu-entries "ra:zoom"
                                                    '(4)
                                                    "")
   "--------Zoom out more"
   (get-keybinding-configuration-popup-menu-entries "ra:zoom"
                                                    '(-4)
                                                    "")
   "--------Zoom in even more"
   (get-keybinding-configuration-popup-menu-entries "ra:zoom"
                                                    '(8)
                                                    "")
   "--------Zoom out even more"
   (get-keybinding-configuration-popup-menu-entries "ra:zoom"
                                                    '(-8)
                                                    "")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
        
(define (FROM_C-show-mixer-rotate-popup-menu)
  (popup-menu
   "--------Direction left"
   (get-keybinding-configuration-popup-menu-entries "ra:set-mixer-rotate"
                                                    '(0)
                                                    "FOCUS_MIXER")
   "--------Direction right"
   (get-keybinding-configuration-popup-menu-entries "ra:set-mixer-rotate"
                                                    '(180)
                                                    "FOCUS_MIXER")
   "--------Direction up"
   (get-keybinding-configuration-popup-menu-entries "ra:set-mixer-rotate"
                                                    '(270)
                                                    "FOCUS_MIXER")
   "--------Direction down"
   (get-keybinding-configuration-popup-menu-entries "ra:set-mixer-rotate"
                                                    '(90)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-mixer-help-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:show-mixer-help-window"
                                                    '()
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

(define (FROM_C-show-mixer-ratio13-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:eval-scheme"
                                                    '("(ra:gui_set-vert-ratio-in-mixer-strips 1/3)")
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-ratio11-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:eval-scheme"
                                                    '("(ra:gui_set-vert-ratio-in-mixer-strips 1)")
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-ratio31-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:eval-scheme"
                                                    '("(ra:gui_set-vert-ratio-in-mixer-strips 3)")
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-R1-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:gui_set-num-rows-in-mixer-strips"
                                                    '(1)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-R2-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:gui_set-num-rows-in-mixer-strips"
                                                    '(2)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-R3-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:gui_set-num-rows-in-mixer-strips"
                                                    '(3)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-R4-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:gui_set-num-rows-in-mixer-strips"
                                                    '(4)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))


;; Note: Used for shortcut
(delafina (insert-new-sound-object-in-mixer :x (<ra> :get-curr-mixer-slot-x)
                                            :y (<ra> :get-curr-mixer-slot-y))
  (define descr (make-instrument-conf :x x
                                      :y y
                                      :connect-to-main-pipe #f
                                      :do-autoconnect #t
                                      :include-load-preset #t
                                      :must-have-inputs #f
                                      :must-have-outputs #f
                                      :parentgui (<gui> :get-main-mixer-gui)))
  (<ra> :create-instrument-description-popup-menu descr))


(define *plugin-usage-prefix* "plugin_usage_")

(define (get-favourite-plugin-menu-entries callback shortcut-func)
  (map (lambda (a)
         (define descr (<ra> :get-audio-instrument-description (<ra> :to-base64 (a :container-name))
                                                               (<ra> :to-base64 (a :type-name))
                                                               (<ra> :to-base64 (a :name))))
         (list (<-> (a :type-name) ": " (a :name) " (" (a :num-uses) ")")
               :shortcut (and shortcut-func (list shortcut-func
                                                  (string->keybinding-string (a :type-name))
                                                  (string->keybinding-string (a :name))))
               (lambda ()
                 (callback descr))))
       (sort (keep (lambda (a)
                     (and (or (string=? (a :type-name) "VST")
                              (string=? (a :type-name) "AU")
                              (string=? (a :type-name) "Ladspa")
                              (string=? (a :type-name) "Pd")
                              (string-starts-with? (a :type-name) "STK"))
                          (not (string=? (a :name) "Calf MultiChorus LADSPA"))))
                   (map (lambda (a)
                          (let* ((key (car a))
                                 (value (cdr a))
                                 (pos (string-length *plugin-usage-prefix*))
                                 (typeandname (string-drop (symbol->string key)
                                                           pos))
                                 (strings (string-split typeandname "_-_"))
                                 (type-name (strings 0))
                                 (container-name (strings 1))
                                 (plugin-name (strings 2)))
                            (hash-table :type-name type-name
                                        :container-name container-name
                                        :name plugin-name
                                        :num-uses (string->number value))))
                        (<ra> :get-all-settings *plugin-usage-prefix*)))
             (lambda (a b)
               (>= (a :num-uses)
                   (b :num-uses))))))


;; Note: Used for shortcut
(define (create-instrument type-name plugin-name)
  (<ra> :create-audio-instrument
        (get-keybinding-string type-name)
        (get-keybinding-string plugin-name)
        ""
        (<ra> :get-curr-mixer-slot-x)
        (<ra> :get-curr-mixer-slot-y)))


;; Note: Used for shortcut
(delafina (show-favourites-menu :x (<ra> :get-curr-mixer-slot-x)
                                :y (<ra> :get-curr-mixer-slot-y)
                                :callback #f
                                :shortcut-func #f)
  (popup-menu (get-favourite-plugin-menu-entries
               (or callback
                   (lambda (descr)
                     (<ra> :create-audio-instrument-from-description descr "" x y)))
               (or shortcut-func
                   (if callback
                       #f
                       create-instrument)))))
  

;; Note: Used for shortcut
(delafina (insert-new-bus-in-mixer :x (<ra> :get-curr-mixer-slot-x)
                                   :y (<ra> :get-curr-mixer-slot-y))
  (undo-block
   (lambda ()
     (define seqtracknum (<ra> :append-bus-seqtrack))
     (<ra> :set-seqtrack-visible seqtracknum #f)
     (<ra> :set-instrument-position x y (<ra> :get-seqtrack-instrument seqtracknum)))))


;; Note: Used for shortcut
(delafina (unsolo-all-instruments)
  (<ra> :set-solo-for-instruments (get-all-audio-instruments) #f))
  
;; Note: Used for shortcut
(delafina (mute-all-instruments)
  (<ra> :set-mute-for-instruments (get-all-audio-instruments) #t))
  
;; Note: Used for shortcut
(delafina (unmute-all-instruments)
  (<ra> :set-mute-for-instruments (get-all-audio-instruments) #f))

;; Note: Used for shortcut
(delafina (unbypass-all-instruments)
  (<ra> :set-bypass-for-instruments (get-all-audio-instruments) #f))

#!!
(for-each c-display (map ra:get-instrument-name (get-all-audio-instruments)))
(let ((id (cadr (get-all-audio-instruments))))
  (<ra> :set-instrument-mute #t id)
  (c-display (<ra> :get-instrument-name id) (<ra> :get-instrument-mute id))
  )


!!#

;; Note: Used for shortcut
(delafina (unsolo-all-selected-instruments)
  (<ra> :set-solo-for-instruments (<ra> :get-curr-mixer-instruments) #f))

;; Note: Used for shortcut
(delafina (unmute-all-selected-instruments)
  (<ra> :set-mute-for-instruments (<ra> :get-curr-mixer-instruments) #f))

;; Note: Used for shortcut
(delafina (save-preset-for-instruments :instruments (<ra> :get-curr-mixer-instruments)
                                       :gui (<gui> :get-main-mixer-gui))
  
  (<ra> :save-instrument-preset instruments gui))

;; Note: Used for shortcut
(delafina (show-mixer-strips-for-instruments :instruments (<ra> :get-curr-mixer-instruments)
                                             :num-rows 1)
  (<ra> :show-mixer-strips2 num-rows instruments))

(define (get-mixer-popup-menu-selected-objects-entries selected-instruments)
  (define enabled (> (<ra> :num-selected-instruments) 0))
  (define num-selected-instruments (<ra> :num-selected-instruments))
  (define header-text (<-> "---------------Selected objects (" num-selected-instruments ")"))
  (if (= 0 num-selected-instruments)
      (list
       header-text
       (list "(no selected objects)"
             :enabled #f
             (lambda ()
               #t)))
      (list
       header-text
       (get-sample-player-mixer-popup-menu-entries selected-instruments)
       (list "Mute "
             :enabled enabled
             ra:switch-mute-for-selected-instruments)
       (list "Solo"
             :enabled enabled
             ra:switch-solo-for-selected-instruments)
       (list "Bypass"
             :enabled enabled
             ra:switch-bypass-for-selected-instruments)
       "-------------"
       (list "Copy"
             :enabled enabled
             ra:copy-selected-mixer-objects)
       (list "Cut"
             :enabled enabled
             ra:cut-selected-mixer-objects)
       (list "Delete"
             :enabled enabled
             ra:delete-selected-mixer-objects)
       "-----------"
       (list (if (> num-selected-instruments 1)
                 "Save multi preset (.mrec)"
                 "Save preset (.rec)")
             :enabled enabled
             save-preset-for-instruments)
       (list "Show mixer strips window"
             :enabled enabled
             show-mixer-strips-for-instruments)
       (list "Configure color"
             :enabled enabled
             show-instrument-color-dialog-for-all-selected-instruments)
       (list "Generate new color"
             :enabled enabled
             ra:generate-new-color-for-all-selected-instruments)
       )))

(define (get-mixer-all-objects-popup-menu-entries x y)
  (define all-audio-instruments (get-all-audio-instruments))
  (list
   "---------------All objects"
   (list "Un-solo"
         :enabled (or (any? ra:get-instrument-solo all-audio-instruments)
                      (any? ra:get-instrument-solo-from-storage all-audio-instruments))
         unsolo-all-instruments)
   (list "Un-mute"
         :enabled (or (any? ra:get-instrument-mute all-audio-instruments)
                      (any? ra:get-instrument-mute-from-storage all-audio-instruments))
         unmute-all-instruments)
   (list "Un-bypass"
         :enabled (or (any? ra:get-instrument-bypass all-audio-instruments)
                      (any? ra:get-instrument-bypass-from-storage all-audio-instruments))
         unbypass-all-instruments)))

(define (get-mixer-popup-menu-entries)
  (list
   "---------------Display"
   (list "Show CPU usage (CPU)"
         :check (<ra> :get-show-cpu-usage-in-mixer)
         :shortcut ra:switch-show-cpu-usage-in-mixer
         (lambda (doit)
           (<ra> :set-show-cpu-usage-in-mixer doit)))
   (list "Show connections (C1)"
         :check (<ra> :get-visible-mixer-connections)
         :shortcut ra:switch-visible-mixer-connections
         (lambda (doit)
           (<ra> :set-visible-mixer-connections doit)))
   (list "Show bus connections (C2)"
         :check (<ra> :get-visible-mixer-bus-connections)
         :shortcut ra:switch-visible-mixer-bus-connections
         (lambda (doit)
           (<ra> :set-visible-mixer-bus-connections doit)))
   "----------Zoom"
   (list "Zoom in"
         :shortcut (list ra:zoom 1)
         (lambda ()
           (<ra> :zoom 1)))
   (list "Zoom in++"
         :shortcut (list ra:zoom 4)
         (lambda ()
           (<ra> :zoom 4)))
   (list "Zoom out"
         :shortcut (list ra:zoom -1)
         (lambda ()
           (<ra> :zoom -1)))
   (list "Zoom out++"
         :shortcut (list ra:zoom -4)
         (lambda ()
           (<ra> :zoom 4)))
   (list "Reset zoom (~Zoom)"
         ra:unzoom)
   "---------------Windows"
   (list "Mixer in it's own window (W)"
         :check (<ra> :main-mixer-in-window)
         :shortcut ra:switch-main-mixer-in-window
         (lambda (doit)
           (<ra> :set-main-mixer-in-window doit)))
   (list "Show mixer-strips (M)"
         :check (not (<ra> :main-mixer-is-modular))
         :shortcut ra:switch-main-mixer-is-modular
         (lambda (doit)
           (<ra> :set-main-mixer-is-modular (not doit))))
   (list "Instrument in mixer (I)"
         :check (<ra> :instrument-in-mixer)
         :shortcut ra:switch-instrument-in-mixer
         (lambda (showit)
           (<ra> :set-instrument-in-mixer showit)))
   (list "Sequencer in mixer (S)"
         :check (<ra> :sequencer-in-mixer)
         :shortcut ra:switch-sequencer-in-mixer
         (lambda (showit)
           (<ra> :set-sequencer-in-mixer showit)))
   "---------------------"
   (list "Mixer Visible"
         :check #t
         :shortcut ra:show-hide-focus-mixer
         (lambda (doit)
           (<ra> :show-hide-mixer-widget)))
   "---------------Help"
   (list "Help"
         ra:show-mixer-help-window)
   ))

(define (show-mixer-popup-menu-no-chip-under x y selected-instruments)
  (popup-menu
   (list
    "-----------Insert module"
    (list "From plugin manager"
          :shortcut new-instrument-from-plugin-manager
          (lambda ()
            (new-instrument-from-plugin-manager -2 x y)))
    (list "From instrument/effect menu" ;;New instrument or effect"
          :shortcut insert-new-sound-object-in-mixer
          (lambda ()
            (insert-new-sound-object-in-mixer x y)))
    (list "From list of most frequently used instruments" ;;"From list of favourites"
          :shortcut show-favourites-menu
          (lambda ()
            (show-favourites-menu x y)))
    "---------------"
    (list "From preset file (.rec/.mrec)"
          :shortcut new-instrument-from-preset
          (lambda ()
            (new-instrument-from-preset -2 x y)))
    "-----------"
    (list "New bus"
          :shortcut insert-new-bus-in-mixer
          (lambda ()
            (insert-new-bus-in-mixer x y)))
    "---------------"
    (list "Paste"
          :enabled (<ra> :instrument-preset-in-clipboard)
          :shortcut ra:paste-mixer-objects
          (lambda ()
            (<ra> :paste-mixer-objects x y)))
    (get-mixer-popup-menu-selected-objects-entries selected-instruments)
    (get-mixer-all-objects-popup-menu-entries x y)
    "------------Mixer"
    (list "Mixer"
          (get-mixer-popup-menu-entries)))))

(define (FROM_C-show-mixer-connection-popup-menu from-instrument to-instrument is-event-connection)
  (popup-menu (get-insert-plugin-entry from-instrument (not is-event-connection))
              (list "Delete connection"
                    :shortcut *shift-right-mouse*
                    (lambda ()
                      (<ra> :undo-mixer-connections)
                      (if is-event-connection
                          (<ra> :delete-event-connection from-instrument to-instrument)
                          (<ra> :delete-audio-connection from-instrument to-instrument)))
                    ;;(get-mixer-popup-menu-entries)
                    )
              "-------------------------"
              (list "Connection enabled"
                    :enabled (not is-event-connection)
                    :check (or is-event-connection
                               (<ra> :get-connection-enabled from-instrument to-instrument))
                    (lambda (val)
                      (<ra> :undo-connection-enabled from-instrument to-instrument)
                      (<ra> :set-connection-enabled from-instrument to-instrument val)
                      ))
              ))
  
(define (get-current-instrument-mixer-popup-menu-entries instrument-id)
  (get-instrument-popup-entries instrument-id (<gui> :get-main-mixer-gui)))
  
(define (show-mixer-popup-menu-several-selected-instruments current-instrument-id selected-instruments x y)
  (popup-menu (list
               (get-current-instrument-mixer-popup-menu-entries current-instrument-id)
               (get-mixer-popup-menu-selected-objects-entries selected-instruments)
               (get-mixer-all-objects-popup-menu-entries x y)
               "------------Mixer"
               (list "Mixer"
                     (get-mixer-popup-menu-entries))
               )))

(define (show-mixer-popup-menu-one-instrument instrument-id x y)
  (popup-menu (get-current-instrument-mixer-popup-menu-entries instrument-id)
              ;;"------------Mixer"
              ;;(list "Mixer"
              (get-mixer-all-objects-popup-menu-entries x y)
              "------------Mixer"
              (list "Mixer"
                    (get-mixer-popup-menu-entries))
              ))

(define (FROM_C-show-mixer-popup-menu curr-instrument-id x y)
  (define selected-instruments (<ra> :get-curr-mixer-instruments))
  (if (not (<ra> :is-legal-instrument curr-instrument-id))
      (show-mixer-popup-menu-no-chip-under x y selected-instruments)
      (if (> (length selected-instruments) 1)
          (show-mixer-popup-menu-several-selected-instruments curr-instrument-id selected-instruments x y)
          (show-mixer-popup-menu-one-instrument curr-instrument-id x y))))


(define (FROM_C-show-mixer-popup-menu-effect instrument-id effect-name)
  (popup-menu (get-effect-popup-entries instrument-id effect-name)
              "------------Mixer"
              (list "Mixer"
                    (get-mixer-popup-menu-entries))))

  
#!!
(<ra> :num-selected-instruments)
!!#

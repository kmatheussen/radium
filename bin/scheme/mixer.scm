(provide 'mixer.scm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-keybinding-help-window)
  (FROM-C-show-help-window "help/keybindings_gui_framed.html"))

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
!!#




(define (FROM_C-show-mixer-config-reset-popup-menu num)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:reset-mixer-config-num"
                                                    (list num)
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))

                                                    

(define (FROM_C-window-mode-popup-menu) 
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:switch-mixer-is-in-window"
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
   (get-keybinding-configuration-popup-menu-entries "ra:switch-instrument-widget-in-mixer"
                                                    '()
                                                    "FOCUS_MIXER")
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
   (get-keybinding-configuration-popup-menu-entries "ra:eval-scheme"
                                                    '("(ra:gui_set-num-rows-in-mixer-strips 1)")
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-R2-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:eval-scheme"
                                                    '("(ra:gui_set-num-rows-in-mixer-strips 2)")
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-R3-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:eval-scheme"
                                                    '("(ra:gui_set-num-rows-in-mixer-strips 3)")
                                                    "FOCUS_MIXER")
   "-------------"
   "Help keybindings" show-keybinding-help-window
   ))
  
(define (FROM_C-show-mixer-R4-popup-menu)
  (popup-menu
   (get-keybinding-configuration-popup-menu-entries "ra:eval-scheme"
                                                    '("(ra:gui_set-num-rows-in-mixer-strips 4)")
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
  (<ra> :set-solo-for-instruments (<ra> :get-selected-instruments) #f))

;; Note: Used for shortcut
(delafina (unmute-all-selected-instruments)
  (<ra> :set-mute-for-instruments (<ra> :get-selected-instruments) #f))
  
(define (FROM_C-show-mixer-popup-menu x y)
  (popup-menu
   "-----------Insert"
   (list "Insert new sound object"
         :shortcut insert-new-sound-object-in-mixer
         (lambda ()
           (insert-new-sound-object-in-mixer x y)))
   "---------------"
   (list "Paste"
         :enabled (<ra> :instrument-preset-in-clipboard)
         :shortcut ra:paste-mixer-objects
         (lambda ()
           (<ra> :paste-mixer-objects x y)))
   (<-> "---------------Selected objects (" (<ra> :num-selected-instruments) ")")
   (list "Mute "
         :enabled (> (<ra> :num-selected-instruments) 0)
         ra:switch-mute-for-selected-instruments)
   (list "Solo"
         :enabled (> (<ra> :num-selected-instruments) 0)
         ra:switch-solo-for-selected-instruments)
   (list "Bypass"
         :enabled (> (<ra> :num-selected-instruments) 0)
         ra:switch-bypass-for-selected-instruments)
   (list "Copy"
         :enabled (> (<ra> :num-selected-instruments) 0)
         ra:copy-selected-mixer-objects)
   (list "Cut"
         :enabled (> (<ra> :num-selected-instruments) 0)
         ra:cut-selected-mixer-objects)
   (list "Delete"
         :enabled (> (<ra> :num-selected-instruments) 0)
         ra:delete-selected-mixer-objects)
   "---------------All objects"
   (list "Un-solo"
         unsolo-all-instruments)
   (list "Un-mute"
         unmute-all-instruments)
   (list "Un-bypass"
         unbypass-all-instruments)
   "---------------GUI"
   (list "Mixer in it's own window (W)"
         :check (<ra> :main-mixer-is-in-window)
         :shortcut ra:switch-mixer-is-in-window
         (lambda (doit)
           (<ra> :set-main-mixer-in-window doit)))
   (list "Show mixer-strips (M)"
         :check (not (<ra> :main-mixer-is-modular))
         :shortcut ra:switch-main-mixer-is-modular
         (lambda (doit)
           (<ra> :set-main-mixer-is-modular (not doit))))
   (list "Instrument widget in mixer (I)"
         :check (<ra> :instrument-widget-is-in-mixer)
         :shortcut ra:switch-instrument-widget-in-mixer
         (lambda (showit)
           (<ra> :set-instrument-widget-in-mixer showit)))
   "---------------------"
   (list "Mixer Visible"
         :check #t
         :shortcut ra:show-hide-focus-mixer
         (lambda (doit)
           (<ra> :show-hide-mixer-widget)))
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
   (list "Reset zoom (~Zoom)"
         ra:unzoom)
   "---------------Help"
   (list "Help"
         ra:show-mixer-help-window)
   ))


#!!
(<ra> :num-selected-instruments)
!!#

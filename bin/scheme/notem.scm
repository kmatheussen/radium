
(provide 'notem.scm)

(my-require 'notes.scm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit tab in the lower tabs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define first-time? (not (defined? '*notem-gui*)))




;;    HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-displayable-keybinding rafuncname . args)
  (let ((keybinding (<ra> :get-keybinding (apply get-python-ra-funccall (cons rafuncname args)))))
    (c-display "keybinding" (hash-table? keybinding) "name:" (apply get-python-ra-funccall (cons rafuncname args)))
    ;(c-display (to-list (keybinding :qualifiers))
    ;           (to-list (keybinding :keys)))
    (if (not (hash-table? keybinding))
        ""
        (string-join (map (lambda (key)
                            (let ((a (ra:get-qualifier-name key)))
                              (if (string=? "" a)
                                  key
                                  a)))
                          (append (to-list (keybinding :qualifiers))
                                  (to-list (keybinding :keys))))
                     " + "))))

#!!
(let ((gakk (get-displayable-keybinding "ra:transpose-block" 1)))
  (c-display "gakk:" gakk)
  gakk)
!!#


(define (notem-group-name name qualifier)
  name)
;;  (<-> name "  -  " (get-displayable-qualifier qualifier) ""))
  

(define (create-under-construction)
  (mid-horizontal-layout (<gui> :text "Under construction.")))

(define (create-notem-button groupname ra-funcname)
  (define funcname-contains-range (string-contains? ra-funcname "range"))
  (define ra-func (eval-string ra-funcname))
  (define (func)
    (if (and funcname-contains-range
             (not (<ra> :has-range)))
        (show-async-message :text "No range in block. Select range by using Left Meta + b")
        (ra-func)))
  
  (let ((keybinding (get-displayable-keybinding ra-funcname)))
    (if (string=? keybinding "")
        (<gui> :button groupname func)
        (let ((ret (<gui> :group groupname (<gui> :button keybinding func))))
          ;;(<gui> :set-layout-spacing ret 6 9 9 9 9)
          ;;(<gui> :set-background-color ret "color9")
          ret))))

(define (create-notem-layout . elements)
  (define ret (<gui> :horizontal-layout))
  (for-each (lambda (element)
              (<gui> :add ret element))
            elements)
  (<gui> :set-layout-spacing ret 18 9 0 9 0)
  (<gui> :set-size-policy ret #t #t)
  ret)

(define (create-notem-flow-layout . elements)
  (define vertical-layout (<gui> :vertical-layout))
  (<gui> :add vertical-layout (<gui> :widget 3 3) 1)
  (define flow-layout (<gui> :flow-layout))
  (for-each (lambda (element)
              (<gui> :add flow-layout element))
            elements)
  (<gui> :add vertical-layout flow-layout 3)
  (<gui> :add vertical-layout (<gui> :widget 3 3) 3)
  
  (<gui> :scroll-area #t #t vertical-layout))
  
  


;;        MAIN EDIT TAB
;;;;;;;;;;;;;;;;;;;;;;;;;


(define-constant *notem-gui* (if first-time?
                                 (begin
                                   (let ((gui (my-tabs #t)))
                                     (<gui> :set-static-toplevel-widget gui #t)
                                     
                                     ;; Just hide window when closing it.
                                     (<gui> :add-close-callback gui
                                            (lambda (radium-runs-custom-exec)
                                              ;;(<gui> :set-parent *notem-gui* -3)
                                              (c-display "              GAKK GAKK GAKK")
                                              (<gui> :hide *notem-gui*)
                                              #f))
                                     gui))
                                 *notem-gui*))


(define (add-notem-tab name gui)

  (<gui> :add-tab *notem-gui* name gui)

  ;;(reopen-gui-at-curr-pos *notem-gui*)
  ;;(<gui> :update *notem-gui*)
  )

#!!
(add-notem-tab "testing2" (<gui> :button "hello hello2"))
(<gui> :set-background-color *notem-gui* "blue")
!!#



;;          TRANSPOSE
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-transpose-buttons groupname ra-funcname)
  (define funcname-contains-range (string-contains? ra-funcname "range"))
  (define ra-func (eval-string ra-funcname))
  (define (func . args)
    (if (and funcname-contains-range
             (not (<ra> :has-range)))
        (show-async-message :text "No range in block. Select range by using Left Meta + b")
        (apply ra-func args)))
  
  (define (create-button how-much)
    (define arrow (if (> how-much 0) "Up" "Down")) ;; "↑" "↓"))
    (<gui> :group (<-> arrow " " how-much ": ")
           ;;(<gui> :text (<-> arrow " " how-much ": "))
           (<gui> :button
                  (let ((a (get-displayable-keybinding ra-funcname how-much)))
                    (if (string=? "" a)
                        "Click me"
                        a))
                  (lambda ()
                    (func how-much)))))

  (define horizontal (<gui> :horizontal-layout
                            (<gui> :vertical-layout
                                   (create-button 1)
                                   (create-button -1))
                            (<gui> :vertical-layout
                                   (create-button 7)
                                   (create-button -7))
                            (<gui> :vertical-layout
                                   (create-button 12)
                                   (create-button -12))))
  (<gui> :set-layout-spacing horizontal 6 9 9 9 9)
  
  (define ret (<gui> :group groupname horizontal))
                     
  (<gui> :set-layout-spacing ret 6 9 0 9 0)
  ;;(<gui> :set-background-color ret "color9")
  ret)

(define *transpose-tab* #f)

(define (create-transpose-notem)
  (define ret (create-notem-flow-layout (create-transpose-buttons (notem-group-name "Note" "ALT_R")    "ra:transpose-note")
                                        (create-transpose-buttons (notem-group-name "Range" "EXTRA_L") "ra:transpose-range")
                                        (create-transpose-buttons (notem-group-name "Track" "ALT_L")   "ra:transpose-track")
                                        (create-transpose-buttons (notem-group-name "Block" "CTRL_L")  "ra:transpose-block")))
  (set! *transpose-tab* ret)
  ret)


(when (not first-time?)
  ;;(<gui> :show (create-transpose-notem))
  #t
  )





;;        Various
;;;;;;;;;;;;;;;;;;;;;;;;;

(define *various-tab* #f)

(define (replace-with-random-pitches! area)  
  (undo-editor-area area)  
  (replace-notes! (map-area-notes (get-area-notes area)
                                  (lambda (note)
                                    (define (changepitch pitch)
                                      (if (in-editor-area (+ (area :start-place)
                                                             (note :place)
                                                             (pitch :place))
                                                          :area area)
                                          (begin
                                            (define pitchvalue (pitch :value))
                                            (define range (* 12 (- 1 (sqrt (myrand 0 1))))) ;; A number between 0 and 12, but on average closer to 0.
                                            (define max-pitch (min 127 (+ pitchvalue range)))
                                            (define min-pitch (max 1 (- pitchvalue range)))
                                            (copy-pitch pitch :value (myrand min-pitch max-pitch)))
                                          pitch))                                    
                                    (copy-note note
                                               :pitches (if (= 0 ((last (note :pitches)) :value))
                                                            (map-butlast (note :pitches)
                                                                         changepitch)
                                                            (map changepitch (note :pitches))))))
                  area))

(delafina (replace-with-random-notes-in-range :blocknum -1)
  (replace-with-random-pitches! (get-ranged-editor-area blocknum)))

(delafina (replace-with-random-notes-in-track :tracknum -1 :blocknum -1)
  (replace-with-random-pitches! (get-track-editor-area tracknum blocknum)))

(define (replace-with-random-notes-in-block)
  (replace-with-random-pitches! (get-block-editor-area)))

#!!
(* 5 0.5)
(/ 5 0.5)
(replace-with-random-notes-in-track)
(<ra> :get-num-notes)
!!#

(define (replace-with-random-velocities! area)  
  (undo-editor-area area)  
  (replace-notes! (map-area-notes (get-area-notes area)
                                  (lambda (note)
                                    (define (changevelocity velocity)
                                      (if (in-editor-area (+ (area :start-place)
                                                             (note :place)
                                                             (velocity :place))
                                                          :area area)
                                          (begin
                                            (define velocityvalue (velocity :value))
                                            (define range (* 1.0 (- 1 (sqrt (myrand 0 1))))) ;; A number between 0 and 1.0, but on average closer to 0.
                                            (define max-velocity (min 1.0 (+ velocityvalue range)))
                                            (define min-velocity (max 0.0 (- velocityvalue range)))
                                            (copy-velocity velocity :value (myrand min-velocity max-velocity)))
                                          velocity))                                    
                                    (copy-note note
                                               :velocities (map changevelocity (note :velocities)))))
                  area))

(delafina (replace-with-random-velocities-in-range :blocknum -1)
  (replace-with-random-velocities! (get-ranged-editor-area blocknum)))

(delafina (replace-with-random-velocities-in-track :tracknum -1 :blocknum -1)
  (replace-with-random-velocities! (get-track-editor-area tracknum blocknum)))

(define (replace-with-random-velocities-in-block)
  (replace-with-random-velocities! (get-block-editor-area)))


(define (create-various-notem)

  (define lines-layout (create-notem-layout (create-notem-button (notem-group-name "Range" "EXTRA_L") "ra:expand-range")
                                            (create-notem-button (notem-group-name "Block" "CTRL_L")  "ra:expand-block")))
  
  
  (define pitches-layout (create-notem-layout (create-notem-button (notem-group-name "Range" "EXTRA_L") "ra:pexpand-range")
                                              (create-notem-button (notem-group-name "Track" "ALT_L") "ra:pexpand-track")
                                              (create-notem-button (notem-group-name "Block" "CTRL_L")  "ra:pexpand-block")))
  
  (define invert-layout (create-notem-layout (create-notem-button (notem-group-name "Range" "EXTRA_L") "ra:invert-range")
                                             (create-notem-button (notem-group-name "Track" "ALT_L") "ra:invert-track")
                                             (create-notem-button (notem-group-name "Block" "CTRL_L")  "ra:invert-block")))
  
  
  (define backwards-layout (create-notem-layout (create-notem-button (notem-group-name "Range" "EXTRA_L") "ra:backwards-range")
                                                (create-notem-button (notem-group-name "Track" "ALT_L") "ra:backwards-track")
                                                (create-notem-button (notem-group-name "Block" "CTRL_L")  "ra:backwards-block")))
  
  
  (define glissando-layout (create-notem-layout (create-notem-button "Apply glissando between two notes" "ra:glissando")))
  
  (define monophonic-layout (create-notem-layout (create-notem-button "Make track monophonic" "ra:make-track-monophonic")
                                           (create-notem-button "Split track into several monophonic tracks" "ra:split-track-into-monophonic-tracks")))

  (define random-layout (create-notem-layout (create-notem-button "Range" "replace-with-random-notes-in-range")
                                             (create-notem-button "Track" "replace-with-random-notes-in-track")
                                             (create-notem-button "Block" "replace-with-random-notes-in-block")))

  (define random-velocities-layout (create-notem-layout (create-notem-button "Range" "replace-with-random-velocities-in-range")
                                                        (create-notem-button "Track" "replace-with-random-velocities-in-track")
                                                        (create-notem-button "Block" "replace-with-random-velocities-in-block")))

  (define ret (create-notem-flow-layout (<gui> :group "Expand/shrink Pitch" pitches-layout)
                                        (<gui> :group "Expand/shrink Lines" lines-layout)
                                        (<gui> :group "Invert Pitches" invert-layout)
                                        (<gui> :group "Reverse notes" backwards-layout)
                                        (<gui> :group "Glissando" glissando-layout)
                                        (<gui> :group "Polyphonic tracks" monophonic-layout)
                                        (<gui> :group "Randomize pitch" random-layout)
                                        (<gui> :group "Randomize velocities" random-velocities-layout)
                                        ))
  
  ;;(<gui> :set-size-policy vertical-layout #t #t)
  (set! *various-tab* ret)
  ret)

(when (not first-time?)
  ;;(<gui> :show (create-expand/shrink-notem))
  #t
  )




;;         INIT
;;;;;;;;;;;;;;;;;;;;;;;;;

(define *quanititize-tab* #f)

(define (add-edit-tabs)
  (set! *quanititize-tab* (create-quantitize-gui-for-tab))
  (add-notem-tab "Quantization" *quanititize-tab*)
  (add-notem-tab "Transpose" (create-transpose-notem))
  (add-notem-tab "Various" (create-various-notem))
  ;;(add-notem-tab "More" (mid-vertical-layout (create-under-construction)))
  )

(define (replace-edit-tabs)
  (while (> (<gui> :num-tabs *notem-gui*) 0)
    (<gui> :remove-tab *notem-gui* 0))
  (add-edit-tabs))

(if first-time?
    (add-edit-tabs)
    (replace-edit-tabs))


    



;;      DRODL
;;;;;;;;;;;;;;;;;;;;;;;;;



#!!
(<gui> :height *various-tab*)
(<gui> :height *notem-gui*)
(<gui> :height *transpose-tab*)
(<gui> :height *quanititize-tab*)
(<gui> :minimize-as-much-as-possible *transpose-tab*)

(begin
  (define group (<gui> :group "hello"))
  (define widg (<gui> :widget))
  (<gui> :set-background-color widg "color9")
  (<gui> :add widg (create-transpose-notem) 0 0 500 500)
  (<gui> :show widg))

                   
(define transp (create-transpose-notem))
(<gui> :show transp)
(<gui> :set-background-color transp "color9")

(add-notem-tab "Transpose" (create-transpose-notem))
(add-notem-tab "Transpose2" (<gui> :flow-layout
                                  (<gui> :button "b1")
                                  (<gui> :button "b2")))

(<gui> :set-style-sheet group
       (<-> "QGroupBox {"
            "background-color: rgba(40, 80, 0, 40);"
            "border: 1px solid rgba(10, 10, ff, 50);"
            "border-radius: 20px;"
            "margin-top: 1.5em;"
            "}"))


(<gui> :set-style-sheet-recursively widg "")

;; main stylesheet:
(<gui> :set-style-sheet-recursively group "QGroupBox {    background-color: rgba(0, 0, 0, 10);    border: 1px solid rgba(10, 10, 10, 50);    border-radius: 2px;    margin-top: 1.5em;}QGroupBox::title {    subcontrol-origin: margin;    padding: 2px 2px;    background-color: transparent;}QScrollArea { background: transparent; }QScrollArea > QWidget > QWidget { background: transparent; }QScrollArea > QWidget > QScrollBar { background: rgba(ff, ff, ff, 50); }QTabBar::pane { border: 0; }")


(<gui> :set-style-sheet-recursively transp
       (<-> "QScrollArea"
            "{"
            "  background-color: transparent;"
            "}"
            "QScrollArea > QWidget > QWidget { background: transparent; }"
            ))

(<gui> :set-style-sheet-recursively transp
       (<-> "QAbstractScrollArea"
            "{"
            "  background-color: transparent;"
            "}"
            ))

(let ((tab (create-transpose-notem)))
  (<gui> :set-style-sheet *notem-gui*
         (<-> "QAbstractScrollArea"
              "{"
              "  background-color: transparent;"
              "}"
              "QWidget#scrollAreaWidgetContents{"
              "  background-color: transparent;"
              "}"
              ))
  (add-notem-tab "Transpose" tab))

(define tabWidget (<gui> :child ui "tabWidget"))
(<gui> :add-tab tabWidget "aiai" (<gui> :flow-layout
                                  (<gui> :button "b1")
                                  (<gui> :button "b2")))
(<gui> :add-tab tabWidget "aiai" (create-transpose-notem))

(define (create-callback-creator func)
  (lambda (how-much)
    (lambda ()
      (func how-much))))

(add-notem-tab "hepp"
       (<gui> :flow-layout              
              (create-transpose-buttons "Note" (create-callback-creator ra:transpose-note))
              (create-transpose-buttons "Range" (create-callback-creator ra:transpose-range))
              (create-transpose-buttons "Track" (create-callback-creator ra:transpose-track))
              (create-transpose-buttons "Block" (create-callback-creator ra:transpose-block))))

(let* ((w (<gui> :horizontal-layout))
       (flow (<gui> :flow-layout              
              (create-transpose-buttons "Note" (create-callback-creator ra:transpose-note))
              (create-transpose-buttons "Range" (create-callback-creator ra:transpose-range))
              (create-transpose-buttons "Track" (create-callback-creator ra:transpose-track))
              (create-transpose-buttons "Block" (create-callback-creator ra:transpose-block)))))
  (<gui> :add w flow)
  (add-notem-tab "hepp" w))
       


!!#


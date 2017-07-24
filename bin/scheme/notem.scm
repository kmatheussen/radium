
(provide 'notem.scm)


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
  (define func (eval-string ra-funcname))

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
  (define func (eval-string ra-funcname))
  
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

(delafina (replace-with-random-notes-in-track :tracknum -1)
  (<ra> :undo-notes tracknum)
  (for-each (lambda (notenum)              
              (for-each (lambda (pitchnum)
                          ;;(c-display :set-pitch (1+ (random 127)) 'same-place pitchnum notenum)
                          (define pitchvalue (<ra> :get-pitch-value pitchnum notenum tracknum))
                          (define range (* 12 (- 1 (sqrt (/ (random 9999) 9999.0))))) ;; A number between 0 and 12, but on average closer to 0.
                          (define max-pitch (min 127 (+ pitchvalue range)))
                          (define min-pitch (max 1 (- pitchvalue range)))
                          (<ra> :set-pitch (myrand min-pitch max-pitch) 'same-place pitchnum notenum tracknum)
                          )
                        (iota (1- (<ra> :get-num-pitches notenum tracknum))))) ;; Use 1- to avoid setting end note pitch.
            (iota (<ra> :get-num-notes tracknum))))

(define (replace-with-random-notes-in-block)
  (undo-block
   (lambda ()
     (for-each replace-with-random-notes-in-track
               (iota (<ra> :get-num-tracks))))))

#!!
(* 5 0.5)
(/ 5 0.5)
(replace-with-random-notes-in-track)
(<ra> :get-num-notes)
!!#

(delafina (replace-with-random-velocities-in-track :tracknum -1)
  (<ra> :undo-notes tracknum)
  (for-each (lambda (notenum)
              (for-each (lambda (velocitynum)
                          ;;(c-display :set-velocity (1+ (random 127)) 'same-place velocitynum notenum)
                          (define velocityvalue (<ra> :get-velocity-value velocitynum notenum tracknum))
                          (define range (* 1.0 (- 1 (sqrt (/ (random 9999) 9999.0))))) ;; A number between 0 and 0.5, but on average closer to 0.
                          (define max-velocity (min 1.0 (+ velocityvalue range)))
                          (define min-velocity (max 0.0 (- velocityvalue range)))
                          (<ra> :set-velocity (myrand min-velocity max-velocity) 'same-place velocitynum notenum tracknum)
                          )
                        (iota (<ra> :get-num-velocities notenum tracknum))))
            (iota (<ra> :get-num-notes tracknum))))

(define (replace-with-random-velocities-in-block)
  (undo-block
   (lambda ()
     (for-each replace-with-random-velocities-in-track
               (iota (<ra> :get-num-tracks))))))


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

  (define random-layout (create-notem-layout (create-notem-button "Track" "replace-with-random-notes-in-track")
                                             (create-notem-button "Block" "replace-with-random-notes-in-block")))

  (define random-velocities-layout (create-notem-layout (create-notem-button "Track" "replace-with-random-velocities-in-track")
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


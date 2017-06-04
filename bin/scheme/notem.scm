
(provide 'notem.scm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit tab in the lower tabs


(define-constant *notem-gui* (if (not (defined? '*notem-gui*))
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



(define (create-transpose-buttons groupname func-creator)
  (define (up how-much)
    (<-> "↑  " how-much))
  (define (down how-much)
    (<-> "↓  " how-much))
  (define ret (<gui> :group groupname
                     (<gui> :horizontal-layout
                            (<gui> :vertical-layout
                                   (<gui> :button (up 1) (func-creator 1))
                                   (<gui> :button (down 1) (func-creator -1)))
                            (<gui> :vertical-layout
                                   (<gui> :button (up 7) (func-creator 7))
                                   (<gui> :button (down 7) (func-creator -7)))
                            (<gui> :vertical-layout
                                   (<gui> :button (up 12) (func-creator 12))
                                   (<gui> :button (down 12) (func-creator -12))))))
  ret)

  
         
(define (create-under-construction)
  (mid-horizontal-layout (<gui> :text "Under construction. Look at the Editor menu.")))

(define (create-transpose-notem)
  (define (create-callback-creator func)
    (lambda (how-much)
      (lambda ()
        (func how-much))))

  (<gui> :scroll-area #t #t (<gui> :vertical-layout
                                   (<gui> :flow-layout
                                          (create-transpose-buttons "Note" (create-callback-creator ra:transpose-note))
                                          (create-transpose-buttons "Range" (create-callback-creator ra:transpose-range))
                                          (create-transpose-buttons "Track" (create-callback-creator ra:transpose-track))
                                          (create-transpose-buttons "Block" (create-callback-creator ra:transpose-block))
                                          )
                                   (mid-vertical-layout (create-under-construction)))))
         
  
(add-notem-tab "Quantization" (create-quantitize-gui-for-tab))
(add-notem-tab "Transpose" (create-transpose-notem))
(add-notem-tab "More" (mid-vertical-layout (create-under-construction)))




#!!
(<gui> :show (create-transpose-notem))

(add-notem-tab "Transpose" (create-transpose-notem))
(add-notem-tab "Transpose2" (<gui> :flow-layout
                                  (<gui> :button "b1")
                                  (<gui> :button "b2")))

(<gui> :set-style-sheet-recursively *notem-gui*
       (<-> "QScrollArea"
            "{"
            "  background-color: transparent;"
            "}"
            "QScrollArea > QWidget > QWidget { background: transparent; }"
            ))

(<gui> :set-style-sheet-recursively *notem-gui*
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


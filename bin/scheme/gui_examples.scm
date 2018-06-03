
(define slider1 (<gui> :horizontal-int-slider "helloslider1: " -5 10 100 (lambda (asdf) (c-display "moved" asdf)) ))
(define slider2 (<gui> :horizontal-int-slider "helloslider2: " -5 10 100 (lambda (asdf) (c-display "moved" asdf)) ))
(define checkbox1 (<gui> :checkbox "hello1" #f (lambda (val) (c-display "checkbox" val))))
(define checkbox2 (<gui> :checkbox "hello1" #f (lambda (val) (c-display "checkbox" val))))

(define group (<gui> :group "stuff"
                     (<gui> :vertical-layout
                            slider1
                            slider2
                            (<gui> :horizontal-layout
                                   checkbox1
                                   checkbox2)
                            (<gui> :vertical-slider "aiai" 0 5 10 (lambda (i) (c-display "aiai" i))))))

(<gui> :show group)

(macroexpand (<gui> :show group))
 
(<gui> :vertical-slider "aiai" 0 5 10 (lambda (i) (c-display "aiai" i)))
(macroexpand (<gui> :vertical-slider "aiai" 0 5 10 (lambda (i) (c-display "aiai" i))))

(aritable? ra:gui_vertical-layout 0)

(<gui> :group "stuff"
       (<gui> :table-layout
              (list
               (list slider1        (<gui> :text "this slider does that"))
               (list slider2        (<gui> :text "and that slider does that"))
               (list (<gui> :empty) (<gui> :text "Some extra text here in the bottom right corner")))))

(define tablelayout (<gui> :table-layout
                           (list slider1        (<gui> :text "this slider does that"))
                           (list slider2        (<gui> :text "and that slider does that"))
                           (list (<gui> :empty) (<gui> :text "Some extra text here in the bottom right corner"))))

(<gui> :show tablelayout)


(define checkbox (<gui> :checkbox "hello" #f (lambda (val) (c-display "checkbox" val))))
(define button (<gui> :button "hello" (lambda () (c-display "clicked"))))
(define hslider (<gui> :horizontal-int-slider "helloslider: " -5 10 100 (lambda (asdf) (c-display "moved" asdf)) ))
(define vslider (<gui> :vertical-slider "helloslider: "  -5 10 100 (lambda (asdf) (c-display "moved" asdf))))
(<gui> :show checkbox)
(<gui> :show button)
(<gui> :add-callback button (lambda () (c-display "clicked 2")))
(<gui> :show vslider)
(<gui> :show hslider)
(<gui> :get-value vslider)
(<gui> :set-value vslider 80)
       
(define vbox (<gui> :vertical-layout))
(<gui> :show vbox)
(<gui> :add vbox button)
(<gui> :add vbox (<gui> :button "hello2" (lambda () (c-display "clicked2"))))
(<gui> :close button)

(define hbox (<gui> :horizontal-layout))
(<gui> :add hbox (<gui> :button "hello3" (lambda () (c-display "clicked3"))))
(<gui> :add hbox (<gui> :button "hello4" (lambda () (c-display "clicked4"))))

(define group (<gui> :group "dasgroupbox"))
(<gui> :show group)
(<gui> :add group vbox)
(<gui> :add group hbox)

(<gui> :show group)
(<gui> :hide group)
(<gui> :close group)

(define hbox2 (<gui> :horizontal-layout))
(<gui> :add hbox2 group)
(<gui> :add hbox2 (<gui> :button "hello5" (lambda () (c-display "clicked5"))))
(<gui> :show hbox2)

(define table (<gui> :table-layout 4))
(<gui> :show table)
(<gui> :add table (<gui> :button "hello5" (lambda () (c-display "clicked5"))))
(<gui> :add table (<gui> :vertical-slider "helloslider: "  -5 10 100 (lambda (asdf) (c-display "moved" asdf))))
(<gui> :add table (<gui> :group "dasgroupbox"))
(<gui> :close table)


(begin
  (define stuff
    (<gui> :group "stuff"
           (<gui> :table-layout
                  (list (<gui> :button "hello5" (lambda ()
                                                  (c-display "clicked5")))
                        (<gui> :group "V int stuff"
                               (<gui> :text "this int slider does that")
                               (<gui-number-input> "aiai"
                                                   :input-type 'int
                                                   :direction 'vertical
                                                   :min 50
                                                   :curr 5
                                                   :max -2
                                                   :callback (lambda (val)
                                                               (c-display "got" val))))
                        (<gui> :group "V stuff"
                               (<gui> :text "this float slider does that")
                               (<gui-number-input> "aiai"
                                                   :input-type 'float
                                                   :direction 'vertical
                                                   :min -2
                                                   :curr 5
                                                   :max 50
                                                   :callback (lambda (val)
                                                               (c-display "got" val))))
                        (<gui> :group "int stuff"
                               (<gui> :text "this int slider does that")
                               (<gui-number-input> "aiai"
                                                   :input-type 'int
                                                   :min -2
                                                   :curr 5
                                                   :max 50
                                                   :callback (lambda (val)
                                                               (c-display "got" val))))
                        (<gui> :group "more stuff"
                               (<gui> :text "this float slider does that")
                               (<gui-number-input> "aiai"
                                                   :curr 0.5
                                                   :callback (lambda (val)
                                                               (c-display "got" val)))))
                  (list (<gui> :button "hello6" (lambda ()
                                                  (c-display "clicked6")))
                        (<gui> :checkbox "checkbox" #t (lambda (val)
                                                         (c-display "checkbox" val)))
                        (<gui> :radiobutton "radio1" #t (lambda (val)
                                                          (c-display "radio1" val)))
                        (<gui> :radiobutton "radio2" #f (lambda (val)
                                                          (c-display "radio2" val)))
                        (<gui> :horizontal-int-slider "helloslider1: " -5 10 100 (lambda (asdf) (c-display "moved1" asdf)) )
                        (<gui> :horizontal-slider "helloslider2: " -5 10 100 (lambda (asdf) (c-display "moved2" asdf)) )
                        (<gui> :vertical-int-slider "helloslider3: "  -5 10 100 (lambda (asdf) (c-display "moved3" asdf)))
                        (<gui> :vertical-slider "helloslider4: "  -5 10 100 (lambda (asdf) (c-display "moved4" asdf)))
                        (<gui> :text "and that slider does that"))
                  (list (<gui> :int-text -2 8 20 (lambda (val)
                                                   (c-display "inttext" val)))
                        (<gui> :line "hello" (lambda (val)
                                               (c-display "line" val)))                                               
                        (<gui> :text-edit "hello2" (lambda (val)
                                                     (c-display "text" val)))                                               
                        (<gui> :float-text -2 8 20 2 0.5 (lambda (val)
                                                           (c-display "floattext" val))))
                  (list (<gui> :text "Some extra text <A href=\"http://www.notam02.no\">here</A> in the bottom <h1>right</h1> corner" "red")                        
                        (<gui> :horizontal-layout
                               (<gui> :text "atext1")
                               (<gui> :text "atext2")
                               (<gui> :text "atext2"))
                        (<gui> :vertical-layout
                               (<gui> :text "text1")
                               (<gui> :text "text2")
                               (<gui> :text "text2"))))))
  (<gui> :show stuff)
  )


;; flow layout
(begin
  (define stuff
    (<gui> :group "stuff"
           (<gui> :flow-layout
                  (list (<gui> :button "hello5" (lambda ()
                                                  (c-display "clicked5")))
                        (<gui> :group "V int stuff"
                               (<gui> :text "this int slider does that")
                               (<gui-number-input> "aiai"
                                                   :input-type 'int
                                                   :direction 'vertical
                                                   :min 50
                                                   :curr 5
                                                   :max -2
                                                   :callback (lambda (val)
                                                               (c-display "got" val))))
                        (<gui> :group "V stuff"
                               (<gui> :text "this float slider does that")
                               (<gui-number-input> "aiai"
                                                   :input-type 'float
                                                   :direction 'vertical
                                                   :min -2
                                                   :curr 5
                                                   :max 50
                                                   :callback (lambda (val)
                                                               (c-display "got" val))))
                        (<gui> :group "int stuff"
                               (<gui> :text "this int slider does that")
                               (<gui-number-input> "aiai"
                                                   :input-type 'int
                                                   :min -2
                                                   :curr 5
                                                   :max 50
                                                   :callback (lambda (val)
                                                               (c-display "got" val))))
                        (<gui> :group "more stuff"
                               (<gui> :text "this float slider does that")
                               (<gui-number-input> "aiai"
                                                   :curr 0.5
                                                   :callback (lambda (val)
                                                               (c-display "got" val)))))
                  (list (<gui> :button "hello6" (lambda ()
                                                  (c-display "clicked6")))
                        (<gui> :checkbox "checkbox" #t (lambda (val)
                                                         (c-display "checkbox" val)))
                        (<gui> :radiobutton "radio1" #t (lambda (val)
                                                          (c-display "radio1" val)))
                        (<gui> :radiobutton "radio2" #f (lambda (val)
                                                          (c-display "radio2" val)))
                        (<gui> :horizontal-int-slider "helloslider1: " -5 10 100 (lambda (asdf) (c-display "moved1" asdf)) )
                        (<gui> :horizontal-slider "helloslider2: " -5 10 100 (lambda (asdf) (c-display "moved2" asdf)) )
                        (<gui> :vertical-int-slider "helloslider3: "  -5 10 100 (lambda (asdf) (c-display "moved3" asdf)))
                        (<gui> :vertical-slider "helloslider4: "  -5 10 100 (lambda (asdf) (c-display "moved4" asdf)))
                        (<gui> :text "and that slider does that"))
                  (list (<gui> :int-text -2 8 20 (lambda (val)
                                                   (c-display "inttext" val)))
                        (<gui> :line "hello" (lambda (val)
                                               (c-display "line" val)))                                               
                        (<gui> :text-edit "hello2" (lambda (val)
                                                     (c-display "text" val)))                                               
                        (<gui> :float-text -2 8 20 2 0.5 (lambda (val)
                                                           (c-display "floattext" val))))
                  (list (<gui> :text "Some extra text <A href=\"http://www.notam02.no\">here</A> in the bottom <h1>right</h1> corner" "red")                        
                        (<gui> :horizontal-layout
                               (<gui> :text "atext1")
                               (<gui> :text "atext2")
                               (<gui> :text "atext2"))
                        (<gui> :vertical-layout
                               (<gui> :text "text1")
                               (<gui> :text "text2")
                               (<gui> :text "text2"))))))
  (<gui> :show stuff)
  )

(define flow (<gui> :flow-layout (map (lambda (n)
                                        (<gui> :text (<-> "hello" n)))
                                      (iota 30))))


(for-each (lambda (n)
            (<gui> :add flow (<gui> :text (<-> "hello" n))))
          (iota 30))

(<gui> :show flow)


             

       

(define ui (<gui> :ui "/home/kjetil/radium/Qt/qt4_preferences.ui"))
(define ui (<gui> :ui "/home/kjetil/radium/Qt/qt4_soundfilesaver_widget.ui"))
(define ui (<gui> :ui "/home/kjetil/radium/Qt/qt4_bottom_bar_widget.ui"))
(define ui (<gui> :ui "/home/kjetil/radium/Qt/test.ui"))
(define ui (<gui> :ui "/home/kjetil/radium/bin/test.ui"))
(<gui> :show ui)
(<gui> :close ui)

(define push (<gui> :child ui "pushButton"))
(define check (<gui> :child ui "checkBox"))
(define radio (<gui> :child ui "radioButton"))
(define spinbox (<gui> :child ui "spinBox"))
(define doublespinbox (<gui> :child ui "doubleSpinBox"))
(define label (<gui> :child ui "label"))
(define plainTextEdit (<gui> :child ui "plainTextEdit"))
(define lineEdit (<gui> :child ui "lineEdit"))
(<gui> :show push)
(<gui> :hide push)

(<gui> :get-value push)
(<gui> :get-value check)
(<gui> :get-value radio)
(<gui> :get-value spinbox)
(<gui> :get-value label)
(<gui> :get-value plainTextEdit)
(<gui> :get-value lineEdit)

(<gui> :add-callback push (lambda ()
                            (c-display "Im pressed")))

(<gui> :add-callback check (lambda (val)
                            (c-display "check called" val)))

(<gui> :add-callback radio (lambda (val)
                             (c-display "radio called" val)))

(<gui> :add-callback lineEdit (lambda (val)
                                (c-display "lineEdit2:" val)))

(<gui> :add-callback spinbox (lambda (val)
                               (c-display "spinBox:" val)))

(<gui> :add-callback doublespinbox (lambda (val)
                                     (c-display "doubleSpinBox:" val)))



(define line (<gui> :line "hello" (lambda (val)
                                    (c-display "line" val))))


(<gui> :show line)

(define widget (<gui> :widget 300 300))
(<gui> :show widget)

(<gui> :add-mouse-callback widget (lambda (button state x y)
                                    (c-display "button:" button "x/y:" x y)
                                    #t))

(<gui> :draw-line widget "#xff0000" 5 10 100 150 2.0)
(<gui> :draw-line widget #x00ff00 59 60 150 190 2.0)
(<gui> :draw-line widget #x80ff0060 159 60 150 190 2.0)
(<gui> :draw-line widget #xffff0060 59 60 150 190 2.0)
(<gui> :draw-line widget #x00ff0060 9 60 150 190 2.0)

(<gui> :draw-line widget  "#80ff0000" 30 60 150 190 40.0)
(<gui> :filled-box widget "#800000ff" 20 20 180 190)

(<gui> :draw-text widget "#80000050" "hello" 50 50 100 120)
(<gui> :draw-vertical-text widget "white" "hello" 50 50 100 120)

(let ()
  (define widget (<gui> :widget 600 600))
  (<gui> :show widget)
  (define x 0)
  (define y 0)
  (<gui> :add-paint-callback widget
         (lambda (width height)
           (<gui> :filled-box widget "white" 0 0 width height)
           (define x1 150)
           (define x2 450)
           (define y1 150)
           (define y2 450)
           (<gui> :draw-box widget "black" x y (+ x 200) (+ y 200) 2)
           (<gui> :draw-text widget "red" "hello" x y (+ x 200) (+ y 200))
           (<gui> :draw-text widget "red" "hello" x y (+ x 200) (+ y 200) #t #f #f 200)
           ))

  (<gui> :add-mouse-callback widget
         (lambda (button state x_ y_)
           (set! x x_)
           (set! y y_)
           (c-display "UPDATING A")
           (<gui> :update widget)
           (c-display "FINISHED UPDATING A")
           #t)))
  
(define hslider (<gui> :horizontal-int-slider "helloslider: " -5 10 100 (lambda (asdf) (c-display "moved" asdf)) ))

(<gui> :add widget hslider 50 50 290 100)

(<gui> :show hslider)

(define scroll-area (<gui> :scroll-area #t #t))
(<gui> :show scroll-area)
(<gui> :hide scroll-area)
(<gui> :add scroll-area
       (<gui> :horizontal-int-slider "helloslider: " -5 10 100 (lambda (asdf) (c-display "moved" asdf)) )
       10 10 100 100)
(<gui> :add scroll-area
       (<gui> :horizontal-int-slider "2helloslider: " -5 10 100 (lambda (asdf) (c-display "moved" asdf)) )
       110 110 100 200)


(define scroll-area (<gui> :vertical-scroll))

(<gui> :show scroll-area)

(<gui> :add scroll-area (<gui> :horizontal-int-slider "2helloslider: " -5 10 100 (lambda (asdf) (c-display "moved" asdf))))

(define scroll-area (<gui> :horizontal-scroll))

(<gui> :show scroll-area)

(<gui> :add scroll-area (<gui> :vertical-int-slider "s:" -5 10 100 (lambda (asdf) (c-display "moved" asdf))))

(begin
  #xf)

(define widget (<gui> :widget 100 100))

(<gui> :show widget)
(<gui> :cloxse widget)

(<ra> :obtain-keyboard-focus widget);

(<gui> :add-key-callback widget
       (lambda (presstype key)
         (c-display presstype key)
         #f))

(<ra> :release-keyboard-focus)

;; Prints out number of open GUIs every second.
(<ra> :schedule 0
      (lambda ()
        (c-display "Num guis:" (<gui> :num-open-guis))
        1000))


(define requester (<gui> :file-requester
                         "header text"
                         ""
                         "Radium song files" 
                         "*.rad *.mmd1 *.cpp"
                         #f
                         (lambda (name)
                           (c-display "GOT" name))))

(<gui> :show requester)


(define ui (<gui> :ui "/home/kjetil/radium/bin/test.ui"))
(<gui> :show ui)

(<gui> :add-mouse-callback (<gui> :get-tab-bar (<gui> :child ui "tabWidget"))
       (lambda (button state x y)
         (c-display "tab bar callback" button state x y)
         #f))



(define (add-notem-tab name gui)
  ...)


(define ratio (<gui> :ratio 5/2))

(<gui> :show ratio)

(<gui> :get-value ratio)




(define aline (<gui> :line "hello"))

(<gui> :show aline)

(<gui> :add-callback aline
       (lambda (val)
         (c-display "val:" val)))


(define fontreq
  (let ((fontreq (<gui> :font-requester "")))

    (define org-font (<ra> :get-system-font))
    
    (<gui> :add-callback fontreq
           (lambda (fontstring-or-buttons)
             (c-display "fontstring:" fontstring-or-buttons ". gui:" fontreq)
             (if (boolean? fontstring-or-buttons)
                 (begin
                   (if (not fontstring-or-buttons)
                       (<ra> :set-system-font org-font))
                   ;;(<gui> :close fontreq)
                   )
                 (<ra> :set-system-font fontstring-or-buttons))))

    (<gui> :set-parent fontreq -2)
    
    (<gui> :show fontreq)
    fontreq))

(popup-menu
 (list
  "hello-1" (lambda ()
              #t)
  "----Radibuttons"
  (list
   :radio-buttons
   (list "hello0"
         :check #f
         (lambda (ison)
           (c-display "hello0:" ison)))
   (list "hello1"
         :check #t
         :enabled #t
         (lambda (ison)
           (c-display "hello1" ison))))
  (list "hello2"
        :check #f
        :icon (<ra> :to-base64 "/home/kjetil/radium/temp/radium_64bit_linux-5.4.8/bin/radium_256x256x32.png")
        (lambda (ison)
          (c-display "hello2" ison)))
  "--------------"
  "hello3" (lambda ()
             #t)))
)



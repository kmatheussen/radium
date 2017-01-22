
(<gui> :group "stuff"
       (<gui> :vertical-layout
              slider1
              slider2
              (<gui> :horizontal-layout
                     checkbox1
                     checkbox2)
              (<gui> :vertical-slider "aiai" 0 5 10 1 (lambda (i) (c-display "aiai" i)))))


(<gui> :group "stuff"
       (<gui> :table-layout
              (list
               (list slider1        (<gui> :text "this slider does that"))
               (list slider2        (<gui> :text "and that slider does that"))
               (list (<gui> :empty) (<gui> :text "Some extra text here in the bottom right corner")))))




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


(define ui (<gui> :ui "/home/kjetil/radium/Qt/qt4_preferences.ui"))
(define ui (<gui> :ui "/home/kjetil/radium/Qt/qt4_soundfilesaver_widget.ui"))
(define ui (<gui> :ui "/home/kjetil/radium/Qt/qt4_bottom_bar_widget.ui"))
(define ui (<gui> :ui "/home/kjetil/radium/Qt/test.ui"))
(<gui> :show ui)

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

(<gui> :draw-line widget  #x80ff0000 30 60 150 190 40.0)
(<gui> :filled-box widget "#800000ff" 20 20 180 190)

(<gui> :draw-text widget "#80000050" "hello" 50 50 100 120)

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
(<gui> :close widget)

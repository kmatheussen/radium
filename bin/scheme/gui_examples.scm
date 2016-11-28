


(define layout (create-vertical-layout))

(define slider1 (create-vslider "int" 0 5 10 1 (lambda (i) (c-display "int" i))))
(define slider2 (create-vslider "float" 0.0 5.0 10.0 (lambda (f) (c-display "float" f))))

(layout :add slider1)
(layout :add slider2)


(vertical-layout "stuff"
                 slider1
                 slider2
                 (horizontal-layout ""
                                    checkbox1
                                    checkbox2)
                 slider3)


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





(define button (<gui> :button "hello" (lambda () (c-display "clicked"))))
(define hslider (<gui> :horizontal-int-slider "helloslider: " -5 10 100 (lambda (asdf) (c-display "moved" asdf)) ))
(define vslider (<gui> :vertical-slider "helloslider: "  -5 10 100 (lambda (asdf) (c-display "moved" asdf))))
(<gui> :show button)
(<gui> :add-callback button (lambda () (c-display "clicked 2")))
(<gui> :show vslider)
(<gui> :get-value vslider)
(<gui> :set-value vslider 80)
       
(define vbox (<gui> :vertical))
(<gui> :show vbox)
(<gui> :add vbox button)
(<gui> :add vbox (<gui> :button "hello2" (lambda () (c-display "clicked2"))))
(<gui> :close button)

(define hbox (<gui> :horizontal))
(<gui> :add hbox (<gui> :button "hello3" (lambda () (c-display "clicked3"))))
(<gui> :add hbox (<gui> :button "hello4" (lambda () (c-display "clicked4"))))

(define group (<gui> :group "dasgroupbox"))
(<gui> ::show group)
(<gui> :add group vbox)
(<gui> :add group hbox)

(<gui> :show guinum)
(<gui> :hide guinum)
(<gui> :close guinum)

(define hbox2 (<gui> :horizontal))
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
(define label (<gui> :child ui "label"))

(<gui> :show push)
(<gui> :hide push)

(<gui> :get-value push)
(<gui> :get-value check)
(<gui> :get-value radio)
(<gui> :get-value spinbox)
(<gui> :get-value label)

(<gui> :add-callback push (lambda ()
                            (c-display "Im pressed")))



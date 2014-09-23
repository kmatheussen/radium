(provide 'mouse/mouse.scm)

;; Mouse move handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *mouse-move-handlers* '())

(delafina (add-mouse-move-handler :move)
  (push-back! *mouse-move-handlers* move))


(define (run-mouse-move-handlers button x y)
  (for-each (lambda (move-handler)
              (move-handler button x y))
            *mouse-move-handlers*))


;; Mouse cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct mouse-cycle
  :press-func 
  :drag-func (lambda x #f)
  :release-func (lambda x #f)
  )

(define *mouse-cycles* '())
(define *current-mouse-cycle* #f)

(define (add-mouse-cycle $mouse-cycle)
  (push-back! *mouse-cycles*
              $mouse-cycle))


(define (get-mouse-cycle button x y)
  #f)


;; Functions called from radium
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (radium-mouse-press $button $x $y)
  (if (not *current-mouse-cycle*)
      (set! *current-mouse-cycle* (get-mouse-cycle $button $x $y)))
  current-mouse-cycle)

(define (get-mouse-cycle $button $x $y)
  (find-if (lambda (cycle)
             ((cycle :press-func) $button $x $y))
           *mouse-cycles*))
  
(define (radium-mouse-press $button $x $y)
  ;;(c-display "mouse press" $button $x $y)
  (if (not *current-mouse-cycle*)
      (set! *current-mouse-cycle* (get-mouse-cycle $button $x $y)))
  *current-mouse-cycle*)

(define (radium-mouse-move $button $x $y)
  ;;(c-display "mouse move" $button $x $y)
  (if *current-mouse-cycle*
      (begin 
        ((*current-mouse-cycle* :drag-func) $button $x $y)
        #t)
      (begin
        (run-mouse-move-handlers $button $x $y)
        #f)))

(define (radium-mouse-release $button $x $y)
  ;;(c-display "mouse release" $button $x $y)
  (if *current-mouse-cycle*
      (begin
        ((*current-mouse-cycle* :release-func) $button $x $y)
        (set! *current-mouse-cycle* #f)
        #t)
      #f))


#|
(add-mouse-cycle (make-mouse-cycle
                  :press-func (lambda ($button $x $y)
                                (c-display "pressed it" $x $y)
                                #t)
                  :drag-func  (lambda ($button $x $y)
                                (c-display "moved it" $x $y))
                  :release-func (lambda ($button $x $y)
                                  (c-display "released it" $x $y))))
|#


;; reltempo
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-mouse-move-handler
 :move (lambda (button x y)
         (if (inside-box (r-get-box reltempo-slider) x y)
             (r-show-reltempo-in-statusbar))))


(delafina (add-delta-mouse-handler :press :move-and-release)
  (define start-x #f)
  (define start-y #f)
  (define value #f)
  (define (call-move-and-release $button $x $y)
    (move-and-release $button
                      start-x
                      start-y
                      (- $x start-x)
                      (- $y start-y)
                      value))
  (add-mouse-cycle (make-mouse-cycle
                    :press-func (lambda ($button $x $y)
                                  (set! value (press $button $x $y))
                                  (if value
                                      (begin
                                        (set! start-x $x)
                                        (set! start-y $y)
                                        #t)
                                      #f))
                    :drag-func  call-move-and-release
                    :release-func call-move-and-release)))
  

;; slider
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-delta-mouse-handler
 :press (lambda ($button $x $y)
          ;;(c-display "inside? " (inside-box (r-get-box reltempo-slider) $x $y) $x $y "box:" (box-to-string (r-get-box reltempo-slider)))
          (and (or #t (= $button *left-button*))
               (inside-box (r-get-box reltempo-slider) $x $y)
               (begin
                 (r-undo-reltempo)
                 (r-get-reltempo))))

 :move-and-release (lambda ($button $start-x $start-y $dx $dy $org-reltempo)
                     (define box          (r-get-box reltempo-slider))
                     (define reltempo     (r-get-reltempo))
                     (define min-reltempo (r-get-min-reltempo))
                     (define max-reltempo (r-get-max-reltempo))
                     (r-set-reltempo (+ $org-reltempo
                                        (scale $dx
                                               0 (box :width)
                                               min-reltempo max-reltempo))))
 )



#|

(define (mouse-press button x* y*)
  (if (not curr-mouse-cycle)
      (set! curr-mouse-cycle (get-mouse-cycle button x* y*))))

(define (mouse-drag button x* y*)
  (if curr-mouse-cycle
      ((cadr curr-mouse-cycle) button x* y*)))

(define (mouse-release button x* y*)
  (let ((mouse-cycle curr-mouse-cycle))
    (set! curr-mouse-cycle #f)
    (if mouse-cycle
        ((caddr mouse-cycle) button x* y*))))

|#

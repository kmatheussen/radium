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

(define (add-mouse-cycle mouse-cycle)
  (push-back! mouse-cycle
              mouse-cycles)
  )


;; Functions called from radium
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (radium-mouse-press button x y)
  (c-display "mouse press" button x y)
  #f)

(define (radium-mouse-move button x y)
  (c-display "mouse move" button x y)
  (if *current-mouse-cycle*
      (begin 
        ((*current-mouse-cycle* :drag-func) button x y)
        #t)
      (begin
        (run-mouse-move-handlers button x y)
        #f)))

(define (radium-mouse-release button x y)
  (c-display "mouse release" button x y)
  #f)




;; reltempo
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-mouse-move-handler
 :move (lambda (button x y)
         (if (inside-box (r-get-box reltempo-slider) x y)
             (r-show-reltempo-in-statusbar))))


#|
;; Tempo slider
(add-delta-mouse-handler
 :press (lambda (button x y)
          (and (= button *left-button*)
               (r-inside-box (r-get-reltempo-slider-box) x y)
               (begin
                 (r-undo-reltemposlider)
                 (r-get-reltempo))))

 :move-and-release (lambda (button start-x start-y dx dy org-reltempo)
                     (define box          (r-get-reltempo-slider-box))
                     (define reltempo     (r-get-reltempo))
                     (define min-reltempo (r-get-min-reltempo))
                     (define max-reltempo (r-get-max-reltempo))
                     (r-set-reltempo (+ org-reltempo
                                        (scale dx
                                               0 (box :width)
                                               min-reltempo max-reltempo))))
 )


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

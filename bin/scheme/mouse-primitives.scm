(provide 'mouse-primitives.scm)

(define-constant *left-button* 1) ;; TR_LEFTMOUSE
(define-constant *middle-button* 3) ;; TR_MIDDLEMOUSE
(define-constant *right-button* 5) ;; TR_RIGHTMOUSE
(define-constant *delete-button* 15) ;; TR_DELETEMOUSEDOWN

(define-constant *is-pressing* 1) ;; API_MOUSE_PRESSING
(define-constant *is-moving* 2) ;; API_MOUSE_MOVING
(define-constant *is-releasing* 3)  ;; API_MOUSE_RELEASING
(define-constant *is-leaving* 4)  ;; API_MOUSE_LEAVING (mouse has left the qt widget)
(define-constant *is-entering* 5)  ;; API_MOUSE_ENTERING (mouse has entered the qt widget)

(define-constant *shift-right-mouse* ra:simulate-delete-mouse-button)
;;(define-constant *shift-right-mouse* "Shift + Right Mouse")


(define (select-button? Button)
  (= *left-button* Button))

(define (menu-button? Button)
  (= *right-button* Button))

(define (delete-button? Button)
  (or (= Button *delete-button*)
      (and (=  Button *right-button*)
           (<ra> :shift-pressed))))

(define (left-or-right-button? Button)
  (or (= *left-button* Button)
      (= *right-button* Button)))

(define-struct mouse-cycle
  :press-func 
  :drag-func #f
  :release-func #f
  :inside? #f ;; used by the nonpress-mouse-cycle in area.scm
  )

(define2 *current-mouse-cycle* (curry-or not hash-table?) #f) ;; Current mouse cycle in mouse.scm


(define *check-mouse-horizontal-modifier* #t)


(define (only-y-direction)
  (and *check-mouse-horizontal-modifier*
       (not (<ra> :horizontal-modifier-pressed))
       (<ra> :vertical-modifier-pressed)))

(define (only-x-direction)
  (and (not (<ra> :vertical-modifier-pressed))
       (<ra> :horizontal-modifier-pressed)))

(define2 mouse-pointer-has-been-set boolean? #f)
(define2 mouse-pointer-guinum number? -4)
(define (set-mouse-pointer func guinum)
  ;;(c-display "  setting mouse func to" func)
  ;;(c-display (safe-history-ow!))
  ;;(if (eq? func ra:set-normal-mouse-pointer)
  ;;    (pretty-print (*s7* 'stack)))
  (set! mouse-pointer-has-been-set #t)
  (set! mouse-pointer-guinum guinum)
  ;;(c-display "  setting mouse func to" func)
  (func guinum)
  )


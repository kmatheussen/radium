(provide 'mouse/mouse.scm)

(define (radium-mouse-press button x y)
  (c-display "mouse press" button x y)
  #f)

(define (radium-mouse-drag button x y)
  (c-display "mouse drag" button x y)
  #f)

(define (radium-mouse-release button x y)
  (c-display "mouse release" button x y)
  #f)

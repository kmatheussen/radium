
(define (radium-mouse-press button x y)
  (c-display "mouse press" button x y))

(define (radium-mouse-drag button x y)
  (c-display "mouse drag" button x y))

(define (radium-mouse-release button x y)
  (c-display "mouse release" button x y))


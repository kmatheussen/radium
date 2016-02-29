

;; get-range-fx and get-track-fx returns:
'((fxname1 ((pos1 value2) ;; Er navn bra nok som identifikator? Er range alltid mellom 0 og 1? Kan bare legalisere range.
            (pos2 value2)
            ...))
  (fxname2 ...)
  ...)

(define (get-range-fx rangetracknum)
  ...)

(define (get-track-fx blocknum tracknum)
  ...)

(define (merge-fx fx1 fx2)
  ...)

(define (paste-track-fx blocknum tracknum fx)
  ...)

(define (paste-range blocknum starttrack)
  (let loop ((rangetracknum 0))
    (define tracknum (+ rangetracknum starttrack))  
    (let ((range-fx (get-range-fx rangetracknum))
          (track-fx (get-track-fx blocknum tracknum)))
      (if (or (not range-fx)
              (not track-fx))
          #t
          (begin
            (paste-track-fx blocknum tracknum (merge-fx track-fx range-fx)))
            (loop (1+ rangetracknum)))))))

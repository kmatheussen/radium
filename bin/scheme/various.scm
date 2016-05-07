(provide 'various.scm)


(define (line-zoom-out-exponentially)
  (define curr (<ra> :get-line-zoom-block))
  ;;(c-display "z1" curr)
  (cond ((< curr 0 )
         (<ra> :line-zoom-block (- (floor (/ curr 2)))))
        (else
         (if (< curr 40)
             (<ra> :line-zoom-block curr)))))

(define (line-zoom-in-exponentially)
  (define curr (<ra> :get-line-zoom-block))
  ;;(c-display "z2" curr)
  (cond ((= 1 curr)
         (<ra> :line-zoom-block -1))
        ((< curr 0 )
         (if (> curr -32)
             (<ra> :line-zoom-block curr)))
        (else
         (<ra> :line-zoom-block (- (floor (/ curr 2)))))))

#!!
(line-zoom-out-exponentially)
(line-zoom-in-exponentially)
!!#

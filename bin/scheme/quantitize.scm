(provide 'quantitize.scm)


(delafina (quantitize-note :start
                           :end
                           :q
                           :max-length
                           :quantitize-start #t
                           :quantitize-end #t
                           :keep-note-length #f
                           :type 3 ;; See GUI. Type 1 is "Move start position ...", type 2 is "Move end ...", etc.
                           )

  (define delete-it #f)
  
  (define new-start (if quantitize-start
                        (quantitize start q)
                        start))

  (define new-end (if quantitize-end
                      (quantitize end q)
                      end))
  
  (define org-length (- end start))

  (define (keep-original-note-length!)
    (begin
      (if quantitize-start
          (set! new-end (+ new-start org-length)))
      (if quantitize-end
          (set! new-start (- new-end org-length)))))

  (if (<= q 0)
      (throw 'illegal-quantitize-value q))

  (c-display "*** Calling quantitizenote" start end ", len:" org-length ", q:" q ", quant start:" quantitize-start ", quant end:" quantitize-end ", keep-note-length:" keep-note-length ", type:" type)
  
  (if keep-note-length
      (keep-original-note-length!))

  (define (legalize-length!)
    (c-display "calling legalize-length? " (>= new-start new-end))
    (if (>= new-start new-end)
        (cond ((= type 1) ;; move-start-to-previous
               (set! new-start (quantitize new-start q))
               (while (>= new-start new-end)
                 (set! new-start (- new-start q))))
              
              ((= type 2)
               (set! new-end (quantitize new-end q))
               (while (>= new-start new-end)
                 (set! new-end (+ new-end q))))
              
              ((= type 3)
               (keep-original-note-length!))
              
              ((= type 4)
               (set! new-end end)
               (while (>= new-start new-end)
                 (set! new-end (+ new-end q))))
              
              ((= type 5)
               (set! delete-it #t)))))


  (define (legal-pos pos)
    (cond ((< pos 0)
           0)
          ((> pos max-length)
           max-length)
          (else
           pos)))

  (define (legalize!)
    (legalize-length!)
    (set! new-start (legal-pos new-start))
    (set! new-end (legal-pos new-end)))

  (c-display "bef: new-start/new-end" new-start new-end ", org-len:" org-length)
  ;;(keep-original-note-length!)
  ;;(c-display "bef2: new-start/new-end" new-start new-end)

  (c-display "type: " type ", empty:" (>= new-start new-end))
  
  (legalize!)

  (if (>= new-start new-end)
      (legalize!))
  
  (if (>= new-start new-end)
      (legalize!))

  (c-display "aft: new-start/new-end" new-start new-end)
  
  (if delete-it
      #f
      (cons new-start new-end))
  )


         

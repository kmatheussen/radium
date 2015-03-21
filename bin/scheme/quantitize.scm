

(delafina (quantitize-note :start
                           :end
                           :q
                           :max-length
                           :quantitize-start #t
                           :quantitize-end #t
                           :keep-note-length #f
                           :type 3
                           )

  (define new-start (if quantitize-start
                        (quantitize start q)
                        start))

  (define new-end (if quantitize-end
                      (quantitize end q)
                      end))
  
  (define org-length (- end start))

  (cond ((and keep-note-length new-start)
         (set! new-end (+ start org-length)))
        ((and keep-note-length new-end)
         (set! new-start (- end org-length))))

  (if (< start 0)
      (set! start 0))

  (if (> 

         

(provide 'solo.scm)

(my-require 'instruments.scm)


(define-struct audio-connection
  :source-id
  :target-id)

(define (audio-connection<? a b)
  (cond ((< (<ra> :get-audio-instrument-num (a :source-id))
            (<ra> :get-audio-instrument-num (b :source-id)))
         #t)                             
        ((> (<ra> :get-audio-instrument-num (a :source-id))
            (<ra> :get-audio-instrument-num (b :source-id)))
         #f)
        (else
         (< (<ra> :get-audio-instrument-num (a :target-id))
            (<ra> :get-audio-instrument-num (b :target-id))))))

(define (audio-connection=? a b)
  (and (equal? (a :source-id)
               (b :source-id))
       (equal? (a :target-id)
               (b :target-id))))

(define (make-audio-connection-hash-table instruments)
  (define len (max 1 (length instruments)))
  (make-hash-table len
                   (cons audio-connection=? (lambda (x)
                                              (modulo (+ (* len (<ra> :get-audio-instrument-num (x :source-id)))
                                                         (<ra> :get-audio-instrument-num (x :target-id)))
                                                      len)))))

(define (pp-audio-connections audio-connections)
  (pp (map (lambda (audio-connection)
             (<-> (<ra> :get-instrument-name (audio-connection :source-id))
                  "=>"
                  (<ra> :get-instrument-name (audio-connection :target-id))))
           (if (hash-table? audio-connections)
               (map car audio-connections)
               audio-connections))))

(define (get-all-solo-connections-in-mixer solo-instrument-id-or-ids)
  (define ret (make-audio-connection-hash-table solo-instrument-id-or-ids))
  
  (define solo-instrument-ids (if (number? solo-instrument-id-or-ids)
                                  (list solo-instrument-id-or-ids)
                                  solo-instrument-id-or-ids))
  
  (define (add-outputs! source-id)
    (for-each (lambda (target-id)
                (hash-table-set! ret
                                 (make-audio-connection :source-id source-id
                                                        :target-id target-id)
                                 #t)
                (add-outputs! target-id))
              (get-instruments-connecting-from-instrument source-id)))

  (define (add-inputs! target-id)
    (for-each (lambda (source-id)
                (hash-table-set! ret
                                 (make-audio-connection :source-id source-id
                                                        :target-id target-id)
                                 #t)
                (add-inputs! source-id))
              (get-instruments-connecting-to-instrument target-id)))

  (for-each add-inputs! solo-instrument-ids)
  (for-each add-outputs! solo-instrument-ids)

  ret)

(***assert*** (get-all-solo-connections-in-mixer (list))
              (hash-table))

#!!
(pp-audio-connections (get-all-solo-connections-in-mixer (list 23)))
(pp-audio-connections (get-all-solo-connections-in-mixer (list 17 22)))
(pp-audio-connections (get-all-solo-connections-in-mixer (list 17 22 33)))
(pp-audio-connections (get-all-solo-connections-in-mixer (list 17)))
(pp-audio-connections (get-all-solo-connections-in-mixer (list 22)))
(pp-audio-connections (get-all-solo-connections-in-mixer (list 25)))
(pp-audio-connections (get-all-solo-connections-in-mixer (list)))
(ra:get-instrument-name 17)
(ra:get-instrument-name 22)
(ra:get-instrument-name 23)
(ra:get-instrument-name 27)
(list "\n"
      (get-all-audio-instruments) "\n"
      (map ra:get-instrument-name (get-all-audio-instruments)))
(<ra> :get-connection-enabled 0 1)
(get-instruments-connecting-from-instrument 0)
(get-instruments-connecting-from-instrument 22)
(get-buses-connecting-from-instrument (get-instrument-from-name "Click") #t)
!!#

(define (get-all-connections-in-mixer instruments)
  (define ret (make-audio-connection-hash-table instruments))
  
  (for-each (lambda (target-id)
              (for-each (lambda (source-id)
                          (hash-table-set! ret
                                           (make-audio-connection :source-id source-id
                                                                  :target-id target-id)
                                           #t))
                        (get-instruments-connecting-to-instrument target-id)))
            instruments)
  ret)

#!!
(<ra> :get-connection-enabled 0 1)
(hepp)
(get-all-connections-in-mixer (get-all-audio-instruments))
!!#


(delafina (get-all-solo-instruments :all-instruments (get-all-audio-instruments)
                                    :from-storage #t)  
  (keep (lambda (instrument-id)
          (if from-storage
              (<ra> :get-instrument-solo-from-storage instrument-id)
              (<ra> :get-instrument-solo instrument-id)))
        all-instruments))

#!!
(get-all-solo-instruments)
!!#

;; not used.
(delafina (get-all-non-solo-connections-in-mixer :all-instruments (get-all-audio-instruments)
                                                 :from-storage #t)
  (define all-connections (<new> :container (get-all-connections-in-mixer all-instruments)))
  (define all-solo-connections (<new> :container (get-all-solo-connections-in-mixer (get-all-solo-instruments all-instruments from-storage))))
  ;;(c-display (all-connections :length)
  ;;           (all-solo-connections :length)
  ;;           (length (get-all-connections-in-mixer all-instruments)))
             
  (define hepp (all-solo-connections :set-difference all-connections))
  ;;(c-display "hepp:" hepp)
  (hepp :hash))


#!!
(pretty-print (get-all-audio-instruments))

(get-all-solo-instruments (get-all-audio-instruments))

(pretty-print (get-all-solo-connections-in-mixer (get-all-solo-instruments (get-all-audio-instruments))))

(pretty-print (get-all-connections-in-mixer (get-all-audio-instruments)))

(pretty-print (get-all-non-solo-connections-in-mixer (get-all-audio-instruments)))
(pretty-print (begin
                (get-all-non-solo-connections-in-mixer (get-all-audio-instruments))
                #t))

(get-all-connections-in-mixer (get-all-audio-instruments))

(c-display "result2:" (list (hash-table-entries (get-all-connections-in-mixer (get-all-audio-instruments)))
                            (hash-table-entries (get-all-non-solo-connections-in-mixer (get-all-audio-instruments)))))

(equal? (get-all-connections-in-mixer (get-all-audio-instruments))
        (get-all-non-solo-connections-in-mixer (get-all-audio-instruments)))
!!#

(define (update-implicite-instrument-solo/mutes! all-instruments all-explicit-solo-instruments)
  (define len (length all-instruments))

  (define all-solo-instruments (make-hash-table (i-max 1 len) equal?))
  
  ;;(for-each (lambda (instrument-id)
  ;;            (hash-table-set! all-solo-instruments instrument-id #t))
  ;;          all-explicit-solo-instruments)
  
  (define (set-implicit-solo! instrument-id do-forward)
    (let loop ((instrument-id instrument-id))
      (when (not (all-solo-instruments instrument-id))
        (set! (all-solo-instruments instrument-id) #t)
        (<ra> :set-instrument-is-implicitly-soloed #t instrument-id)
        ;;(c-display "******** solo / implicit solo: " (<ra> :get-instrument-name instrument-id))
        )
      (if do-forward
          (for-each loop (get-instruments-connecting-to-instrument instrument-id))
          (for-each loop (get-instruments-connecting-from-instrument instrument-id)))))

  (if (null? all-explicit-solo-instruments)
      (for-each (lambda (instrument-id)
                  (<ra> :set-instrument-is-implicitly-soloed #f instrument-id)
                  (<ra> :set-instrument-is-implicitly-muted #f instrument-id)
                  ;;(c-display "******** NOT solo or mute: " (<ra> :get-instrument-name instrument-id))
                  )
                all-instruments)
      (begin
        (for-each (lambda (instrument-id)
                    (set-implicit-solo! instrument-id #t)
                    (set-implicit-solo! instrument-id #f))
                  all-explicit-solo-instruments)
        
        (for-each (lambda (instrument-id)
                    (if (not (all-solo-instruments instrument-id))
                        (begin
                          (<ra> :set-instrument-is-implicitly-muted #t instrument-id)
                          (<ra> :set-instrument-is-implicitly-soloed #f instrument-id)
                          ;;(c-display "******** solo / implicit mute: " (<ra> :get-instrument-name instrument-id))
                          )
                        (<ra> :set-instrument-is-implicitly-muted #f instrument-id))
                    )
                  all-instruments)))
  )


(define (update-implicit-solo-connections! all-instruments)
  (define all-solo-instruments (get-all-solo-instruments all-instruments :from-storage #f))
  (define all-connections (<new> :container (get-all-connections-in-mixer all-instruments)))
  (define all-solo-connections (<new> :container (get-all-solo-connections-in-mixer all-solo-instruments)))
  ;;(c-display "solo len:" (all-solo-connections :size) ". solo:" (pp-audio-connections (all-solo-connections :hash)))
  (define changes '())

  (define no-one-has-solo (null? all-solo-instruments))

  ;; TODO: Don"t set connection-type. (It seems to be set to *auto-connection-type* now. This must be fixed in create-audio-connection-change though.
  
  (all-connections :for-each
                   (lambda (connection)
                     ;;(c-display "isthere:" (all-solo-connections :contains connection))
                     (push-audio-connection-implicitly-enabled-change! changes 
                                                                       (list :source (connection :source-id)
                                                                             :target (connection :target-id)
                                                                             :implicitly-enabled (or no-one-has-solo
                                                                                                     (all-solo-connections :contains connection))))))

  ;;(c-display "\n\nchanges:" (pp changes))
  (<ra> :change-audio-connections changes)
  
  (update-implicite-instrument-solo/mutes! all-instruments all-solo-instruments)
  )
                   
#!!
(update-implicit-solo-connections! (get-all-audio-instruments))
!!#

(define (FROM_C-update-implicit-solo-connections!)
  (update-implicit-solo-connections! (get-all-audio-instruments)))


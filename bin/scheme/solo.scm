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


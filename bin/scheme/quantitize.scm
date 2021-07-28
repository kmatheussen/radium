(provide 'quantitize.scm)

(my-require 'keybindings.scm)


(define *curr-quantitize-gui* #f)

(define (quantitize Place Q)
  (* (roundup (/ Place Q))
     Q))

#||
(quantitize 8 2)
(quantitize 18341/2134 1/3)

(begin
  (test (quantitize 0 0.5)
        0.5)
  (test (quantitize 0.5 0.5)
        0.5)
  (test (quantitize 1.0 0.5)
        1.0)
  (test (quantitize 10.0 0.5)
        10)
  (test (quantitize 10.3 0.5)
        10.5)
  (test (quantitize 10.5 0.5)
        10.5)
  (test (quantitize 10.6 0.5)
        10.5)
  (test (quantitize 10.9 0.5)
        11))
||#

(delafina (quantitize-note :start
                           :end
                           :q
                           :max-length
                           :type 3 ;; See GUI. Type 1 is "Move start position ...", type 2 is "Move end ...", etc.
                           )

  ;;(c-display "args:" start end q max-length type)
  
  (define delete-it #f)

  (define quantitize-start (if *curr-quantitize-gui*
                               (<gui> :get-value (<gui> :child *curr-quantitize-gui* "quant_start"))
                               #t))

  (define quantitize-end (if *curr-quantitize-gui*
                             (<gui> :get-value (<gui> :child *curr-quantitize-gui* "quant_end"))
                             #t))

  (define keep-note-length (if *curr-quantitize-gui*
                               (<gui> :get-value (<gui> :child *curr-quantitize-gui* "keep_length"))
                               #f))

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
      (error 'illegal-quantitize-value q))

  ;;(c-display "*** Calling quantitizenote" start end ", len:" org-length ", q:" q ", quant start:" quantitize-start ", quant end:" quantitize-end ", keep-note-length:" keep-note-length ", type:" type)
  
  (if keep-note-length
      (keep-original-note-length!))

  (define (legalize-length!)
    ;;(c-display "calling legalize-length? " (>= new-start new-end) ". type:" type)
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

  ;;(c-display "bef: new-start/new-end" new-start new-end ", org-len:" org-length)
  ;;(keep-original-note-length!)
  ;;(c-display "bef2: new-start/new-end" new-start new-end)

  ;;(c-display "type: " type ", empty:" (>= new-start new-end))
  
  (legalize!)

  (if (>= new-start new-end)
      (legalize!))
  
  (if (>= new-start new-end)
      (legalize!))

  ;;(c-display "aft: new-start/new-end" new-start new-end)
  
  (if delete-it
      #f
      (cons new-start new-end))
  )


         

(define (FROM_C-create-quantitize-gui)
  (define quant-gui (<gui> :ui (<ra> :get-path "quantization.ui")))

  (define (set-me-as-current!)
    (set! *curr-quantitize-gui* quant-gui))

  ;; Quantitize Options
  ;;
  (define quant-type-range (integer-range 1 5))
  
  (define quant-type-guis (map (lambda (n)
                                 (<gui> :child quant-gui (<-> "type" n)))
                               quant-type-range))

  (<gui> :set-value (quant-type-guis (1- (<ra> :get-quantitize-type))) #t)

  (define curr-quantitize-type (<ra> :get-quantitize-type))

  (<ra> :schedule 1000
        (lambda ()
          ;;(c-display "hepp" quant-gui)
          (if (not (<gui> :is-open quant-gui))
              #f
              (let ((new-quantitize-type (<ra> :get-quantitize-type)))                    
                (if (not (= curr-quantitize-type new-quantitize-type))
                    (let ((type-gui (quant-type-guis (- new-quantitize-type 1))))
                      (<gui> :set-value type-gui #t)
                      (set! curr-quantitize-type new-quantitize-type)))
                (if (<ra> :release-mode)
                    (+ 400 (random 100))
                    2))))) ;; low value because there was a bug happening now and then inside here. Trying to provoce it more often by setting this value lower.
  
  (for-each (lambda (type-gui n)
              (<gui> :add-callback type-gui
                     (lambda (is-on)
                       (when is-on
                         (<ra> :set-quantitize-type n)))))
            quant-type-guis
            quant-type-range)

  
  ;; Quantitize value 
  ;;
  (define value-gui (<gui> :child quant-gui "quantization_value"))

  (define (set-global-quantitize-value!)
    <ra> :set-quantitize (<gui> :get-value value-gui))
  
  (<gui> :set-value value-gui (<ra> :get-quantitize #t))

  (<gui> :add-callback value-gui
         (lambda (val)
           (c-display "Quant: " val (string? val))
           (if (not (string=? val (<ra> :get-quantitize #t)))
               (<ra> :set-quantitize val))
           (<gui> :set-value value-gui (<ra> :get-quantitize #t))))

  ;; Buttons
  ;;
  (define button-layout (<gui> :child quant-gui "button_layout"))
  
  (define (fix-button name guiname funcname)
    (let ((button (<gui> :child quant-gui guiname)))
      (<gui> :close button)
      (<gui> :add button-layout (create-keybinding-button name funcname '()))))
  
  ;    (define func (eval funcname))
  ;    (<gui> :add-callback button
  ;           (lambda ()
  ;             (set-me-as-current!)
  ;             (set-global-quantitize-value!)
  ;             (func)))
  ;    (<gui> :set-tool-tip button (get-displayable-keybinding funcname '()))
  ;    (add-keybinding-configuration-to-gui button funcname '())))

  (fix-button "Quantitize Range" "quantitize_range" "ra:quantitize-range")
  (fix-button "Quantitize Track" "quantitize_track" "ra:general-track-quantitize")
  ;;(fix-button "quantitize_fx" "ra:quantitize-fx")
  (fix-button "Quantitize Block" "quantitize_block" "ra:quantitize-block")

  
  ;; Set me as current quantitize gui, and return me
  ;;
  (set-me-as-current!)
  quant-gui)


(define (create-quantitize-gui-for-tab)
  (if #t
      (let ((hor (<gui> :horizontal-layout)))
        (<gui> :add-layout-space hor 10 10 #t #f)
        (<gui> :add hor (FROM_C-create-quantitize-gui))
        (<gui> :add-layout-space hor 10 10 #t #f)
        (<gui> :set-layout-spacing hor 0 0 0 0 0)
        hor)
      (FROM_C-create-quantitize-gui)))

#!!
(let ((gui (FROM_C-create-quantitize-gui)))
  (<gui> :show gui))

(load "notem.scm")
(add-notem-tab "Quantization" (let ((hor (<gui> :horizontal-layout)))
                                (<gui> :add hor (FROM_C-create-quantitize-gui))
                                (<gui> :add-layout-space hor 10 10 #t #f)
                                hor))
!!#


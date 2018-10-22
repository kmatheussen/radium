
(provide 'sequencer.scm)

;; see enum SeqblockBoxSelected in nsmtracker.h
(define (get-selected-box-num boxname)
  (cond ((eq? boxname 'non) 0)
        ((eq? boxname 'fade-left) 1)
        ((eq? boxname 'fade-right) 2)
        ((eq? boxname 'interior-left) 3)
        ((eq? boxname 'interior-right) 4)
        ((eq? boxname 'speed-left) 5)
        ((eq? boxname 'speed-right) 6)
        ((eq? boxname 'stretch-left) 7)
        ((eq? boxname 'stretch-right) 8)
        (else
         (c-display "************** boxname:" boxname)
         (assert #f))))
        


(define (get-interior-displayable-string value)
  (if (= value 0)
      "0.00s"
      (let ((seconds (/ value
                        (<ra> :get-sample-rate))))
        (if (< seconds 0.01)
            (let* ((ms (* 1000 seconds))                         
                   (sms (two-decimal-string ms)))
              (if (string=? sms "0.00")
                  "0.01ms"
                  (<-> sms "ms")))
            (<-> (two-decimal-string seconds) "s")))))

(define (get-left-interior-string2 value)
  (<-> "----|: " (get-interior-displayable-string value)))

(define (get-left-interior-string seqblocknum seqtracknum)
  (get-left-interior-string2 (<ra> :get-seqblock-interior-start seqblocknum seqtracknum #t)))

(define (left-interior-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-interior-start seqblocknum seqtracknum #t)))
    (not (= value 0.0))))

(define (set-left-interior-status-bar2 seqblocknum seqtracknum value)
  (if (and seqblocknum seqtracknum)
      (set-seqblock-selected-box 'interior-left seqblocknum seqtracknum))
  (set-editor-statusbar (get-left-interior-string2 value)))

(define (set-left-interior-status-bar seqblocknum seqtracknum)
  (set-left-interior-status-bar2 seqblocknum seqtracknum (<ra> :get-seqblock-interior-start seqblocknum seqtracknum #t)))

(define (get-right-interior-string2 seqblocknum seqtracknum right-interior-value)
  (<-> "|----: " (get-interior-displayable-string (- (get-original-seqblock-duration seqblocknum seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))
                                                     right-interior-value))))

(define (get-right-interior-string seqblocknum seqtracknum)
  (get-right-interior-string2 seqblocknum seqtracknum (<ra> :get-seqblock-interior-end seqblocknum seqtracknum #t)))

(define (right-interior-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-interior-end seqblocknum seqtracknum #t)))
    (not (= value (get-original-seqblock-duration seqblocknum seqtracknum (<ra> :get-seqblock-id seqblocknum seqtracknum))))))

(define (set-right-interior-status-bar2 seqblocknum seqtracknum right-interior-value)
  (set-seqblock-selected-box 'interior-right seqblocknum seqtracknum)
  (set-editor-statusbar (get-right-interior-string2 seqblocknum seqtracknum right-interior-value)))

(define (set-right-interior-status-bar seqblocknum seqtracknum)
  (set-right-interior-status-bar2 seqblocknum seqtracknum (<ra> :get-seqblock-interior-end seqblocknum seqtracknum #t)))

(define (get-speed-string2 value)
  (<-> "Speed: " (two-decimal-string (/ 1.0 value))))

(define (get-speed-string seqblockid)
  (get-speed-string2 (<ra> :get-seqblock-speed seqblockid #t)))

(define (speed-touched? seqblockid)
  (let ((speed (<ra> :get-seqblock-speed seqblockid #t)))
    (not (= speed 1.0))))

(define (get-stretch-string2 value)
  (<-> "Stretch: " (two-decimal-string value)))

(define (get-stretch-string seqblockid)
  (get-stretch-string2 (<ra> :get-seqblock-stretch seqblockid #t)))

(define (stretch-touched? seqblockid)
  (let ((stretch (<ra> :get-seqblock-stretch seqblockid #t)))
    (not (= stretch 1.0))))

(define (get-fade-string value seqblocknum seqtracknum)
  (<-> (if (= value 0)
           "0.00"
           (let* ((ms (* 1000
                         (/ (* value (- (<ra> :get-seqblock-end-time seqblocknum seqtracknum)
                                        (<ra> :get-seqblock-start-time seqblocknum seqtracknum)))
                            (<ra> :get-sample-rate))))
                  (sms (two-decimal-string ms)))
             (if (string=? sms "0.00")
                 "0.01"
                 sms)))
       "ms"))

(define (get-fade-string-left2 value seqblocknum seqtracknum)
  (<-> "Fade in: " (get-fade-string value seqblocknum seqtracknum)))

(define (get-fade-string-left seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-in seqblocknum seqtracknum)))
    (get-fade-string-left2 value seqblocknum seqtracknum)))

(define (fade-left-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-in seqblocknum seqtracknum)))
    (not (= value 0))))

(define (get-fade-string-right2 value seqblocknum seqtracknum)
  (<-> "Fade out: " (get-fade-string value seqblocknum seqtracknum)))

(define (get-fade-string-right seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-out seqblocknum seqtracknum)))
    (get-fade-string-right2 value seqblocknum seqtracknum)))

(define (fade-right-touched? seqblocknum seqtracknum)
  (let ((value (<ra> :get-seqblock-fade-out seqblocknum seqtracknum)))
    (not (= value 0))))

(define (set-fade-status-bar is-left seqblocknum seqtracknum)
  (if is-left
      (begin
        (set-seqblock-selected-box 'fade-left seqblocknum seqtracknum)
        (set-editor-statusbar (get-fade-string-left seqblocknum seqtracknum)))
      (begin
        (set-seqblock-selected-box 'fade-right seqblocknum seqtracknum)
        (set-editor-statusbar (get-fade-string-right seqblocknum seqtracknum)))))





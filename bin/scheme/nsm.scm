(provide 'nsm.scm)


#!!
(define osc-server (<ra> :create-osc-server "9126" 1))

(define osc-method (<ra> :add-osc-method osc-server "gakk" "iii" (lambda (url i1 i2 i3)
                                                                   (c-display "----ai:" url i1 i2 i3)
                                                                   'grab-and-close
                                                                   'grab
                                                                   )))

(<ra> :send-osc-message-from "osc.udp://127.0.0.1:9126" osc-server "gakk" "iii" (list 23 2 5))
!!#


(define *nsm-osc-server* (if (defined? '*nsm-osc-server*)
                             *nsm-osc-server*
                             #f))

(define *nsm-is-active* (if (defined? '*nsm-is-active*)
                            *nsm-is-active*
                             #f))

(define *nsm-has-inited* (if (defined? '*nsm-has-inited*)
                             *nsm-has-inited*
                             #f))

(define *nsm-url* (getenv "NSM_URL"))

(define *nsm-addr* (if (defined? '*nsm-addr*)
                       *nsm-addr*
                       #f))

(define *nsm-client-id* (if (defined? '*nsm-client-id*)
                            *nsm-client-id*
                            #f))

(define (nsm-open-method url stringpath display-name client-id)
  (c-display "   NSM-OPEN:" url stringpath display-name client-id)
  (set! *nsm-client-id* client-id)

  ;; Schedule to run later. Don't want to call ra:load-song or ra:save-as before program has started up. (We need to set *nsm-client-id* during startup in order to set jack client name)
  (<ra> :schedule 0
        (lambda ()
          (define error-code -1) ;; ERR_GENERAL
  
          (define error-string
            (call-with-exit
             (lambda (return)
               (define path (<ra> :get-path stringpath))
               
               (if (not (<ra> :dir-exists path))
                   (when (not (<ra> :create-dir path))
                     (set! error-code -10) ;; ERR_CREATE_FAILED
                     (return (<-> "Unable to create directory \"" stringpath "\""))))
               
               (define filename (<ra> :append-file-paths
                                      path
                                      (<ra> :get-path (<-> (<ra> :get-path-string ((<ra> :get-file-info path) :filename))
                                                           ".rad"))))
               
               (if (<ra> :file-exists filename)
                   (if (not (<ra> :load-song filename))
                       (return (<-> "Could not load \"" (<ra> :get-path-string filename) "\"")))
                   (if (not (<ra> :save-as filename #f #t))
                       (return (<-> "Could not save file \"" (<ra> :get-path-string filename) "\""))))

               ;(<ra> :schedule 100
               ;      (lambda ()
               ;        (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/client/is_dirty" "" '())
               ;        #f))
                       
               (return #f))))
          
          (if error-string
              (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/error" "sis" (list url error-code error-string))
              (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/reply" "ss" (list url "OK")))

          (if (not error-string)
              (<ra> :add-dirty-status-change-callback
                    (lambda (is-dirty)
                      (if (not (equal? client-id *nsm-client-id*))
                          #f
                          (begin
                            (if is-dirty
                                (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/client/is_dirty" "" '())
                                (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/client/is_clean" "" '()))
                            #t)))))
          #f))
  
  'grab)

(define (nsm-save-method url)
  (c-display "   NSM-SAVE:" url)
  (if (<ra> :save)
      (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/reply" "ss" (list url "OK"))
      (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/error" "sis" (list url -1 "Saving Failed")))
  'grab)

(define (get-nsm-projects callback)

  (define projects '())

  (define reply-method
    (<ra> :add-osc-method *nsm-osc-server* "/reply" "ss"
          (lambda (url s1 s2)
            (c-display " NSM-REPLY:" url s1 s2)
            (cond ((string=? s1 "/nsm/server/list")
                   (push-back! projects s2)
                   'grab)
                  (else
                   'pass)))))

  (<ra> :add-osc-method *nsm-osc-server* "/nsm/server/list" "is"
        (lambda x
          (c-display "------------==================------------GAKKKK" x)
          (<ra> :schedule 0 ;; schedule to run a little bit later to avoid the method not to be removed if callback fails.
                (lambda ()
                  (callback projects)
                  #f))
          (<ra> :close-osc-method reply-method)
          'grab-and-close))

  (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/server/list" "" '()))

  
#!!
(get-nsm-projects
 (lambda (projects)
   (c-display "projects:" projects)))
!!#

(define-class (<nsm-select-project-gui> :projects)

  (define has-started #f)
  
  (define window (<gui> :vertical-layout))

  (<gui> :set-window-title window "Select NSM project" #f)
  (<gui> :set-size window
         (ceiling (* 1.2 (<gui> :text-width "A typical project name length" window))) ;; width
         (ceiling (* 2 (+ 2 (length projects)) (<gui> :get-system-fontheight)))) ;; height

  ;; From area.scm :
  (<declare-variable> make-qtarea)
  (<declare-variable> vertically-layout-areas)
  (<declare-variable> new_instance_of_button)
  
  
  (define area (make-qtarea))
  
  (define gui (area :get-gui))
  
  (<gui> :add window gui)
  
  (define close-button (<gui> :button "Close" (lambda ()
                                                (c-display "------------Close called")
                                                (if has-started
                                                    (<gui> :close window)))))

  (<gui> :add window close-button)

  (<gui> :set-takes-keyboard-focus window #f)
  (<gui> :set-parent window -1)

  (<gui> :add-resize-callback gui
         (lambda (width height)
           (c-display "-------------resized:" width height)
           (if has-started
               (this->reorganize))))

  (set! has-started #t)
  
  :reorganize ()
  (let ()
    
    (c-display "\n\n  ======================     reorganize ================ \n\n\n")

    (area :reset! 0 0 (<gui> :width gui) (<gui> :height gui))
  
    (area :get-position
          (lambda (x1_ y1_ x2_ y2_ width height)
            (c-display "width:" width ". height:" height)
            (vertically-layout-areas x1_ y1_ x2_ y2_
                                     projects
                                     :spacing 0
                                     :callback
                                     (lambda (project x1 y1 x2 y2)
                                       (define button (<new> :button gui x1 y1 x2 y2
                                                          :text project
                                                          :callback
                                                          (lambda ()                                                            
                                                            (c-display "Selected" project)
                                                            (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/server/open" "s" (list project))
                                                            (<gui> :close window))))
                                       (area :add-sub-area-plain! button)))
  
            (area :update-me!))))
  
  :show ()
  (<gui> :show window)
  )

#!!
(<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/server/abort" "" '())

(let ()
  (define gui (<new> :nsm-select-project-gui (list "gakk2" "gakk3")))
  (gui :show)))

(get-nsm-projects
 (lambda (projects)
   ;; 
   (define gui (<new> :nsm-select-project-gui projects))
   (gui :show)))

!!#

(define (FROM_C-nsm-open)
  (get-nsm-projects
   (lambda (projects)
     (define gui (<new> :nsm-select-project-gui (sort projects string<?)))
     (gui :show))))

#!!
(FROM_C-nsm-open)
!!#

(define (FROM_C-nsm-new-song)
  (get-nsm-projects
   (lambda (projects)
     (define project-name (<ra> :request-string "Project name:" #t))
     (if (not (string=? "" project-name))
         (if (member project-name projects)
             (<ra> :show-async-message (<-> "\"" project-name "\" already exists"))
             (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/server/new" "s" (list project-name)))))))



(define (FROM_C-nsm-save)
  (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/server/save" "" '()))



(define (FROM_C-nsm-save-as)
  (get-nsm-projects
   (lambda (projects)
     (define project-name (<ra> :request-string "Project name:" #t))
     (if (not (string=? "" project-name))
         (if (member project-name projects)
             (<ra> :show-async-message (<-> "\"" project-name "\" already exists"))
             (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/server/duplicate" "s" (list project-name)))))))



(define (FROM_C-nsm-quit)
  (<ra> :send-osc-message-from *nsm-addr* *nsm-osc-server* "/nsm/server/abort" "" '()))



(define (setup-nsm-methods osc-server)
  (<ra> :add-osc-method osc-server "/nsm/client/open" "sss" (lambda x (apply nsm-open-method x)))
  (<ra> :add-osc-method osc-server "/nsm/client/save" "" (lambda x (apply nsm-save-method x)))
  )

#!!
(begin *nsm-osc-server*)
(begin *nsm-url*)
(setup-nsm-methods *nsm-osc-server*)
(<ra> :get-argv0)
(get-nsm-executable-name)

(<ra> :get-path-string
      ((<ra> :get-file-info (<ra> :get-path "/dir/gakk")) :filename))

 
 
!!#

(define (get-nsm-executable-name)
  (define maybe (getenv "RADIUM_NSM_EXECUTABLE_NAME"))
  (if (string=? "" maybe)
      (<ra> :get-argv0)
      maybe))

(define (init-nsm-osc-server osc-server)
  (setup-nsm-methods osc-server)

  (define reply-method #f)
  (define error-method #f)

  (set! reply-method
        (<ra> :add-osc-method osc-server "/reply" "ssss"
              (lambda (url s1 s2 s3 s4)
                (c-display " NSM-REPLY:" url s1 s2 s3 s4)
                (cond ((string=? s1 "/nsm/server/announce")
                       (c-display "NSM: Successfully registered. NSM says: " s2)
                       (set! *nsm-addr* url)
                       (c-display "----NSM-addr: " *nsm-addr*)
                       (set! *nsm-is-active* #t)
                       (set! *nsm-has-inited* #t)
                       (<ra> :close-osc-method error-method)
                       'grab-and-cancel)
                      (else
                       'pass)))))


  (set! error-method
        (<ra> :add-osc-method osc-server "/error" "sis"
              (lambda (url s1 i2 s3)
                (c-display " NSM-ERROR-METHOD:" url s1 i2 s3)
                (cond ((string=? s1 "/nsm/server/announce")
                       (c-display "NSM: Failed to register with NSM server: " s3)
                       (set! *nsm-is-active* #f)
                       (set! *nsm-has-inited* #t)
                       (<ra> :close-osc-method reply-method)
                       'grab-and-cancel)
                      (else
                       'pass)))))

  (<ra> :send-osc-message-from *nsm-url* osc-server "/nsm/server/announce"
        "sssiii"
        (list
         "Radium"
         (if (<ra> :supports-switch-nsm-capability)
             ":switch:dirty:"
             ":dirty:")
         (get-nsm-executable-name)
         ;;"radium"
         1 ;; major nsm api version
         0 ;; minor nsm api version
         (<ra> :get-pid))))
    

(define (create-nsm-osc-server)
  (if (string=? *nsm-url* "")
      (begin
        (set! *nsm-has-inited* #t)
        #f)
      (let ((osc-server (<ra> :create-osc-server "" (<ra> :get-osc-protocol-from-url *nsm-url*))))
        (c-display "osc-server:" osc-server)
        (and (>= osc-server 0)
             (begin
               (init-nsm-osc-server osc-server)
               osc-server)))))

(if (not *nsm-osc-server*)
    (set! *nsm-osc-server* (create-nsm-osc-server)))


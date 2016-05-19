(provide 'define-match.scm)

(load "define-match-bootstrapped.scm")

(define-macro (define-match funcname . matchers)
  (create-matcher-func funcname matchers))

#!
(define-expansion (define-match funcname . matchers)
  (let ((ret (create-matcher-func funcname matchers)))
    (pretty-print ret)
    ret))
!#


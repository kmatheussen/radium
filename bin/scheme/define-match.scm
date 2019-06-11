(provide 'define-match.scm)

(<declare-variable> error-no-match)
(<declare-variable> Main-Func-Name)

(load "define-match-bootstrapped.scm")

(c-define-macro (*define-match* funcname . matchers)
  (create-matcher-func funcname matchers))

#!
(define-expansion (*define-match* funcname . matchers)
  (let ((ret (create-matcher-func funcname matchers)))
    (pretty-print ret)
    ret))
!#


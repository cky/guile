;; "stdio.scm" compatability stub

(require 'scanf)
(require 'printf)

(define stdin (current-input-port))
(define stdout (current-output-port))
(define stderr (current-error-port))

;;;; This file will be installed as "and-let*.scm" on systems that
;;;; support it.  It will go away in the future, use the module
;;;; (and-let-star) instead.

(define-module (ice-9 and-let*)
  :use-module (ice-9 and-let-star))

(display ";;; The module name (ice-9 and-let*) is deprecated.\n")
(display ";;; Use (ice-9 and-let-star) instead.\n\n")

(export-syntax and-let*)

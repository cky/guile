(define-module (lang elisp internals null)
  #:export (null))

(define (null obj)
  (or (not obj)
      (null? obj)))

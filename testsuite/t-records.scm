;;; SRFI-9 Records.
;;;

;; SRFI-9 is a compile-time dependency (it exports the `define-record-type'
;; macro), hence the `use-syntax'.
;;
;; FIXME: The current definition of `use-syntax' in `boot-9.scm' is broken
;; and is not consistent with what happens when using:
;;
;;  (define-module (module) :use-syntax (chbouib))
;;
;; This precludes the test-suite from running this program using the
;; interpreter.
(use-syntax (srfi srfi-9))

(define-record-type <stuff>
  (%make-stuff chbouib)
  stuff?
  (chbouib stuff:chbouib stuff:set-chbouib!))


(and (stuff? (%make-stuff 12))
     (= 7 (stuff:chbouib (%make-stuff 7)))
     (not (stuff? 12))
     (not (false-if-exception (%make-stuff))))

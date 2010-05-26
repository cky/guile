;;; SRFI-9 Records.
;;;

(use-modules (srfi srfi-9))

(define-record-type <stuff>
  (%make-stuff chbouib)
  stuff?
  (chbouib stuff:chbouib stuff:set-chbouib!))


(and (stuff? (%make-stuff 12))
     (= 7 (stuff:chbouib (%make-stuff 7)))
     (not (stuff? 12)))

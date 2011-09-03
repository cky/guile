;;; Pattern matching with `(ice-9 match)'.
;;;

(use-modules (ice-9 match)
             (srfi srfi-9))  ;; record type (FIXME: See `t-records.scm')

(define-record-type <stuff>
  (%make-stuff chbouib)
  stuff?
  (chbouib stuff:chbouib stuff:set-chbouib!))

(define (matches? obj)
;  (format #t "matches? ~a~%" obj)
  (match obj
	 (($ <stuff>) #t)
;	 (blurps    #t)
	 ("hello"   #t)
	 (else #f)))


;(format #t "go!~%")
(and (matches? (%make-stuff 12))
     (matches? (%make-stuff 7))
     (matches? "hello")
;     (matches? 'blurps)
     (not (matches? 66)))

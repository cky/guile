;;;"promise.scm" promise for force and delay
;;; From Revised^4 Report on the Algorithmic Language Scheme
;;; Editors: William Clinger and Jonathon Rees
;
; We intend this report to belong to the entire Scheme community, and so
; we grant permission to copy it in whole or in part without fee.  In
; particular, we encourage implementors of Scheme to use this report as
; a starting point for manuals and other documentation, modifying it as
; necessary.

(define promise:force (lambda (object) (object)))

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
	  (result #f))
      (lambda ()
	(if result-ready?
	    result
	    (let ((x (proc)))
	      (if result-ready?
		  result
		  (begin (set! result-ready? #t)
			 (set! result x)
			 result))))))))

;;; change occurences of (DELAY <expression>) to
;;; (MAKE-PROMISE (LAMBDA () <expression>))
;;; and (define force promise:force)

;;; {Functions for browsing modules}

(define-module (ice-9 ls)
  :use-module (ice-9 common-list))

;;;;
;;;	local-definitions-in root name
;;;		Returns a list of names defined locally in the named
;;;		subdirectory of root.
;;;	definitions-in root name
;;;		Returns a list of all names defined in the named
;;;		subdirectory of root.  The list includes alll locally
;;;		defined names as well as all names inherited from a
;;;		member of a use-list.
;;;
;;; A convenient interface for examining the nature of things:
;;;
;;;	ls . various-names
;;;
;;;		With just one argument, interpret that argument as the
;;;		name of a subdirectory of the current module and
;;;		return a list of names defined there.
;;;
;;;		With more than one argument, still compute
;;;		subdirectory lists, but return a list:
;;;			((<subdir-name> . <names-defined-there>)
;;;			 (<subdir-name> . <names-defined-there>)
;;;			 ...)
;;;

(define-public (local-definitions-in root names)
  (let ((m (nested-ref root names))
	(answer '()))
    (if (not (module? m))
	(set! answer m)
	(module-for-each (lambda (k v) (set! answer (cons k answer))) m))
    answer))

(define-public (definitions-in root names)
  (let ((m (nested-ref root names)))
    (if (not (module? m))
	m
	(reduce union
		(cons (local-definitions-in m  '())
		      (map (lambda (m2) (definitions-in m2 '()))
			   (module-uses m)))))))

(define-public (ls . various-refs)
  (and various-refs
       (if (cdr various-refs)
	   (map (lambda (ref)
		  (cons ref (definitions-in (current-module) ref)))
		various-refs)
	   (definitions-in (current-module) (car various-refs)))))

(define-public (lls . various-refs)
  (and various-refs
       (if (cdr various-refs)
	   (map (lambda (ref)
		  (cons ref (local-definitions-in (current-module) ref)))
		various-refs)
	   (local-definitions-in (current-module) (car various-refs)))))

(define-public (recursive-local-define name value)
  (let ((parent (reverse! (cdr (reverse name)))))
    (and parent (make-modules-in (current-module) parent))
    (local-define name value)))

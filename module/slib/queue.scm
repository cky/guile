; "queue.scm"  Queues/Stacks for Scheme
; Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;
; This code is in the public domain.

(require 'record)

; Elements in a queue are stored in a list.  The last pair in the list
; is stored in the queue type so that datums can be added in constant
; time.

(define queue:record-type
  (make-record-type "queue" '(first-pair last-pair)))
(define make-queue
  (let ((construct-queue (record-constructor queue:record-type)))
    (lambda ()
      (construct-queue '() '()))))

(define queue? (record-predicate queue:record-type))

(define queue:first-pair (record-accessor queue:record-type
					  'first-pair))
(define queue:set-first-pair! (record-modifier queue:record-type
					       'first-pair))
(define queue:last-pair (record-accessor queue:record-type
					 'last-pair))
(define queue:set-last-pair! (record-modifier queue:record-type
					      'last-pair))

(define (queue-empty? q)
  (null? (queue:first-pair q)))

(define (queue-front q)
  (let ((first-pair (queue:first-pair q)))
    (if (null? first-pair)
	(slib:error "queue is empty" q))
    (car first-pair)))

(define (queue-rear q)
  (let ((last-pair (queue:last-pair q)))
    (if (null? last-pair)
	(slib:error "queue is empty" q))
    (car last-pair)))

(define (queue-push! q datum)
  (let* ((old-first-pair (queue:first-pair q))
	 (new-first-pair (cons datum old-first-pair)))
    (queue:set-first-pair! q new-first-pair)
    (if (null? old-first-pair)
	(queue:set-last-pair! q new-first-pair)))
  q)

(define (enqueue! q datum)
  (let ((new-pair (cons datum '())))
    (cond ((null? (queue:first-pair q))
	   (queue:set-first-pair! q new-pair))
	  (else
	   (set-cdr! (queue:last-pair q) new-pair)))
    (queue:set-last-pair! q new-pair))
  q)

(define (dequeue! q)
  (let ((first-pair (queue:first-pair q)))
    (if (null? first-pair)
	(slib:error "queue is empty" q))
    (let ((first-cdr (cdr first-pair)))
      (queue:set-first-pair! q first-cdr)
      (if (null? first-cdr)
	  (queue:set-last-pair! q '()))
      (car first-pair))))

(define queue-pop! dequeue!)

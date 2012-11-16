;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (ice-9 futures)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 q)
  #:export (future make-future future? touch))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module provides an implementation of futures, a mechanism for
;;; fine-grain parallelism.  Futures were first described by Henry Baker
;;; in ``The Incremental Garbage Collection of Processes'', 1977, and
;;; then implemented in MultiLisp (an implicit variant thereof, i.e.,
;;; without `touch'.)
;;;
;;; This modules uses a fixed thread pool, normally one per CPU core.
;;; Futures are off-loaded to these threads, when they are idle.
;;;
;;; Code:


;;;
;;; Futures.
;;;

(define-record-type <future>
  (%make-future thunk state mutex completion)
  future?
  (thunk        future-thunk)
  (state        future-state  set-future-state!)  ; done | started | queued
  (result       future-result set-future-result!)
  (mutex        future-mutex)
  (completion   future-completion))               ; completion cond. var.

(set-record-type-printer!
 <future>
 (lambda (future port)
   (simple-format port "#<future ~a ~a ~s>"
                  (number->string (object-address future) 16)
                  (future-state future)
                  (future-thunk future))))

(define (make-future thunk)
  "Return a new future for THUNK.  Execution may start at any point
concurrently, or it can start at the time when the returned future is
touched."
  (create-workers!)
  (let ((future (%make-future thunk 'queued
                              (make-mutex) (make-condition-variable))))
    (register-future! future)
    future))


;;;
;;; Future queues.
;;;

(define %futures (make-q))
(define %futures-mutex (make-mutex))
(define %futures-available (make-condition-variable))

(define-syntax-rule (with-mutex m e0 e1 ...)
  ;; Copied from (ice-9 threads) to avoid circular dependency.
  (let ((x m))
    (dynamic-wind
      (lambda () (lock-mutex x))
      (lambda () (begin e0 e1 ...))
      (lambda () (unlock-mutex x)))))

(define (register-future! future)
  ;; Register FUTURE as being processable.
  (lock-mutex %futures-mutex)
  (enq! %futures future)
  (signal-condition-variable %futures-available)
  (unlock-mutex %futures-mutex))

(define (process-future! future)
  ;; Process FUTURE, and update its result.
  (set-future-result! future
                      (catch #t
                        (lambda ()
                          (call-with-values (future-thunk future)
                            (lambda results
                              (lambda ()
                                (apply values results)))))
                        (lambda args
                          (lambda ()
                            (apply throw args))))))

(define (process-futures)
  ;; Wait for futures to be available and process them.
  (lock-mutex %futures-mutex)
  (let loop ()
    (when (q-empty? %futures)
      (wait-condition-variable %futures-available
                               %futures-mutex))

    (or (q-empty? %futures)
        (let ((future (deq! %futures)))
          (lock-mutex (future-mutex future))
          (case (future-state future)
            ((done started)
             ;; Nothing to do.
             (unlock-mutex (future-mutex future)))
            (else
             ;; Do the actual work.

             ;; We want to release %FUTURES-MUTEX so that other workers can
             ;; progress.  However, to avoid deadlocks, we have to unlock
             ;; FUTURE as well, to preserve lock ordering.
             (unlock-mutex (future-mutex future))
             (unlock-mutex %futures-mutex)

             (lock-mutex (future-mutex future))
             (if (eq? (future-state future) 'queued) ; lost the race?
                 (begin                              ; no, so let's process it
                   (set-future-state! future 'started)
                   (unlock-mutex (future-mutex future))

                   (process-future! future)

                   (with-mutex (future-mutex future)
                     (set-future-state! future 'done))

                   (broadcast-condition-variable (future-completion future)))
                 (unlock-mutex (future-mutex future))) ; yes

             (lock-mutex %futures-mutex)))))

    ;; Look for more work.
    (loop)))

(define (touch future)
  "Return the result of FUTURE, computing it if not already done."
  (lock-mutex (future-mutex future))
  (case (future-state future)
    ((done)
     (unlock-mutex (future-mutex future)))
    ((started)
     ;; Wait for completion.
     (wait-condition-variable (future-completion future)
                              (future-mutex future))
     (unlock-mutex (future-mutex future)))
    ((queued)
     (begin
       ;; Do the actual work.  Unlock FUTURE first to preserve lock
       ;; ordering.
       (unlock-mutex (future-mutex future))

       (lock-mutex %futures-mutex)
       (q-remove! %futures future)
       (unlock-mutex %futures-mutex)

       (lock-mutex (future-mutex future))
       (if (eq? (future-state future) 'queued) ; lost the race?
           (begin                              ; no, so let's process it
             (set-future-state! future 'started)
             (unlock-mutex (future-mutex future))

             (process-future! future)

             (with-mutex (future-mutex future)
               (set-future-state! future 'done))

             (broadcast-condition-variable (future-completion future)))
           (begin                              ; yes, so try again
             (unlock-mutex (future-mutex future))
             (touch future))))))
  ((future-result future)))


;;;
;;; Workers.
;;;

(define %worker-count
  (if (provided? 'threads)
      (- (current-processor-count) 1)
      0))

;; A dock of workers that stay here forever.

;; TODO
;; 1. Allow the pool to be shrunk, as in libgomp (though that we'd
;;    need semaphores, which aren't yet in libguile!).
;; 2. Provide a `worker-count' fluid.
(define %workers '())

(define (%create-workers!)
  (with-mutex
   %futures-mutex
   ;; Setting 'create-workers!' to a no-op is an optimization, but it is
   ;; still possible for '%create-workers!' to be called more than once
   ;; from different threads.  Therefore, to avoid creating %workers more
   ;; than once (and thus creating too many threads), we check to make
   ;; sure %workers is empty within the critical section.
   (when (null? %workers)
     (set! %workers
           (unfold (lambda (i) (>= i %worker-count))
                   (lambda (i) (call-with-new-thread process-futures))
                   1+
                   0))
     (set! create-workers! (lambda () #t)))))

(define create-workers!
  (lambda () (%create-workers!)))


;;;
;;; Syntax.
;;;

(define-syntax-rule (future body)
  "Return a new future for BODY."
  (make-future (lambda () body)))

;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2010 Free Software Foundation, Inc.
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
  (%make-future thunk done? mutex)
  future?
  (thunk     future-thunk)
  (done?     future-done?  set-future-done?!)
  (result    future-result set-future-result!)
  (mutex     future-mutex))

(define (make-future thunk)
  "Return a new future for THUNK.  Execution may start at any point
concurrently, or it can start at the time when the returned future is
touched."
  (let ((future (%make-future thunk #f (make-mutex))))
    (register-future! future)
    future))


;;;
;;; Future queues.
;;;

(define %futures (make-q))
(define %futures-mutex (make-mutex))
(define %futures-available (make-condition-variable))

(define (register-future! future)
  ;; Register FUTURE as being processable.
  (lock-mutex %futures-mutex)
  (enq! %futures future)
  (signal-condition-variable %futures-available)
  (unlock-mutex %futures-mutex))

(define (process-future! future)
  ;; Process FUTURE, assuming its mutex is already taken.
  (set-future-result! future
                      (catch #t
                        (lambda ()
                          (call-with-values (future-thunk future)
                            (lambda results
                              (lambda ()
                                (apply values results)))))
                        (lambda args
                          (lambda ()
                            (apply throw args)))))
  (set-future-done?! future #t))

(define (process-futures)
  ;; Wait for futures to be available and process them.
  (lock-mutex %futures-mutex)
  (let loop ()
    (wait-condition-variable %futures-available
                             %futures-mutex)
    (or (q-empty? %futures)
        (let ((future (deq! %futures)))
          (lock-mutex (future-mutex future))
          (or (and (future-done? future)
                   (unlock-mutex (future-mutex future)))
              (begin
                ;; Do the actual work.

                ;; We want to release %FUTURES-MUTEX so that other workers
                ;; can progress.  However, to avoid deadlocks, we have to
                ;; unlock FUTURE as well, to preserve lock ordering.
                (unlock-mutex (future-mutex future))
                (unlock-mutex %futures-mutex)

                (lock-mutex (future-mutex future))
                (or (future-done? future)            ; lost the race?
                    (process-future! future))
                (unlock-mutex (future-mutex future))

                (lock-mutex %futures-mutex)))))
    (loop)))

(define (touch future)
  "Return the result of FUTURE, computing it if not already done."
  (lock-mutex (future-mutex future))
  (or (future-done? future)
      (begin
        ;; Do the actual work.  Unlock FUTURE first to preserve lock
        ;; ordering.
        (unlock-mutex (future-mutex future))

        (lock-mutex %futures-mutex)
        (q-remove! %futures future)
        (unlock-mutex %futures-mutex)

        (lock-mutex (future-mutex future))
        (or (future-done? future)            ; lost the race?
            (process-future! future))))
  (unlock-mutex (future-mutex future))
  ((future-result future)))


;;;
;;; Workers.
;;;

(define %worker-count
  (if (provided? 'threads)
      (- (current-processor-count) 1)
      0))

(define %workers
  ;; A dock of workers that stay here forever.

  ;; TODO
  ;; 1. Allocate lazily.
  ;; 2. Allow the pool to be shrunk, as in libgomp (though that we'd
  ;;    need semaphores, which aren't yet in libguile!).
  ;; 3. Provide a `worker-count' fluid.
  (unfold (lambda (i) (>= i %worker-count))
          (lambda (i)
            (call-with-new-thread process-futures))
          1+
          0))


;;;
;;; Syntax.
;;;

(define-syntax future
  (syntax-rules ()
    "Return a new future for BODY."
    ((_ body)
     (make-future (lambda () body)))))

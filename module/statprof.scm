;;;; (statprof) -- a statistical profiler for Guile
;;;; -*-scheme-*-
;;;;
;;;; 	Copyright (C) 2009  Free Software Foundation, Inc.
;;;;    Copyright (C) 2004, 2009 Andy Wingo <wingo at pobox dot com>
;;;;    Copyright (C) 2001 Rob Browning <rlb at defaultvalue dot org>
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 


;;; Commentary:
;;
;;@code{(statprof)} is intended to be a fairly simple
;;statistical profiler for guile. It is in the early stages yet, so
;;consider its output still suspect, and please report any bugs to
;;@email{guile-devel at gnu.org}, or to me directly at @email{rlb at
;;defaultvalue.org}.
;;
;;A simple use of statprof would look like this:
;;
;;@example
;;  (statprof-reset 0 50000 #t)
;;  (statprof-start)
;;  (do-something)
;;  (statprof-stop)
;;  (statprof-display)
;;@end example
;;
;;This would reset statprof, clearing all accumulated statistics, then
;;start profiling, run some code, stop profiling, and finally display a
;;gprof flat-style table of statistics which will look something like
;;this:
;;
;;@example
;;  %   cumulative      self              self    total
;; time    seconds   seconds    calls  ms/call  ms/call  name
;; 35.29      0.23      0.23     2002     0.11     0.11  -
;; 23.53      0.15      0.15     2001     0.08     0.08  positive?
;; 23.53      0.15      0.15     2000     0.08     0.08  +
;; 11.76      0.23      0.08     2000     0.04     0.11  do-nothing
;;  5.88      0.64      0.04     2001     0.02     0.32  loop
;;  0.00      0.15      0.00        1     0.00   150.59  do-something
;; ...
;;@end example
;;
;;All of the numerical data with the exception of the calls column is
;;statistically approximate. In the following column descriptions, and
;;in all of statprof, "time" refers to execution time (both user and
;;system), not wall clock time.
;;
;;@table @asis
;;@item % time
;;The percent of the time spent inside the procedure itself
;;(not counting children).
;;@item cumulative seconds
;;The total number of seconds spent in the procedure, including
;;children.
;;@item self seconds
;;The total number of seconds spent in the procedure itself (not counting
;;children).
;;@item calls
;;The total number of times the procedure was called.
;;@item self ms/call
;;The average time taken by the procedure itself on each call, in ms.
;;@item total ms/call
;;The average time taken by each call to the procedure, including time
;;spent in child functions.
;;@item name
;;The name of the procedure.
;;@end table
;;
;;The profiler uses @code{eq?} and the procedure object itself to
;;identify the procedures, so it won't confuse different procedures with
;;the same name. They will show up as two different rows in the output.
;;
;;Right now the profiler is quite simplistic.  I cannot provide
;;call-graphs or other higher level information.  What you see in the
;;table is pretty much all there is. Patches are welcome :-)
;;
;;@section Implementation notes
;;
;;The profiler works by setting the unix profiling signal
;;@code{ITIMER_PROF} to go off after the interval you define in the call
;;to @code{statprof-reset}. When the signal fires, a sampling routine is
;;run which looks at the current procedure that's executing, and then
;;crawls up the stack, and for each procedure encountered, increments
;;that procedure's sample count. Note that if a procedure is encountered
;;multiple times on a given stack, it is only counted once. After the
;;sampling is complete, the profiler resets profiling timer to fire
;;again after the appropriate interval.
;;
;;Meanwhile, the profiler keeps track, via @code{get-internal-run-time},
;;how much CPU time (system and user -- which is also what
;;@code{ITIMER_PROF} tracks), has elapsed while code has been executing
;;within a statprof-start/stop block.
;;
;;The profiler also tries to avoid counting or timing its own code as
;;much as possible.
;;
;;; Code:

;; When you add new features, please also add tests to ./tests/ if you
;; have time, and then add the new files to ./run-tests.  Also, if
;; anyone's bored, there are a lot of existing API bits that don't
;; have tests yet.

;; TODO
;;
;; Check about profiling C functions -- does profiling primitives work?
;; Also look into stealing code from qprof so we can sample the C stack
;; Call graphs?

(define-module (statprof)
  #:use-module (srfi srfi-1)
  #:autoload   (ice-9 format) (format)
  #:export (statprof-active?
            statprof-start
            statprof-stop
            statprof-reset

            statprof-accumulated-time
            statprof-sample-count
            statprof-fold-call-data
            statprof-proc-call-data
            statprof-call-data-name
            statprof-call-data-calls
            statprof-call-data-cum-samples
            statprof-call-data-self-samples
            statprof-call-data->stats
           
            statprof-stats-proc-name
            statprof-stats-%-time-in-proc
            statprof-stats-cum-secs-in-proc
            statprof-stats-self-secs-in-proc
            statprof-stats-calls
            statprof-stats-self-secs-per-call
            statprof-stats-cum-secs-per-call

            statprof-display
            statprof-display-anomolies

            statprof-fetch-stacks
            statprof-fetch-call-tree

            with-statprof))


;; This profiler tracks two numbers for every function called while
;; it's active.  It tracks the total number of calls, and the number
;; of times the function was active when the sampler fired.
;;
;; Globally the profiler tracks the total time elapsed and the number
;; of times the sampler was fired.
;;
;; Right now, this profiler is not per-thread and is not thread safe.

(define accumulated-time #f)            ; total so far.
(define last-start-time #f)             ; start-time when timer is active.
(define sample-count #f)                ; total count of sampler calls.
(define sampling-frequency #f)          ; in (seconds . microseconds)
(define remaining-prof-time #f)         ; time remaining when prof suspended.
(define profile-level 0)                ; for user start/stop nesting.
(define %count-calls? #t)               ; whether to catch apply-frame.
(define gc-time-taken 0)                ; gc time between statprof-start and
                                        ; statprof-stop.
(define record-full-stacks? #f)         ; if #t, stash away the stacks
                                        ; for later analysis.
(define stacks '())

;; procedure-data will be a hash where the key is the function object
;; itself and the value is the data. The data will be a vector like
;; this: #(name call-count cum-sample-count self-sample-count)
(define procedure-data #f)

;; If you change the call-data data structure, you need to also change
;; sample-uncount-frame.
(define (make-call-data name call-count cum-sample-count self-sample-count)
  (vector (or name (error "internal error (we don't count anonymous procs)"))
          call-count cum-sample-count self-sample-count))
(define (call-data-name cd) (vector-ref cd 0))
(define (call-data-call-count cd) (vector-ref cd 1))
(define (call-data-cum-sample-count cd) (vector-ref cd 2))
(define (call-data-self-sample-count cd) (vector-ref cd 3))

(define (set-call-data-name! cd name)
  (vector-set! cd 0 name))
(define (inc-call-data-call-count! cd)
  (vector-set! cd 1 (1+ (vector-ref cd 1))))
(define (inc-call-data-cum-sample-count! cd)
  (vector-set! cd 2 (1+ (vector-ref cd 2))))
(define (inc-call-data-self-sample-count! cd)
  (vector-set! cd 3 (1+ (vector-ref cd 3))))

(define-macro (accumulate-time stop-time)
  `(set! accumulated-time
         (+ accumulated-time 0.0 (- ,stop-time last-start-time))))

(define (get-call-data proc)
  (or (hashq-ref procedure-data proc)
      (let ((call-data (make-call-data (procedure-name proc) 0 0 0)))
        (hashq-set! procedure-data proc call-data)
        call-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIGPROF handler

(define (sample-stack-procs stack)
  (let ((stacklen (stack-length stack))
        (hit-count-call? #f))

    (if record-full-stacks?
        (set! stacks (cons stack stacks)))

    (set! sample-count (+ sample-count 1))
    ;; Now accumulate stats for the whole stack.
    (let loop ((frame (stack-ref stack 0))
               (procs-seen (make-hash-table 13))
               (self #f))
      (cond
       ((not frame)
        (hash-fold
         (lambda (proc val accum)
           (inc-call-data-cum-sample-count!
            (get-call-data proc)))
         #f
         procs-seen)
        (and=> (and=> self get-call-data)
               inc-call-data-self-sample-count!))
       ((frame-procedure frame)
        => (lambda (proc)
             (cond
              ((eq? proc count-call)
               ;; We're not supposed to be sampling count-call and
               ;; its sub-functions, so loop again with a clean
               ;; slate.
               (set! hit-count-call? #t)
               (loop (frame-previous frame) (make-hash-table 13) #f))
              ((procedure-name proc)
               (hashq-set! procs-seen proc #t)
               (loop (frame-previous frame)
                     procs-seen
                     (or self proc)))
              (else
               (loop (frame-previous frame) procs-seen self)))))
       (else
        (loop (frame-previous frame) procs-seen self))))
    hit-count-call?))

(define inside-profiler? #f)

(define (profile-signal-handler sig)
  (set! inside-profiler? #t)

  ;; FIXME: with-statprof should be able to set an outer frame for the
  ;; stack cut
  (if (positive? profile-level)
      (let* ((stop-time (get-internal-run-time))
             ;; cut down to the signal handler, then we rely on
             ;; knowledge of guile: it dispatches signal handlers
             ;; through a thunk, so cut one more procedure
             (stack (make-stack #t profile-signal-handler 0 1))
             (inside-apply-trap? (sample-stack-procs stack)))

        (if (not inside-apply-trap?)
            (begin
              ;; disabling here is just a little more efficient, but
              ;; not necessary given inside-profiler?.  We can't just
              ;; disable unconditionally at the top of this function
              ;; and eliminate inside-profiler? because it seems to
              ;; confuse guile wrt re-enabling the trap when
              ;; count-call finishes.
              (if %count-calls? (trap-disable 'apply-frame))
              (accumulate-time stop-time)))
        
        (setitimer ITIMER_PROF
                   0 0
                   (car sampling-frequency)
                   (cdr sampling-frequency))
        
        (if (not inside-apply-trap?)
            (begin
              (set! last-start-time (get-internal-run-time))
              (if %count-calls? (trap-enable 'apply-frame))))))

  (set! inside-profiler? #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count total calls.

(define (count-call trap-name continuation tail)
  (if (not inside-profiler?)
      (begin
        (accumulate-time (get-internal-run-time))

        (and=> (frame-procedure (last-stack-frame continuation))
               (lambda (proc)
                 (if (procedure-name proc)
                     (inc-call-data-call-count!
                      (get-call-data proc)))))
        
        (set! last-start-time (get-internal-run-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (statprof-active?)
  "Returns @code{#t} if @code{statprof-start} has been called more times
than @code{statprof-stop}, @code{#f} otherwise."
  (positive? profile-level))

;; Do not call this from statprof internal functions -- user only.
(define (statprof-start)
  "Start the profiler.@code{}"
  ;; After some head-scratching, I don't *think* I need to mask/unmask
  ;; signals here, but if I'm wrong, please let me know.
  (set! profile-level (+ profile-level 1))
  (if (= profile-level 1)
      (let* ((rpt remaining-prof-time)
             (use-rpt? (and rpt
                            (or (positive? (car rpt))
                                (positive? (cdr rpt))))))
        (set! remaining-prof-time #f)
        (set! last-start-time (get-internal-run-time))
        (set! gc-time-taken
              (cdr (assq 'gc-time-taken (gc-stats))))
        (if use-rpt?
            (setitimer ITIMER_PROF 0 0 (car rpt) (cdr rpt))
            (setitimer ITIMER_PROF
                       0 0
                       (car sampling-frequency)
                       (cdr sampling-frequency)))
        (trap-enable 'apply-frame)
        #t)))
  
;; Do not call this from statprof internal functions -- user only.
(define (statprof-stop)
  "Stop the profiler.@code{}"
  ;; After some head-scratching, I don't *think* I need to mask/unmask
  ;; signals here, but if I'm wrong, please let me know.
  (set! profile-level (- profile-level 1))
  (if (zero? profile-level)
      (begin
        (set! gc-time-taken
              (- (cdr (assq 'gc-time-taken (gc-stats))) gc-time-taken))
        (trap-disable 'apply-frame)
        ;; I believe that we need to do this before getting the time
        ;; (unless we want to make things even more complicated).
        (set! remaining-prof-time (setitimer ITIMER_PROF 0 0 0 0))
        (accumulate-time (get-internal-run-time))
        (set! last-start-time #f))))

(define (statprof-reset sample-seconds sample-microseconds count-calls?
                        . full-stacks?)
  "Reset the statprof sampler interval to @var{sample-seconds} and
@var{sample-microseconds}. If @var{count-calls?} is true, arrange to
instrument procedure calls as well as collecting statistical profiling
data. If @var{full-stacks?} is true, collect all sampled stacks into a
list for later analysis.

Enables traps and debugging as necessary."
  (if (positive? profile-level)
      (error "Can't reset profiler while profiler is running."))
  (set! %count-calls? count-calls?)
  (set! accumulated-time 0)
  (set! last-start-time #f)
  (set! sample-count 0)
  (set! sampling-frequency (cons sample-seconds sample-microseconds))
  (set! remaining-prof-time #f)
  (set! procedure-data (make-hash-table 131))
  (if %count-calls?
      (begin
        (trap-set! apply-frame-handler count-call)
        (trap-enable 'traps)))
  (set! record-full-stacks? (and (pair? full-stacks?) (car full-stacks?)))
  (set! stacks '())
  (debug-enable 'debug)
  (sigaction SIGPROF profile-signal-handler)
  #t)

(define (statprof-fold-call-data proc init)
  "Fold @var{proc} over the call-data accumulated by statprof. Cannot be
called while statprof is active. @var{proc} should take two arguments,
@code{(@var{call-data} @var{prior-result})}.

Note that a given proc-name may appear multiple times, but if it does,
it represents different functions with the same name."
  (if (positive? profile-level)
      (error "Can't call statprof-fold-called while profiler is running."))

  (hash-fold
   (lambda (key value prior-result)
     (proc value prior-result))
   init
   procedure-data))

(define (statprof-proc-call-data proc)
  "Returns the call-data associated with @var{proc}, or @code{#f} if
none is available."
  (if (positive? profile-level)
      (error "Can't call statprof-fold-called while profiler is running."))

  (hashq-ref procedure-data proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stats

(define (statprof-call-data->stats call-data)
  "Returns an object of type @code{statprof-stats}."
  ;; returns (vector proc-name
  ;;                 %-time-in-proc
  ;;                 cum-seconds-in-proc
  ;;                 self-seconds-in-proc
  ;;                 num-calls
  ;;                 self-secs-per-call
  ;;                 total-secs-per-call)

  (let* ((proc-name (call-data-name call-data))
         (self-samples (call-data-self-sample-count call-data))
         (cum-samples (call-data-cum-sample-count call-data))
         (all-samples (statprof-sample-count))
         (secs-per-sample (/ (statprof-accumulated-time)
                             (statprof-sample-count)))
         (num-calls (and %count-calls? (statprof-call-data-calls call-data))))

    (vector proc-name
            (* (/ self-samples all-samples) 100.0)
            (* cum-samples secs-per-sample 1.0)
            (* self-samples secs-per-sample 1.0)
            num-calls
            (and num-calls ;; maybe we only sampled in children
                 (if (zero? self-samples) 0.0
                     (/ (* self-samples secs-per-sample) 1.0 num-calls)))
            (and num-calls ;; cum-samples must be positive
                 (/ (* cum-samples secs-per-sample) 1.0 num-calls)))))

(define (statprof-stats-proc-name stats) (vector-ref stats 0))
(define (statprof-stats-%-time-in-proc stats) (vector-ref stats 1))
(define (statprof-stats-cum-secs-in-proc stats) (vector-ref stats 2))
(define (statprof-stats-self-secs-in-proc stats) (vector-ref stats 3))
(define (statprof-stats-calls stats) (vector-ref stats 4))
(define (statprof-stats-self-secs-per-call stats) (vector-ref stats 5))
(define (statprof-stats-cum-secs-per-call stats) (vector-ref stats 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stats-sorter x y)
  (let ((diff (- (statprof-stats-self-secs-in-proc x)
                 (statprof-stats-self-secs-in-proc y))))
    (positive?
     (if (= diff 0)
         (- (statprof-stats-cum-secs-in-proc x)
            (statprof-stats-cum-secs-in-proc y))
         diff))))

(define (statprof-display . port)
  "Displays a gprof-like summary of the statistics collected. Unless an
optional @var{port} argument is passed, uses the current output port."
  (if (null? port) (set! port (current-output-port)))
  
  (cond
   ((zero? (statprof-sample-count))
    (format port "No samples recorded.\n"))
   (else
    (let* ((stats-list (statprof-fold-call-data
                        (lambda (data prior-value)
                          (cons (statprof-call-data->stats data)
                                prior-value))
                        '()))
           (sorted-stats (sort stats-list stats-sorter)))

      (define (display-stats-line stats)
        (if %count-calls?
            (format  port "~6,2f ~9,2f ~9,2f ~8r ~8,2f ~8,2f  "
                     (statprof-stats-%-time-in-proc stats)
                     (statprof-stats-cum-secs-in-proc stats)
                     (statprof-stats-self-secs-in-proc stats)
                     (statprof-stats-calls stats)
                     (* 1000 (statprof-stats-self-secs-per-call stats))
                     (* 1000 (statprof-stats-cum-secs-per-call stats)))
            (format  port "~6,2f ~9,2f ~9,2f  "
                     (statprof-stats-%-time-in-proc stats)
                     (statprof-stats-cum-secs-in-proc stats)
                     (statprof-stats-self-secs-in-proc stats)))
        (display (statprof-stats-proc-name stats) port)
        (newline port))
    
      (if %count-calls?
          (begin
            (format  port "~5a ~10a   ~7a ~8a ~8a ~8a  ~8@a\n"
                     "%  " "cumulative" "self" "" "self" "total" "")
            (format  port "~5a  ~9a  ~8a ~8a ~8a ~8a  ~8@a\n"
                     "time" "seconds" "seconds" "calls" "ms/call" "ms/call" "name"))
          (begin
            (format  port "~5a ~10a   ~7a  ~8@a\n"
                     "%" "cumulative" "self" "")
            (format  port "~5a  ~10a  ~7a  ~8@a\n"
                     "time" "seconds" "seconds" "name")))

      (for-each display-stats-line sorted-stats)

      (display "---\n" port)
      (simple-format #t "Sample count: ~A\n" (statprof-sample-count))
      (simple-format #t "Total time: ~A seconds (~A seconds in GC)\n"
                     (statprof-accumulated-time)
                     (/ gc-time-taken internal-time-units-per-second))))))

(define (statprof-display-anomolies)
  "A sanity check that attempts to detect anomolies in statprof's
statistics.@code{}"
  (statprof-fold-call-data
   (lambda (data prior-value)
     (if (and %count-calls?
              (zero? (call-data-call-count data))
              (positive? (call-data-sample-count data)))
         (simple-format #t
                        "==[~A ~A ~A]\n"
                        (call-data-name data)
                        (call-data-call-count data)
                        (call-data-sample-count data))))
   #f)
  (simple-format #t "Total time: ~A\n" (statprof-accumulated-time))
  (simple-format #t "Sample count: ~A\n" (statprof-sample-count)))

(define (statprof-accumulated-time)
  "Returns the time accumulated during the last statprof run.@code{}"
  (if (positive? profile-level)
      (error "Can't get accumulated time while profiler is running."))
  (/ accumulated-time internal-time-units-per-second))

(define (statprof-sample-count)
  "Returns the number of samples taken during the last statprof run.@code{}"
  (if (positive? profile-level)
      (error "Can't get accumulated time while profiler is running."))
  sample-count)

(define statprof-call-data-name call-data-name)
(define statprof-call-data-calls call-data-call-count)
(define statprof-call-data-cum-samples call-data-cum-sample-count)
(define statprof-call-data-self-samples call-data-self-sample-count)

(define (statprof-fetch-stacks)
  "Returns a list of stacks, as they were captured since the last call
to @code{statprof-reset}.

Note that stacks are only collected if the @var{full-stacks?} argument
to @code{statprof-reset} is true."
  stacks)

(define procedure=?
  (if (false-if-exception (resolve-interface '(system base compile)))
      (lambda (a b)
        (cond
         ((eq? a b))
         ((and ((@ (system vm program) program?) a)
               ((@ (system vm program) program?) b))
          (eq? ((@ (system vm program) program-objcode) a)
               ((@ (system vm program) program-objcode) b)))
         ((and (closure? a) (closure? b)
               (procedure-source a) (procedure-source b))
          (and (eq? (procedure-name a) (procedure-name b))
               (equal? (procedure-source a) (procedure-source b))))
         (else
          #f)))
      (lambda (a b)
        (cond
         ((eq? a b))
         ((and (closure? a) (closure? b)
               (procedure-source a) (procedure-source b))
          (and (eq? (procedure-name a) (procedure-name b))
               (equal? (procedure-source a) (procedure-source b))))
         (else
          #f)))))

;; tree ::= (car n . tree*)

(define (lists->trees lists equal?)
  (let lp ((in lists) (n-terminal 0) (tails '()))
    (cond
     ((null? in)
      (let ((trees (map (lambda (tail)
                          (cons (car tail)
                                (lists->trees (cdr tail) equal?)))
                        tails)))
        (cons (apply + n-terminal (map cadr trees))
              (sort trees
                    (lambda (a b) (> (cadr a) (cadr b)))))))
     ((null? (car in))
      (lp (cdr in) (1+ n-terminal) tails))
     ((find (lambda (x) (equal? (car x) (caar in)))
            tails)
      => (lambda (tail)
           (lp (cdr in)
               n-terminal
               (assq-set! tails
                          (car tail)
                          (cons (cdar in) (cdr tail))))))
     (else
      (lp (cdr in)
          n-terminal
          (acons (caar in) (list (cdar in)) tails))))))

(define (stack->procedures stack)
  (filter identity
          (unfold-right (lambda (x) (not x))
                        frame-procedure
                        frame-previous
                        (stack-ref stack 0))))

(define (statprof-fetch-call-tree)
  "Return a call tree for the previous statprof run.

The return value is a list of nodes, each of which is of the type:
@code
 node ::= (@var{proc} @var{count} . @var{nodes})
@end code"
  (cons #t (lists->trees (map stack->procedures stacks) procedure=?)))

(define-macro (with-statprof . args)
  "Profiles the expressions in its body.

Keyword arguments:

@table @code
@item #:loop
Execute the body @var{loop} number of times, or @code{#f} for no looping

default: @code{#f}
@item #:hz
Sampling rate

default: @code{20}
@item #:count-calls?
Whether to instrument each function call (expensive)

default: @code{#f}
@item #:full-stacks?
Whether to collect away all sampled stacks into a list

default: @code{#f}
@end table"
  (define (kw-arg-ref kw args def)
    (cond
     ((null? args) (error "Invalid macro body"))
     ((keyword? (car args))
      (if (eq? (car args) kw)
          (cadr args)
          (kw-arg-ref kw (cddr args) def)))
     ((eq? kw #f def) ;; asking for the body
      args)
     (else def))) ;; kw not found
  (let ((loop (kw-arg-ref #:loop args #f))
        (hz (kw-arg-ref #:hz args 20))
        (count-calls? (kw-arg-ref #:count-calls? args #f))
        (full-stacks? (kw-arg-ref #:full-stacks? args #f))
        (body (kw-arg-ref #f args #f)))
    `(dynamic-wind
         (lambda ()
            (statprof-reset (inexact->exact (floor (/ 1 ,hz)))
                            (inexact->exact (* 1e6 (- (/ 1 ,hz)
                                                      (floor (/ 1 ,hz)))))
                            ,count-calls?
                            ,full-stacks?)
            (statprof-start))
         (lambda ()
           ,(if loop
                (let ((lp (gensym "statprof ")) (x (gensym)))
                  `(let ,lp ((,x ,loop))
                        (if (not (zero? ,x))
                            (begin ,@body (,lp (1- ,x))))))
                `(begin ,@body)))
         (lambda ()
            (statprof-stop)
            (statprof-display)
            (set! (@@ (statprof) procedure-data) #f)))))

;;; arch-tag: 83969178-b576-4c52-a31c-6a9c2be85d10

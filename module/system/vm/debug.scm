;;; Guile VM debugging facilities

;;; Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.
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

;;; Code:

(define-module (system vm debug)
  #:use-module (system base pmatch)
  #:use-module (system base syntax)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module (system vm program)
  #:export (run-debugger debug-pre-unwind-handler))


(define (reverse-hashq h)
  (let ((ret (make-hash-table)))
    (hash-for-each
     (lambda (k v)
       (hashq-set! ret v (cons k (hashq-ref ret v '()))))
     h)
    ret))

(define (catch-bad-arguments thunk bad-args-thunk)
  (catch 'wrong-number-of-args
    (lambda ()
      (catch 'keyword-argument-error
        thunk
        (lambda (k . args)
          (bad-args-thunk))))
    (lambda (k . args)
      (bad-args-thunk))))

(define (read-args prompt)
  (define (read* reader)
    (repl-reader prompt reader))
  (define (next)
    (read* read-char))
  (define (cmd chr)
    (cond
     ((eof-object? chr) (list chr))
     ((char=? chr #\newline) (cmd (next)))
     ((char-whitespace? chr) (cmd (next)))
     (else
      (unread-char chr)
      (let ((tok (read* read)))
        (args (list tok) (next))))))
  (define (args out chr)
    (cond
     ((eof-object? chr) (reverse out))
     ((char=? chr #\newline) (reverse out))
     ((char-whitespace? chr) (args out (next)))
     (else
      (unread-char chr)
      (let ((tok (read* read)))
        (args (cons tok out) (next))))))
  (cmd (next)))

(define* (print-locals frame #:optional (port (current-output-port))
                       #:key (width 72) (per-line-prefix ""))
  (let ((bindings (frame-bindings frame)))
    (cond
     ((null? bindings)
      (format port "~aNo local variables.~%" per-line-prefix))
     (else
      (format port "~aLocal variables:~%" per-line-prefix)
      (for-each
       (lambda (binding)
         (format port "~a~4d ~a~:[~; (boxed)~] = ~v:@y\n"
                 per-line-prefix
                 (binding:index binding)
                 (binding:name binding)
                 (binding:boxed? binding)
                 width
                 (let ((x (frame-local-ref frame (binding:index binding))))
                   (if (binding:boxed? binding)
                       (variable-ref x)
                       x))))
       (frame-bindings frame))))))

(define* (collect-frames frame #:key count)
  (cond
   ((not count)
    (let lp ((frame frame) (out '()))
      (if (not frame)
          out
          (lp (frame-previous frame) (cons frame out)))))
   ;; should also have a from-end option, either via negative count or
   ;; another kwarg
   ((>= count 0)
    (let lp ((frame frame) (out '()) (count count))
      (if (or (not frame) (zero? count))
          out
          (lp (frame-previous frame) (cons frame out) (1- count)))))))

(define* (print-frames frames #:optional (port (current-output-port))
                       #:key (start-index (1- (length frames))) (width 72)
                       (full? #f))
  (let lp ((frames frames) (i start-index) (last-file ""))
    (if (pair? frames)
        (let* ((frame (car frames))
               (source (frame-source frame))
               (file (and source
                          (or (source:file source)
                              "current input")))
               (line (and=> source source:line)))
          (if (and file (not (equal? file last-file)))
              (format port "~&In ~a:~&" file))
          (format port "~:[~*~6_~;~5d:~]~3d ~v:@y~%" line line
                  i width (frame-call-representation frame))
          (if full?
              (print-locals frame #:width width
                            #:per-line-prefix "     "))
          (lp (cdr frames) (1- i) (or file last-file))))))


;;;
;;; Debugger
;;;

(define-record <debugger> vm level breakpoints module)

(define (make-debugger-module)
  (let ((m (make-fresh-user-module)))
    m))

(define vm-debugger
  (let ((prop (make-object-property)))
    (lambda (vm)
      (or (prop vm)
          (let ((debugger (make-debugger vm 0 '() (make-debugger-module))))
            (set! (prop vm) debugger)
            debugger)))))

(define* (run-debugger frame #:optional (vm (the-vm)))
  (let* ((db (vm-debugger vm))
         (level (debugger-level db)))
    (dynamic-wind
      (lambda () (set! (debugger-level db) (1+ level)))
      (lambda () (debugger-repl db frame))
      (lambda () (set! (debugger-level db) level)))))

(define (debugger-repl db frame)
  (let ((top frame)
        (cur frame)
        (index 0)
        (level (debugger-level db)))
    (define (frame-index frame)
      (let lp ((idx 0) (walk top))
        (if (= (frame-return-address frame) (frame-return-address walk))
            idx
            (lp (1+ idx) (frame-previous walk)))))
    (define (frame-at-index idx)
      (let lp ((idx idx) (walk top))
        (cond
         ((not walk) #f)
         ((zero? idx) walk)
         (else (lp (1- idx) (frame-previous walk))))))
    (define (show-frame)
      ;;      #2  0x009600e0 in do_std_select (args=0xbfffd9e0) at threads.c:1668
      ;;      1668	    select (select_args->nfds,
      (format #t "#~2a 0x~8,'0x in ~60@y~%"
              index
              (frame-instruction-pointer cur)
              (frame-call-representation cur)))

    (define-syntax define-command
      (syntax-rules ()
        ((_ ((mod cname alias ...) . args) body ...)
         (define cname
           (let ((c (lambda* args body ...)))
             (set-procedure-property! c 'name 'cname)
             (module-define! mod 'cname c)
             (module-add! mod 'alias (module-local-variable mod 'cname))
             ...
             c)))))

    (let ((commands (make-module)))
      (define (prompt)
        (format #f "~a~a debug> "
                (if (= level 1)
                    ""
                    (format #f "~a:" level))
                index))
      
      (define (print* . vals)
        (define (print x)
          (run-hook before-print-hook x)
          (pretty-print x))
        (if (and (pair? vals)
                 (not (and (null? (cdr vals))
                           (unspecified? (car vals)))))
            (for-each print vals)))

      (define-command ((commands backtrace bt) #:optional count
                       #:key (width 72) full?)
        "Print a backtrace of all stack frames, or innermost COUNT frames."
        (print-frames (collect-frames top #:count count)
                      #:width width
                      #:full? full?))
      
      (define-command ((commands up) #:optional (count 1))
        "Select and print stack frames that called this one.
An argument says how many frames up to go"
        (if (or (not (integer? count)) (<= count 0))
            (format #t "Invalid argument to `up': expected a positive integer for COUNT.~%")
            (let lp ((n count))
              (cond
               ((zero? n) (show-frame))
               ((frame-previous cur)
                => (lambda (new)
                     (set! cur new)
                     (set! index (1+ index))
                     (lp (1- n))))
               ((= n count)
                (format #t "Already at outermost frame.\n"))
               (else
                (format #t "Reached outermost frame after walking ~a frames.\n"
                        (- count n))
                (show-frame))))))
      
      (define-command ((commands down) #:optional (count 1))
        "Select and print stack frames called by this one.
An argument says how many frames down to go"
        (cond
         ((or (not (integer? count)) (<= count 0))
          (format #t "Invalid argument to `down': expected a positive integer for COUNT.~%"))
         ((= index 0)
          (format #t "Already at innermost frame.~%"))
         (else
          (set! index (max (- index count) 0))
          (set! cur (frame-at-index index))
          (show-frame))))
      
      (define-command ((commands frame f) #:optional idx)
        "Show the selected frame.
With an argument, select a frame by index, then show it."
        (cond
         (idx
          (cond
           ((or (not (integer? idx)) (< idx 0))
            (format #t "Invalid argument to `frame': expected a non-negative integer for IDX.~%"))
           ((frame-at-index idx)
            => (lambda (f)
                 (set! cur f)
                 (set! index idx)
                 (show-frame)))
           (else
            (format #t "No such frame.~%"))))
         (else (show-frame))))

      (define-command ((commands locals))
        "Show locally-bound variables in the selected frame."
        (print-locals cur))
      
      (define-command ((commands quit q continue cont c))
        "Quit the debugger and let the program continue executing."
        (throw 'quit))
      
      (define-command ((commands help h ?) #:optional cmd)
        "Show this help message."
        (let ((rhash (reverse-hashq (module-obarray commands))))
          (define (help-cmd cmd)
            (let* ((v (module-local-variable commands cmd))
                   (p (variable-ref v))
                   (canonical-name (procedure-name p)))
              ;; la la la
              (format #t "~a~{ ~:@(~a~)~}~?~%~a~&~%"
                      canonical-name (program-lambda-list p)
                      "~#[~:;~40t(aliases: ~@{~a~^, ~})~]"
                      (delq canonical-name (hashq-ref rhash v))
                      (procedure-documentation p))))
          (cond
           (cmd
            (cond
             ((and (symbol? cmd) (module-local-variable commands cmd))
              (help-cmd cmd))
             (else
              (format #t "Invalid command ~s.~%" cmd)
              (format #t "Try `help' for a list of commands~%"))))
           (else
            (let ((names (sort
                           (hash-map->list
                            (lambda (k v)
                              (procedure-name (variable-ref k)))
                            rhash)
                           (lambda (x y)
                             (string<? (symbol->string x)
                                       (symbol->string y))))))
              (format #t "Available commands:~%~%")
              (for-each help-cmd names))))))

      (define (handle cmd . args)
        (cond
         ((and (symbol? cmd)
               (module-local-variable commands cmd))
          => (lambda (var)
               (let ((proc (variable-ref var)))
                 (catch-bad-arguments
                  (lambda ()
                    (apply (variable-ref var) args))
                  (lambda ()
                    (format (current-error-port)
                            "Invalid arguments to ~a. Try `help ~a'.~%"
                            (procedure-name proc) (procedure-name proc)))))))
         ((and (integer? cmd) (exact? cmd))
          (frame cmd))
         ((eof-object? cmd)
          (newline)
          (throw 'quit))
         (else
          (format (current-error-port)
                  "~&Unknown command: ~a. Try `help'.~%" cmd)
          *unspecified*)))

      (catch 'quit
        (lambda ()
          (let loop ()
            (apply
             handle
             (save-module-excursion
              (lambda ()
                (set-current-module commands)
                (read-args prompt))))
            (loop)))
        (lambda (k . args)
          (apply values args))))))


;; TODO:
;;
;; eval expression in context of frame
;; set local variable in frame
;; step until next instruction
;; step until next function call/return
;; step until return from frame
;; step until different source line
;; step until greater source line
;; watch expression
;; break on a function
;; remove breakpoints
;; set printing width
;; display a truncated backtrace
;; go to a frame by index
;; (reuse gdb commands perhaps)
;; disassemble a function
;; disassemble the current function
;; inspect any object
;; hm, trace via reassigning global vars. tricksy.
;; (state associated with vm ?)

(define (debug-pre-unwind-handler key . args)
  (let ((stack (make-stack #t debug-pre-unwind-handler)))
    (pmatch args
      ((,subr ,msg ,args . ,rest)
       (format #t "Throw to key `~a':\n" key)
       (display-error stack (current-output-port) subr msg args rest))
      (else
       (format #t "Throw to key `~a' with args `~s'." key args)))
    (format #t "Entering the debugger. Type `bt' for a backtrace or `c' to continue.\n")
    (run-debugger (stack-ref stack 0)))
  (save-stack debug-pre-unwind-handler)
  (apply throw key args))

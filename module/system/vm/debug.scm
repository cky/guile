;;; Guile VM debugging facilities

;;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.
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
        (index 0)
        (level (debugger-level db)))
    (define (frame-index frame)
      (let lp ((idx 0) (walk top))
        (if (= (frame-return-address frame) (frame-return-address walk))
            idx
            (lp (1+ idx) (frame-previous walk)))))

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

      (define-command ((commands backtrace bt) #:optional count)
        "Print a backtrace of all stack frames, or innermost COUNT frames."
        (display-backtrace (make-stack frame) (current-output-port)))
      
      (define-command ((commands quit q continue cont c))
        "Quit the debugger and let the program continue executing."
        (throw 'quit))
      
      #;
      (case cmd
        ((bt)
         (display-backtrace (make-stack frame) (current-output-port)))
        ((bindings)
         (format #t "~a\n" (frame-bindings frame)))
        ((frame f)
         (format #t "~s\n" frame))
        ((up)
         (let ((prev (frame-previous frame)))
           (if prev
               (begin
                 (set! index (1+ index))
                 (set! frame prev)
                 (format #t "~s\n" frame))
               (format #t "Already at outermost frame.\n"))))
        ((down)
         (if (zero? index)
             (format #t "Already at innermost frame.\n")
             (begin
               (set! frame (let lp ((n (1- index)) (frame top))
                             (if (zero? n)
                                 frame
                                 (lp (1- n) (frame-previous top)))))
               (format #t "~s\n" frame))))
        ((help ?)
         (format #t "Type `c' to continue.\n"))
        (else
         (format #t "Unknown command: ~A\n" cmd)))
                
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
                    (format (current-error-port) "Invalid arguments to ~a~%"
                            (procedure-name proc))
                    (help cmd))))))
         #;
         ((and (integer? cmd) (exact? cmd))
          (nth cmd))
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
            (call-with-values
                (lambda ()
                  (apply
                   handle
                   (save-module-excursion
                    (lambda ()
                      (set-current-module commands)
                      (read-args prompt)))))
              print*)
            (loop)))
        (lambda (k . args)
          (apply values args))))))

;; things this debugger should do:
;;
;; eval expression in context of frame
;; up/down stack for inspecting
;; print procedure and args for frame
;; print local variables for frame
;; set local variable in frame
;; display backtrace
;; display full backtrace
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
;; help
;; disassemble a function
;; disassemble the current function
;; inspect any object
;; hm, trace via reassigning global vars. tricksy.
;; (state associated with vm ?)

(define (debug-pre-unwind-handler key . args)
  ;; herald
  (format #t "Throw to key `~a' with args `~s'.
Entering the debugger. Type `bt' for a backtrace or `c' to continue.
This debugger implementation is temporary. See system/vm/debug.scm for
some ideas on how to make it better.\n" key args)
  (run-debugger (stack-ref (make-stack #t) 1))
  (save-stack 1)
  (apply throw key args))

;;; Repl commands

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (system repl command)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (system repl common)
  #:use-module (system vm objcode)
  #:use-module (system vm program)
  #:use-module (system vm vm)
  #:autoload (system base language) (lookup-language language-reader)
  #:autoload (system vm debug) (vm-debugger vm-backtrace)
  #:autoload (system vm trace) (vm-trace vm-trace-on vm-trace-off)
  #:autoload (system vm profile) (vm-profile)
  #:use-module (ice-9 format)
  #:use-module (ice-9 session)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 rdelim)
  #:export (meta-command))


;;;
;;; Meta command interface
;;;

(define *command-table*
  '((help     (help h) (apropos a) (describe d) (option o) (quit q))
    (module   (module m) (import i) (load l) (binding b))
    (language (language L))
    (compile  (compile c) (compile-file cc)
	      (disassemble x) (disassemble-file xx))
    (profile  (time t) (profile pr))
    (debug    (backtrace bt) (debugger db) (trace tr) (step st))
    (system   (gc) (statistics stat))))

(define (group-name g) (car g))
(define (group-commands g) (cdr g))

;; Hack, until core can be extended.
(define procedure-documentation
  (let ((old-definition procedure-documentation))
    (lambda (p)
      (if (program? p)
          (program-documentation p)
          (old-definition p)))))

(define *command-module* (current-module))
(define (command-name c) (car c))
(define (command-abbrev c) (if (null? (cdr c)) #f (cadr c)))
(define (command-procedure c) (module-ref *command-module* (command-name c)))
(define (command-doc c) (procedure-documentation (command-procedure c)))

(define (command-usage c)
  (let ((doc (command-doc c)))
    (substring doc 0 (string-index doc #\newline))))

(define (command-summary c)
  (let* ((doc (command-doc c))
	 (start (1+ (string-index doc #\newline))))
    (cond ((string-index doc #\newline start)
	   => (lambda (end) (substring doc start end)))
	  (else (substring doc start)))))

(define (lookup-group name)
  (assq name *command-table*))

(define (lookup-command key)
  (let loop ((groups *command-table*) (commands '()))
    (cond ((and (null? groups) (null? commands)) #f)
	  ((null? commands)
	   (loop (cdr groups) (cdar groups)))
	  ((memq key (car commands)) (car commands))
	  (else (loop groups (cdr commands))))))

(define (display-group group . opts)
  (format #t "~:(~A~) Commands [abbrev]:~2%" (group-name group))
  (for-each (lambda (c)
	      (display-summary (command-usage c)
			       (command-abbrev c)
			       (command-summary c)))
	    (group-commands group))
  (newline))

(define (display-command command)
  (display "Usage: ")
  (display (command-doc command))
  (newline))

(define (display-summary usage abbrev summary)
  (let ((abbrev (if abbrev (format #f "[,~A]" abbrev) "")))
    (format #t " ,~24A ~8@A - ~A\n" usage abbrev summary)))

(define (read-datum repl)
  (read))

(define read-line
  (let ((orig-read-line read-line))
    (lambda (repl)
      (orig-read-line))))

(define (meta-command repl)
  (let ((command (read-datum repl)))
    (if (not (symbol? command))
        (user-error "Meta-command not a symbol: ~s" command))
    (let ((c (lookup-command command)))
      (if c
          ((command-procedure c) repl)
          (user-error "Unknown meta command: ~A" command)))))

(define-syntax define-meta-command
  (syntax-rules ()
    ((_ (name repl (expression0 ...) . datums) docstring b0 b1 ...)
     (define (name repl)
       docstring
       (let* ((expression0
               (with-fluid* current-reader
                            (language-reader (repl-language repl))
                 (lambda () (repl-reader ""))))
              ...)
         (apply (lambda datums b0 b1 ...)
                (let ((port (open-input-string (read-line repl))))
                  (let lp ((out '()))
                    (let ((x (read port)))
                      (if (eof-object? x)
                          (reverse out)
                          (lp (cons x out))))))))))
    ((_ (name repl . datums) docstring b0 b1 ...)
     (define-meta-command (name repl () . datums)
       docstring b0 b1 ...))))



;;;
;;; Help commands
;;;

(define-meta-command (help repl . args)
  "help 
help GROUP
help [-c] COMMAND

Gives help on the meta-commands available at the REPL.

With one argument, tries to look up the argument as a group name, giving
help on that group if successful. Otherwise tries to look up the
argument as a command, giving help on the command.

If there is a command whose name is also a group name, use the ,help
-c COMMAND form to give help on the command instead of the group.

Without any argument, a list of help commands and command groups
are displayed."
  (pmatch args
    (()
     (display-group (lookup-group 'help))
     (display "Command Groups:\n\n")
     (display-summary "help all" #f "List all commands")
     (for-each (lambda (g)
		 (let* ((name (symbol->string (group-name g)))
			(usage (string-append "help " name))
			(header (string-append "List " name " commands")))
		   (display-summary usage #f header)))
	       (cdr *command-table*))
     (newline)
     (display "Type `,COMMAND -h' to show documentation of each command.")
     (newline))
    ((all)
     (for-each display-group *command-table*))
    ((,group) (guard (lookup-group group))
     (display-group (lookup-group group)))
    ((,command) (guard (lookup-command command))
     (display-command (lookup-command command)))
    ((-c ,command) (guard (lookup-command command))
     (display-command (lookup-command command)))
    ((,command)
     (user-error "Unknown command or group: ~A" command))
    ((-c ,command)
     (user-error "Unknown command: ~A" command))
    (else
     (user-error "Bad arguments: ~A" args))))

(define guile:apropos apropos)
(define-meta-command (apropos repl regexp)
  "apropos REGEXP
Find bindings/modules/packages."
  (guile:apropos (->string regexp)))

(define-meta-command (describe repl (form))
  "describe OBJ
Show description/documentation."
  (display (object-documentation (repl-eval repl (repl-parse repl form))))
  (newline))

(define-meta-command (option repl . args)
  "option [KEY VALUE]
List/show/set options."
  (pmatch args
    (()
     (for-each (lambda (key+val)
		 (format #t "~A\t~A\n" (car key+val) (cdr key+val)))
	       (repl-options repl)))
    ((,key)
     (display (repl-option-ref repl key))
     (newline))
    ((,key ,val)
     (repl-option-set! repl key val)
     (case key
       ((trace)
        (let ((vm (repl-vm repl)))
          (if val
              (apply vm-trace-on vm val)
              (vm-trace-off vm))))))))

(define-meta-command (quit repl)
  "quit
Quit this session."
  (throw 'quit))


;;;
;;; Module commands
;;;

(define-meta-command (module repl . args)
  "module [MODULE]
Change modules / Show current module."
  (pmatch args
    (() (puts (module-name (current-module))))
    ((,mod-name) (guard (list? mod-name))
     (set-current-module (resolve-module mod-name)))
    (,mod-name (set-current-module (resolve-module mod-name)))))

(define-meta-command (import repl . args)
  "import [MODULE ...]
Import modules / List those imported."
  (let ()
    (define (use name)
      (let ((mod (resolve-interface name)))
        (if mod
            (module-use! (current-module) mod)
            (user-error "No such module: ~A" name))))
    (if (null? args)
        (for-each puts (map module-name (module-uses (current-module))))
        (for-each use args))))

(define-meta-command (load repl file . opts)
  "load FILE
Load a file in the current module.

  -f    Load source file (see `compile')"
  (let* ((file (->string file))
	 (objcode (if (memq #:f opts)
		      (apply load-source-file file opts)
		      (apply load-file file opts))))
    (vm-load (repl-vm repl) objcode)))

(define-meta-command (binding repl)
  "binding
List current bindings."
  (module-for-each (lambda (k v) (format #t "~23A ~A\n" k v))
                   (current-module)))


;;;
;;; Language commands
;;;

(define-meta-command (language repl name)
  "language LANGUAGE
Change languages."
  (set! (repl-language repl) (lookup-language name))
  (repl-welcome repl))


;;;
;;; Compile commands
;;;

(define-meta-command (compile repl (form) . opts)
  "compile FORM
Generate compiled code.

  -e    Stop after expanding syntax/macro
  -t    Stop after translating into GHIL
  -c    Stop after generating GLIL

  -O    Enable optimization
  -D    Add debug information"
  (let ((x (apply repl-compile repl (repl-parse repl form) opts)))
    (cond ((objcode? x) (guile:disassemble x))
          (else (repl-print repl x)))))

(define guile:compile-file compile-file)
(define-meta-command (compile-file repl file . opts)
  "compile-file FILE
Compile a file."
  (guile:compile-file (->string file) #:opts opts))

(define (guile:disassemble x)
  ((@ (language assembly disassemble) disassemble) x))

(define-meta-command (disassemble repl (form))
  "disassemble PROGRAM
Disassemble a program."
  (guile:disassemble (repl-eval repl (repl-parse repl form))))

(define-meta-command (disassemble-file repl file)
  "disassemble-file FILE
Disassemble a file."
  (guile:disassemble (load-objcode (->string file))))


;;;
;;; Profile commands
;;;

(define-meta-command (time repl (form))
  "time FORM
Time execution."
  (let* ((gc-start (gc-run-time))
	 (tms-start (times))
	 (result (repl-eval repl (repl-parse repl form)))
	 (tms-end (times))
	 (gc-end (gc-run-time)))
    (define (get proc start end)
      (exact->inexact (/ (- (proc end) (proc start)) internal-time-units-per-second)))
    (repl-print repl result)
    (display "clock utime stime cutime cstime gctime\n")
    (format #t "~5,2F ~5,2F ~5,2F ~6,2F ~6,2F ~6,2F\n"
	    (get tms:clock tms-start tms-end)
	    (get tms:utime tms-start tms-end)
	    (get tms:stime tms-start tms-end)
	    (get tms:cutime tms-start tms-end)
	    (get tms:cstime tms-start tms-end)
	    (get identity gc-start gc-end))
    result))

(define-meta-command (profile repl form . opts)
  "profile FORM
Profile execution."
  (apply vm-profile
         (repl-vm repl)
         (repl-compile repl (repl-parse repl form))
         opts))


;;;
;;; Debug commands
;;;

(define-meta-command (backtrace repl)
  "backtrace
Display backtrace."
  (vm-backtrace (repl-vm repl)))

(define-meta-command (debugger repl)
  "debugger
Start debugger."
  (vm-debugger (repl-vm repl)))

(define-meta-command (trace repl form . opts)
  "trace FORM
Trace execution.

  -s    Display stack
  -l    Display local variables
  -b    Bytecode level trace"
  (apply vm-trace (repl-vm repl)
         (repl-compile repl (repl-parse repl form))
         opts))

(define-meta-command (step repl)
  "step FORM
Step execution."
  (display "Not implemented yet\n"))


;;;
;;; System commands 
;;;

(define guile:gc gc)
(define-meta-command (gc repl)
  "gc
Garbage collection."
  (guile:gc))

(define-meta-command (statistics repl)
  "statistics
Display statistics."
  (let ((this-tms (times))
	(this-vms (vm-stats (repl-vm repl)))
	(this-gcs (gc-stats))
	(last-tms (repl-tm-stats repl))
	(last-vms (repl-vm-stats repl))
	(last-gcs (repl-gc-stats repl)))
    ;; GC times
    (let ((this-times  (assq-ref this-gcs 'gc-times))
	  (last-times  (assq-ref last-gcs 'gc-times)))
      (display-diff-stat "GC times:" #t this-times last-times "times")
      (newline))
    ;; Memory size
    (let ((this-cells  (assq-ref this-gcs 'cells-allocated))
	  (this-heap   (assq-ref this-gcs 'cell-heap-size))
	  (this-bytes  (assq-ref this-gcs 'bytes-malloced))
	  (this-malloc (assq-ref this-gcs 'gc-malloc-threshold)))
      (display-stat-title "Memory size:" "current" "limit")
      (display-stat "heap" #f this-cells this-heap "cells")
      (display-stat "malloc" #f this-bytes this-malloc "bytes")
      (newline))
    ;; Cells collected
    (let ((this-marked (assq-ref this-gcs 'cells-marked))
	  (last-marked (assq-ref last-gcs 'cells-marked))
	  (this-swept  (assq-ref this-gcs 'cells-swept))
	  (last-swept  (assq-ref last-gcs 'cells-swept)))
      (display-stat-title "Cells collected:" "diff" "total")
      (display-diff-stat "marked" #f this-marked last-marked "cells")
      (display-diff-stat "swept" #f this-swept last-swept "cells")
      (newline))
    ;; GC time taken
    (let ((this-mark  (assq-ref this-gcs 'gc-mark-time-taken))
	  (last-mark  (assq-ref last-gcs 'gc-mark-time-taken))
	  (this-total (assq-ref this-gcs 'gc-time-taken))
	  (last-total (assq-ref last-gcs 'gc-time-taken)))
      (display-stat-title "GC time taken:" "diff" "total")
      (display-time-stat "mark" this-mark last-mark)
      (display-time-stat "total" this-total last-total)
      (newline))
    ;; Process time spent
    (let ((this-utime  (tms:utime this-tms))
	  (last-utime  (tms:utime last-tms))
	  (this-stime  (tms:stime this-tms))
	  (last-stime  (tms:stime last-tms))
	  (this-cutime (tms:cutime this-tms))
	  (last-cutime (tms:cutime last-tms))
	  (this-cstime (tms:cstime this-tms))
	  (last-cstime (tms:cstime last-tms)))
      (display-stat-title "Process time spent:" "diff" "total")
      (display-time-stat "user" this-utime last-utime)
      (display-time-stat "system" this-stime last-stime)
      (display-time-stat "child user" this-cutime last-cutime)
      (display-time-stat "child system" this-cstime last-cstime)
      (newline))
    ;; VM statistics
    (let ((this-time  (vms:time this-vms))
	  (last-time  (vms:time last-vms))
	  (this-clock (vms:clock this-vms))
	  (last-clock (vms:clock last-vms)))
      (display-stat-title "VM statistics:" "diff" "total")
      (display-time-stat "time spent" this-time last-time)
      (display-diff-stat "bogoclock" #f this-clock last-clock "clock")
      (display-mips-stat "bogomips" this-time this-clock last-time last-clock)
      (newline))
    ;; Save statistics
    ;; Save statistics
    (set! (repl-tm-stats repl) this-tms)
    (set! (repl-vm-stats repl) this-vms)
    (set! (repl-gc-stats repl) this-gcs)))

(define (display-stat title flag field1 field2 unit)
  (let ((str (format #f "~~20~AA ~~10@A /~~10@A ~~A~~%" (if flag "" "@"))))
    (format #t str title field1 field2 unit)))

(define (display-stat-title title field1 field2)
  (display-stat title #t field1 field2 ""))

(define (display-diff-stat title flag this last unit)
  (display-stat title flag (- this last) this unit))

(define (display-time-stat title this last)
  (define (conv num)
    (format #f "~10,2F" (exact->inexact (/ num internal-time-units-per-second))))
  (display-stat title #f (conv (- this last)) (conv this) "s"))

(define (display-mips-stat title this-time this-clock last-time last-clock)
  (define (mips time clock)
    (if (= time 0) "----" (format #f "~10,2F" (/ clock time 1000000.0))))
  (display-stat title #f
		(mips (- this-time last-time) (- this-clock last-clock))
		(mips this-time this-clock) "mips"))

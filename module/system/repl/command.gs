;;; Repl commands

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define (puts x) (display x) (newline))

(define (user-error msg . args)
  (throw 'user-error #f msg args #f))


;;;
;;; Meta command
;;;

(define *command-table*
  '((help     (help h) (apropos a) (describe d) (option o) (quit q))
    (module   (module m) (use u) (import i) (load l) (binding b) (lsmod lm))
    (package  (package p) (lspkg lp) (autopackage) (globals g))
    (language (language L))
    (compile  (compile c) (compile-file cc)
	      (disassemble x) (disassemble-file xx))
    (profile  (time t) (profile pr))
    (debug    (backtrace bt) (debugger db) (trace tr) (step st))
    (system   (statistics stat) (gc))))

(define (group-name g) (car g))
(define (group-commands g) (cdr g))

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

(define (meta-command repl line)
  (let ((input (call-with-input-string (string-append "(" line ")") read)))
    (if (not (null? input))
	(do ((key (car input))
	     (args (cdr input) (cdr args))
	     (opts '() (cons (make-keyword-from-dash-symbol (car args)) opts)))
	    ((or (null? args)
		 (not (symbol? (car args)))
		 (not (eq? (string-ref (symbol->string (car args)) 0) #\-)))
	     (let ((c (lookup-command key)))
	       (if c
		   (cond ((memq :h opts) (display-command c))
			 (else (apply (command-procedure c)
				      repl (append! args opts))))
		   (user-error "Unknown meta command: ~A" key))))))))


;;;
;;; Help commands
;;;

(define (help repl . args)
  "help [GROUP]
Show help messages.
The optional argument can be either one of command groups or
command names.  Without argument, a list of help commands and
all command groups are displayed, as you have already seen :)"
  (match args
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
     (display "Enter `,COMMAND -h' to display documentation of each command.")
     (newline))
    (('all)
     (for-each display-group *command-table*))
    ((? lookup-group group)
     (display-group (lookup-group group)))
    (else (user-error "Unknown command group: ~A" (car args)))))

(define guile-apropos apropos)
(define (apropos repl regexp)
  "apropos [options] REGEXP
Find bindings/modules/packages."
  (guile-apropos (object->string regexp display)))

(define (describe repl obj)
  "describe OBJ
Show description/documentation."
  (display "Not implemented yet\n"))

(define (option repl . args)
  "option [KEY [VALUE]]
List/show/set options."
  (display "Not implemented yet\n"))

(define (quit repl)
  "quit
Quit this session."
  (throw 'quit))


;;;
;;; Module commands
;;;

(define (module repl . args)
  "module [MODULE]
Change modules / Show current module."
  (match args
    (() (puts (binding repl.module)))))

(define (use repl . args)
  "use [MODULE ...]
Use modules."
  (define (use name)
    (let ((mod (resolve-interface name)))
      (if mod
	  (module-use! repl.module mod)
	  (user-error "No such module: ~A" name))))
  (if (null? args)
      (for-each puts (map module-name
			  (cons repl.module (module-uses repl.module))))
      (for-each (lambda (name)
		  (cond
		   ((pair? name) (use name))
		   ((symbol? name)
		    (cond ((find-one-module (symbol->string name)) => use)))
		   (else (user-error "Invalid module name: ~A" name))))
		args)))

(define (import repl . args)
  "import [MODULE ...]
Import modules / List those imported."
  (define (use name)
    (let ((mod (resolve-interface name)))
      (if mod
	  (module-use! repl.module mod)
	  (user-error "No such module: ~A" name))))
  (if (null? args)
      (for-each puts (map module-name
			  (cons repl.module (module-uses repl.module))))
      (for-each (lambda (name)
		  (cond
		   ((pair? name) (use name))
		   ((symbol? name)
		    (and-let* ((m (find-one-module (symbol->string name))))
		      (puts m) (use m)))
		   (else (user-error "Invalid module name: ~A" name))))
		args)))

(define (load repl file . opts)
  "load [options] FILE
Load a file in the current module."
  (apply repl-load-file repl (->string file) opts))

(define (binding repl . opts)
  "binding [-a]
List current bindings."
  (fold (lambda (s v d) (format #t "~23A ~A\n" s v)) #f repl.module))

(define (lsmod repl . args)
  "lsmod
."
  (define (use name)
    (set! repl.module (resolve-module name))
    (module-use! repl.module repl.value-history))
  (if (null? args)
      (use '(guile-user))
      (let ((name (car args)))
	(cond
	 ((pair? name) (use name))
	 ((symbol? name)
	  (and-let* ((m (find-one-module (symbol->string name))))
	    (puts m) (use m)))
	 (else (user-error "Invalid module name: ~A" name))))))


;;;
;;; Package commands
;;;

(define (package repl)
  "package [PACKAGE]
List available packages/modules."
  (for-each puts (find-module "")))

(define (lspkg repl)
  "lspkg
List available packages/modules."
  (for-each puts (find-module "")))

(define (autopackage repl)
  "autopackage
List available packages/modules."
  (for-each puts (find-module "")))

(define (globals repl)
  "globals
List all global variables."
  (global-fold (lambda (s v d) (format #t "~A\t~S\n" s v)) #f))


;;;
;;; Language commands
;;;

(define (language repl name)
  "language LANGUAGE
Change languages."
  (set! repl.language (lookup-language name))
  (repl-welcome repl))


;;;
;;; Compile commands
;;;

(define (compile repl form . opts)
  "compile [options] FORM
Generate compiled code.

  -e    Stop after expanding syntax/macro
  -t    Stop after translating into GHIL
  -c    Stop after generating GLIL
  -l    Stop before linking
  -o    Compile into bytecode

  -O    Enable optimization
  -D    Add debug information"
  (let ((x (apply repl-compile repl form opts)))
    (cond ((null? opts)
	   (puts x))
	  ((memq :l opts)
	   (disassemble-bytecode x))
	  ((memq :c opts)
	   (pprint-glil x))
	  (else
	   (puts x)))))

(define (compile-file repl file . opts)
  "compile-file [options] FILE
Compile a file."
  (apply repl-compile-file repl (->string file) opts))

(define (disassemble repl prog)
  "disassemble PROGRAM
Disassemble a program."
  (disassemble-program (repl-eval repl prog)))

(define (disassemble-file repl file)
  "disassemble-file FILE
Disassemble a file."
  (disassemble-bytecode (load-file-in (->string file)
				      repl.module
				      repl.language)))

(define (->string x)
  (object->string x display))


;;;
;;; Profile commands
;;;

(define (profile repl form . opts)
  "profile FORM
Profile execution."
  (apply vm-profile repl.vm (repl-compile repl form) opts))


;;;
;;; Debug commands
;;;

(define guile-backtrace backtrace)
(define (backtrace repl)
  "backtrace
Show backtrace (if any)."
  (guile-backtrace))

(define (debugger repl)
  "debugger
Start debugger."
  (debug))

(define (trace repl form . opts)
  "trace [-a] FORM
Trace execution."
  (apply vm-trace repl.vm (repl-compile repl form) opts))

(define (step repl)
  "step FORM
Step execution."
  (display "Not implemented yet\n"))


;;;
;;; System commands 
;;;

(define (time repl form)
  "time FORM
Time execution."
  (let* ((vms-start (vm-stats repl.vm))
	 (gc-start (gc-run-time))
	 (tms-start (times))
	 (result (repl-eval repl form))
	 (tms-end (times))
	 (gc-end (gc-run-time))
	 (vms-end (vm-stats repl.vm)))
    (define (get proc start end)
      (/ (- (proc end) (proc start)) internal-time-units-per-second))
    (repl-print repl result)
    (display "clock utime stime cutime cstime gctime\n")
    (format #t "~5,2F ~5,2F ~5,2F ~6,2F ~6,2F ~6,2F\n"
	    (get tms:clock tms-start tms-end)
	    (get tms:utime tms-start tms-end)
	    (get tms:stime tms-start tms-end)
	    (get tms:cutime tms-start tms-end)
	    (get tms:cstime tms-start tms-end)
	    (get id gc-start gc-end))
    result))

;;;
;;; Statistics
;;;

(define guile-gc gc)
(define (gc repl)
  "gc
Garbage collection."
  (guile-gc))

(define (display-stat title flag field1 field2 unit)
  (let ((str (format #f "~~20~AA ~~10@A /~~10@A ~~A~~%" (if flag "" "@"))))
    (format #t str title field1 field2 unit)))

(define (display-stat-title title field1 field2)
  (display-stat title #t field1 field2 ""))

(define (display-diff-stat title flag this last unit)
  (display-stat title flag (- this last) this unit))

(define (display-time-stat title this last)
  (define (conv num)
    (format #f "~10,2F" (/ num internal-time-units-per-second)))
  (display-stat title #f (conv (- this last)) (conv this) "s"))

(define (display-mips-stat title this-time this-clock last-time last-clock)
  (define (mips time clock)
    (if (= time 0) "----" (format #f "~10,2F" (/ clock time 1000000))))
  (display-stat title #f
		(mips (- this-time last-time) (- this-clock last-clock))
		(mips this-time this-clock) "mips"))

(define (statistics repl)
  "statistics
Display statistics."
  (let ((this-tms (times))
	(this-vms (vm-stats repl.vm))
	(this-gcs (gc-stats))
	(last-tms repl.tm-stats)
	(last-vms repl.vm-stats)
	(last-gcs repl.gc-stats))
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
	  (this-sweep (assq-ref this-gcs 'gc-sweep-time-taken))
	  (last-sweep (assq-ref last-gcs 'gc-sweep-time-taken))
	  (this-total (assq-ref this-gcs 'gc-time-taken))
	  (last-total (assq-ref last-gcs 'gc-time-taken)))
      (display-stat-title "GC time taken:" "diff" "total")
      (display-time-stat "mark" this-mark last-mark)
      (display-time-stat "sweep" this-sweep last-sweep)
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
    (set! repl.tm-stats this-tms)
    (set! repl.vm-stats this-vms)
    (set! repl.gc-stats this-gcs)))

;;;; test-suite/lib.scm --- generic support for testing
;;;; Copyright (C) 1999, 2000 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA

(define-module (test-suite lib)
  :use-module (ice-9 regex))

(export

 ;; Exceptions which are commonly being tested for.
 exception:out-of-range exception:wrong-type-arg

 ;; Reporting passes and failures.
 run-test
 pass-if expect-fail
 pass-if-exception expect-fail-exception

 ;; Naming groups of tests in a regular fashion.
 with-test-prefix with-test-prefix* current-test-prefix

 ;; Reporting results in various ways.
 register-reporter unregister-reporter reporter-registered?
 make-count-reporter print-counts
 make-log-reporter 
 full-reporter
 user-reporter
 format-test-name

 ;; Noticing whether an error occurs.
 signals-error? signals-error?*)


;;;; If you're using Emacs's Scheme mode:
;;;;   (put 'with-test-prefix 'scheme-indent-function 1)


;;;; CORE FUNCTIONS
;;;;
;;;; The function (run-test name expected-result thunk) is the heart of the
;;;; testing environment.  The first parameter NAME is a unique name for the
;;;; test to be executed (for an explanation of this parameter see below under
;;;; TEST NAMES).  The second parameter EXPECTED-RESULT is a boolean value
;;;; that indicates whether the corresponding test is expected to pass.  If
;;;; EXPECTED-RESULT is #t the test is expected to pass, if EXPECTED-RESULT is
;;;; #f the test is expected to fail.  Finally, THUNK is the function that
;;;; actually performs the test.  For example:
;;;;
;;;;    (run-test "integer addition" #t (lambda () (= 2 (+ 1 1))))
;;;;
;;;; To report success, THUNK should either return #t or throw 'pass.  To
;;;; report failure, THUNK should either return #f or throw 'fail.  If THUNK
;;;; returns a non boolean value or throws 'unresolved, this indicates that
;;;; the test did not perform as expected.  For example the property that was
;;;; to be tested could not be tested because something else went wrong.
;;;; THUNK may also throw 'untested to indicate that the test was deliberately
;;;; not performed, for example because the test case is not complete yet.
;;;; Finally, if THUNK throws 'unsupported, this indicates that this test
;;;; requires some feature that is not available in the configured testing
;;;; environment.  All other exceptions thrown by THUNK are considered as
;;;; errors.
;;;;
;;;;
;;;; Convenience macros for tests expected to pass or fail
;;;;
;;;; * (pass-if name body) is a short form for 
;;;;   (run-test name #t (lambda () body))
;;;; * (expect-fail name body) is a short form for 
;;;;   (run-test name #f (lambda () body))
;;;;
;;;; For example:  
;;;;
;;;;    (pass-if "integer addition" (= 2 (+ 1 1)))
;;;;
;;;;
;;;; Convenience macros to test for exceptions
;;;;
;;;; The following macros take exception parameters which are pairs
;;;; (type . message), where type is a symbol that denotes an exception type
;;;; like 'wrong-type-arg or 'out-of-range, and message is a string holding a
;;;; regular expression that describes the error message for the exception
;;;; like "Argument .* out of range".
;;;;
;;;; * (pass-if-exception name exception body) will pass if the execution of
;;;;   body causes the given exception to be thrown.  If no exception is
;;;;   thrown, the test fails.  If some other exception is thrown, is is an
;;;;   error.
;;;; * (expect-fail-exception name exception body) will pass unexpectedly if
;;;;   the execution of body causes the given exception to be thrown.  If no
;;;;   exception is thrown, the test fails expectedly.  If some other
;;;;   exception is thrown, it is an error.


;;;; TEST NAMES
;;;;
;;;; Every test in the test suite has a unique name, to help
;;;; developers find tests that are failing (or unexpectedly passing),
;;;; and to help gather statistics.
;;;;
;;;; A test name is a list of printable objects.  For example:
;;;; ("ports.scm" "file" "read and write back list of strings")
;;;; ("ports.scm" "pipe" "read")
;;;;
;;;; Test names may contain arbitrary objects, but they always have
;;;; the following properties:
;;;; - Test names can be compared with EQUAL?.
;;;; - Test names can be reliably stored and retrieved with the standard WRITE
;;;;   and READ procedures; doing so preserves their identity.
;;;; 
;;;; For example:
;;;; 
;;;;    (pass-if "simple addition" (= 4 (+ 2 2)))
;;;; 
;;;; In that case, the test name is the list ("simple addition").
;;;;
;;;; The WITH-TEST-PREFIX syntax and WITH-TEST-PREFIX* procedure establish
;;;; a prefix for the names of all tests whose results are reported
;;;; within their dynamic scope.  For example:
;;;; 
;;;; (begin
;;;;   (with-test-prefix "basic arithmetic"
;;;;     (pass-if "addition" (= (+ 2 2) 4))
;;;;     (pass-if "subtraction" (= (- 4 2) 2)))
;;;;   (pass-if "multiplication" (= (* 2 2) 4)))
;;;; 
;;;; In that example, the three test names are:
;;;;   ("basic arithmetic" "addition"),
;;;;   ("basic arithmetic" "subtraction"), and
;;;;   ("multiplication").
;;;;
;;;; WITH-TEST-PREFIX can be nested.  Each WITH-TEST-PREFIX postpends
;;;; a new element to the current prefix:
;;;; 
;;;; (with-test-prefix "arithmetic"
;;;;   (with-test-prefix "addition"
;;;;     (pass-if "integer" (= (+ 2 2) 4))
;;;;     (pass-if "complex" (= (+ 2+3i 4+5i) 6+8i)))
;;;;   (with-test-prefix "subtraction"
;;;;     (pass-if "integer" (= (- 2 2) 0))
;;;;     (pass-if "complex" (= (- 2+3i 1+2i) 1+1i))))
;;;; 
;;;; The four test names here are:
;;;;   ("arithmetic" "addition" "integer")
;;;;   ("arithmetic" "addition" "complex")
;;;;   ("arithmetic" "subtraction" "integer")
;;;;   ("arithmetic" "subtraction" "complex")
;;;;
;;;; To print a name for a human reader, we DISPLAY its elements,
;;;; separated by ": ".  So, the last set of test names would be
;;;; reported as:
;;;; 
;;;;   arithmetic: addition: integer
;;;;   arithmetic: addition: complex
;;;;   arithmetic: subtraction: integer
;;;;   arithmetic: subtraction: complex
;;;;
;;;; The Guile benchmarks use with-test-prefix to include the name of
;;;; the source file containing the test in the test name, to help
;;;; developers to find failing tests, and to provide each file with its
;;;; own namespace.


;;;; REPORTERS
;;;; 
;;;; A reporter is a function which we apply to each test outcome.
;;;; Reporters can log results, print interesting results to the
;;;; standard output, collect statistics, etc.
;;;; 
;;;; A reporter function takes two mandatory arguments, RESULT and TEST, and
;;;; possibly additional arguments depending on RESULT; its return value
;;;; is ignored.  RESULT has one of the following forms:
;;;;
;;;; pass         - The test named TEST passed.  
;;;;                Additional arguments are ignored.
;;;; upass        - The test named TEST passed unexpectedly.
;;;;                Additional arguments are ignored.
;;;; fail         - The test named TEST failed.
;;;;                Additional arguments are ignored.
;;;; xfail        - The test named TEST failed, as expected.
;;;;                Additional arguments are ignored.
;;;; unresolved   - The test named TEST did not perform as expected, for
;;;;                example the property that was to be tested could not be
;;;;                tested because something else went wrong.
;;;;                Additional arguments are ignored.
;;;; untested     - The test named TEST was not actually performed, for
;;;;                example because the test case is not complete yet. 
;;;;                Additional arguments are ignored.
;;;; unsupported  - The test named TEST requires some feature that is not
;;;;                available in the configured testing environment.
;;;;                Additional arguments are ignored.
;;;; error        - An error occurred while the test named TEST was
;;;;                performed.  Since this result means that the system caught
;;;;                an exception it could not handle, the exception arguments
;;;;                are passed as additional arguments.
;;;;
;;;; This library provides some standard reporters for logging results
;;;; to a file, reporting interesting results to the user, and
;;;; collecting totals.
;;;;
;;;; You can use the REGISTER-REPORTER function and friends to add
;;;; whatever reporting functions you like.  If you don't register any
;;;; reporters, the library uses FULL-REPORTER, which simply writes
;;;; all results to the standard output.


;;;; MISCELLANEOUS
;;;;

;;; Define some exceptions which are commonly being tested for.
(define exception:out-of-range
  (cons 'out-of-range "^Argument .*out of range"))
(define exception:wrong-type-arg
  (cons 'wrong-type-arg "^Wrong type argument"))

;;; Display all parameters to the default output port, followed by a newline.
(define (display-line . objs)
  (for-each display objs)
  (newline))

;;; Display all parameters to the given output port, followed by a newline.
(define (display-line-port port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))


;;;; CORE FUNCTIONS
;;;;

;;; The central testing routine.
;;; The idea is taken from Greg, the GNUstep regression test environment.
(define run-test #f)
(let ((test-running #f))
  (define (local-run-test name expect-pass thunk)
    (if test-running
	(error "Nested calls to run-test are not permitted.")
	(let ((test-name (full-name name)))
	  (set! test-running #t)
	  (catch #t
	    (lambda ()
	      (let ((result (thunk)))
		(if (eq? result #t) (throw 'pass))
		(if (eq? result #f) (throw 'fail))
		(throw 'unresolved)))
	    (lambda (key . args)
	      (case key
		((pass) 
		 (report (if expect-pass 'pass 'upass) test-name))
		((fail) 
		 (report (if expect-pass 'fail 'xfail) test-name))
		((unresolved untested unsupported) 
		 (report key test-name))
		((quit) 
		 (report 'unresolved test-name)
		 (quit))
		(else 
		 (report 'error test-name (cons key args))))))
	  (set! test-running #f))))
  (set! run-test local-run-test))

;;; A short form for tests that are expected to pass, taken from Greg.
(defmacro pass-if (name body . rest)
  `(run-test ,name #t (lambda () ,body ,@rest)))

;;; A short form for tests that are expected to fail, taken from Greg.
(defmacro expect-fail (name body . rest)
  `(run-test ,name #f (lambda () ,body ,@rest)))

;;; A helper function to implement the macros that test for exceptions.
(define (run-test-exception name exception expect-pass thunk)
  (run-test name expect-pass
    (lambda ()
      (catch (car exception)
	(lambda () (thunk) #f)
	(lambda (key proc message . rest) 
	  (if (not (string-match (cdr exception) message))
	      (apply throw key proc message rest)
	      #t))))))

;;; A short form for tests that expect a certain exception to be thrown.
(defmacro pass-if-exception (name exception body . rest)
  `(,run-test-exception ,name ,exception #t (lambda () ,body ,@rest)))

;;; A short form for tests expected to fail to throw a certain exception.
(defmacro expect-fail-exception (name exception body . rest)
  `(,run-test-exception ,name ,exception #f (lambda () ,body ,@rest)))


;;;; TEST NAMES
;;;;

;;;; Turn a test name into a nice human-readable string.
(define (format-test-name name)
  (call-with-output-string
   (lambda (port)
     (let loop ((name name)
		(separator ""))
       (if (pair? name)
	   (begin
	     (display separator port)
	     (display (car name) port)
	     (loop (cdr name) ": ")))))))

;;;; For a given test-name, deliver the full name including all prefixes.
(define (full-name name)
  (append (current-test-prefix) (list name)))

;;; A fluid containing the current test prefix, as a list.
(define prefix-fluid (make-fluid))
(fluid-set! prefix-fluid '())
(define (current-test-prefix)
  (fluid-ref prefix-fluid))

;;; Postpend PREFIX to the current name prefix while evaluting THUNK.
;;; The name prefix is only changed within the dynamic scope of the
;;; call to with-test-prefix*.  Return the value returned by THUNK.
(define (with-test-prefix* prefix thunk)
  (with-fluids ((prefix-fluid
		 (append (fluid-ref prefix-fluid) (list prefix))))
    (thunk)))

;;; (with-test-prefix PREFIX BODY ...)
;;; Postpend PREFIX to the current name prefix while evaluating BODY ...
;;; The name prefix is only changed within the dynamic scope of the
;;; with-test-prefix expression.  Return the value returned by the last
;;; BODY expression.
(defmacro with-test-prefix (prefix . body)
  `(with-test-prefix* ,prefix (lambda () ,@body)))


;;;; REPORTERS
;;;; 

;;; The global list of reporters.
(define reporters '())

;;; The default reporter, to be used only if no others exist.
(define default-reporter #f)

;;; Add the procedure REPORTER to the current set of reporter functions.
;;; Signal an error if that reporter procedure object is already registered.
(define (register-reporter reporter)
  (if (memq reporter reporters)
      (error "register-reporter: reporter already registered: " reporter))
  (set! reporters (cons reporter reporters)))

;;; Remove the procedure REPORTER from the current set of reporter
;;; functions.  Signal an error if REPORTER is not currently registered.
(define (unregister-reporter reporter)
  (if (memq reporter reporters)
      (set! reporters (delq! reporter reporters))
      (error "unregister-reporter: reporter not registered: " reporter)))

;;; Return true iff REPORTER is in the current set of reporter functions.
(define (reporter-registered? reporter)
  (if (memq reporter reporters) #t #f))

;;; Send RESULT to all currently registered reporter functions.
(define (report . args)
  (if (pair? reporters)
      (for-each (lambda (reporter) (apply reporter args))
		reporters)
      (apply default-reporter args)))


;;;; Some useful standard reporters:
;;;; Count reporters count the occurrence of each test result type.
;;;; Log reporters write all test results to a given log file.
;;;; Full reporters write all test results to the standard output.
;;;; User reporters write interesting test results to the standard output.

;;; The complete list of possible test results.
(define result-tags 
  '((pass        "PASS"        "passes:                 ")
    (fail        "FAIL"        "failures:               ")
    (upass       "UPASS"       "unexpected passes:      ")
    (xfail       "XFAIL"       "expected failures:      ")
    (unresolved  "UNRESOLVED"  "unresolved test cases:  ")
    (untested    "UNTESTED"    "untested test cases:    ")
    (unsupported "UNSUPPORTED" "unsupported test cases: ")
    (error       "ERROR"       "errors:                 ")))

;;; The list of important test results.
(define important-result-tags 
  '(fail upass unresolved error))

;;; Display a single test result in formatted form to the given port
(define (print-result port result name . args)
  (let* ((tag (assq result result-tags))
	 (label (if tag (cadr tag) #f)))
    (if label
	(begin
	  (display label port)
	  (display ": " port)
	  (display (format-test-name name) port)
	  (if (pair? args)
	      (begin
		(display " - arguments: " port)
		(write args port)))
	  (newline port))
	(error "(test-suite lib) FULL-REPORTER: unrecognized result: "
	       result))))

;;; Return a list of the form (COUNTER RESULTS), where:
;;; - COUNTER is a reporter procedure, and
;;; - RESULTS is a procedure taking no arguments which returns the
;;;   results seen so far by COUNTER.  The return value is an alist
;;;   mapping outcome symbols (`pass', `fail', etc.) onto counts.
(define (make-count-reporter)
  (let ((counts (map (lambda (tag) (cons (car tag) 0)) result-tags)))
    (list
     (lambda (result name . args)
       (let ((pair (assq result counts)))
	 (if pair 
	     (set-cdr! pair (+ 1 (cdr pair)))
	     (error "count-reporter: unexpected test result: " 
		    (cons result (cons name args))))))
     (lambda ()
       (append counts '())))))

;;; Print a count reporter's results nicely.  Pass this function the value
;;; returned by a count reporter's RESULTS procedure.
(define (print-counts results . port?)
  (let ((port (if (pair? port?) 
		  (car port?)
		  (current-output-port))))
    (newline port)
    (display-line-port port "Totals for this test run:")
    (for-each
     (lambda (tag)
       (let ((result (assq (car tag) results)))
	 (if result
	     (display-line-port port (caddr tag) (cdr result))
	     (display-line-port port
				"Test suite bug: "
				"no total available for `" (car tag) "'"))))
     result-tags)
    (newline port)))

;;; Return a reporter procedure which prints all results to the file
;;; FILE, in human-readable form.  FILE may be a filename, or a port.
(define (make-log-reporter file)
  (let ((port (if (output-port? file) file
		  (open-output-file file))))
    (lambda args
      (apply print-result port args)
      (force-output port))))

;;; A reporter that reports all results to the user.
(define (full-reporter . args)
  (apply print-result (current-output-port) args))

;;; A reporter procedure which shows interesting results (failures,
;;; unexpected passes etc.) to the user.
(define (user-reporter result name . args)
  (if (memq result important-result-tags)
      (apply full-reporter result name args)))

(set! default-reporter full-reporter)


;;;; Detecting whether errors occur

;;; (signals-error? KEY BODY ...)
;;; Evaluate the expressions BODY ... .  If any errors occur, return #t;
;;; otherwise, return #f.
;;;
;;; KEY indicates the sort of errors to look for; it can be a symbol,
;;; indicating that only errors with that name should be caught, or
;;; #t, meaning that any kind of error should be caught.
(defmacro signals-error? key-and-body
  `(signals-error?* ,(car key-and-body)
		    (lambda () ,@(cdr key-and-body))))

;;; (signals-error?* KEY THUNK)
;;; Apply THUNK, catching errors.  If any errors occur, return #t;
;;; otherwise, return #f.
;;;
;;; KEY indicates the sort of errors to look for; it can be a symbol,
;;; indicating that only errors with that name should be caught, or
;;; #t, meaning that any kind of error should be caught.
(define (signals-error?* key thunk)
  (catch key
	 (lambda () (thunk) #f)
	 (lambda args #t)))

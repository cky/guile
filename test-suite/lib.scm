;;;; test-suite/lib.scm --- generic support for testing
;;;; 	Copyright (C) 1999 Free Software Foundation, Inc.
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

(define-module (test-suite lib))

(export

 ;; Reporting passes and failures.
 pass fail pass-if

 ;; Indicating tests that are expected to fail.
 expect-failure expect-failure-if expect-failure-if*

 ;; Marking independent groups of tests.
 catch-test-errors catch-test-errors*

 ;; Naming groups of tests in a regular fashion.
 with-test-prefix with-test-prefix* current-test-prefix

 ;; Reporting results in various ways.
 register-reporter unregister-reporter reporter-registered?
 make-count-reporter print-counts
 make-log-reporter 
 full-reporter
 user-reporter
 format-test-name)


;;;; If you're using Emacs's Scheme mode:
;;;;   (put 'expect-failure 'scheme-indent-function 0)
;;;;   (put 'with-test-prefix 'scheme-indent-function 1)


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
;;;; The functions for reporting results (PASS, FAIL, PASS-IF, ...)
;;;; take the name of the passing/failing test as an argument.
;;;; For example:
;;;; 
;;;;    (if (= 4 (+ 2 2))
;;;;      (pass "simple addition"))
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
;;;;     (pass-if "division" (= (- 4 2) 2)))
;;;;   (pass-if "multiplication" (= (* 2 2) 4)))
;;;; 
;;;; In that example, the three test names are:
;;;;   ("basic arithmetic" "addition"),
;;;;   ("basic arithmetic" "division"), and
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

;;;; A reporter is a function which we apply to each test outcome.
;;;; Reporters can log results, print interesting results to the
;;;; standard output, collect statistics, etc.
;;;; 
;;;; A reporter function takes one argument, RESULT; its return value
;;;; is ignored.  RESULT has one of the following forms:
;;;;
;;;; (pass TEST)    - The test named TEST passed.
;;;; (fail TEST)    - The test named TEST failed.
;;;; (xpass TEST)   - The test named TEST passed unexpectedly.
;;;; (xfail TEST)   - The test named TEST failed, as expected.
;;;; (error PREFIX) - An error occurred, with TEST as the current
;;;;                  test name prefix.  Some tests were
;;;;                  probably not executed because of this.
;;;;
;;;; This library provides some standard reporters for logging results
;;;; to a file, reporting interesting results to the user, and
;;;; collecting totals.
;;;;
;;;; You can use the REGISTER-REPORTER function and friends to add
;;;; whatever reporting functions you like.  If you don't register any
;;;; reporters, the library uses FULL-REPORTER, which simply writes
;;;; all results to the standard output.


;;;; with-test-prefix: naming groups of tests
;;;; See the discussion of TEST

;;; A fluid containing the current test prefix, as a list.
(define prefix-fluid (make-fluid))
(fluid-set! prefix-fluid '())

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

(define (current-test-prefix)
  (fluid-ref prefix-fluid))


;;;; register-reporter, etc. --- the global reporter list

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
(define (report result)
  (if (pair? reporters)
      (for-each (lambda (reporter) (reporter result))
		reporters)
      (default-reporter result)))


;;;; Some useful reporter functions.

;;; Return a list of the form (COUNTER RESULTS), where:
;;; - COUNTER is a reporter procedure, and
;;; - RESULTS is a procedure taking no arguments which returns the
;;;   results seen so far by COUNTER.  The return value is an alist
;;;   mapping outcome symbols (`pass', `fail', etc.) onto counts.
(define (make-count-reporter)
  (let ((counts (map (lambda (outcome) (cons outcome 0))
		     '(pass fail xpass xfail error))))
    (list
     (lambda (result)
       (let ((pair (assq (car result) counts)))
	 (if pair (set-cdr! pair (+ 1 (cdr pair)))
	     (error "count-reporter: unexpected test result: " result))))
     (lambda ()
       (append counts '())))))

;;; Print a count reporter's results nicely.  Pass this function the value
;;; returned by a count reporter's RESULTS procedure.
(define print-counts
  (let ((tags '(pass fail xpass xfail error))
	(labels
	 '("passes:               "
	   "failures:             "
	   "unexpected passes:    "
	   "unexpected failures:  "
	   "errors:               ")))
    (lambda (results . port?)
      (let ((port (if (pair? port?) 
		      (car port?)
		      (current-output-port))))
	(newline port)
	(display-line-port port "Totals for this test run:")
	(for-each
	 (lambda (tag label)
	   (let ((result (assq tag results)))
	     (if result
		 (display-line-port port label (cdr result))
		 (display-line-port port
				    "Test suite bug: "
				    "no total available for `" tag "'"))))
	 tags labels)
	(newline port)))))
				 
;;; Handy functions.  Should be in a library somewhere.
(define (display-line . objs)
  (for-each display objs)
  (newline))
(define (display-line-port port . objs)
  (for-each (lambda (obj) (display obj port))
	    objs)
  (newline port))

;;; Turn a test name into a nice human-readable string.
(define (format-test-name name)
  (call-with-output-string
   (lambda (port)
     (let loop ((name name))
       (if (pair? name) 
	   (begin 
	     (display (car name) port)
	     (if (pair? (cdr name))
		 (display ": " port))
	     (loop (cdr name))))))))

;;; Return a reporter procedure which prints all results to the file
;;; FILE, in human-readable form.  FILE may be a filename, or a port.
(define (make-log-reporter file)
  (let ((port (if (output-port? file) file
		  (open-output-file file))))
    (lambda (result)
      (display (car result) port)
      (display ": " port)
      (display (format-test-name (cadr result)) port)
      (newline port)
      (force-output port))))

;;; A reporter that reports all results to the user.
(define (full-reporter result)
  (let ((label (case (car result)
		 ((pass) "pass")
		 ((fail) "FAIL")
		 ((xpass) "XPASS")
		 ((xfail) "xfail")
		 ((error) "ERROR")
		 (else #f))))
    (if label
	(display-line label ": " (format-test-name (cadr result)))
	(error "(test-suite lib) FULL-REPORTER: unrecognized result: "
	       result))))

;;; A reporter procedure which shows interesting results (failures,
;;; unexpected passes) to the user.
(define (user-reporter result)
  (case (car result)
    ((fail xpass) (full-reporter result))))

(set! default-reporter full-reporter)


;;;; Marking independent groups of tests.

;;; When test code encounters an error (like "file not found" or "()
;;; is not a pair"), that may mean that that particular test can't
;;; continue, or that some nearby tests shouldn't be run, but it
;;; doesn't mean the whole test suite must be aborted.
;;;
;;; Wrap each group of interdependent tests in a CATCH-TEST-ERRORS
;;; form, so that if an error occurs, that group will be aborted, but
;;; control will continue after the catch-test-errors form.

;;; Evaluate thunk, catching errors.  If THUNK returns without
;;; signalling any errors, return a list containing its value.
;;; Otherwise, return #f.
(define (catch-test-errors* thunk)

  (letrec ((handler
	    (lambda (key . args)
	      (display-line "ERROR in test "
			    (format-test-name (current-test-prefix))
			    ":")
	      (apply display-error
		     (make-stack #t handler)
		     (current-error-port)
		     args)
	      (throw 'catch-test-errors))))

    ;; I don't know if we should really catch everything here.  If you
    ;; find a case where an error is signalled which really should abort
    ;; the whole test case, feel free to adjust this appropriately.
    (catch 'catch-test-errors
	   (lambda ()
	     (lazy-catch #t
			 (lambda () (list (thunk)))
			 handler))
	   (lambda args
	     (report (list 'error (current-test-prefix)))
	     #f))))

;;; (catch-test-errors BODY ...)
;;; Evaluate the expressions BODY ...  If a BODY expression signals an
;;; error, record that in the test results, and return #f.  Otherwise,
;;; return a list containing the value of the last BODY expression.
(defmacro catch-test-errors body
  `(catch-test-errors* (lambda () ,@body)))


;;;; Indicating tests that are expected to fail.

;;; Fluid indicating whether we're currently expecting tests to fail.
(define expected-failure-fluid (make-fluid))

;;; Hmm.  The documentation treats EXPECT-FAILURE-IF as the primitive,
;;; but in the implementation, EXPECT-FAILURE-IF* is the primitive.

;;; (expect-failure-if TEST BODY ...)
;;; Evaluate the expression TEST, then evaluate BODY ...
;;; If TEST evaluates to a true value, expect all tests whose results
;;; are reported by the BODY expressions to fail.
;;; Return the value of the last BODY form.
(defmacro expect-failure-if (test . body)
  `(expect-failure-if* ,test (lambda () ,@body)))

;;; Call THUNK; if SHOULD-FAIL is true, expect any tests whose results
;;; are reported by THUNK to fail.  Return the value returned by THUNK.
(define (expect-failure-if* should-fail thunk)
  (with-fluids ((expected-failure-fluid (not (not should-fail))))
    (thunk)))

;;; (expect-failure BODY ...)
;;; Evaluate the expressions BODY ..., expecting all tests whose results
;;; they report to fail.
(defmacro expect-failure body 
  `(expect-failure-if #t ,@body))

(define (pessimist?)
  (fluid-ref expected-failure-fluid))


;;;; Reporting passes and failures.

(define (full-name name)
  (append (current-test-prefix) (list name)))

(define (pass name)
  (report (list (if (pessimist?) 'xpass 'pass)
		(full-name name))))

(define (fail name)
  (report (list (if (pessimist?) 'xfail 'fail)
		(full-name name))))

(define (pass-if name condition)
  ((if condition pass fail) name))

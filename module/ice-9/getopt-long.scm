;;; Copyright (C) 1998, 2001, 2006, 2009, 2011 Free Software Foundation, Inc.
;;;
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

;;; Author: Russ McManus (rewritten by Thien-Thi Nguyen)

;;; Commentary:

;;; This module implements some complex command line option parsing, in
;;; the spirit of the GNU C library function `getopt_long'.  Both long
;;; and short options are supported.
;;;
;;; The theory is that people should be able to constrain the set of
;;; options they want to process using a grammar, rather than some arbitrary
;;; structure.  The grammar makes the option descriptions easy to read.
;;;
;;; `getopt-long' is a procedure for parsing command-line arguments in a
;;; manner consistent with other GNU programs.  `option-ref' is a procedure
;;; that facilitates processing of the `getopt-long' return value.

;;; (getopt-long ARGS GRAMMAR)
;;; Parse the arguments ARGS according to the argument list grammar GRAMMAR.
;;;
;;; ARGS should be a list of strings.  Its first element should be the
;;; name of the program; subsequent elements should be the arguments
;;; that were passed to the program on the command line.  The
;;; `program-arguments' procedure returns a list of this form.
;;;
;;; GRAMMAR is a list of the form:
;;; ((OPTION (PROPERTY VALUE) ...) ...)
;;;
;;; Each OPTION should be a symbol.  `getopt-long' will accept a
;;; command-line option named `--OPTION'.
;;; Each option can have the following (PROPERTY VALUE) pairs:
;;;
;;;   (single-char CHAR) --- Accept `-CHAR' as a single-character
;;;		equivalent to `--OPTION'.  This is how to specify traditional
;;;		Unix-style flags.
;;;   (required? BOOL) --- If BOOL is true, the option is required.
;;;		getopt-long will raise an error if it is not found in ARGS.
;;;   (value BOOL) --- If BOOL is #t, the option accepts a value; if
;;;		it is #f, it does not; and if it is the symbol
;;;		`optional', the option may appear in ARGS with or
;;;		without a value.
;;;   (predicate FUNC) --- If the option accepts a value (i.e. you
;;;		specified `(value #t)' for this option), then getopt
;;;		will apply FUNC to the value, and throw an exception
;;;		if it returns #f.  FUNC should be a procedure which
;;;		accepts a string and returns a boolean value; you may
;;;		need to use quasiquotes to get it into GRAMMAR.
;;;
;;; The (PROPERTY VALUE) pairs may occur in any order, but each
;;; property may occur only once.  By default, options do not have
;;; single-character equivalents, are not required, and do not take
;;; values.
;;;
;;; In ARGS, single-character options may be combined, in the usual
;;; Unix fashion: ("-x" "-y") is equivalent to ("-xy").  If an option
;;; accepts values, then it must be the last option in the
;;; combination; the value is the next argument.  So, for example, using
;;; the following grammar:
;;;      ((apples    (single-char #\a))
;;;       (blimps    (single-char #\b) (value #t))
;;;       (catalexis (single-char #\c) (value #t)))
;;; the following argument lists would be acceptable:
;;;    ("-a" "-b" "bang" "-c" "couth")     ("bang" and "couth" are the values
;;;                                         for "blimps" and "catalexis")
;;;    ("-ab" "bang" "-c" "couth")         (same)
;;;    ("-ac" "couth" "-b" "bang")         (same)
;;;    ("-abc" "couth" "bang")             (an error, since `-b' is not the
;;;                                         last option in its combination)
;;;
;;; If an option's value is optional, then `getopt-long' decides
;;; whether it has a value by looking at what follows it in ARGS.  If
;;; the next element is does not appear to be an option itself, then
;;; that element is the option's value.
;;;
;;; The value of a long option can appear as the next element in ARGS,
;;; or it can follow the option name, separated by an `=' character.
;;; Thus, using the same grammar as above, the following argument lists
;;; are equivalent:
;;;   ("--apples" "Braeburn" "--blimps" "Goodyear")
;;;   ("--apples=Braeburn" "--blimps" "Goodyear")
;;;   ("--blimps" "Goodyear" "--apples=Braeburn")
;;;
;;; If the option "--" appears in ARGS, argument parsing stops there;
;;; subsequent arguments are returned as ordinary arguments, even if
;;; they resemble options.  So, in the argument list:
;;;         ("--apples" "Granny Smith" "--" "--blimp" "Goodyear")
;;; `getopt-long' will recognize the `apples' option as having the
;;; value "Granny Smith", but it will not recognize the `blimp'
;;; option; it will return the strings "--blimp" and "Goodyear" as
;;; ordinary argument strings.
;;;
;;; The `getopt-long' function returns the parsed argument list as an
;;; assocation list, mapping option names --- the symbols from GRAMMAR
;;; --- onto their values, or #t if the option does not accept a value.
;;; Unused options do not appear in the alist.
;;;
;;; All arguments that are not the value of any option are returned
;;; as a list, associated with the empty list.
;;;
;;; `getopt-long' throws an exception if:
;;; - it finds an unrecognized property in GRAMMAR
;;; - the value of the `single-char' property is not a character
;;; - it finds an unrecognized option in ARGS
;;; - a required option is omitted
;;; - an option that requires an argument doesn't get one
;;; - an option that doesn't accept an argument does get one (this can
;;;   only happen using the long option `--opt=value' syntax)
;;; - an option predicate fails
;;;
;;; So, for example:
;;;
;;; (define grammar
;;;   `((lockfile-dir (required? #t)
;;;                   (value #t)
;;;                   (single-char #\k)
;;;                   (predicate ,file-is-directory?))
;;;     (verbose (required? #f)
;;;              (single-char #\v)
;;;              (value #f))
;;;     (x-includes (single-char #\x))
;;;     (rnet-server (single-char #\y)
;;;                  (predicate ,string?))))
;;;
;;; (getopt-long '("my-prog" "-vk" "/tmp" "foo1" "--x-includes=/usr/include"
;;;                "--rnet-server=lamprod" "--" "-fred" "foo2" "foo3")
;;;                grammar)
;;; => ((() "foo1" "-fred" "foo2" "foo3")
;;; 	(rnet-server . "lamprod")
;;; 	(x-includes . "/usr/include")
;;; 	(lockfile-dir . "/tmp")
;;; 	(verbose . #t))

;;; (option-ref OPTIONS KEY DEFAULT)
;;; Return value in alist OPTIONS using KEY, a symbol; or DEFAULT if not
;;; found.  The value is either a string or `#t'.
;;;
;;; For example, using the `getopt-long' return value from above:
;;;
;;; (option-ref (getopt-long ...) 'x-includes 42) => "/usr/include"
;;; (option-ref (getopt-long ...) 'not-a-key! 31) => 31

;;; Code:

(define-module (ice-9 getopt-long)
  #:use-module ((ice-9 common-list) #:select (remove-if-not))
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (getopt-long option-ref))

(define %program-name (make-fluid))
(define (program-name)
  (or (fluid-ref %program-name) "guile"))

(define (fatal-error fmt . args)
  (format (current-error-port) "~a: " (program-name))
  (apply format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define-record-type option-spec
  (%make-option-spec name value required? single-char predicate
                     value-policy)
  option-spec?
  (name
   option-spec->name set-option-spec-name!)
  (value 
   option-spec->value set-option-spec-value!)
  (required?
   option-spec->required? set-option-spec-required?!)
  (option-spec->single-char
   option-spec->single-char set-option-spec-single-char!)
  (predicate
   option-spec->predicate set-option-spec-predicate!)
  (value-policy
   option-spec->value-policy set-option-spec-value-policy!))

(define (make-option-spec name)
  (%make-option-spec name #f #f #f #f #f))

(define (parse-option-spec desc)
  (let ((spec (make-option-spec (symbol->string (car desc)))))
    (for-each (match-lambda
               (('required? val)
                (set-option-spec-required?! spec val))
               (('value val)
                (set-option-spec-value-policy! spec val))
               (('single-char val)
                (or (char? val)
                    (error "`single-char' value must be a char!"))
                (set-option-spec-single-char! spec val))
               (('predicate pred)
                (set-option-spec-predicate!
                 spec (lambda (name val)
                        (or (not val)
                            (pred val)
                            (fatal-error "option predicate failed: --~a"
                                         name)))))
               ((prop val)
                (error "invalid getopt-long option property:" prop)))
              (cdr desc))
    spec))

(define (split-arg-list argument-list)
  ;; Scan ARGUMENT-LIST for "--" and return (BEFORE-LS . AFTER-LS).
  ;; Discard the "--".  If no "--" is found, AFTER-LS is empty.
  (let loop ((yes '()) (no argument-list))
    (cond ((null? no)               (cons (reverse yes) no))
	  ((string=? "--" (car no)) (cons (reverse yes) (cdr no)))
	  (else (loop (cons (car no) yes) (cdr no))))))

(define short-opt-rx           (make-regexp "^-([a-zA-Z]+)(.*)"))
(define long-opt-no-value-rx   (make-regexp "^--([^=]+)$"))
(define long-opt-with-value-rx (make-regexp "^--([^=]+)=(.*)"))

(define (expand-clumped-singles opt-ls)
  ;; example: ("--xyz" "-abc5d") => ("--xyz" "-a" "-b" "-c" "5d")
  (let loop ((opt-ls opt-ls) (ret-ls '()))
    (cond ((null? opt-ls)
           (reverse ret-ls))                                    ;;; retval
          ((regexp-exec short-opt-rx (car opt-ls))
           => (lambda (match)
                (let ((singles (reverse
                                (map (lambda (c)
                                       (string-append "-" (make-string 1 c)))
                                     (string->list
                                      (match:substring match 1)))))
                      (extra (match:substring match 2)))
                  (loop (cdr opt-ls)
                        (append (if (string=? "" extra)
                                    singles
                                    (cons extra singles))
                                ret-ls)))))
          (else (loop (cdr opt-ls)
                      (cons (car opt-ls) ret-ls))))))

(define (looks-like-an-option string)
  (or (regexp-exec short-opt-rx string)
      (regexp-exec long-opt-with-value-rx string)
      (regexp-exec long-opt-no-value-rx string)))

(define (process-options specs argument-ls)
  ;; Use SPECS to scan ARGUMENT-LS; return (FOUND . ETC).
  ;; FOUND is an unordered list of option specs for found options, while ETC
  ;; is an order-maintained list of elements in ARGUMENT-LS that are neither
  ;; options nor their values.
  (let ((idx (map (lambda (spec)
                    (cons (option-spec->name spec) spec))
                  specs))
        (sc-idx (map (lambda (spec)
                       (cons (make-string 1 (option-spec->single-char spec))
                             spec))
                     (remove-if-not option-spec->single-char specs))))
    (let loop ((argument-ls argument-ls) (found '()) (etc '()))
      (define (eat! spec ls)
        (define (val!loop val n-ls n-found n-etc)
          (set-option-spec-value!
           spec
           ;; handle multiple occurrances
           (cond ((option-spec->value spec)
                  => (lambda (cur)
                       ((if (list? cur) cons list)
                        val cur)))
                 (else val)))
          (loop n-ls n-found n-etc))
        (cond
         ((eq? 'optional (option-spec->value-policy spec))
          (if (or (null? ls)
                  (looks-like-an-option (car ls)))
              (val!loop #t ls (cons spec found) etc)
              (val!loop (car ls) (cdr ls) (cons spec found) etc)))
         ((eq? #t (option-spec->value-policy spec))
          (if (or (null? ls)
                  (looks-like-an-option (car ls)))
              (fatal-error "option must be specified with argument: --~a"
                           (option-spec->name spec))
              (val!loop (car ls) (cdr ls) (cons spec found) etc)))
         (else
          (val!loop #t ls (cons spec found) etc))))
      
      (match argument-ls
        (()
         (cons found (reverse etc)))
        ((opt . rest)
         (cond
          ((regexp-exec short-opt-rx opt)
           => (lambda (match)
                (let* ((c (match:substring match 1))
                       (spec (or (assoc-ref sc-idx c)
                                 (fatal-error "no such option: -~a" c))))
                  (eat! spec rest))))
          ((regexp-exec long-opt-no-value-rx opt)
           => (lambda (match)
                (let* ((opt (match:substring match 1))
                       (spec (or (assoc-ref idx opt)
                                 (fatal-error "no such option: --~a" opt))))
                  (eat! spec rest))))
          ((regexp-exec long-opt-with-value-rx opt)
           => (lambda (match)
                (let* ((opt (match:substring match 1))
                       (spec (or (assoc-ref idx opt)
                                 (fatal-error "no such option: --~a" opt))))
                  (if (option-spec->value-policy spec)
                      (eat! spec (cons (match:substring match 2) rest))
                      (fatal-error "option does not support argument: --~a"
                                   opt)))))
          (else
           (loop rest found (cons opt etc)))))))))

(define (getopt-long program-arguments option-desc-list)
  "Process options, handling both long and short options, similar to
the glibc function 'getopt_long'.  PROGRAM-ARGUMENTS should be a value
similar to what (program-arguments) returns.  OPTION-DESC-LIST is a
list of option descriptions.  Each option description must satisfy the
following grammar:

    <option-spec>           :: (<name> . <attribute-ls>)
    <attribute-ls>          :: (<attribute> . <attribute-ls>)
                               | ()
    <attribute>             :: <required-attribute>
                               | <arg-required-attribute>
                               | <single-char-attribute>
                               | <predicate-attribute>
                               | <value-attribute>
    <required-attribute>    :: (required? <boolean>)
    <single-char-attribute> :: (single-char <char>)
    <value-attribute>       :: (value #t)
                               (value #f)
                               (value optional)
    <predicate-attribute>   :: (predicate <1-ary-function>)

    The procedure returns an alist of option names and values.  Each
option name is a symbol.  The option value will be '#t' if no value
was specified.  There is a special item in the returned alist with a
key of the empty list, (): the list of arguments that are not options
or option values.
    By default, options are not required, and option values are not
required.  By default, single character equivalents are not supported;
if you want to allow the user to use single character options, you need
to add a `single-char' clause to the option description."
  (with-fluids ((%program-name (car program-arguments)))
    (let* ((specifications (map parse-option-spec option-desc-list))
           (pair (split-arg-list (cdr program-arguments)))
           (split-ls (expand-clumped-singles (car pair)))
           (non-split-ls (cdr pair))
           (found/etc (process-options specifications split-ls))
           (found (car found/etc))
           (rest-ls (append (cdr found/etc) non-split-ls)))
      (for-each (lambda (spec)
                  (let ((name (option-spec->name spec))
                        (val (option-spec->value spec)))
                    (and (option-spec->required? spec)
                         (or (memq spec found)
                             (fatal-error "option must be specified: --~a"
                                          name)))
                    (and (memq spec found)
                         (eq? #t (option-spec->value-policy spec))
                         (or val
                             (fatal-error
                              "option must be specified with argument: --~a"
                              name)))
                    (let ((pred (option-spec->predicate spec)))
                      (and pred (pred name val)))))
                specifications)
      (cons (cons '() rest-ls)
            (let ((multi-count (map (lambda (desc)
                                      (cons (car desc) 0))
                                    option-desc-list)))
              (map (lambda (spec)
                     (let ((name (string->symbol (option-spec->name spec))))
                       (cons name
                             ;; handle multiple occurrances
                             (let ((maybe-ls (option-spec->value spec)))
                               (if (list? maybe-ls)
                                   (let* ((look (assq name multi-count))
                                          (idx (cdr look))
                                          (val (list-ref maybe-ls idx)))
                                     (set-cdr! look (1+ idx)) ; ugh!
                                     val)
                                   maybe-ls)))))
                   found))))))

(define (option-ref options key default)
  "Return value in alist OPTIONS using KEY, a symbol; or DEFAULT if not found.
The value is either a string or `#t'."
  (or (assq-ref options key) default))

;;; getopt-long.scm ends here

;; popen emulation, for non-stdio based ports.

;;;; Copyright (C) 1998, 1999, 2000, 2001, 2003, 2006, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

(define-module (ice-9 popen)
  :export (port/pid-table open-pipe* open-pipe close-pipe open-input-pipe
	   open-output-pipe open-input-output-pipe))

(eval-when (load eval compile)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_popen"))

(define (make-rw-port read-port write-port)
  (make-soft-port
   (vector
    (lambda (c) (write-char c write-port))
    (lambda (s) (display s write-port))
    (lambda () (force-output write-port))
    (lambda () (read-char read-port))
    (lambda () (close-port read-port) (close-port write-port)))
   "r+"))

;; a guardian to ensure the cleanup is done correctly when
;; an open pipe is gc'd or a close-port is used.
(define pipe-guardian (make-guardian))

;; a weak hash-table to store the process ids.
(define port/pid-table (make-weak-key-hash-table 31))

(define (open-pipe* mode command . args)
  "Executes the program @var{command} with optional arguments
@var{args} (all strings) in a subprocess.
A port to the process (based on pipes) is created and returned.
@var{mode} specifies whether an input, an output or an input-output
port to the process is created: it should be the value of
@code{OPEN_READ}, @code{OPEN_WRITE} or @code{OPEN_BOTH}."
  (call-with-values (lambda ()
                      (apply open-process mode command args))
    (lambda (read-port write-port pid)
      (let ((port (or (and read-port write-port
                           (make-rw-port read-port write-port))
                      read-port
                      write-port
                      (%make-void-port mode))))
        (pipe-guardian port)
        (hashq-set! port/pid-table port pid)
        port))))

(define (open-pipe command mode)
  "Executes the shell command @var{command} (a string) in a subprocess.
A port to the process (based on pipes) is created and returned.
@var{mode} specifies whether an input, an output or an input-output
port to the process is created: it should be the value of
@code{OPEN_READ}, @code{OPEN_WRITE} or @code{OPEN_BOTH}."
  (open-pipe* mode "/bin/sh" "-c" command))

(define (fetch-pid port)
  (let ((pid (hashq-ref port/pid-table port)))
    (hashq-remove! port/pid-table port)
    pid))

(define (close-process port pid)
  (close-port port)
  (cdr (waitpid pid)))

;; for the background cleanup handler: just clean up without reporting
;; errors.  also avoids blocking the process: if the child isn't ready
;; to be collected, puts it back into the guardian's live list so it
;; can be tried again the next time the cleanup runs.
(define (close-process-quietly port pid)
  (catch 'system-error
	 (lambda ()
	   (close-port port))
	 (lambda args #f))
  (catch 'system-error
	 (lambda ()
	   (let ((pid/status (waitpid pid WNOHANG)))
             (when (zero? (car pid/status))
               ;; not ready for collection
               (pipe-guardian port)
               (hashq-set! port/pid-table port pid))))
	 (lambda args #f)))

(define (close-pipe p)
  "Closes the pipe created by @code{open-pipe}, then waits for the process
to terminate and returns its status value, @xref{Processes, waitpid}, for
information on how to interpret this value."
  (let ((pid (fetch-pid p)))
    (unless pid (error "close-pipe: pipe not in table"))
    (close-process p pid)))

(define (reap-pipes)
  (let loop ()
    (let ((p (pipe-guardian)))
      (when p
        ;; maybe removed already by close-pipe.
        (let ((pid (fetch-pid p)))
          (when pid (close-process-quietly p pid)))
        (loop)))))

(add-hook! after-gc-hook reap-pipes)

(define (open-input-pipe command)
  "Equivalent to @code{open-pipe} with mode @code{OPEN_READ}"
  (open-pipe command OPEN_READ))

(define (open-output-pipe command)
  "Equivalent to @code{open-pipe} with mode @code{OPEN_WRITE}"
  (open-pipe command OPEN_WRITE))

(define (open-input-output-pipe command)
  "Equivalent to @code{open-pipe} with mode @code{OPEN_BOTH}"
  (open-pipe command OPEN_BOTH))


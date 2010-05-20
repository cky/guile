;;; files.scm --- The R6RS file system library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
;;
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs files (6))
  (export file-exists? 
	  delete-file

	  &i/o make-i/o-error i/o-error?
	  &i/o-read make-i/o-read-error i/o-read-error?
	  &i/o-write make-i/o-write-error i/o-write-error?

	  &i/o-invalid-position 
	  make-i/o-invalid-position-error 
	  i/o-invalid-position-error? 
	  i/o-error-position
	  
	  &i/o-filename
	  make-i/o-filename-error
	  i/o-filename-error?
	  i/o-error-filename
	  
	  &i/o-file-protection 
	  make-i/o-file-protection-error
	  i/o-file-protection-error?

	  &i/o-file-is-read-only
	  make-i/o-file-is-read-only-error
	  i/o-file-is-read-only-error?

	  &i/o-file-already-exists
	  make-i/o-file-already-exists-error
	  i/o-file-already-exists-error?

	  &i/o-file-does-not-exist
	  make-i/o-file-does-not-exist-error
	  i/o-file-does-not-exist-error?

	  &i/o-port
	  make-i/o-port-error
	  i/o-port-error?
	  i/o-error-port)

  (import (rename (only (guile) file-exists? delete-file catch @@) 
		  (delete-file delete-file-internal))
	  (rnrs base (6))
	  (rnrs conditions (6))
	  (rnrs exceptions (6)))

  (define (delete-file filename)
    (catch #t 
	   (lambda () (delete-file-internal filename))
	   (lambda (key . args) (raise (make-i/o-filename-error filename)))))

  (define &i/o (@@ (rnrs conditions) &i/o))
  (define make-i/o-error (@@ (rnrs conditions) make-i/o-error))
  (define i/o-error? (@@ (rnrs conditions) i/o-error?))

  (define &i/o-read (@@ (rnrs conditions) &i/o-read))
  (define make-i/o-read-error (@@ (rnrs conditions) make-i/o-read-error))
  (define i/o-read-error? (@@ (rnrs conditions) i/o-read-error?))

  (define &i/o-write (@@ (rnrs conditions) &i/o-write))
  (define make-i/o-write-error (@@ (rnrs conditions) make-i/o-write-error))
  (define i/o-write-error? (@@ (rnrs conditions) i/o-write-error?))

  (define &i/o-invalid-position (@@ (rnrs conditions) &i/o-invalid-position))
  (define make-i/o-invalid-position-error 
    (@@ (rnrs conditions) make-i/o-invalid-position-error))
  (define i/o-invalid-position-error? 
    (@@ (rnrs conditions) i/o-invalid-position-error?))
  (define i/o-error-position (@@ (rnrs conditions) i/o-error-position))

  (define &i/o-filename (@@ (rnrs conditions) &i/o-filename))
  (define make-i/o-filename-error 
    (@@ (rnrs conditions) make-i/o-filename-error))
  (define i/o-filename-error? (@@ (rnrs conditions) i/o-filename-error?))
  (define i/o-error-filename (@@ (rnrs conditions) i/o-error-filename))

  (define &i/o-file-protection (@@ (rnrs conditions) &i/o-file-protection))
  (define make-i/o-file-protection-error 
    (@@ (rnrs conditions) make-i/o-file-protection-error))
  (define i/o-file-protection-error? 
    (@@ (rnrs conditions) i/o-file-protection-error?))

  (define &i/o-file-is-read-only (@@ (rnrs conditions) &i/o-file-is-read-only))
  (define make-i/o-file-is-read-only-error
    (@@ (rnrs conditions) make-i/o-file-is-read-only-error))
  (define i/o-file-is-read-only-error?
    (@@ (rnrs conditions) i/o-file-is-read-only-error?))

  (define &i/o-file-already-exists 
    (@@ (rnrs conditions) &i/o-file-already-exists))
  (define make-i/o-file-already-exists-error
    (@@ (rnrs conditions) make-i/o-file-already-exists-error))
  (define i/o-file-already-exists-error?
    (@@ (rnrs conditions) i/o-file-already-exists-error?))

  (define &i/o-file-does-not-exist
    (@@ (rnrs conditions) &i/o-file-does-not-exist))
  (define make-i/o-file-does-not-exist-error
    (@@ (rnrs conditions) make-i/o-file-does-not-exist-error))
  (define i/o-file-does-not-exist-error?
    (@@ (rnrs conditions) i/o-file-does-not-exist-error?))

  (define &i/o-port (@@ (rnrs conditions) &i/o-port))
  (define make-i/o-port-error (@@ (rnrs conditions) make-i/o-port-error))
  (define i/o-port-error? (@@ (rnrs conditions) i/o-port-error?))
  (define i/o-error-port (@@ (rnrs conditions) i/o-error-port))
)

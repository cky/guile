;;; simple.scm --- The R6RS simple I/O library

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


(library (rnrs io simple (6))
  (export eof-object 
          eof-object?

	  call-with-input-file
	  call-with-output-file
	  
	  input-port?
	  output-port?

	  current-input-port
	  current-output-port
	  current-error-port

	  with-input-from-file
	  with-output-to-file

	  open-input-file
	  open-output-file

	  close-input-port
	  close-output-port

	  read-char
	  peek-char
	  read
	  write-char
	  newline
	  display
	  write)
  (import (only (rnrs io ports) eof-object 
		                eof-object? 
 
                                input-port? 
				output-port?)
          (only (guile) call-with-input-file
			call-with-output-file

			current-input-port
			current-output-port
			current-error-port

			with-input-file
			with-output-file

			open-input-file
			open-output-file
			
			close-input-port
			close-output-port

			read-char
			peek-char
			read
			write-char
			newline
			display
			write)))

;;; syntax-case.scm --- R6RS support for `syntax-case' macros

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


(library (rnrs syntax-case (6))
  (export make-variable-transformer
	  syntax-case
	  syntax
	  
	  identifier?
	  bound-identifier=?
	  free-identifier=?

	  syntax->datum
	  datum->syntax
	  generate-temporaries
	  with-syntax

	  quasisyntax
	  unsyntax
	  unsyntax-splicing

	  syntax-violation)
  (import (only (guile) syntax-case
		        syntax
		        
		        identifier?
			bound-identifier=?
			free-identifier=?
			
			syntax->datum
			datum->syntax
			generate-temporaries
			with-syntax

			quasisyntax
			unsyntax
			unsyntax-splicing

			syntax-violation)))

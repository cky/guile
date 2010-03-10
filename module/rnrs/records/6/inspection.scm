;;; inspection.scm --- Inspection support for R6RS records

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


(library (rnrs records inspection (6))
  (export record? 
          record-rtd 
	  record-type-name 
	  record-type-parent 
	  record-type-uid 
	  record-type-generative? 
	  record-type-sealed? 
	  record-type-opaque? 
	  record-type-field-names 
	  record-field-mutable?)
  (import (rnrs base (6))
	  (rnrs conditions (6))
          (rnrs exceptions (6))
	  (rnrs records procedural (6))
	  (only (guile) struct-ref vtable-index-layout))

  (define record-internal? (@@ (rnrs records procedural) record-internal?))

  (define record-index-rtd (@@ (rnrs records procedural) record-index-rtd))

  (define rtd-index-name (@@ (rnrs records procedural) rtd-index-name))
  (define rtd-index-parent (@@ (rnrs records procedural) rtd-index-parent))
  (define rtd-index-uid (@@ (rnrs records procedural) rtd-index-uid))
  (define rtd-index-sealed? (@@ (rnrs records procedural) rtd-index-sealed?))
  (define rtd-index-opaque? (@@ (rnrs records procedural) rtd-index-opaque?))
  (define rtd-index-field-names 
    (@@ (rnrs records procedural) rtd-index-field-names))
  (define rtd-index-field-vtable 
    (@@ (rnrs records procedural) rtd-index-field-vtable))

  (define (record? obj)
    (and (record-internal? obj) 
	 (not (record-type-opaque? (struct-ref obj record-index-rtd)))))

  (define (record-rtd record)
    (or (and (record-internal? record)
	     (let ((rtd (struct-ref record record-index-rtd)))
	       (and (not (struct-ref rtd rtd-index-opaque?)) rtd)))
	(raise (make-assertion-violation))))

  (define (ensure-rtd rtd)
    (if (not (record-type-descriptor? rtd)) (raise (make-assertion-violation))))

  (define (record-type-name rtd) 
    (ensure-rtd rtd) (struct-ref rtd rtd-index-name))
  (define (record-type-parent rtd) 
    (ensure-rtd rtd) (struct-ref rtd rtd-index-parent))
  (define (record-type-uid rtd) (ensure-rtd rtd) (struct-ref rtd rtd-index-uid))
  (define (record-type-generative? rtd) 
    (ensure-rtd rtd) (and (record-type-uid rtd) #t))
  (define (record-type-sealed? rtd) 
    (ensure-rtd rtd) (struct-ref rtd rtd-index-sealed?))
  (define (record-type-opaque? rtd) 
    (ensure-rtd rtd) (struct-ref rtd rtd-index-opaque?))
  (define (record-type-field-names rtd)
    (ensure-rtd rtd) (struct-ref rtd rtd-index-field-names))
  (define (record-field-mutable? rtd k)
    (ensure-rtd rtd)
    (let ((vt (struct-ref rtd rtd-index-field-vtable)))
      (eqv? (string-ref (symbol->string (struct-ref vt vtable-index-layout))
			(+ (* 2 (+ k 2)) 1))
	    #\w)))
)

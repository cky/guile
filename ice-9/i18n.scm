;;;; i18n.scm --- internationalization support

;;;;	Copyright (C) 2006 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Author: Ludovic Courtès <ludovic.courtes@laas.fr>

;;; Commentary:
;;;
;;; This module provides a number of routines that support
;;; internationalization (e.g., locale-dependent text collation, character
;;; mapping, etc.).  It also defines `locale' objects, representing locale
;;; settings, that may be passed around to most of these procedures.
;;;

;;; Code:

(define-module (ice-9 i18n)
  :export (;; `locale' type
           make-locale locale?

           ;; locale category masks (standard)
           LC_ALL_MASK
           LC_COLLATE_MASK LC_CTYPE_MASK LC_MESSAGES_MASK
           LC_MONETARY_MASK LC_NUMERIC_MASK LC_TIME_MASK

           ;; locale category masks (non-standard)
           LC_PAPER_MASK LC_NAME_MASK LC_ADDRESS_MASK
           LC_TELEPHONE_MASK LC_MEASUREMENT_MASK
           LC_IDENTIFICATION_MASK

           ;; text collation
           string-locale<? string-locale>?
           string-locale-ci<? string-locale-ci>? string-locale-ci=?

           char-locale<? char-locale>?
           char-locale-ci<? char-locale-ci>? char-locale-ci=?

           ;; character mapping
           char-locale-downcase char-locale-upcase
           string-locale-downcase string-locale-upcase

           ;; reading numbers
           locale-string->integer locale-string->inexact))


(load-extension "libguile-i18n-v-0" "scm_init_i18n")


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; i18n.scm ends here

;;; Disassemble --- Disassemble .go files into something human-readable

;; Copyright 2005,2008 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Author: Ludovic Courtès <ludovic.courtes@laas.fr>
;;; Author: Andy Wingo <wingo@pobox.com>

;;; Commentary:

;; Usage: disassemble [ARGS]

;;; Code:

(define-module (scripts disassemble)
  #:use-module (system vm objcode)
  #:use-module (language assembly disassemble)
  #:export (disassemble))

(define (disassemble args)
  (for-each (lambda (file)
              (disassemble (load-objcode file)))
            (cdr args)))

(define main disassemble)

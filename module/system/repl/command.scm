;;; REPL commands

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (system repl command)
  :use-module (oop goops)
  :use-syntax (system base syntax)
  :use-module (system base language)
  :use-module (system repl common)
  :use-module (system il glil)
  :use-module (system vm core)
  :use-module (system vm load)
  :use-module (system vm trace)
  :use-module (system vm disasm)
  :use-module (system vm profile)
  :use-module (ice-9 format)
  :use-module (ice-9 session)
  :use-module (ice-9 debugger)
  :export (meta-command))

(load "command.gsm")

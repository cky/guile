;;; Guile Low Intermediate Language

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

(define-module (language glil)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:export
  (<glil-asm> make-glil-asm glil-asm?
   glil-asm-nargs glil-asm-nrest glil-asm-nlocs glil-asm-nexts
   glil-asm-meta glil-asm-body

   <glil-bind> make-glil-bind glil-bind?
   glil-bind-vars

   <glil-mv-bind> make-glil-mv-bind glil-mv-bind?
   glil-mv-bind-vars glil-mv-bind-rest

   <glil-unbind> make-glil-unbind glil-unbind?

   <glil-source> make-glil-source glil-source?
   glil-source-loc

   <glil-void> make-glil-void glil-void?

   <glil-const> make-glil-const glil-const?
   glil-const-obj

   <glil-argument> make-glil-argument glil-argument?
   glil-argument-op glil-argument-index

   <glil-local> make-glil-local glil-local?
   glil-local-op glil-local-index

   <glil-external> make-glil-external glil-external?
   glil-external-op glil-external-depth glil-external-index

   <glil-toplevel> make-glil-toplevel glil-toplevel?
   glil-toplevel-op glil-toplevel-name

   <glil-module> make-glil-module glil-module?
   glil-module-op glil-module-mod glil-module-name glil-module-public?

   <glil-label> make-glil-label glil-label?
   glil-label-label

   <glil-branch> make-glil-branch glil-branch?
   glil-branch-inst glil-branch-label

   <glil-call> make-glil-call glil-call?
   glil-call-inst glil-call-nargs

   <glil-mv-call> make-glil-mv-call glil-mv-call?
   glil-mv-call-nargs glil-mv-call-ra

   parse-glil unparse-glil))

(define (print-glil x port)
  (format port "#<glil ~s>" (unparse-glil x)))

(define-type (<glil> #:printer print-glil)
  ;; Meta operations
  (<glil-asm> nargs nrest nlocs nexts meta body)
  (<glil-bind> vars)
  (<glil-mv-bind> vars rest)
  (<glil-unbind>)
  (<glil-source> loc)
  ;; Objects
  (<glil-void>)
  (<glil-const> obj)
  ;; Variables
  (<glil-argument> op index)
  (<glil-local> op index)
  (<glil-external> op depth index)
  (<glil-toplevel> op name)
  (<glil-module> op mod name public?)
  ;; Controls
  (<glil-label> label)
  (<glil-branch> inst label)
  (<glil-call> inst nargs)
  (<glil-mv-call> nargs ra))


(define (parse-glil x)
  (pmatch x
    ((asm ,nargs ,nrest ,nlocs ,nexts ,meta . ,body)
     (make-glil-asm nargs nrest nlocs nexts meta (map parse-glil body)))
    ((bind . ,vars) (make-glil-bind vars))
    ((mv-bind ,vars . ,rest) (make-glil-mv-bind vars (map parse-glil rest)))
    ((unbind) (make-glil-unbind))
    ((source ,loc) (make-glil-source loc))
    ((void) (make-glil-void))
    ((const ,obj) (make-glil-const obj))
    ((argument ,op ,index) (make-glil-argument op index))
    ((local ,op ,index) (make-glil-local op index))
    ((external ,op ,depth ,index) (make-glil-external op depth index))
    ((toplevel ,op ,name) (make-glil-toplevel op name))
    ((module public ,op ,mod ,name) (make-glil-module op mod name #t))
    ((module private ,op ,mod ,name) (make-glil-module op mod name #f))
    ((label ,label) (make-label ,label))
    ((branch ,inst ,label) (make-glil-branch inst label))
    ((call ,inst ,nargs) (make-glil-call inst nargs))
    ((mv-call ,nargs ,ra) (make-glil-mv-call nargs ra))
    (else (error "invalid glil" x))))

(define (unparse-glil glil)
  (record-case glil
    ;; meta
    ((<glil-asm> nargs nrest nlocs nexts meta body)
     `(asm ,nargs ,nrest ,nlocs ,nexts ,meta ,@(map unparse-glil body)))
    ((<glil-bind> vars) `(bind ,@vars))
    ((<glil-mv-bind> vars rest) `(mv-bind ,vars ,@rest))
    ((<glil-unbind>) `(unbind))
    ((<glil-source> loc) `(source ,loc))
    ;; constants
    ((<glil-void>) `(void))
    ((<glil-const> obj) `(const ,obj))
    ;; variables
    ((<glil-argument> op index)
     `(argument ,op ,index))
    ((<glil-local> op index)
     `(local ,op ,index))
    ((<glil-external> op depth index)
     `(external ,op ,depth ,index))
    ((<glil-toplevel> op name)
     `(toplevel ,op ,name))
    ((<glil-module> op mod name public?)
     `(module ,(if public? 'public 'private) ,op ,mod ,name))
    ;; controls
    ((<glil-label> label) (label ,label))
    ((<glil-branch> inst label) `(branch ,inst ,label))
    ((<glil-call> inst nargs) `(call ,inst ,nargs))
    ((<glil-mv-call> nargs ra) `(mv-call ,nargs ,(unparse-glil ra)))))

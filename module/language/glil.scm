;;; Guile Low Intermediate Language

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language glil)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export
  (<glil-program> make-glil-program glil-program?
   glil-program-nargs glil-program-nrest glil-program-nlocs
   glil-program-meta glil-program-body
   
   <glil-bind> make-glil-bind glil-bind?
   glil-bind-vars

   <glil-mv-bind> make-glil-mv-bind glil-mv-bind?
   glil-mv-bind-vars glil-mv-bind-rest

   <glil-unbind> make-glil-unbind glil-unbind?

   <glil-source> make-glil-source glil-source?
   glil-source-props

   <glil-void> make-glil-void glil-void?

   <glil-const> make-glil-const glil-const?
   glil-const-obj

   <glil-lexical> make-glil-lexical glil-lexical?
   glil-lexical-local? glil-lexical-boxed? glil-lexical-op glil-lexical-index

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
  (<glil-program> nargs nrest nlocs meta body)
  (<glil-bind> vars)
  (<glil-mv-bind> vars rest)
  (<glil-unbind>)
  (<glil-source> props)
  ;; Objects
  (<glil-void>)
  (<glil-const> obj)
  ;; Variables
  (<glil-lexical> local? boxed? op index)
  (<glil-toplevel> op name)
  (<glil-module> op mod name public?)
  ;; Controls
  (<glil-label> label)
  (<glil-branch> inst label)
  (<glil-call> inst nargs)
  (<glil-mv-call> nargs ra))



(define (parse-glil x)
  (pmatch x
    ((program ,nargs ,nrest ,nlocs ,meta . ,body)
     (make-glil-program nargs nrest nlocs meta (map parse-glil body)))
    ((bind . ,vars) (make-glil-bind vars))
    ((mv-bind ,vars ,rest) (make-glil-mv-bind vars rest))
    ((unbind) (make-glil-unbind))
    ((source ,props) (make-glil-source props))
    ((void) (make-glil-void))
    ((const ,obj) (make-glil-const obj))
    ((lexical ,local? ,boxed? ,op ,index) (make-glil-lexical local? boxed? op index))
    ((toplevel ,op ,name) (make-glil-toplevel op name))
    ((module public ,op ,mod ,name) (make-glil-module op mod name #t))
    ((module private ,op ,mod ,name) (make-glil-module op mod name #f))
    ((label ,label) (make-label label))
    ((branch ,inst ,label) (make-glil-branch inst label))
    ((call ,inst ,nargs) (make-glil-call inst nargs))
    ((mv-call ,nargs ,ra) (make-glil-mv-call nargs ra))
    (else (error "invalid glil" x))))

(define (unparse-glil glil)
  (record-case glil
    ;; meta
    ((<glil-program> nargs nrest nlocs meta body)
     `(program ,nargs ,nrest ,nlocs ,meta ,@(map unparse-glil body)))
    ((<glil-bind> vars) `(bind ,@vars))
    ((<glil-mv-bind> vars rest) `(mv-bind ,vars ,rest))
    ((<glil-unbind>) `(unbind))
    ((<glil-source> props) `(source ,props))
    ;; constants
    ((<glil-void>) `(void))
    ((<glil-const> obj) `(const ,obj))
    ;; variables
    ((<glil-lexical> local? boxed? op index)
     `(lexical ,local? ,boxed? ,op ,index))
    ((<glil-toplevel> op name)
     `(toplevel ,op ,name))
    ((<glil-module> op mod name public?)
     `(module ,(if public? 'public 'private) ,op ,mod ,name))
    ;; controls
    ((<glil-label> label) `(label ,label))
    ((<glil-branch> inst label) `(branch ,inst ,label))
    ((<glil-call> inst nargs) `(call ,inst ,nargs))
    ((<glil-mv-call> nargs ra) `(mv-call ,nargs ,ra))))

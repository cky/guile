;;; Guile VM core

;;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:export (vm? the-vm make-vm vm-version
            vm:ip vm:sp vm:fp vm:last-ip

            vm-load vm-option set-vm-option! vm-version
            vms:time vms:clock

            vm-trace-level set-vm-trace-level!
            vm-next-hook vm-apply-hook vm-boot-hook vm-return-hook
            vm-break-hook vm-exit-hook vm-halt-hook vm-enter-hook))

(load-extension "libguile" "scm_init_vm")

(define (vms:time stat) (vector-ref stat 0))
(define (vms:clock stat) (vector-ref stat 1))

(define (vm-load vm objcode)
  (vm (make-program objcode)))

;;; Guile VM core

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

(define-module (system vm vm)
  :use-module (system vm frame)
  :use-module (system vm objcode)
  :export (vm? the-vm make-vm vm-version
           vm:ip vm:sp vm:fp vm:last-ip

           vm-load vm-return-value

           vm-option set-vm-option! vm-version

           vm-fetch-locals vm-fetch-externals
           vm-last-frame vm-this-frame vm-fetch-stack vm-save-stack
           vm-current-frame-chain vm-last-frame-chain

           vm-stats vms:time vms:clock

           vm-next-hook vm-apply-hook vm-boot-hook vm-return-hook
           vm-break-hook vm-exit-hook vm-halt-hook vm-enter-hook))

(dynamic-call "scm_init_vm" (dynamic-link "libguile"))

(define (vm-current-frame-chain vm)
  (make-frame-chain (vm-this-frame vm) (vm:ip vm)))

(define (vm-last-frame-chain vm)
  (make-frame-chain (vm-last-frame vm) (vm:last-ip vm)))

(define (vm-fetch-locals vm)
  (frame-local-variables (vm-this-frame vm)))

(define (vm-fetch-externals vm)
  (frame-external-variables (vm-this-frame vm)))

(define (vm-return-value vm)
  (car (vm-fetch-stack vm)))

(define (vms:time stat) (vector-ref stat 0))
(define (vms:clock stat) (vector-ref stat 1))

(define (vm-load vm objcode)
  (vm (objcode->program objcode)))

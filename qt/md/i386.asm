;; i386.asm -- assembly support.

;;
;; QuickThreads -- Threads-building toolkit.
;; Copyright (c) 2001 Free Software Foundation, Inc.
;;
;; Permission to use, copy, modify and distribute this software and
;; its documentation for any purpose and without fee is hereby
;; granted, provided that the above copyright notice and this notice
;; appear in all copies.  This software is provided as a
;; proof-of-concept and for demonstration purposes; there is no
;; representation about the suitability of this software for any
;; purpose.

;; NOTE: double-labeled `_name' and `name' for System V compatability.
;; NOTE: Comment lines start like this one, or with '//' ONLY.  Sorry!

;; Callee-save: %esi, %edi, %ebx, %ebp
;; Caller-save: %eax, %ecx
;; Can't tell: %edx (seems to work w/o saving it.)
;;
;; Assignment:
;;
;; See ``i386.h'' for the somewhat unconventional stack layout. 


	.386p
	.model flat
	.code

	public _qt_abort
	public qt_abort
	public _qt_block
	public qt_block
	public _qt_blocki
	public qt_blocki

;; These all have the type signature
;;
;;	void *blocking (helper, arg0, arg1, new)
;;
;; On procedure entry, the helper is at 4(sp), args at 8(sp) and
;; 12(sp) and the new thread's sp at 16(sp).  It *appears* that the
;; calling convention for the 8X86 requires the caller to save all
;; floating-point registers, this makes our life easy. 

;; Halt the currently-running thread.  Save it's callee-save regs on
;; to the stack, 32 bytes.  Switch to the new stack (next == 16+32(sp))
;; and call the user function (f == 4+32(sp) with arguments: old sp
;; arg1 (8+32(sp)) and arg2 (12+32(sp)).  When the user function is
;; done, restore the new thread's state and return.
;;
;; `qt_abort' is (currently) an alias for `qt_block' because most of
;; the work is shared.  We could save the insns up to `qt_common' by
;; replicating, but w/o replicating we need an inital subtract (to
;; offset the stack as if it had been a qt_block) and then a jump
;; to qt_common.  For the cost of a jump, we might as well just do
;; all the work.
;;
;; The helper function (4(sp)) can return a void* that is returned
;; by the call to `qt_blockk{,i}'.  Since we don't touch %eax in
;; between, we get that ``for free''.

_qt_abort:
qt_abort:
_qt_block:
qt_block:
_qt_blocki:
qt_blocki:
	push ebp		; Save callee-save, sp-=4.
	push esi		; Save callee-save, sp-=4.
	push edi		; Save callee-save, sp-=4.
	push ebx		; Save callee-save, sp-=4.
	mov eax, esp		; Remember old stack pointer.

qt_common:
	mov esp, [esp+32]	; Move to new thread.
	push [eax+28]		; Push arg 2.
	push [eax+24]		; Push arg 1.
	push eax		; Push arg 0.
	mov ebx, [eax+20]	; Get function to call.
	call ebx		; Call f.
	add esp, 12		; Pop args.

	pop ebx			; Restore callee-save, sp+=4.
	pop edi			; Restore callee-save, sp+=4.
	pop esi			; Restore callee-save, sp+=4.
	pop ebp			; Restore callee-save, sp+=4.
	ret			; Resume the stopped function.
	hlt


;; Start a varargs thread.

	public _qt_vstart
	public qt_vstart

_qt_vstart:
qt_vstart:
	push edi		; Push `pt' arg to `startup'.
	call ebp		; Call `startup'.
	pop eax			; Clean up the stack.

	call ebx		; Call the user's function.

	push eax		; Push return from user's.
	push edi		; Push `pt' arg to `cleanup'.
	call esi		; Call `cleanup'.

	hlt			; `cleanup' never returns.

	end

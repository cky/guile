	.text
	.align	2
    	.global	qt_abort
    	.global	qt_block
       	.global	qt_blocki

	# r0:	helper
	# r1:	arg1
	# r2:	arg2
	# r3:	new_sp
qt_abort:
qt_block:
qt_blocki:
	stmfd	sp!, {r4-r11,lr}
	mov	ip, r0
	mov	r0, sp
	mov     sp, r3
	mov	lr, pc
	mov	pc, ip
	ldmfd	sp!, {r4-r11,pc}


	.global qt_start
	.global qt_error
	.type	qt_start,function
qt_start:
	ldr     r0, [sp]
    	ldr     r1, [sp, #4]
    	ldr     r2, [sp, #8]
        ldr     lr, qt_error_loc
	ldr     pc, [sp, #12]

qt_error_loc:
	.word qt_error

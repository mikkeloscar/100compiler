	.text	0x00400000
	.globl	main
	la	$28, _heap_
	jal	main
	ori	$2, $0, 10
	syscall
# 
main:
	addi	$29, $29, -8
	sw	$31, 4($29)
	sw	$16, 0($29)
	jal	getint
# was:	jal	getint, 
# 	ori	_exps__3_,2,0
# 	ori	2,_exps__3_,0
	jal	fib
# was:	jal	fib, 2
# 	ori	_exps__2_,2,0
# 	ori	2,_exps__2_,0
	jal	putint
# was:	jal	putint, 2
# 	ori	_return__1_,2,0
# 	ori	2,_return__1_,0
	j	main_exit
main_exit:
	lw	$16, 0($29)
	lw	$31, 4($29)
	addi	$29, $29, 8
	jr	$31
# 
fib:
	addi	$29, $29, -16
	sw	$31, 12($29)
	sw	$18, 8($29)
	sw	$17, 4($29)
	sw	$16, 0($29)
	ori	$16, $2, 0
# was:	ori	n_4_, 2, 0
# 	ori	m_5_,3,0
# 	ori	_less1__9_,n_4_,0
	ori	$2, $0, 2
# was:	ori	_less2__10_, 0, 2
	slt	$2, $16, $2
# was:	slt	_if__6_, _less1__9_, _less2__10_
	beq	$2, $0, _else__7_
# was:	beq	_if__6_, 0, _else__7_
	ori	$2, $16, 0
# was:	ori	_return__11_, n_4_, 0
# 	ori	2,_return__11_,0
	j	fib_exit
	j	_endif__8_
_else__7_:
# 	ori	_minus1__16_,n_4_,0
	ori	$2, $0, 1
# was:	ori	_minus2__17_, 0, 1
	sub	$2, $16, $2
# was:	sub	_exps__15_, _minus1__16_, _minus2__17_
# 	ori	2,_exps__15_,0
	jal	fib
# was:	jal	fib, 2
	ori	$17, $2, 0
# was:	ori	_plus1__13_, 2, 0
# 	ori	_minus1__19_,n_4_,0
	ori	$2, $0, 2
# was:	ori	_minus2__20_, 0, 2
	sub	$2, $16, $2
# was:	sub	_exps__18_, _minus1__19_, _minus2__20_
# 	ori	2,_exps__18_,0
	jal	fib
# was:	jal	fib, 2
# 	ori	_plus2__14_,2,0
	add	$2, $17, $2
# was:	add	_return__12_, _plus1__13_, _plus2__14_
# 	ori	2,_return__12_,0
	j	fib_exit
_endif__8_:
fib_exit:
	lw	$18, 8($29)
	lw	$17, 4($29)
	lw	$16, 0($29)
	lw	$31, 12($29)
	addi	$29, $29, 16
	jr	$31
putint:
	addi	$29, $29, -8
	sw	$2, 0($29)
	sw	$4, 4($29)
	ori	$4, $2, 0
	ori	$2, $0, 1
	syscall
	ori	$2, $0, 4
	la	$4, _cr_
	syscall
	lw	$2, 0($29)
	lw	$4, 4($29)
	addi	$29, $29, 8
	jr	$31
getint:
	ori	$2, $0, 5
	syscall
	jr	$31
	.data	
	.align	2
_cr_:
	.asciiz	"\n"
	.align	2
_heap_:
	.space	100000

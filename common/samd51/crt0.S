	.syntax unified
	.cpu cortex-m4
	.thumb

	.text
	.globl __vectors
	.p2align 9
	.section .vectors,"a"
__vectors:
	/* Cortex-M core interrupts */
	.word   __stack_end          /* stack top address */
	.word   _start               /* 1 Reset */
	.word   fault                /* 2 NMI. */
	.word   fault                /* 3 Hard fault. */
	.word   fault                /* 4 Mem manage. */
	.word   fault                /* 5 Bus fault. */
	.word   fault                /* 6 Usage fault. */
	.word   fault                /* 7 reserved. */
	.word   fault                /* 8 reserved. */
	.word   fault                /* 9 reserved. */
	.word   fault                /* 10 reserved. */
	.word   __gnat_sv_call_trap  /* 11 SVCall. */
	.word   __gnat_bkpt_trap     /* 12 Breakpoint. */
	.word   fault                /* 13 reserved. */
	.word   __gnat_pend_sv_trap  /* 14 PendSV. */
	.word   __gnat_sys_tick_trap /* 15 Systick. */
	/* MCU interrupts */
        .word __unknown_interrupt_handler /* 0 */
        .word __unknown_interrupt_handler /* 1 */
        .word __unknown_interrupt_handler /* 2 */
        .word __unknown_interrupt_handler /* 3 */
        .word __unknown_interrupt_handler /* 4 */
        .word __unknown_interrupt_handler /* 5 */
        .word __unknown_interrupt_handler /* 6 */
        .word __unknown_interrupt_handler /* 7 */
        .word __unknown_interrupt_handler /* 8 */
        .word __unknown_interrupt_handler /* 9 */
        .word __unknown_interrupt_handler /* 10 */
        .word __unknown_interrupt_handler /* 11 */
        .word __eic_extint_0_interrupt_handler /* 12 */
        .word __unknown_interrupt_handler /* 13 */
        .word __eic_extint_2_interrupt_handler /* 14 */
        .word __eic_extint_3_interrupt_handler /* 15 */
        .word __unknown_interrupt_handler /* 16 */
        .word __unknown_interrupt_handler /* 17 */
        .word __eic_extint_6_interrupt_handler /* 18 */
        .word __unknown_interrupt_handler /* 19 */
        .word __unknown_interrupt_handler /* 20 */
        .word __unknown_interrupt_handler /* 21 */
        .word __unknown_interrupt_handler /* 22 */
        .word __unknown_interrupt_handler /* 23 */
        .word __unknown_interrupt_handler /* 24 */
        .word __unknown_interrupt_handler /* 25 */
        .word __eic_extint_14_interrupt_handler /* 26 */
        .word __unknown_interrupt_handler /* 27 */
        .word __unknown_interrupt_handler /* 28 */
        .word __unknown_interrupt_handler /* 29 */
        .word __unknown_interrupt_handler /* 30 */
        .word __dmac_tcmpl_0_handler /* 31 */
        .word __dmac_tcmpl_1_handler /* 32 */
        .word __unknown_interrupt_handler /* 33 */
        .word __unknown_interrupt_handler /* 34 */
        .word __unknown_interrupt_handler /* 35 */
        .word __unknown_interrupt_handler /* 36 */
        .word __unknown_interrupt_handler /* 37 */
        .word __unknown_interrupt_handler /* 38 */
        .word __unknown_interrupt_handler /* 39 */
        .word __unknown_interrupt_handler /* 40 */
        .word __unknown_interrupt_handler /* 41 */
        .word __unknown_interrupt_handler /* 42 */
        .word __unknown_interrupt_handler /* 43 */
        .word __unknown_interrupt_handler /* 44 */
        .word __unknown_interrupt_handler /* 45 */
        .word __unknown_interrupt_handler /* 46 */
        .word __unknown_interrupt_handler /* 47 */
        .word __unknown_interrupt_handler /* 48 */
        .word __unknown_interrupt_handler /* 49 */
        .word __unknown_interrupt_handler /* 50 */
        .word __unknown_interrupt_handler /* 51 */
        .word __unknown_interrupt_handler /* 52 */
        .word __unknown_interrupt_handler /* 53 */
        .word __unknown_interrupt_handler /* 54 */
        .word __unknown_interrupt_handler /* 55 */
        .word __unknown_interrupt_handler /* 56 */
        .word __unknown_interrupt_handler /* 57 */
        .word __unknown_interrupt_handler /* 58 */
        .word __unknown_interrupt_handler /* 59 */
        .word __unknown_interrupt_handler /* 60 */
        .word __unknown_interrupt_handler /* 61 */
        .word __unknown_interrupt_handler /* 62 */
        .word __unknown_interrupt_handler /* 63 */
        .word __unknown_interrupt_handler /* 64 */
        .word __unknown_interrupt_handler /* 65 */
        .word __unknown_interrupt_handler /* 66 */
        .word __unknown_interrupt_handler /* 67 */
        .word __unknown_interrupt_handler /* 68 */
        .word __unknown_interrupt_handler /* 69 */
        .word __unknown_interrupt_handler /* 70 */
        .word __unknown_interrupt_handler /* 71 */
        .word __unknown_interrupt_handler /* 72 */
        .word __unknown_interrupt_handler /* 73 */
        .word __unknown_interrupt_handler /* 74 */
        .word __unknown_interrupt_handler /* 75 */
        .word __unknown_interrupt_handler /* 76 */
        .word __unknown_interrupt_handler /* 77 */
        .word __unknown_interrupt_handler /* 78 */
        .word __unknown_interrupt_handler /* 79 */
        .word __unknown_interrupt_handler /* 80 */
        .word __unknown_interrupt_handler /* 81 */
        .word __unknown_interrupt_handler /* 82 */
        .word __unknown_interrupt_handler /* 83 */
        .word __unknown_interrupt_handler /* 84 */
        .word __unknown_interrupt_handler /* 85 */
        .word __unknown_interrupt_handler /* 86 */
        .word __unknown_interrupt_handler /* 87 */
        .word __unknown_interrupt_handler /* 88 */
        .word __unknown_interrupt_handler /* 89 */
        .word __unknown_interrupt_handler /* 90 */
        .word __unknown_interrupt_handler /* 91 */
        .word __unknown_interrupt_handler /* 92 */
        .word __unknown_interrupt_handler /* 93 */
        .word __unknown_interrupt_handler /* 94 */
        .word __unknown_interrupt_handler /* 95 */
        .word __unknown_interrupt_handler /* 96 */
        .word __unknown_interrupt_handler /* 97 */
        .word __unknown_interrupt_handler /* 98 */
        .word __unknown_interrupt_handler /* 99 */
        .word __unknown_interrupt_handler /* 100 */
        .word __unknown_interrupt_handler /* 101 */
        .word __unknown_interrupt_handler /* 102 */
        .word __unknown_interrupt_handler /* 103 */
        .word __unknown_interrupt_handler /* 104 */
        .word __unknown_interrupt_handler /* 105 */
        .word __unknown_interrupt_handler /* 106 */
        .word __unknown_interrupt_handler /* 107 */
        .word __unknown_interrupt_handler /* 108 */
        .word __unknown_interrupt_handler /* 109 */
        .word __unknown_interrupt_handler /* 110 */
        .word __unknown_interrupt_handler /* 111 */
        .word __unknown_interrupt_handler /* 112 */
        .word __unknown_interrupt_handler /* 113 */
        .word __unknown_interrupt_handler /* 114 */
        .word __unknown_interrupt_handler /* 115 */
        .word __unknown_interrupt_handler /* 116 */
        .word __unknown_interrupt_handler /* 117 */
        .word __unknown_interrupt_handler /* 118 */
        .word __unknown_interrupt_handler /* 119 */
        .word __unknown_interrupt_handler /* 120 */
        .word __unknown_interrupt_handler /* 121 */
        .word __unknown_interrupt_handler /* 122 */
        .word __unknown_interrupt_handler /* 123 */
        .word __unknown_interrupt_handler /* 124 */
        .word __unknown_interrupt_handler /* 125 */
        .word __unknown_interrupt_handler /* 126 */
        .word __unknown_interrupt_handler /* 127 */
        .word __unknown_interrupt_handler /* 128 */
        .word __unknown_interrupt_handler /* 129 */
        .word __unknown_interrupt_handler /* 130 */
        .word __unknown_interrupt_handler /* 131 */
        .word __unknown_interrupt_handler /* 132 */
        .word __unknown_interrupt_handler /* 133 */
        .word __unknown_interrupt_handler /* 134 */
        .word __unknown_interrupt_handler /* 135 */

   .weak      __dmac_tcmpl_0_handler
   .thumb_set __dmac_tcmpl_0_handler,__gnat_irq_trap

	.text

   .weak      __unknown_interrupt_handler
   .thumb_set __unknown_interrupt_handler,__gnat_irq_trap

	.thumb_func
.weak __gnat_irq_trap
.type __gnat_irq_trap, %function
__gnat_irq_trap:
0:	b 0b
	.size __gnat_irq_trap, . - __gnat_irq_trap

	.thumb_func
.weak __gnat_sv_call_trap
.type __gnat_sv_call_trap, %function
__gnat_sv_call_trap:
0:	b 0b
	.size __gnat_sv_call_trap, . - __gnat_sv_call_trap

	.thumb_func
.weak __gnat_pend_sv_trap
.type __gnat_pend_sv_trap, %function
__gnat_pend_sv_trap:
0:	b 0b
	.size __gnat_pend_sv_trap, . - __gnat_pend_sv_trap

	.thumb_func
.weak __gnat_sys_tick_trap
.type __gnat_sys_tick_trap, %function
__gnat_sys_tick_trap:
0:	b 0b
	.size __gnat_sys_tick_trap, . - __gnat_sys_tick_trap

	.thumb_func
fault:	b fault


	.text
	.thumb_func
	.globl _start

_start:

	/* Set the stack pointer */
	ldr	r1,=__stack_end
	mov     sp, r1

	/* Copy .data */
	.thumb_func
_startup_copy_data:
	ldr	r0,=__data_start
	ldr	r1,=__data_words
	ldr	r2,=__data_load
	cmp     r1,#0
	beq     1f
0:	ldr	r4,[r2]
	str	r4,[r0]
        adds    r2,#4
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_copy_data, . - _startup_copy_data

	/* Copy .BKUPRAM_data */
	.thumb_func
_startup_copy_BKUPRAM_data:
	ldr	r0,=__BKUPRAM_data_start
	ldr	r1,=__BKUPRAM_data_words
	ldr	r2,=__BKUPRAM_data_load
	cmp     r1,#0
	beq     1f
0:	ldr	r4,[r2]
	str	r4,[r0]
        adds    r2,#4
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_copy_BKUPRAM_data, . - _startup_copy_BKUPRAM_data

	/* Clear .bss */
	.thumb_func
_startup_clear_bss:
	ldr	r0,=__bss_start
	ldr	r1,=__bss_words
	movs	r2,#0
	cmp     r1,#0
	beq     1f
0:	str	r2,[r0]
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_clear_bss, . - _startup_clear_bss

	/* Clear .BKUPRAM_bss */
	.thumb_func
_startup_clear_BKUPRAM_bss:
	ldr	r0,=__BKUPRAM_bss_start
	ldr	r1,=__BKUPRAM_bss_words
	movs	r2,#0
	cmp     r1,#0
	beq     1f
0:	str	r2,[r0]
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_clear_BKUPRAM_bss, . - _startup_clear_BKUPRAM_bss

        /**************/
        /* Enable FPU */
        /**************/

        ldr      r0, =0xE000ED88
        ldr      r1,[r0]
        orr      r1,r1,#(0xF << 20)
        str      r1,[r0]

        dsb
        isb


	bl	main

	bl	_exit

hang:   b .

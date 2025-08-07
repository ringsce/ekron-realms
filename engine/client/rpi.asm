.section .data
.align 8

bias:           .quad 0  // static unsigned long bias
histogramPtr:   .quad 0  // static unsigned long *histogram
start:          .quad 0  // static unsigned long start
range:          .quad 0  // static unsigned long range

x86_loopindex_i: .quad 0
biastable:      .fill 100, 8, 0  // unsigned long biastable[100]

.global _x86_TimerStart
.global _x86_TimerStop
.global _x86_TimerStopBias
.global _x86_TimerInit
.global _x86_TimerGetHistogram

.section .text
.align 4

// void x86_TimerStart()
_x86_TimerStart:
    mrs x0, cntvct_el0  // Read the counter value
    str x0, [x29, #start]
    ret

// void x86_TimerStop()
_x86_TimerStop:
    stp x0, x1, [sp, #-16]! // Push registers
    ldr x0, histogramPtr
    mrs x1, cntvct_el0
    ldr x2, start
    sub x1, x1, x2
    ldr x3, bias
    sub x1, x1, x3
    cmp x1, #0
    blt discard
    ldr x4, range
    cmp x1, x4
    bge discard
    add x0, x0, x1, lsl #3  // 64-bit pointers (change from lsl #2 to lsl #3)
    ldr x5, [x0]
    add x5, x5, #1
    str x5, [x0]
discard:
    ldp x0, x1, [sp], #16 // Restore registers
    ret

// void x86_TimerStopBias()
_x86_TimerStopBias:
    mrs x0, cntvct_el0
    ldr x1, start
    sub x0, x0, x1
    ret

// void x86_TimerInit( unsigned long smallest, unsigned length )
_x86_TimerInit:
    stp x29, x30, [sp, #-16]!  // Push frame pointer and link register
    mov x29, sp
    stp x19, x20, [sp, #-16]!  // Save registers

    mov x20, x1  // range = length
    mov x19, x0  // smallest

    mov x0, #10000
    str x0, bias
    str x20, range

    mov x3, #0  // Loop index
x86_Loop1:
    mrs x0, cntvct_el0
    str x0, start
    mrs x0, cntvct_el0
    ldr x1, start
    sub x0, x0, x1
    str x0, biastable, x3, lsl #3

    ldr x2, bias
    cmp x2, x0
    ble x86_minore
    str x0, bias

x86_minore:
    add x3, x3, #1
    cmp x3, #100
    blt x86_Loop1

    ldr x1, bias
    add x1, x1, x19
    str x1, bias

    // Call Z_Malloc(range * 8) for 64-bit
    mov x0, x20
    lsl x0, x0, #3  // Multiply by 8 instead of 4 for 64-bit pointers
    bl _Z_Malloc
    str x0, histogramPtr

    ldp x19, x20, [sp], #16  // Restore registers
    ldp x29, x30, [sp], #16  // Restore frame pointer and link register
    ret

// unsigned long *x86_TimerGetHistogram()
_x86_TimerGetHistogram:
    ldr x0, histogramPtr
    ret
.section .data
.align 8

bias:           .quad 0  // static unsigned long bias
histogramPtr:   .quad 0  // static unsigned long *histogram
start:          .quad 0  // static unsigned long start
range:          .quad 0  // static unsigned long range

x86_loopindex_i: .quad 0
biastable:      .fill 100, 8, 0  // unsigned long biastable[100]

.global _x86_TimerStart
.global _x86_TimerStop
.global _x86_TimerStopBias
.global _x86_TimerInit
.global _x86_TimerGetHistogram

.section .text
.align 4

// void x86_TimerStart()
_x86_TimerStart:
    mrs x0, cntvct_el0  // Read the counter value
    str x0, [x29, #start]
    ret

// void x86_TimerStop()
_x86_TimerStop:
    stp x0, x1, [sp, #-16]! // Push registers
    ldr x0, histogramPtr
    mrs x1, cntvct_el0
    ldr x2, start
    sub x1, x1, x2
    ldr x3, bias
    sub x1, x1, x3
    cmp x1, #0
    blt discard
    ldr x4, range
    cmp x1, x4
    bge discard
    add x0, x0, x1, lsl #3  // 64-bit pointers (change from lsl #2 to lsl #3)
    ldr x5, [x0]
    add x5, x5, #1
    str x5, [x0]
discard:
    ldp x0, x1, [sp], #16 // Restore registers
    ret

// void x86_TimerStopBias()
_x86_TimerStopBias:
    mrs x0, cntvct_el0
    ldr x1, start
    sub x0, x0, x1
    ret

// void x86_TimerInit( unsigned long smallest, unsigned length )
_x86_TimerInit:
    stp x29, x30, [sp, #-16]!  // Push frame pointer and link register
    mov x29, sp
    stp x19, x20, [sp, #-16]!  // Save registers

    mov x20, x1  // range = length
    mov x19, x0  // smallest

    mov x0, #10000
    str x0, bias
    str x20, range

    mov x3, #0  // Loop index
x86_Loop1:
    mrs x0, cntvct_el0
    str x0, start
    mrs x0, cntvct_el0
    ldr x1, start
    sub x0, x0, x1
    str x0, biastable, x3, lsl #3

    ldr x2, bias
    cmp x2, x0
    ble x86_minore
    str x0, bias

x86_minore:
    add x3, x3, #1
    cmp x3, #100
    blt x86_Loop1

    ldr x1, bias
    add x1, x1, x19
    str x1, bias

    // Call Z_Malloc(range * 8) for 64-bit
    mov x0, x20
    lsl x0, x0, #3  // Multiply by 8 instead of 4 for 64-bit pointers
    bl _Z_Malloc
    str x0, histogramPtr

    ldp x19, x20, [sp], #16  // Restore registers
    ldp x29, x30, [sp], #16  // Restore frame pointer and link register
    ret

// unsigned long *x86_TimerGetHistogram()
_x86_TimerGetHistogram:
    ldr x0, histogramPtr
    ret

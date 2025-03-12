%include '../functions.asm'

section .data
    matrix dd 4, 1, 1, 1, 1, 1, 1, 1, 1
    result_msg_total db "Sum of elements: ", 0
    result_msg_diag db "Sum of diagonal: ", 0

section .text
    global _start

_start:
    xor eax, eax
    xor ebx, ebx
    xor ecx, ecx

    mov esi, matrix
    xor edi, edi

matrix_loop:
    mov edx, [esi + edi*4]

    add eax, edx

    test edi, 3
    jnz next_element
    add ebx, edx

next_element:
    cmp edi, 8
    je store_sums
    inc edi
    jmp matrix_loop

store_sums:
    push eax
    mov eax, result_msg_total
    call sprint

    pop eax
    call iprintLF

    push ebx
    mov eax, result_msg_diag
    call sprint

    pop eax
    call iprintLF

    call quit

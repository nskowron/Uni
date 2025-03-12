 %include '../functions.asm'

section .data
    result_msg_total db "Sum of numbers: ", 0

section .text
    global  _start

_start:
 
    pop ecx
    cmp ecx, 2
    jne quit

    pop eax
    pop eax

    xor ebx, ebx


sum:
    movzx ecx, byte [eax]
    cmp ecx, 0
    je done

    sub ecx, '0'
    add ebx, ecx

    inc eax
    jmp sum


done:
    push ebx
    mov eax, result_msg_total
    call sprint
    
    pop eax
    call iprintLF

    call quit
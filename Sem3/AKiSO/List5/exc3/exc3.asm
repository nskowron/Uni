%include '../functions.asm'

section .data
    number dd 255
    number_hex db "00000000", 0
    result_msg_total db "Hexadecimal: ", 0

section .text
    global _start

_start:
    mov eax, result_msg_total
    call sprint

    mov ebx, 16
    mov eax, [number]
    lea ecx, [number_hex + 8]

getDigit:
    dec ecx
    xor edx, edx
    div ebx
    cmp dl, 9
    jg handleLetters
    add dl, '0'

saveDigit:
    mov byte [ecx], dl

    cmp eax, 0
    jz print
    jmp getDigit

handleLetters:
    sub dl, 10
    add dl, 'A'
    jmp saveDigit

print:
    mov eax, number_hex
    call sprintLF

    call quit
    
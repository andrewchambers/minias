.text
.globl main
main:
  jmp z
  # x
  nop
  # y 
  nop
  # z
  xorl %eax, %eax
  ret

.set z, y+1
.set x, main+2
.set y, x+1

.section .note.GNU-stack,"",@progbits

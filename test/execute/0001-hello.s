.data
.balign 1
.Lstring.2:
  .ascii "hello\000"

.text
.globl main
main:
  pushq %rbp
  movq %rsp, %rbp
  leaq .Lstring.2(%rip), %rdi
  callq puts
  xorl %eax, %eax
  leave
  ret

.section .note.GNU-stack,"",@progbits

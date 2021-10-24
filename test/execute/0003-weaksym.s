.data
.weak foo
foo:
  .quad 0

.text
.globl main
main:
  movq foo(%rip), %rax
  ret

.section .note.GNU-stack,"",@progbits

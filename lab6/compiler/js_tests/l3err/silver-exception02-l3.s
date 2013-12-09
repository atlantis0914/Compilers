.globl _c0_main
_c0_main:
  subq $8, %rsp
  movl $100, %ecx
  movl $1, %eax
  movl %eax, %edx
  negl %edx
  movl %ecx, %eax
  cmpl $32, %edx
  jae error
  movb %dl, %cl
  sarl %cl, %eax
  addq $8, %rsp
  ret
error:
  movw $1, %ax
  movw $0, %bx
  divw %bx
mem_error:
  jmp 0


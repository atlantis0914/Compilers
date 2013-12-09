.globl _c0_main
_c0_main:
  subq $8, %rsp

_c0_mainlabel0:
  movl $1, %eax
  testb %al, %al
  jnz _c0_mainlabel1
  jmp _c0_mainlabel2

_c0_mainlabel1:
  movl $411, %ecx
  jmp _c0_mainlabel0

_c0_mainlabel2:
  movl $1, %eax
  addl %eax, %ecx
  movl %ecx, %eax
  addq $8, %rsp
  ret
error:
  movw $1, %ax
  movw $0, %bx
  divw %bx
mem_error:
  jmp 0


.globl _c0_main
_c0_main:
  subq $8, %rsp
  movl $2147483648, %eax
  movl %eax, %edi
  movl %eax, %ecx
  movl $1, %eax
  movl %eax, %r8d
  negl %r8d
  movl %ecx, %eax
  movl %eax, %eax
  cltd
  idivl %r8d
  movl %edx,%eax
  addq $8, %rsp
  ret
.globl _c0_leches
_c0_leches:
  subq $8, %rsp
  movl %edi, %eax
  movl %eax, %ecx
  movl $1, %eax
  movl %eax, %r8d
  negl %r8d
  movl %ecx, %eax
  movl %eax, %eax
  cltd
  idivl %r8d
  movl %edx,%eax
  addq $8, %rsp
  ret
error:
  movw $1, %ax
  movw $0, %bx
  divw %bx
mem_error:
  jmp 0


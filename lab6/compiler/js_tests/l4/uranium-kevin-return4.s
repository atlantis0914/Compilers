.globl _c0_nikolaiissohotohmygodsohotplshelp
_c0_nikolaiissohotohmygodsohotplshelp:
  subq $8, %rsp
  movl $4, %ecx
  movl $1, %eax
  movl %ecx, %edi
  movl %eax, %esi
  movq %rcx, -8(%rsp)
  subq $16, %rsp
  call calloc
  movq %rax, %r15
  addq $8, %rsp
  popq %rcx
  movq %r15, %rax
  addq $8, %rsp
  ret
.globl _c0_main
_c0_main:
  subq $8, %rsp
  movl $4, %ecx
  movl $1, %eax
  movl %ecx, %edi
  movl %eax, %esi
  movq %rcx, -8(%rsp)
  subq $16, %rsp
  call calloc
  movq %rax, %r15
  addq $8, %rsp
  popq %rcx
  movq %r15, %rax
  call _c0_nikolaiissohotohmygodsohotplshelp
  movq %rax, %r15
  movq %r15, %rax
  movl 0(%rax), %eax
  addq $8, %rsp
  ret
error:
  movw $1, %ax
  movw $0, %bx
  divw %bx
mem_error:
  jmp 0


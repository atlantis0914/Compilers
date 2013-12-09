.globl _c0_main
_c0_main:
  subq $8, %rsp
  movl $500, %ecx
  movl %ecx, %r15d
  cmpl $0, %r15d
  setl %r15b
  testb %r15b, %r15b
  jnz mem_error
  movl %ecx, %r15d
  imull $8, %r15d
  cmpl $0, %r15d
  setl %r15b
  testb %r15b, %r15b
  jnz mem_error
  movl %ecx, %eax
  addl $1, %eax
  movl $8, %edx
  movl %edx, %edi
  movl %eax, %esi
  movq %rdx, -8(%rsp)
  movq %rcx, -16(%rsp)
  subq $16, %rsp
  call calloc
  movq %rax, %r15
  popq %rcx
  popq %rdx
  movq %r15, %r8
  movl %ecx, 0(%r8)
  movl $0, %edx

_c0_mainlabel0:
  movl %edx, %ecx
  movl $500, %eax
  cmpl %eax, %ecx
  setl %cl
  testb %cl, %cl
  jnz _c0_mainlabel1
  jmp _c0_mainlabel2

_c0_mainlabel1:
  movq %r8, %rcx
  movl %edx, %r9d
  movl %r9d, %r15d
  cmpl 0(%rcx), %r15d
  setae %r15b
  testb %r15b, %r15b
  jnz mem_error
  imull $8, %r9d
  movl %r9d, %edi
  movq 8(%rcx,%rdi,1), %rcx
  movl $500, %r9d
  movl %r9d, %r15d
  cmpl $0, %r15d
  setl %r15b
  testb %r15b, %r15b
  jnz mem_error
  movl %r9d, %r15d
  imull $8, %r15d
  cmpl $0, %r15d
  setl %r15b
  testb %r15b, %r15b
  jnz mem_error
  movl %r9d, %eax
  addl $1, %eax
  movl $8, %r10d
  movl %r10d, %edi
  movl %eax, %esi
  movq %rdx, -8(%rsp)
  movq %rcx, -16(%rsp)
  movq %r8, -24(%rsp)
  movq %r9, -32(%rsp)
  movq %r10, -40(%rsp)
  subq $48, %rsp
  call calloc
  movq %rax, %r15
  addq $8, %rsp
  popq %r10
  popq %r9
  popq %r8
  popq %rcx
  popq %rdx
  movq %r15, %rax
  movl %r9d, 0(%rax)
  movq %rax, 0(%rcx)
  movq %r8, %rcx
  movl %edx, %r9d
  movl %r9d, %r15d
  cmpl 0(%rcx), %r15d
  setae %r15b
  testb %r15b, %r15b
  jnz mem_error
  imull $8, %r9d
  movl %r9d, %edi
  movq 8(%rcx,%rdi,1), %rax
  movq %r8, %rcx
  movq %rcx, 0(%rax)
  movl %edx, %ecx
  movl $1, %eax
  movl %ecx, %edx
  addl %eax, %edx
  jmp _c0_mainlabel0

_c0_mainlabel2:
  movl $0, %eax
  addq $8, %rsp
  ret
error:
  movw $1, %ax
  movw $0, %bx
  divw %bx
mem_error:
  jmp 0


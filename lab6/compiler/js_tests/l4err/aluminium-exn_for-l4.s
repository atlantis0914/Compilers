.globl _c0_main
_c0_main:
  pushq %rbx
  pushq %r12
  subq $8, %rsp
  movl $1234, %r12d
  movl $4, %edx
  movl %edx, %r15d
  cmpl $0, %r15d
  setl %r15b
  testb %r15b, %r15b
  jnz mem_error
  movl %edx, %r15d
  imull $4, %r15d
  cmpl $0, %r15d
  setl %r15b
  testb %r15b, %r15b
  jnz mem_error
  movl %edx, %eax
  addl $2, %eax
  movl $4, %r8d
  movl %r8d, %edi
  movl %eax, %esi
  movq %rdx, -8(%rsp)
  movq %rcx, -16(%rsp)
  movq %r8, -24(%rsp)
  subq $32, %rsp
  call calloc
  movq %rax, %r15
  addq $8, %rsp
  popq %r8
  popq %rcx
  popq %rdx
  movq %r15, %r9
  movl %edx, 0(%r9)
  movl $0, %r8d

_c0_mainlabel0:
  movl %r12d, %edx
  movl $0, %eax
  cmpl %eax, %edx
  setg %dl
  testb %dl, %dl
  jnz _c0_mainlabel1
  jmp _c0_mainlabel2

_c0_mainlabel1:
  movl %r8d, %r11d
  movl %r11d, %r15d
  cmpl 0(%r9), %r15d
  setae %r15b
  testb %r15b, %r15b
  jnz mem_error
  movl %r12d, %eax
  movl $10, %ebx
  movl %eax, %r10d
  cltd
  idivl %ebx
  movl %edx,%r10d
  imull $4, %r11d
  movl %r11d, %edi
  movl %r10d, 8(%r9,%rdi,1)
  movl $10, %r10d
  movl %r12d, %eax
  cltd
  idivl %r10d
  movl %eax,%r12d
  movl %r8d, %edx
  movl $1, %eax
  movl %edx, %r8d
  addl %eax, %r8d
  jmp _c0_mainlabel0

_c0_mainlabel2:
  movl $0, %ecx
  movl $0, %r8d

_c0_mainlabel3:
  movl %r8d, %edx
  movl $4, %eax
  cmpl %eax, %edx
  setl %dl
  testb %dl, %dl
  jnz _c0_mainlabel4
  jmp _c0_mainlabel5

_c0_mainlabel4:
  movl $10, %eax
  movl %ecx, %edx
  imull %eax, %edx
  movq %r9, %rcx
  movl %r8d, %r10d
  movl %r10d, %r15d
  cmpl 0(%rcx), %r15d
  setae %r15b
  testb %r15b, %r15b
  jnz mem_error
  imull $4, %r10d
  movl %r10d, %edi
  movl 8(%rcx,%rdi,1), %eax
  movl %edx, %ecx
  addl %eax, %ecx
  movl %r8d, %edx
  movl $1, %eax
  movl %edx, %r8d
  subl %eax, %r8d
  jmp _c0_mainlabel3

_c0_mainlabel5:
  movl %ecx, %eax
  addq $8, %rsp
  popq %r12
  popq %rbx
  ret
error:
  movw $1, %ax
  movw $0, %bx
  divw %bx
mem_error:
  jmp 0


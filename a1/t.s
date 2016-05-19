.globl _scheme_entry 
_scheme_entry: 
movq $5, %rax 
movq $6, %rbx 
movq $0, %rcx 
addq %rax, %rcx 
addq %rbx, %rcx 
movq $1, %rdx 
imulq %rax, %rdx 
imulq %rbx, %rdx 
movq %rdx, %rax 
ret 

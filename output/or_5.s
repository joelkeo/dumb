section .text
extern ?error
extern ?input
extern ?print
extern ?print_stack
extern ?equal
extern ?try_gc
extern ?naive_print_heap
extern ?HEAP
extern ?HEAP_END
extern ?set_stack_bottom
global ?our_code_starts_here
  jmp near ?our_code_starts_hereend
?our_code_starts_here:
  push RBP
  mov RBP, RSP
  mov R11, [RBP+16]
  mov R12, 0
  mov R13, 0
  mov R14, 0
  mov RBX, 0
  mov R8, 0
  mov R9, 0
  mov R10, RDI
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, RBP
  call ?set_stack_bottom
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
  mov RDI, R10
  ;; heap start
  mov QWORD R15, RDI ; Load heap_reg with our argument, the heap pointer
  add QWORD R15, 15 ; Align it to the nearest multiple of 16
  mov R11, 0xfffffffffffffff0
  and QWORD R15, R11 ; by adding no more than 15 to it
  mov R13, RSI
  shl R13, 3
  mov RAX, 0xffffffffffffffff
  test RAX, 0x1
  mov R11, RAX
  jz near ?err_logic_not_bool
  mov RAX, 0xffffffffffffffff
  test RAX, 0x1
  mov R11, RAX
  jz near ?err_if_not_bool
  cmp RAX, 0xffffffffffffffff
  je near thencase#1
  mov RAX, 4
  test RAX, 0x1
  mov R11, RAX
  jnz near ?err_arith_not_num
  mov RAX, 0x7fffffffffffffff
  test RAX, 0x1
  mov R11, RAX
  jnz near ?err_arith_not_num
  mov RAX, 4
  mov R11, 0x7fffffffffffffff
  mov RAX, 4
  mov R11, 0x7fffffffffffffff
  sar RAX, 1
  sar R11, 1
  imul RAX, R11
  jo near ?err_overflow
  imul RAX, 2
  jo near ?err_overflow
  test RAX, 0x1
  mov R11, RAX
  jz near ?err_logic_not_bool
  jmp near end#1
thencase#1:
  mov RAX, 0xffffffffffffffff
end#1:
  mov RSP, RBP
  pop RBP
  ret
?our_code_starts_hereend:
  mov R10, 4
  mov [R15+0], R10
  lea R10, [rel ?our_code_starts_here]
  mov [R15+8], R10
  mov R10, 0
  mov [R15+16], R10
  mov RAX, R15
  add RAX, 5
  add R15, 32
?err_comp_not_num:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 1
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_arith_not_num:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 2
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_logic_not_bool:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 3
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_if_not_bool:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 4
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_overflow:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 5
  mov QWORD RSI, RAX
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_get_not_tuple:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 6
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_get_low_index:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 7
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_get_high_index:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 8
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_nil_deref:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 9
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_out_of_memory:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 10
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_set_not_tuple:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 11
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_set_low_index:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 12
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_set_high_index:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 13
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_call_not_closure:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 14
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
?err_call_arity_err:
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, 15
  mov QWORD RSI, R11
  call ?error
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12


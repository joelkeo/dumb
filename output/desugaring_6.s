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
  push QWORD 0
  push QWORD 0
  push QWORD 0
  push QWORD 0
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
  lea RAX, [rel ?HEAP_END] ; Reserving 4 words
  sub RAX, 32
  cmp RAX, R15
  jge near $memcheck_18
  push R12
  push R13
  push R14
  push RBX
  push R8
  push R9
  mov QWORD RDI, QWORD R15
  mov QWORD RSI, QWORD 32
  mov QWORD RDX, QWORD RBP
  mov QWORD RCX, QWORD RSP
  call ?try_gc
  pop R9
  pop R8
  pop RBX
  pop R14
  pop R13
  pop R12
  mov R15, RAX ; assume gc success if returning here, so RAX holds the new heap_reg value
$memcheck_18:
  mov R11, 4
  mov [R15+0], R11
  mov R11, 0
  mov [R15+8], R11
  mov R11, 200
  mov [R15+16], R11
  mov R11, 0
  mov [R15+24], R11
  mov RAX, R15
  add RAX, 0x1
  mov R10, 32
  add R15, R10
  mov [RBP-8], RAX
  mov RAX, [RBP-8]
  and RAX, 0x7
  cmp RAX, 0x1
  jne near ?err_get_not_tuple
  mov RAX, 0
  test RAX, 0x1
  mov R11, RAX
  jnz near ?err_arith_not_num
  mov RAX, [RBP-8]
  sub RAX, 1
  mov R11, 0
  sar R11, 1
  ;; IDX OOB START ---------------------
  mov R10, RAX
  mov RAX, R11
  mov R11, R10
  mov R10, [R11+0]
  sar R10, 1
  imul RAX, 2
  imul R10, 2
  cmp RAX, R10
  jge near ?err_get_high_index
  cmp RAX, 0
  jl near ?err_get_low_index
  sar RAX, 1
  mov R10, RAX
  mov RAX, R11
  mov R11, R10
  ;; IDX OOB END ^^^^^^^^^^^^^^^^^^^^^^
  mov RAX, [RAX + R11 * 8 + 8]
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
  test RAX, 0x1
  mov R11, RAX
  jnz near ?err_arith_not_num
  mov RAX, 2
  test RAX, 0x1
  mov R11, RAX
  jnz near ?err_arith_not_num
  mov RAX, [RBP-16]
  mov R11, 2
  add RAX, R11
  jo near ?err_overflow
  mov [RBP-24], RAX
  mov RAX, [RBP-8]
  and RAX, 0x7
  cmp RAX, 0x1
  jne near ?err_set_not_tuple
  mov RAX, 0
  test RAX, 0x1
  mov R11, RAX
  jnz near ?err_arith_not_num
  mov RAX, [RBP-8]
  sub RAX, 1
  mov R11, 0
  sar R11, 1
  ;; IDX OOB START ---------------------
  mov R10, RAX
  mov RAX, R11
  mov R11, R10
  mov R10, [R11+0]
  sar R10, 1
  imul RAX, 2
  imul R10, 2
  cmp RAX, R10
  jge near ?err_set_high_index
  cmp RAX, 0
  jl near ?err_set_low_index
  sar RAX, 1
  mov R10, RAX
  mov RAX, R11
  mov R11, R10
  ;; IDX OOB END ^^^^^^^^^^^^^^^^^^^^^^
  mov R10, [RBP-24]
  mov [RAX + R11 * 8 + 8], R10
  mov RAX, [RBP-8]
  and RAX, 0x7
  cmp RAX, 0x1
  jne near ?err_get_not_tuple
  mov RAX, 0
  test RAX, 0x1
  mov R11, RAX
  jnz near ?err_arith_not_num
  mov RAX, [RBP-8]
  sub RAX, 1
  mov R11, 0
  sar R11, 1
  ;; IDX OOB START ---------------------
  mov R10, RAX
  mov RAX, R11
  mov R11, R10
  mov R10, [R11+0]
  sar R10, 1
  imul RAX, 2
  imul R10, 2
  cmp RAX, R10
  jge near ?err_get_high_index
  cmp RAX, 0
  jl near ?err_get_low_index
  sar RAX, 1
  mov R10, RAX
  mov RAX, R11
  mov R11, R10
  ;; IDX OOB END ^^^^^^^^^^^^^^^^^^^^^^
  mov RAX, [RAX + R11 * 8 + 8]
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


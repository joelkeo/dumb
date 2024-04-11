#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "gc.h"

typedef uint64_t SNAKEVAL;

void printHelp(FILE* out, SNAKEVAL val);
extern uint64_t NUM_TAG_MASK;
extern uint64_t CLOSURE_TAG_MASK;
extern uint64_t TUPLE_TAG_MASK;
extern uint64_t CLOSURE_TAG;
extern uint64_t TUPLE_TAG;
extern uint64_t NIL;
extern uint64_t tupleCounter;
extern uint64_t* STACK_BOTTOM;
extern uint64_t* FROM_S;
extern uint64_t* FROM_E;
extern uint64_t* TO_S;
extern uint64_t* TO_E;

/*
Forward-Pointer/Metadata Block:

WORD 1: Pointer to location in new heap [tag]
WORD 2: Pointer to next Forward-Pointer/Metadata block 

The [tag] is so that, as we walk accross this linked list, we can determine the type of the nth
element allocated in the to-heap. Also indicates that this is a fwd pointer rather than a 
SNAKEVAL size/arity
*/

void naive_print_heap(uint64_t* heap, uint64_t* heap_end) {
  printf("In naive_print_heap from %p to %p\n", heap, heap_end);
  for(uint64_t i = 0; i < (uint64_t)(heap_end - heap); i += 1) {
    printf("  %llu/%p: %p (%llu)\n", i, (heap + i), (uint64_t*)(*(heap + i)), *(heap + i));
  }
}

void smarter_print_heap(uint64_t* from_start, uint64_t* from_end, uint64_t* to_start, uint64_t* to_end) {
  // Print out the entire heap (both semispaces), and
  // try to print values readably when possible
}

uint64_t isOdd(uint64_t val) {
  if (val == 0) {
    return 0;
  }
  else if (val % 2 == 1) {
    return 1;
  }
  else {
    return 0;
  }
}

uint64_t is_tuple(uint64_t* htw) {
  uint64_t htw_val = *htw;
  uint64_t tag = htw_val & TUPLE_TAG_MASK;
  if (htw_val != 1 && tag == TUPLE_TAG) {
    return 1;
  }
  else {
    return 0;
  }
}

uint64_t is_closure(uint64_t* htw) {
  uint64_t htw_val = *htw;
  uint64_t tag = htw_val & CLOSURE_TAG_MASK;
  if (tag == CLOSURE_TAG) {
    return 1;
  }
  else {
    return 0;
  }
}

// Returns the actual HTW, treated as a pointer to the heap
uint64_t* untag_tuple(uint64_t* htw) {
  return (uint64_t*)(*htw - TUPLE_TAG);
}
uint64_t* untag_closure(uint64_t* htw) {
  return (uint64_t*)(*htw - CLOSURE_TAG);
}

// Only runs if we know its some type of heap thing
// Returns true if the HTW pointed to by val is a forwarding pointer
// Side effect: also updates the HTW so it points to new heap
uint64_t is_forwarding_pointer(uint64_t* htw) {
  uint64_t* actual_address_to_heap;
  if (is_tuple(htw)) {
    actual_address_to_heap = untag_tuple(htw);
  }
  else if (is_closure(htw)) {
    actual_address_to_heap = untag_closure(htw);
  }
  // If SNAKEVAL number??
  if (! isOdd(*actual_address_to_heap)) {
    return 0;
  }
  else {
    // forwarding pointers are pre_tagged, so this is okay
    *htw = *actual_address_to_heap;
    return 1;
  }
}

// heap_top will increment and return
uint64_t* copy(uint64_t* garter_val_addr, uint64_t* heap_top, uint64_t ***prev_to_link) {
  uint64_t new_untagged_htw = (uint64_t) heap_top;
  if (is_tuple(garter_val_addr)) {
    uint64_t* addr_to_old = untag_tuple(garter_val_addr);
    *garter_val_addr = (new_untagged_htw + TUPLE_TAG);
    // size
    uint64_t size = *addr_to_old;
    // Set heap top to its size, hold off on addr to old inc
    *heap_top = *addr_to_old;
    // FWD PTR STUFF BEGIN -------------------------------------
    *addr_to_old = new_untagged_htw + TUPLE_TAG;
    **prev_to_link = addr_to_old;
    *prev_to_link = (uint64_t**) addr_to_old + 1; // linked list
    // FWD PTR STUFF END ---------------------------------------
    // increment pointers after the fact
    heap_top++;
    addr_to_old++;
    for (uint64_t i = 0; i < (size / 2); i++) {
      *heap_top++ = *addr_to_old++;
    }
    // account for padding
    if (! isOdd(size / 2)) {
      *heap_top++ = 0;
    }
    return heap_top;
  }
  else if (is_closure(garter_val_addr)) {
    uint64_t* addr_to_old = untag_closure(garter_val_addr);
    *garter_val_addr = (new_untagged_htw + CLOSURE_TAG);
    // ARITY && INSTRUCTION POINTER
    uint64_t arity = *addr_to_old;
     // FWD PTR STUFF BEGIN -------------------------------------
    *addr_to_old = new_untagged_htw + CLOSURE_TAG;
    **prev_to_link = addr_to_old;
    *prev_to_link = (uint64_t**) addr_to_old + 1; // linked list
    // FWD PTR STUFF END ---------------------------------------
    addr_to_old++;
    *heap_top++ = arity;
    *heap_top++ = *addr_to_old++;
    uint64_t size = *addr_to_old;
    // Size
    *heap_top++ = *addr_to_old++;
    for (uint64_t i = 0; i < (size / 2); i++) {
      *heap_top++ = *addr_to_old++;
    }
    // account for padding
    if (! isOdd(size / 2)) {
      *heap_top++ = 0;
    }
    return heap_top;
  }
}


/*
  Copies a Garter value from the given address to the new heap, 
  but only if the value is heap-allocated and needs copying.

  Arguments:
    garter_val_addr: the *address* of some Garter value, which contains a Garter value,
                     i.e. a tagged word.  
                     It may or may not be a pointer to a heap-allocated value...
    heap_top: the location at which to begin copying, if any copying is needed

  Return value:
    The new top of the heap, at which to continue allocations

  Side effects:
    If the data needed to be copied, then this replaces the value at its old location 
    with a forwarding pointer to its new location
 */
uint64_t* copy_if_needed(uint64_t* garter_val_addr, uint64_t* heap_top, uint64_t*** prev_to_link) {
  // no-op for now

  // FIRST guarantees its a HTW, THEN sees if its a forwarding pointer (update and return) or if we actually need to copy
  // TODO: add check for is not forwarding pointer
  if ((is_tuple(garter_val_addr) || is_closure(garter_val_addr)) && ! (is_forwarding_pointer(garter_val_addr))) {
    return copy(garter_val_addr, heap_top, prev_to_link);
  }
  else {
    return heap_top;
  }
}

uint64_t* bfs_investigate(uint64_t** object_start_byref, uint64_t* metadata_block, uint64_t* to_start, uint64_t*** prev_to_link_byref) {
  uint64_t* object_start = *object_start_byref;
  if (is_tuple(metadata_block)) {
    uint64_t size = *object_start++;
    for (uint64_t i = 0; i < (size / 2); i++) {
      to_start = copy_if_needed(object_start++, to_start, prev_to_link_byref);
    }
    if (! isOdd(size / 2)) {
    object_start++;
    }
  }
  else if (is_closure(metadata_block)) {
    object_start += 2;
    uint64_t size = *object_start++;
    for (uint64_t i = 0; i < (size / 2); i++) {
      to_start = copy_if_needed(object_start++, to_start, prev_to_link_byref);
    }
    if (! isOdd(size / 2)) {
    object_start++;
    }
  }
  *object_start_byref = object_start;
  return to_start;
}

/*
  Implements Cheney's garbage collection algorithm.

  Arguments:
    bottom_frame: the base pointer of our_code_starts_here, i.e. the bottommost Garter frame
    top_frame: the base pointer of the topmost Garter stack frame
    top_stack: the current stack pointer of the topmost Garter stack frame
    from_start and from_end: bookend the from-space of memory that is being compacted
    to_start: the beginning of the to-space of memory

  Returns:
    The new location within to_start at which to allocate new data
 */
uint64_t* gc(uint64_t* bottom_frame, uint64_t* top_frame, uint64_t* top_stack, uint64_t* from_start, uint64_t* from_end, uint64_t* to_start) {
  // naive_print_heap(from_start, from_end);
  // STEP 1: Iterate over stack and place everything in the new heap
  uint64_t* old_top_frame = top_frame;
  uint64_t* first_forwarding_ptr;
  uint64_t** address_to_previous_forwarding_ptr = &first_forwarding_ptr;
  uint64_t*** prev_to_link_byref = &address_to_previous_forwarding_ptr;
  uint64_t* bfs_i = to_start;
  do {
    for (uint64_t* cur_word = top_stack/* maybe need a +1 here? */; cur_word < top_frame; cur_word++) {
      // printf("ADDR IN THE STACK: %llx\n", (uint64_t) cur_word);
      // printf("VAL: %llx\n", *cur_word);
      to_start = copy_if_needed(cur_word, to_start, prev_to_link_byref);
    }
    /* Shift to next stack frame:
     * [top_frame] points to the saved RBP, which is the RBP of the next stack frame,
     * [top_frame + 8] is the return address, and
     * [top_frame + 16] is therefore the next frame's stack-top
     */
    top_stack = top_frame + 2;   // TODO HUH????
    old_top_frame = top_frame;
    // Sets top_frame to the stored RBP value at old top frame
    top_frame = (uint64_t*)(*top_frame);
  } while (old_top_frame < bottom_frame); // Use the old stack frame to decide if there's more GC'ing to do
  // STEP 2: Iterate over the to heap 
  // printf("ORIGINAL FWD PTR: %llx \n", (uint64_t) first_forwarding_ptr);
  uint64_t* fwd_ptr_i = first_forwarding_ptr;
  while(bfs_i < to_start) {
    to_start = bfs_investigate(&bfs_i, fwd_ptr_i, to_start, prev_to_link_byref);
    fwd_ptr_i = (uint64_t*)*(fwd_ptr_i + 1);
  }

  // after copying and GC'ing all the stack frames, return the new allocation starting point (R15)
  return to_start;       
}

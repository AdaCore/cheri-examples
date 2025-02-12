/**
 *  Copyright (C) 2020-2025, AdaCore
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 **/

#include <alloc.h>
#include <sys/mman.h>
#include <stdio.h>

#define HEAP_SIZE 8589934592 // 8MB

extern _Bool __builtin_cheri_tag_get(void *);
extern size_t __builtin_cheri_round_representable_length(size_t);
extern void *__builtin_cheri_offset_set(void *, size_t);
extern void *__builtin_cheri_bounds_set(void *, size_t);

void *__heap __attribute__((aligned(16))) = 0x0;
static size_t __offset = 0x0;

#ifdef DEBUG
#include <stdint.h>
extern void *__builtin_cheri_program_counter_get(void);
extern _Bool __builtin_cheri_subset_test(void *, void *);

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
#endif

// This is a very simple implementation of an allocator. Its purpose is to
// reflect the general workings of typical allocators used by libc without
// introducing their complexities.
// This allocator does not support deallocation. It will request a fixed sized
// pool from the kernel using mmap and then allocate from this pool. The
// allocation scheme is that of a simple stack, hence it does not support
// deallocation.
void *alloc(size_t size)
{
    // Round up the size to a value representable in a CHERI capability
    size_t adjusted_size = __builtin_cheri_round_representable_length(size);
#ifdef DEBUG
    printf("%s stack at %p\n", __func__, &adjusted_size);
#endif

    if (__offset + adjusted_size > HEAP_SIZE) {
        printf("Heap exhausted\n");
        return 0;
    }

    // If __heap is not initialized yet, allocate HEAP_SIZE memory from the kernel
    if (!__heap) {

        // This code looks for the GOT of the alloc library. It is the same as in
        // malicious_lib/src/malicious_lib.c. It is only used for debugging.
#ifdef DEBUG
        __heap = (void *)0x42;

        intptr_t *got = 0;

        intptr_t pcc = (intptr_t)__builtin_cheri_program_counter_get();

        unsigned instruction;
        do {
            pcc = pcc - 4;
            instruction = *(unsigned *)pcc;
        } while((instruction & 0x9f800000) != 0x90800000);

        got = (intptr_t *)decode_adrp(pcc, instruction);

        pcc = pcc + 4;
        instruction = *(unsigned *)pcc;

        got = (intptr_t *)((intptr_t)got | decode_ldr(instruction));

        if(((void *)*got) == &__heap) {
            printf("found heap got at %p\n", got);
        }else{
            printf("failed to find heap got at %p\n", got);
        }

        printf("Initializing heap at %p ...\n", &__heap);
#endif
        __heap = mmap(0, HEAP_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        if (__heap == (void *) -1) {
            __heap = 0x0;
            perror("mmap failed\n");
            return 0;
        }
        printf("Heap initialized at %p (%p)\n", __heap, &__heap);
    }

    if (!__builtin_cheri_tag_get(__heap)) {
        printf("Invalid heap capability: %p\n", __heap);
        return 0;
    }

    // Create a new capability with its offset set to the first unallocated space
    void *new_alloc = __builtin_cheri_offset_set(__heap, __offset);

#ifdef DEBUG
    printf("New allocation at %p, size: %zu, adjusted size: %zu\n", new_alloc, size, adjusted_size);
#endif

    // Restrict the bounds of the newly created capability to the requested size
    new_alloc = __builtin_cheri_bounds_set(new_alloc, adjusted_size);
    if (!__builtin_cheri_tag_get(new_alloc)){
        printf("Failed to create new heap allocation at %p\n", new_alloc);
        return 0;
    }
    __offset = __offset + adjusted_size;

#ifdef DEBUG
    printf("subset check: %i\n", __builtin_cheri_subset_test(new_alloc, __heap));
#endif
    return new_alloc;
}

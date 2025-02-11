/**
 *  Copyright (C) 2020-2025, AdaCore
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 **/

#include <sys/mman.h>
#include <stdio.h>

#define HEAP_SIZE 8589934592 // 8MB

extern _Bool __builtin_cheri_tag_get(void *);
extern size_t __builtin_cheri_round_representable_length(size_t);
extern void *__builtin_cheri_offset_set(void *, size_t);
extern void *__builtin_cheri_bounds_set(void *, size_t);
extern _Bool __builtin_cheri_subset_test(void *, void *);

static void *__heap __attribute__((aligned(16))) = 0x0;
static size_t __offset = 0x0;

void *alloc(size_t size)
{
    size_t adjusted_size = __builtin_cheri_round_representable_length(size);
    if (__offset + adjusted_size > HEAP_SIZE) {
        printf("Heap exhausted\n");
        return 0;
    }
    if (!__heap) {
        printf("Initializing heap at %p ...\n", &__heap);
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
    void *new_alloc = __builtin_cheri_offset_set(__heap, __offset);
    printf("New allocation at %p, size: %zu, adjusted size: %zu\n", new_alloc, size, adjusted_size);
    new_alloc = __builtin_cheri_bounds_set(new_alloc, adjusted_size);
    if (!__builtin_cheri_tag_get(new_alloc)){
        printf("Failed to create new heap allocation at %p\n", new_alloc);
        return 0;
    }
    __offset = __offset + adjusted_size;
    printf("subset check: %i\n", __builtin_cheri_subset_test(new_alloc, __heap));
    return new_alloc;
}

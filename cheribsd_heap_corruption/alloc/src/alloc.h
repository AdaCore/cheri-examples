/**
 *  Copyright (C) 2020-2025, AdaCore
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 **/

#ifndef __ALLOC_H__
#define __ALLOC_H__

#include <stddef.h>

// Return a capability of a given size allocated from an
// internal buffer. If not enough memory can be allocated
// return 0.
void *alloc(size_t);

#endif

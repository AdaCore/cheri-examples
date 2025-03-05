/**
 *  Copyright (C) 2020-2025, AdaCore
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 **/

#include <alloc.h>
#include <stdio.h>
#include <string.h>
#include <malicious_lib.h>

int main() {
    char *s = alloc(24);
    memset(s, 'a', 24);
    s[23] = '\0';
    printf("%s\n", s);
    malicious_lib();
    printf("%s\n", s);
    return 0;
}

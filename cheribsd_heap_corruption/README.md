# CheriBSD's Process Address Space Properties

A process on CheriBSD, as on many operating systems, is running in its own address space.
That means it cannot access other processes memory and is confined to its own protection domain.
Within this address space the general behavior of CHERI's capabilities is largely the same as in
our [bare metal heap corruption example](../heap_corruption/README.md).

## Shared Libraries

One significant difference to bare metal is BSD's support of shared libraries. These libraries are
linked at runtime and therefore references to them cannot be fully resolved at link time. This is true
for both CHERI and non-CHERI systems. In both cases the global offset table (GOT) is used to resolve
function calls at runtime.

The important difference to the bare-metal target is that each shared library has its own GOT. That means
that a global object of a shared library cannot be found in the applications GOT, unless that object is
referenced by the application itself. If the object is private to the shared library an access through
the GOT is not possible.

It is important to note here that this is not a security feature. There is no inherent guarantee that the GOT
of a shared library cannot be accessed by other shared libraries. The accessibility of parts of other libraries
depends on the code and internal workings of these libraries. While this behavior makes an attack more
difficult we cannot rule out that it is possible.

## The Stack

By default the whole process uses a single stack. That means a function call into a shared library will use
the same space and capability for its stack as the caller. After the call the stack pointer will be decreased
again. However nothing prevents the caller from accessing the stack space behind the stack pointer. If the
called function placed any capabilities to private objects on the stack and did not actively overwrite them
before returning these are also accessible by the caller.

## Accessing Other Libraries Heap Allocations

When building this demo we started out with the code from our [bare metal heap corruption example](../heap_corruption/README.md).
However since CheriBSD uses `jemalloc` as its memory allocator simply finding the correct capability to another
allocation was much more complex. To avoid these complexities we created our own simple allocator. It requests
statically sized section of memory from the kernel via `mmap` and then will fulfil allocation requests from this
memory. It is reduced to the properties relevant for our research, such as crossing the boundaries of shared
libraries. In particular it is correctly setting the bounds and permissions on the capabilites returned to the
caller. Therefore it also does not support deallocation and is not thread safe.

In a monolithic application without shared libraries the original proof of concept code worked mostly unaltered.
To provide a more realistic and flexible scenario both the allocator and the malicious code were moved into
separate shared libraries. This prevents a direct access to the heap capability through the GOT since each of
the libraries has their own GOT.

As explained above the stack is shared between shared libraries if they call into each other. This allows the
malicious library to access stack objects of the main function. It is able to find a matching capability and
modify the state of the main function without having direct access through e.g. function parameters.

## Impact of CheriBSD's Compartmentalization

CheriBSD implements a compartmentalization that is supposed to prevent these kinds of accesses and isolates
shared libraries from each other. Enabling this feature successfully prevents our malicious library from
accessing the stack of other parts of the program. From the stack addresses we were able to validate that
different stack capabilities are used for each compartment.

Additionally we could verify that we could not cross compartment boundaries by preparing a signal handler
with a location and then unwind into our code by triggering a protection fault in the other library.
We also searched the GOT and the accessible stack space for any capability that could possibly allow us to
extend the libraries access beyond its own compartment. However we were not able to find such a capability.

## Proof of Concept

Our proof of concept consists of three components: the main program, a shared library with an allocator and
a malicious shared library. The main program allocates a small region of memory and initializes it with a
constant value. It will then call the malicious library without passing any arguments in to ensure that no
information is leaked into that library. After the library call it will check the data inside the allocation.

```C
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
```

When being executed without compartmentalization enabled the malicious library is able to successfully modify
the allocation:

```Shell
$ ./main
Heap initialized at 0x41400000 (0x4019de20)
aaaaaaaaaaaaaaaaaaaaaaa
found got at 0x4018b940 (2c177)
search_cap_space: mapping from 0xfffffff7fe80 to 0xfffffff80000, 1
Helloaaaaaaaaaaaaaaaaaa
```

Once compartmentalization is enabled this method stops working:

```Shell
$ proccontrol -m cheric18n -s enable ./main
Heap initialized at 0x42800000 (0x401a7e20)
aaaaaaaaaaaaaaaaaaaaaaa
found got at 0x4018c940 (2c175)
search_cap_space: mapping from 0x421fff50 to 0x42200000, 1
could not find matching capability on stack
aaaaaaaaaaaaaaaaaaaaaaa
```
The output shows that most addresses stay mostly the same when executing with compartmentalization. The one
visible difference is the stack space that is searched by `search_cap_space`. Without compartmentalization
it is at the top of the address space while with compartmentalization it is in the same range as the GOT and
the heap, indicating that it is a separately allocated block of memory.

The second important part of this proof of concept is the allocator. On operating systems like CheriBSD a
process needs to request memory from the kernel. This is often done with `mmap`. Since this allocation
involves a context switch it is expensive. To avoid this inefficiency the memory is handled by an allocator.
On CheriBSD this is `jemalloc` which implements `malloc` and `free`. It requests memory for multiple pools
from the kernel and then allows for efficient and thread-safe memory allocation by the application through
`malloc`.

This efficiency comes at the cost of complexity, complexity that is not needed for our proof of concept and
that would also impact the focus on the important parts of this demo. It is important to note that this does
not imply that `jemalloc` is more or less secure in that regard. An attacker with sufficient time and knowledge
might be able to reproduce these results within `jemalloc`.

To avoid the complexity we implement our own allocator library. It only has a single pool of memory from which
it allocates and does not support deallocation. The memory pool is stored in a static global variable, from
the linkers perspective only visible to the shared library itself.

```
#define HEAP_SIZE 8589934592 // 8MB

void *__heap __attribute__((aligned(16))) = 0x0;                 
static size_t __offset = 0x0;

    if (__offset + adjusted_size > HEAP_SIZE) {
        printf("Heap exhausted\n");
        return 0;
    }

    // If __heap is not initialized yet, allocate HEAP_SIZE memory from the kernel
    if (!__heap) {
        __heap = mmap(0, HEAP_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        if (__heap == (void *) -1) {
            __heap = 0x0;
            perror("mmap failed\n");
            return 0;
        }
        printf("Heap initialized at %p (%p)\n", __heap, &__heap);
    }

    // Create a new capability with its offset set to the first unallocated space
    void *new_alloc = __builtin_cheri_offset_set(__heap, __offset);

    // Restrict the bounds of the newly created capability to the requested size
    new_alloc = __builtin_cheri_bounds_set(new_alloc, adjusted_size);
    __offset = __offset + adjusted_size;

    return new_alloc;
}
```

The third part is the malicous library itself. It contains code to search a specific capability within another
one, allowing to search both the stack and the GOT for capabilities containing data from other shared libraries.
It is also the most complext part of the three, so the best way is to take a direct look at the [code](malicious_lib/src/malicious_lib.c).

## Conclusion

As in our bare metal example, CHERI provides a significant improvement in memory safety. This does not change
the addition of shared libraries. On the other hand shared libraries themselves do not offer any significant
benefit in terms of security. While they make manipulating the data of different parts of the program more
complicated they leave many holes that can still be used to access different parts of the process memory.

Compartmentalization effectively isolates libraries from each other. We were not able to access private resources
across compartment boundaries. However, it is important to note that CHERI without compartmentalization already
provides a very high level of security. All examples we have tested here assume an attacker with significant
control over the programs execution.

Assuming all code is trusted and an attacker only has access to an unprivileged interface, e.g. via a network,
it is very difficult if not impossible to gain enough control over the programs execution for compartmentalization
to make a difference. The most likely scenario is a logic bug in the application, but even that makes it
difficult to gain the level of execution freedom required for such an attack. We have evaluated such a scenario
in our tests involving [return oriented programming](../return_oriented_programming/README.md).

Another possible scenario is the execution of an untrusted library. This would give an attacker all possible freedoms
to analyse the system within the restrictions set by CHERI capabilities. While an attacker may still access system
resources, e.g. by opening files, compartmentalization can significantly reduce the attack surface of an application
in that context. If combined with other security measures such as jails.

The worst case would be a successful attack on CHERI itself. If the basic guarantees provided by capabilities can be
broken and capabilities can be forged or extended neither compartmentalization nor the library architecture itself
will provide any protection. In this case the system will still be protected by the operating system but possible
memory safety bugs will be exploitable.

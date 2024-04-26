# CHERI's Single Address Space Properties

CHERI provides improved security and safety for applications by providing fine grained memory safety mechanisms.
If used properly these make it hard if not impossible to bring an application into an undesired state by an attacker with access to the running system.
Bugs that otherwise would cause an attacker to gain information or access to the application can be caught and lead to either the termination of the program or a recovery mechanism.
Even if an attacker gained some control it is still very hard to further manipulate the data or control flow.
An example for this is how CHERI behaves in the context of [return oriented programming](../return_oriented_programming/README.md).

However CHERI does not replace but augment existing security mechanisms such as processes.
A CHERI application exists as a single address space and a single security domain.
This is important to note when third party code is used as a library.
While CHERI can prevent many forms of memory corruption or unintended memory access it provides only very limited protection against an attacker with control over source code.

This example showcases why a CHERI application must be treated as a single security domain.
It shows how the underlying capability management architecture, that is used to provide every part of the application with the capabilities it requires, can be used to circumvent some of the restrictions imposed by CHERI's capabilities.
While both the legit and "malicious" code are in a single application the same problem exists for libraries as long as everything is contained in the same address space.
The application consists of a heap allocation and a call to the malicious library.
The malicious library does not get a capability to the allocated memory by the caller but gains access to the same memory by the same mechanism the allocator uses.

## The Global Offset Table

The important part of this demo is the so called Global Offset Table (GOT).
Due to the way CHERI is implemented on morello it contains capabilities to all statically allocated objects.
This typically includes all objects defined outside the context of a function or class.
Depending on the used programming language this can also include objects that are private on a language level but that are treated as static objects by the compiler.

When building an application the compiler will insert symbols in all locations that reference global objects, such as a global variable in C or a package level variable in Ada.
On non-CHERI platforms when the application is linked the linker will resolve these references to specific memory addresses.
When an access to such an object happens the code will simply dereference the address provided by the linker.
Depending on the target there may be differences on how the address is calculated but especially on bare metal targets it is known at link time.

On a CHERI platform in pure capability mode this mechanism stops working.
CHERI requires all pointers to be a valid capability before being referenced or jumped to.
A valid capability must always be derived from another valid capability and cannot extend beyond the bounds and permissions of the parent.
At startup the CHERI platform provides a root capability which has access to the whole address space with all permission set.
All other capabilities must be derived from that root capability at runtime which is usually done by the startup code.
This property implies that the linker cannot simply replace symbols with addresses as these would not be valid capabilities.

When executing code the startup code creates some specific capabilities that are required to run any code.
The stack pointer and program counter for example are also replaced by the stack pointer capability (SPC) and the program counter capability (PCC).
Objects on the stack are handled by the SPC but the PCC should not include all statically allocated objects.
Instead it contains the GOT which is located after the code section.
This allows all code to access the GOT from where it can then retrieve further capabilities.
The GOT itself is filled at runtime by the startup code.

## Accessing Other Function's Heap Allocations

The architecture described in the previous section shows that a CHERI application is still a single security domain.
Malicious code can access the GOT the same way as any legitimate code.
This also implies that any code has access to the whole heap.
While the heap allocator only returns capabilities limited to the requested allocation it itself accesses the whole heap through the GOT.
Nothing prevents other code from accessing the same capability through the GOT and manipulating heap memory.

To the programmer the access to global objects through the GOT is transparent.
The compiler takes care of creating that indirection and emitting the proper code.
Furthermore, in the absence of a direct capability, the GOT is usually addressed by using the PCC and deriving the GOT address through program counter relative addressing.
This means that the compiler only needs to know the offset between the accessing code and the GOT and that the proper capability can be calculated at runtime from that offset.
This also makes it difficult to obtain arbitrary access to the GOT.
The compiler only generates these accesses if such an object is accessed in the code and for that it needs to be visible in the context.

To access objects invisible or private on the language level our example uses a few tricks.
These tricks require some general knowledge about the internal workings of the heap allocator but no specific data points such as specific symbols or memory addresses.
For a particular heap allocator implementation the example is portable for different applications and does not depend on a specific memory layout as long as the GOT is addressed in the same way by the compiler.
While the heap allocator used in this example comes from the Ada runtime the malicious code itself is written in C to showcase that no Ada-specific information is used.
Additionally it only uses compiler intrinsics and type definition includes.
The only exception is a dependency on `malloc`.
We assume that `malloc` is available, otherwise manipulating the heap doesn't make any sense as there likely is no heap to manipulate.

The general idea is to do a legitimate access to a global object through the GOT and then analyse the compiler gnerated code at runtime to calculate the GOT's location in memory.
After the location of the GOT has been determined a call to `malloc` is made to get any capability derived from the allocators heap capability.
With this capability we can traverse the GOT and check which of the avaliable capabilities is a superset of the one returned by `malloc`.
Once a capability matching these criteria is found it can be used to access the whole heap.
The only knowledge specific to the allocator in use is that the heap capability is not stored in the GOT directly but rather that a capability to another global variable which holds the heap capability is used.
This is just an additional indirection but does not further affect the result.

## Proof of Concept

To validate these assumptions we created a proof-of-concept. The following simple Ada application is used:

```Ada
with Ada.Text_IO;

procedure Heap_Corruption
is
   type String_Access is access String;
   procedure Malicious_Lib with
      Import,
      Convention => C,
      External_Name => "malicious_lib";
   Data : constant String_Access := new String'("Hello");
begin
   Ada.Text_IO.Put_Line ("Data: " & Data.all);
   Malicious_Lib;
   Ada.Text_IO.Put_Line ("Data: " & Data.all);
end Heap_Corruption;
```

It allocates a new string on the heap by using the `new` keyword.
The memory of the string can only be accessed through either the returned capability or through access to the heap capability.
As `Data` is local and not passed into `Malicious_Lib` it should not change after a call to `Malicious_Lib`.
Yet when the program is executed in the Morello FVP we get the following result:

```
Info: Morello_Top: Morello_Top.soc.scp_qspi_loader: FlashLoader: Loaded 87 kB from file '/.../scp_fw.bin'

Info: Morello_Top: Morello_Top.soc.mcp_qspi_loader: FlashLoader: Loaded 21 kB from file '/.../mcp_fw.bin'
Data: Hello
Data: World

Info: /OSCI/SystemC: Simulation stopped by user.
```

It is visible that the contents of `Data` have changed after the call to `Malicious_Lib`.
The access itself is implemented in [`malicious_lib.c`](src/malicious_lib.c).
The implementation works as described in the previous section and each part of the code contains a comment explaining what it does in detail.

## Conclusion

While CHERI does provide a significant improvement in memory safety it does not change the security properties of a monolithic application.
It still depends on all the code, including libraries, being trusted.
While gaining access to different parts of the program is not always trivial there is no mechanism that is able to validate whether a given part of the program is allowed to have access to other parts of the same program.

There are different mechanisms to create different security domains, most notably processes provided by operating systems.
These allow for a strict separation of different applications and are not replaced by the features provided by CHERI.
Processes are already used to separate untrusted libraries from accessing the programs they're used from although by adding additional overhead for inter-process-communication and context switching.

There are also approaches to use CHERI as a lightweight replacement for processes.
While these avoid the context switching in the operating system they don't come free either.
To properly separate parts of the application they build compartments whose boundaries can only be traversed by using a so called switcher that has a higher level of "privilege" than the rest of the application.
These approaches overlap with security concepts used by operating systems but provide different tradeoffs between separation and overhead in context switching.

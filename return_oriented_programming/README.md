# Impact on return oriented programming

Return-oriented programming (ROP) is an exploit technique for circumventing memory access restrictions such as non-executable data sections. Conventional exploits usually encapsulate payloads to be executed on a successful exploit. The exploit payloads are often placed outside the typical code sections and later introduced into the execution, e.g., on the stack or heap. Triggering a payload can be achieved through control flow manipulation, such as unauthorized changes to a jump or return instructions to call and execute the payload code. This access control is usually achieved by exploiting a memory bug that allows writing into the program's control flow metadata, particularly the stack frame. Mitigation of these conventional-style exploits is achieved through the management of non-executable data sections, particularly where attempts to execute the payload within a non-executable data section would lead to a CPU exception.

Return-oriented programming avoids that by not providing a custom payload. Instead, it attempts to reuse fragments of existing application or library code that is already accessible. To do that, it manipulates the return address of the current stack frame to jump to a position inside an accessible function that contains the desired code. A commonly used example is the `system` function in `glibc.` Exploitation amounts to placing a String somewhere in memory, setting the first argument in the system call as a pointer to the string, and executing `system`. This operation does not require placing code in non-executable sections of the address space, as it only uses legitimately executable code instrumented for malicious intent. While the name return-oriented programming stems from the jump being usually a manipulated return address (as this is accessible in the stack frame), alternative approaches exist, e.g., manipulating function pointers.

The approach with `system` is just one example; any existing code satisfying the attacker's intentions can be used. Additionally, many library functions require a particular setup of registers and stack to be called with the correct arguments, which may require a more complex chain of jumps to get the preconditions into place. While this sounds cumbersome and complicated, there are [compilers](https://css.csail.mit.edu/6.858/2015/projects/je25365-ve25411.pdf) that ease the creation of these chains. In addition, each generation of sophisticated memory protections and new tools to detect and avoid exploits results in new and improved exploit techniques, making ROP a continuing realistic and current threat.

## Can CHERI prevent return oriented programming?

The impact of CHERI on return-oriented programming is affected by two aspects: the ability to gain entry to a program and the ability to exploit the program further once entry is gained.

### Gaining entry

Return-oriented programming is usually started by manipulating the stack frame to make it return to a desired location. The typical exploits are still memory bugs, particularly out-of-bounds accesses on stack objects. With CHERI, this is made significantly more complicated. In purecap mode, if the capabilities are sufficiently fine-grained, it is possible to completely prevent a function's variables from accessing the function's own stack frame. Sufficient utilization of CHERI should make access via a buffer overflow virtually impossible.

Alternatively, it can be possible to modify a function pointer unintendedly to alter the program flow. This would require a direct connection between user input and the function pointer calculation, which is then used to jump to a different function or a different place in the same function. The range in which the modified function pointer stays valid depends on the bounds of the original capability.

### Continuing exploitation

Assuming a first jump or return was manipulated successfully, can CHERI prevent further exploitation? CHERI applies the capability system to all references, including return addresses. Assuming an attacker can replace the return address, it still needs to be a valid capability for CHERI to accept this address. Instead of overriding the return address with some input data, an attacker needs to trick the program with a valid capability pointing to the attacker's desired jump target. However, CHERI employs an additional security measure. Capabilities that point to code are sealed and cannot be dereferenced or changed. This restricts the capabilities usable by return-oriented programming to those available during the attack, making it likely impossible to jump to a target that is not a function's entry.

## Conclusion

The following examples demonstrate the security benefits of CHERI in more detail. Four attack properties have been analyzed: two simple checks of memory properties and two examples of ROP code.

# Examples

The following examples demonstrate the security benefits of CHERI in more detail. Four attack properties have been analyzed: two simple checks of memory properties and two examples of ROP code.

All the example code was written for Arm Morello. While it is, in theory, Aarch64 compatible, there are ABI differences. We have tested all of these programs on the ZynqMP, and they all work (with different limitations). The examples are adapted to Arm Morello and won't run on ZynqMP anymore.

## Testing memory properties

### [main_exec_stack](src/main_exec_stack.adb)

This test checks if the stack is executable. It overlays a procedure over an integer that contains a `ret` instruction. This forces the PC to point to the stack and execute an instruction there. On Morello, this fails with a capability permission fault as the stack capability does not allow executing memory. The according ESR value is `0x8600002B`. This code only runs on Morello as the return instruction has a different value than on Aarch64.

### [main_write_code](src/main_write_code.adb)

This test checks if it is possible to modify the code section. It imports a function as an integer and tries to set a value for that integer. On Morello, this action causes a capability-sealed fault. This happens because capabilities pointing to functions are sealed, meaning they can't be dereferenced. The ESR value in this case is `0x96000069`.

## Testing ROP

These programs contain a unit called `Secret` that includes a procedure `Print_Secret`, which prints a secret number when the correct password is provided. The exploits make the program jump to the location after the password check, thereby printing the secret number without knowing the correct password.

### [main_stack](src/main_stack.adb)

This program replicates the conventional memory corruption method to perform return-oriented programming. It contains a flawed copy implementation that does not check the input size properly and causes a stack overflow, stating that the input is too big. The input `12345678AAAAAAAA\xd8\x0d\x00\x80` manipulates the return address of `Flawed_Copy`'s caller (`Valid_Input`). `12345678` is the size of the buffer; this is the part that fills the allocated part of the buffer, `AAAAAAAA` is used as padding up until the return address is reached. Any value would be acceptable; however, `AAAAAAAA` is easily identifiable when looking at the memory in the debugger. The remaining part of the input is the code address that prints the secret number. In the successful case, `Valid_Input` will return to this location instead of the area where it came from. This code works on Aarch64 if the exploit sets the return address properly. On Morello, this causes a capability-bound fault when trying to overflow the stack. The ESR value is `0x9600006A`.

### [main_fp](src/main_fp.adb)

This test is a very optimistic (from an attacker's point of view) implementation of a security flaw that allows a user to input a value that directly affects a function pointer. While it is unlikely that we will come across this technique, this scenario will test what may happen if someone can control a function call on CHERI for whatever reason. While this example makes many assumptions, it has some interesting properties. One is that it works on Ada even with runtime checks enabled. Not only does the code not crash, but it also prints the desired information (meaning it executes the desired code). On Morello, this code fails with a capability tag fault, the ESR value being `0x86000028`, even on the first and valid function call. The reason is that code capabilities are sealed, and running any modifying instruction on them, even adding zero, will invalidate them.

### Using the Program Counter Capability

One (if not the only one) option on Morello to get a valid capability that points to code and is modifiable is the program counter capability. It can be retrieved by a specific instruction and used to create a user-controlled function pointer that could theoretically point to any code location.

However, this requires an attacker to make the program call the code that executes this instruction and modify the desired address. Unless the attacked program already contains code that can do this (which is unlikely to be in user code), this method is probably not usable.

We are yet to try this approach. However, the relevant code in the program (not the exploit) would look like this:

```Ada
declare
   Proc_Addr : System.Address := Interfaces.CHERI.Capability_With_Address (Interfaces.CHERI.Get_PCC, 16#1234_5678#);

   procedure Proc with Import, Convention => C, Address => Proc_Addr;
begin
   Proc;
end;
```

## Conclusion

CHERI, particularly Morello, comes with a much more vigorous defense against ROP than initially assumed. This conclusion is based not only on the bounds checking but also on the fine-grained approach of CHERI capabilities and the proper implementation of the principle of least privilege. The generated capabilities restrict the data and execution flow to the paths defined by the programmer and the compiler, making it hard or impossible to deviate from these paths, even if one of these measures fails.

The execution flow is usually controlled by either the linker's jumps or those generated at runtime. The first kind cannot be modified due to the restricted access to the code. The latter, while in theory modifiable, are still subject to the restrictions applied to all code capabilities. Even if an attacker could modify a function pointer or return address, they must provide a valid capability. Any non-capability-aware modification would result in an exception. It must have the correct permissions even if they could supply a valid capability. The restriction that all code capabilities are sealed is also true for function pointers and return addresses.

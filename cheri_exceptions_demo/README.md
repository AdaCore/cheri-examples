# CHERI Exceptions Demo

This project demonstrates CHERI in the context of an aviation system.
The demo provides a (very crude) simulation of several aircraft subsystems.
Each subsystem is implemented as an Ada task that does some periodic processing.
Each task has a top-level exception handler to catch any exceptions that occur
during operation. The task is placed in a "compromised" state when an exception
occurs but can be reset upon request.

The radar subsystem has been designed to be intentionally vulnerable to a
CHERI bounds fault when it is given a data buffer that is too small.
This demo illustrates how the CHERI error can be caught as an Ada exception and
isolate the fault to the task that triggered it. Other tasks that have a data
dependency on the failed task's data can enter a degraded state where they
continue to operate, but with degraded functionality.

The demo also simulates the concept of "cyber resilience countermeasures" which
can be deployed to defend against an active attack and restore any compromised
tasks to their normal operation. This demo does not simulate any actual
countermeasures, but does reset any compromised tasks to restore them to
normal operation.

## Building the demo

Building this demo requires Alire and a GNAT Pro toolchain for morello-elf,
with both of them in your system PATH. Run this command to build the demo:

```sh
alr build
```

## Running the demo

The demo can be run using either PyCross:
```sh
run-cross2 --target=morello-elf,,qemu-morello bin/main
```

Or by using Morello QEMU directly:
```sh
qemu-system-morello -M morello -monitor none -nographic -semihosting -kernel bin/main
```

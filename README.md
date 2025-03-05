# CHERI Examples

This repository contains code examples and proof of concepts that showcase the security feature set of CHERI.

## CHERI Exceptions

The [`CHERI Exceptions Demo`](cheri_exceptions_demo/README.md) showcases the translation of CHERI exceptions into Ada exception and the handling of these.

## Return Oriented Programming

The [Return Oriented Programming Demo](return_oriented_programming/README.md) evaluates the impact of CHERI's security mechanisms on exploit techniques such as return oriented programming.

## Heap Corruption

The [Heap Corruption Demo](heap_corruption/README.md) dives deeper into the architecure of a CHERI application to show the limits of CHERI's protection mechanisms.

## CheriBSD Heap Corruption

The [CheriBSD Heap Corruption Demo](cheribsd_heap_corruption/README.md) applies the bare metal heap corruption to an application running on CheriBSD. It also evaluates
the effect of CheriBSD's compartmentalization feature on that demo.

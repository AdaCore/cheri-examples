--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Interfaces;

--  This program tests the W^X memory protection by executing code
--  on the stack. A single return instruction is placed as an integer
--  on the stack. It is then dereferenced as a procedure and subsequently
--  called. On CHERI it fails as writable capabilities cannot be executed.

procedure Main_Exec_Stack
is
   Ret : Interfaces.Unsigned_32 := 0;
   procedure Test_Ret with
      Import,
      Address => Ret'Address;
begin
   Ada.Text_IO.Put_Line ("Exec stack test");
   Ret := 16#c2c253c0#; -- aarch64 CHERI ret instruction
   Test_Ret;
   Ada.Text_IO.Put_Line ("Executed stack");
end Main_Exec_Stack;

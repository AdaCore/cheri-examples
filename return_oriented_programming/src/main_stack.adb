--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Rop_Test;
with Exploit;

--  This test simulates a typical buffer overflow used to gain entry for
--  return oriented programming exploits. It calls a test procedure
--  that contains a flawed copy operation with an input that triggers a
--  stack overflow.

procedure Main_Stack
is
begin
   Ada.Text_IO.Put_Line ("Stack overflow test");
   Rop_Test.Run (Exploit.Input);
end Main_Stack;

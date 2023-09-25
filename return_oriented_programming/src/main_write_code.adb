--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Secret;

--  This program tests the W^X memory protection by modifying executable code.
--  It dereferences the address of a function as an integer and subsequently
--  writes to that integer. On CHERI this test fails as code capabilities
--  need to be sealed to execute and sealed capabilities are not allowed to
--  be dereferenced.

procedure Main_Write_Code
is
   Secret_Proc : Integer with
      Import,
      Address => Secret.Print_Secret'Address;
begin
   Ada.Text_IO.Put_Line ("Write code test");
   Secret_Proc := 42;
   Ada.Text_IO.Put_Line ("Written");
end Main_Write_Code;

--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Func_Ptr;
pragma Warnings (Off, "not referenced");
with Secret;
pragma Warnings (On, "not referenced");

--  This test simulates a call via a manipulated function pointer.
--  Func_Ptr.Call will add the offset it is called with to an address
--  and execute a function call to this address. As any offset
--  can be passed it can call into arbitrary code. This test works
--  even with Ada runtime checks enabled but fails on CHERI because
--  function pointers are sealed capabilities that are invalidated
--  on modification.

procedure Main_FP
is
begin
   Ada.Text_IO.Put_Line ("Function pointer test");
   Func_Ptr.Call (0);   -- Call Func_Ptr.P1
   Func_Ptr.Call (112); -- Call Func_Ptr.P2
   Func_Ptr.Call (464); -- Call secret.adb:10
end Main_FP;

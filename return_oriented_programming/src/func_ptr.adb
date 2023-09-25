--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with System;
with System.Storage_Elements;

package body Func_Ptr
is

   procedure P1
   is
   begin
      Ada.Text_IO.Put_Line ("P1");
   end P1;

   procedure P2
   is
   begin
      Ada.Text_IO.Put_Line ("P2");
   end P2;

   procedure Call (Offset : Integer)
   is
      --  To call a procedure at an arbitrary address a procedure is imported
      --  with the Address aspect. The address is calculated using P1 as the
      --  base address and adding an offset to it.
      use type System.Address;
      use type System.Storage_Elements.Storage_Offset;
      Call_Addr : constant System.Address :=
         P1'Address + System.Storage_Elements.Storage_Offset (Off);
      procedure P with
         Import,
         Address => Call_Addr;
   begin
      P;
   end Call;

end Func_Ptr;

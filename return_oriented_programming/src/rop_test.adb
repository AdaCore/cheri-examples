--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Secret;

package body Rop_Test
is

   function Valid_Input (Input : String) return Boolean;

   procedure Run (Input : String)
   is
   begin
      if Valid_Input (Input) then
         Secret.Print_Secret (Input);
      else
         Ada.Text_IO.Put_Line ("Invalid input");
      end if;
   end Run;

   --  This copy procedure does not properly check the length. If given
   --  an input larger than 8 bytes it will simply overflow on the stack. Due
   --  to the stack layout this overflow can overwrite the return address of
   --  the caller which in this case is Valid_Input.
   procedure Flawed_Copy (Input : String)
   is
      Buffer : String (1 .. 8) := (others => Character'Last);
   begin
      Buffer (Buffer'First .. Input'Last) := Input (Input'First .. Input'Last);
      pragma Inspection_Point (Buffer);
   end Flawed_Copy;

   function Valid_Input (Input : String) return Boolean
   is
   begin
      Flawed_Copy (Input);
      return True;
   end Valid_Input;

end Rop_Test;

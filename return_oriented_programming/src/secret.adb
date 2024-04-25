--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;

package body Secret
is

   procedure Print_Secret (Password : String)
   is
   begin
      if Password = "supersecurepassword" then
         Ada.Text_IO.Put_Line ("The secret is 42");
      else
         Ada.Text_IO.Put_Line ("Invalid password");
      end if;
   end Print_Secret;

end Secret;

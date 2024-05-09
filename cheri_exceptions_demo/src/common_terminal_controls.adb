------------------------------------------------------------------------------
--                           GNAT Pro Morello                               --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Common_Terminal_Controls is

   --------------------
   -- Move_Cursor_To --
   --------------------

   procedure Move_Cursor_To (X, Y : Natural) is
      X_Str : constant String := X'Image;
      Y_Str : constant String := Y'Image;
   begin
      Home;

      if X > 0 then
         Put (ESC & "[" & X_Str (2 .. X_Str'Last) & "C");
      end if;

      if Y > 0 then
         Put (ESC & "[" & Y_Str (2 .. Y_Str'Last) & "B");
      end if;
   end Move_Cursor_To;

   ---------------------
   -- Set_Text_Colour --
   ---------------------

   procedure Set_Text_Colour (Colour : Text_Colour) is
   begin

      case Colour is
         when Default =>
            Put (ESC & "[0m");

         when Red =>
            Put (ESC & "[31m");

         when Green =>
            Put (ESC & "[32m");

         when Yellow =>
            Put (ESC & "[33m");

         when Blue =>
            Put (ESC & "[34m");

         when Magenta =>
            Put (ESC & "[35m");

         when Cyan =>
            Put (ESC & "[36m");

         when White =>
            Put (ESC & "[37m");

      end case;
   end Set_Text_Colour;

   -----------------
   -- Hide_Cursor --
   -----------------

   procedure Hide_Cursor is
   begin
      --  Switch off the cursor
      Put (ESC & "[?25l");
   end Hide_Cursor;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Put (ESC & "[2J");
   end Clear;

   ----------
   -- Home --
   ----------

   procedure Home is
   begin
      Put (ESC & "[H");
   end Home;

end Common_Terminal_Controls;

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

package Common_Terminal_Controls is

   procedure Move_Cursor_To (X, Y : Natural);
   --  Move the cursor to a position on the screen. (0, 0) is top left

   type Text_Colour is
     (Default, Green, Red, Yellow, Blue, Magenta, Cyan, White);
   
   procedure Set_Text_Colour (Colour : Text_Colour);
   --  Set the colour of the text

   procedure Hide_Cursor;
   --  Hide the blinking cursor
   
   procedure Clear;
   --  Clear the terminal

   procedure Home;
   --  Move the cursor to the home position (top left)
   
end Common_Terminal_Controls;

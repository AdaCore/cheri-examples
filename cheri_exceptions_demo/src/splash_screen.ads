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

with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;
with Ada.Real_Time;               use Ada.Real_Time;
with System;
with Common_Terminal_Controls; use Common_Terminal_Controls;

package Splash_Screen is

   procedure Display_Splash_Screen;
   --  Launch the splash screen

   procedure Display_Splash_Screen_2;
   --  Launch the QR splash screen

   procedure Display_Dynamic_Splash_Screen;
   --  Animate the screen

   procedure Key_Press;
   --  Notification that a key has been pressed

private

   Allowed_Idle_Timing_Event : Timing_Event;
   Allowed_Idle_Time    : constant Time_Span := Milliseconds (120_000);
   Animate_Time         : constant Time_Span := Milliseconds (2_000);
   Splash_Switch_Time   : constant Time_Span := Milliseconds (15_000);
   Time_To_Next_Animate          : Time := Clock + Animate_Time;
   Time_To_Switch_Splash_Screens : Time := Clock + Splash_Switch_Time;
   Colour_1 : Text_Colour := White;
   Colour_2 : Text_Colour := Yellow;
   Showing_Splash_Screen_1 : Boolean := True;

   protected Protected_Timer_Event with
     Priority => System.Interrupt_Priority'Last
   is
      procedure Idle_Timer_Fired (Event : in out Timing_Event);

   end Protected_Timer_Event;

end Splash_Screen;

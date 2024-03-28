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

with Ada.Real_Time;

package Display is

   procedure Splash_Screen;

   procedure Show_Main_Avioics_Screen;

   procedure Show_Flight_Recorder_Screen;

   procedure Increase_Refresh;

   procedure Decrease_Refresh;

   task Display_Task;

private

   Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   type Requested_Display_Type is
     (Main_Avioics_Screen, Flight_Recorder_Screen);

   protected Display_Control_Data is

      procedure Show_Main_Avioics_Screen;

      procedure Show_Flight_Recorder_Screen;

      function Get_Requested_Display return Requested_Display_Type;

      procedure Set_Current_Display (Display : Requested_Display_Type);

      function Get_Current_Display return Requested_Display_Type;

   private

      Requested_Display : Requested_Display_Type := Main_Avioics_Screen;
      Current_Display   : Requested_Display_Type := Main_Avioics_Screen;

   end Display_Control_Data;

end Display;

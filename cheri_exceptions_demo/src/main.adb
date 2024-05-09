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

with Ada.Text_IO;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Countermeasures_Subsystem;
with Fuel_Subsystem;
with Navigation_Subsystem;
with Radar_Subsystem;
with Stores_Subsystem;
with Targeting_Subsystem;
with Display; use Display;
with Splash_Screen;
with Monitored_Tasking; use Monitored_Tasking;

--  The environment task is responsible for handling input command key presses

procedure Main with
  Priority => System.Priority'First
is
   C : Character;

   Good_Buffer : constant Storage_Array (1 .. Radar_Subsystem.Raw_Data_Size) :=
     (others => 0);

   Bad_Buffer  : constant Storage_Array (1 .. 8) := (others => 0);

begin

   loop

      Ada.Text_IO.Get (C);
      Splash_Screen.Key_Press;

      if Display.Get_Current_Screen = Main_Splash_Screen then

         --  Reset the demo state when transitioning off the splash screen

         Countermeasures_Subsystem.Countermeasures_Control.Reset;
         Fuel_Subsystem.Fuel_Data.Reset;
         Navigation_Subsystem.Navigation_Data.Reset;
         Radar_Subsystem.Radar_Data.Reset;
         Stores_Subsystem.Stores_Data.Reset;
         Targeting_Subsystem.Targeting_Data.Reset;

         if Radar_Subsystem.Radar_Task_Control.Get_Status = Compromised then
            Radar_Subsystem.Radar_Task_Control.Reset;
         end if;

         Display.Show_Main_Avionics_Screen;
      else

         case C is
         when 'a' | 'A' =>
            Radar_Subsystem.Radar_Control.Process_Radar_Data (Good_Buffer);

         when 'c' | 'C' =>
            Radar_Subsystem.Radar_Control.Process_Radar_Data (Bad_Buffer);

         when 'l' | 'L' =>
            Stores_Subsystem.Stores_Control.Request_Launch_Light;

         when 'h' | 'H' =>
            Stores_Subsystem.Stores_Control.Request_Launch_Heavy;

         when 'd' | 'D' =>
            Countermeasures_Subsystem.Countermeasures_Control.Deploy;

         when 'o' | 'O' =>
            Display.Increase_Refresh;

         when 'p' | 'P' =>
            Display.Decrease_Refresh;

         when 'f' | 'F' =>
            Display.Show_Flight_Recorder_Screen;

         when 'm' | 'M' =>
            Display.Show_Main_Avionics_Screen;

         when 's' | 'S' =>
            Display.Show_Splash_Screen_Screen;

         when others =>
            null;
         end case;
      end if;
   end loop;
end Main;

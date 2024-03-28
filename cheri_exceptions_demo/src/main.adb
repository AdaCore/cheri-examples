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
with Display;
with Radar_Subsystem;
with Stores_Subsystem;

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
            Display.Show_Main_Avioics_Screen;

         when 'x' | 'X' =>
            exit;

         when others =>
            null;
      end case;
   end loop;
end Main;

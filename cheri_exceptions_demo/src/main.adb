--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
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

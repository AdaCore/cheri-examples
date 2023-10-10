--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Real_Time; use Ada.Real_Time;

package body Targeting_Subsystem is

   Update_Period : constant Time_Span := Milliseconds (500);

   protected body Targeting_Data is

      function Get_Targets return Target_List is (Targets (1 .. Count));

      procedure Set_Targets (List : Target_List) is
      begin
         Count := List'Length;
         Targets (1 .. Count) := List;
      end Set_Targets;

   end Targeting_Data;

   procedure Simulate_Targeting is
      use type Radar_Subsystem.Track_ID;

      Next_Time : Time := Clock;
   begin
      loop
         if Radar_Subsystem.Radar_Data.Is_Operational then
            Targeting_Task_Control.Set_Status (Monitored_Tasking.Normal);

            declare
               Radar_Tracks : constant Radar_Subsystem.Track_Array :=
                                Radar_Subsystem.Radar_Data.Tracks;

               Targets : Target_List (1 .. Max_Targets);
               Count   : Radar_Subsystem.Track_Count := 0;
            begin
               for Track of Radar_Tracks loop
                  if Track.ID mod 2 = 0 then
                     Count := Count + 1;
                     Targets (Count) := Track.ID;
                  end if;
               end loop;

               Targeting_Data.Set_Targets (Targets (1 .. Count));
            end;

         else
            Targeting_Task_Control.Set_Status (Monitored_Tasking.Degraded);
            Targeting_Data.Set_Targets (Empty_Target_List);
         end if;

         Next_Time := Next_Time + Update_Period;
         delay until Next_Time;
      end loop;
   end Simulate_Targeting;

end Targeting_Subsystem;

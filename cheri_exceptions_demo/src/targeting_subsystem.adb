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

with Ada.Real_Time; use Ada.Real_Time;

package body Targeting_Subsystem is

   Update_Period : constant Time_Span := Milliseconds (500);

   --------------------
   -- Targeting_Data --
   --------------------

   protected body Targeting_Data is

      -------------------------
      -- Get_Targets_Changed --
      -------------------------

      procedure Get_Targets_Changed (Changed : out Boolean) is
      begin
         if Targets_Changed then
            Changed := True;
            Targets_Changed := False;
         else
            Changed := False;
         end if;
      end Get_Targets_Changed;

      -----------------
      -- Get_Targets --
      -----------------

      function Get_Targets return Target_List is (Targets (1 .. Count));

      -----------------
      -- Set_Targets --
      -----------------

      procedure Set_Targets (List : Target_List) is
      begin
         Targets_Changed := Count /= List'Length;
         Count := List'Length;
         Targets (1 .. Count) := List;
      end Set_Targets;

   end Targeting_Data;

   ------------------------
   -- Simulate_Targeting --
   ------------------------

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

   ----------
   -- PBIT --
   ----------

   procedure PBIT is
      use Monitored_Tasking;
   begin
      Targeting_Task_Control.Set_PBIT (In_Progress_BIT_State);

      delay 7.0;

      Targeting_Task_Control.Set_PBIT (Pass_BIT_State);
   end PBIT;

   ----------
   -- CBIT --
   ----------

   procedure CBIT is
      use Monitored_Tasking;
   begin
      Targeting_Task_Control.Set_CBIT (Pass_BIT_State);
   end CBIT;

end Targeting_Subsystem;

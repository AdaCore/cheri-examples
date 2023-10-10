--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;

with Radar_Subsystem;
with Monitored_Tasking;

--  This subsystem implements a task to simulate basic radar targeting.
--  For the purposes of this demo, the list of targets is simply those radar
--  tracks with an even numbered radar track ID.

package Targeting_Subsystem is

   Max_Targets : constant Radar_Subsystem.Track_Count :=
                   Radar_Subsystem.Max_Tracks;

   type Target_List is
     array (Radar_Subsystem.Track_Index range <>)
     of Radar_Subsystem.Track_ID;

   Empty_Target_List : constant Target_List (1 .. 0) := (others => 0);

   protected Targeting_Data is

      function Get_Targets return Target_List;

      procedure Set_Targets (List : Target_List);

   private

      Targets : Target_List (1 .. Radar_Subsystem.Max_Tracks) := (others => 0);
      Count   : Radar_Subsystem.Track_Count := 0;

   end Targeting_Data;

   Targeting_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Targeting;

   Targeting_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Targeting'Access,
      Control   => Targeting_Task_Control'Access,
      Priority  => System.Priority'Last);

end Targeting_Subsystem;

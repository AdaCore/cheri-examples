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

      procedure Get_Targets_Changed (Changed : out Boolean);

      function Get_Targets return Target_List;

      procedure Set_Targets (List : Target_List);

   private

      Targets : Target_List (1 .. Radar_Subsystem.Max_Tracks) := (others => 0);
      Targets_Changed : Boolean := True;
      Count   : Radar_Subsystem.Track_Count := 0;

   end Targeting_Data;

   Targeting_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Targeting;

   procedure PBIT;

   procedure CBIT;

   Targeting_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Targeting'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func       => CBIT'Access,
      Control   => Targeting_Task_Control'Access,
      Priority  => System.Priority'Last);

end Targeting_Subsystem;

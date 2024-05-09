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
with Monitored_Tasking; use Monitored_Tasking;

--  This subsystem implements a task to simulate aircraft armaments.
--  For the purposes of this demo this involves simply tracking the status of
--  each armament and providing a mechanism to simulate the launch of different
--  armaments.

package Stores_Subsystem is

   type Status_Kind is (Ready, Launched, Unavailable);

   type Light_Index is range 1 .. 2;
   type Heavy_Index is range 1 .. 2;

   type Light_Status_Array is array (Light_Index) of Status_Kind;
   type Light_Status_Changed_Array is array (Light_Index) of Boolean;
   type Heavy_Status_Array is array (Heavy_Index) of Status_Kind;
   type Heavy_Status_Changed_Array is array (Heavy_Index) of Boolean;

   protected Stores_Data is

      procedure Get_Light_Status_Changed
        (Idx : Light_Index; Changed : out Boolean);
      --  Set Changed to True if the Light associated with the index has been
      --  updated

      function Get_Light_Status (Idx : Light_Index) return Status_Kind;
      --  Get the status of a light armament

      procedure Get_Heavy_Status_Changed
        (Idx : Heavy_Index; Changed : out Boolean);
      --  Set Changed to True if the Heavy associated with the index has been
      --  updated

      function Get_Heavy_Status (Idx : Heavy_Index) return Status_Kind;
      --  Get the status of a heavy armament

      procedure Launch_Light;
      --  Launch a light armament

      procedure Launch_Heavy;
      --  Launch a heavy armament

      procedure Targetting_System_State_Change (New_State : Task_Status_Kind);
      --  Notification that the targetting system state has changed. Stores
      --  and targetting are tightly coupled by design.

      procedure Reset;

   private

      Light_Status : Light_Status_Array         := (others => Ready);
      Light_Status_Changed : Light_Status_Changed_Array := (others => True);
      Heavy_Status : Heavy_Status_Array         := (others => Ready);
      Heavy_Status_Changed : Heavy_Status_Changed_Array := (others => True);
      Targetting_System_State : Task_Status_Kind := PBIT;

   end Stores_Data;

   protected Stores_Control is

      procedure Request_Launch_Light;
      procedure Request_Launch_Heavy;

      procedure Get_Requests
        (Light_Requested : out Boolean; Heavy_Requested : out Boolean);

   private

      Launch_Light : Boolean := False;
      Launch_Heavy : Boolean := False;

   end Stores_Control;

   Stores_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Stores;

   procedure PBIT;

   procedure CBIT;

   Stores_Task :
     Monitored_Tasking.Monitored_Task
       (Task_Body => Simulate_Stores'Access, PBIT_Func => PBIT'Access,
        CBIT_Func => CBIT'Access, Control => Stores_Task_Control'Access,
        Priority  => System.Priority'Last);

end Stores_Subsystem;

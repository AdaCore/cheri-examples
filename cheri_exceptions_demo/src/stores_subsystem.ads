--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;

with Monitored_Tasking;

--  This subsystem implements a task to simulate aircraft armaments.
--  For the purposes of this demo this involves simply tracking the status of
--  each armament and providing a mechanism to simulate the launch of different
--  armaments.

package Stores_Subsystem is

   type Status_Kind is (Ready, Launched, Unavailable);

   type Light_Index is range 1 .. 2;
   type Heavy_Index is range 1 .. 2;

   type Light_Status_Array is array (Light_Index) of Status_Kind;
   type Heavy_Status_Array is array (Heavy_Index) of Status_Kind;

   protected Stores_Data is

      function Get_Light_Status (Idx : Light_Index) return Status_Kind;
      --  Get the status of a light armament

      function Get_Heavy_Status (Idx : Heavy_Index) return Status_Kind;
      --  Get the status of a heavy armament

      procedure Launch_Light;
      --  Launch a light armament

      procedure Launch_Heavy;
      --  Launch a heavy armament

   private

      Light_Status : Light_Status_Array := (others => Ready);
      Heavy_Status : Heavy_Status_Array := (others => Ready);

   end Stores_Data;

   protected Stores_Control is

      procedure Request_Launch_Light;
      procedure Request_Launch_Heavy;

      procedure Get_Requests
        (Light_Requested : out Boolean;
         Heavy_Requested : out Boolean);

   private

      Launch_Light : Boolean := False;
      Launch_Heavy : Boolean := False;

   end Stores_Control;

   Stores_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Stores;

   procedure PBIT;

   procedure CBIT;

   Stores_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Stores'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Stores_Task_Control'Access,
      Priority  => System.Priority'Last);

end Stores_Subsystem;

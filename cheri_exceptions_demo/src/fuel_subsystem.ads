--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;

with Monitored_Tasking;

--  This subsystem implements a task to simulate a very simple fuel subsystem.
--  For the purposes of this demo this is a fuel quantity indication (FQI) and
--  which decrements based on a fixed fuel consumption rate and a time to
--  minimum fuel based on the current FQI and consumption rate.

package Fuel_Subsystem is

   type Kilograms is delta 0.001 range 0.0 .. 999_999.999 with
     Small => 0.001;

   type Kilograms_Per_Second is delta 0.001 range 0.0 .. 999_999.999 with
     Small => 0.001;

   protected Fuel_Data is

      function Fuel_Quantity_Available return Boolean;
      --  Query whether the fuel quantity data is available

      function Fuel_Quantity return Kilograms;
      --  Read the last calculated fuel quantity

      function Time_To_Minimum_Fuel return Duration;
      --  Read the estimated time remaining until minimum fuel is reached

      function Time_To_Minimum_Available return Boolean;
      --  Query whether time to minimum fuel information is available

      procedure Invalidate_Fuel_Quantity;
      --  Mark the fuel quantity data as invalid

      procedure Invalidate_Time_To_Minimum_Fuel;
      --  Mark the estimated time to minimum fuel data as invalid

      procedure Set_Fuel_Quantity (Value : Kilograms);
      --  Set the fuel quantity value and mark it as valid

      procedure Set_Time_To_Minimum_Fuel (Value : Duration);
      --  Set the time to minimum fuel value and mark it as valid

   private

      FQI_Value       : Kilograms := 2_400.0;
      FQI_Valid       : Boolean   := True;

      Time_To_Minimum_Fuel_Value : Duration  := 0.0;
      Time_To_Minimum_Fuel_Valid : Boolean   := False;

   end Fuel_Data;

   Fuel_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Fuel_Data;

   procedure PBIT;

   procedure CBIT;

   Fuel_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Fuel_Data'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Fuel_Task_Control'Access,
      Priority  => System.Priority'Last);

end Fuel_Subsystem;

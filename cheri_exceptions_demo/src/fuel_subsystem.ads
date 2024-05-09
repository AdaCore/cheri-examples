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

   Initial_FQI_Value : constant Kilograms := 2_400.0;

   protected Fuel_Data is

      function Fuel_Quantity_Available return Boolean;
      --  Query whether the fuel quantity data is available

      procedure Fuel_Quantity_Changed (Changed : out Boolean);
      --  Return true if the fuel level has changed since last read

      function Fuel_Quantity return Kilograms;
      --  Read the last calculated fuel quantity

      procedure Time_To_Minimum_Fuel_Changed (Changed : out Boolean);
      --  Return true if the bingo time has changed since last read

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

      procedure Reset;

   private

      FQI_Value   : Kilograms := Initial_FQI_Value;
      FQI_Valid   : Boolean   := True;
      FQI_Changed : Boolean   := True;

      Time_To_Minimum_Fuel_Value       : Duration := 0.0;
      Time_To_Minimum_Fuel_Valid       : Boolean  := False;
      Time_To_Minimum_Fuel_Has_Changed : Boolean  := True;

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

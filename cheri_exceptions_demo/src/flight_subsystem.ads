--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;
with Monitored_Tasking;

--  This subsystem implements a task to simulate some basic flight
--  instrumentation data.

package Flight_Subsystem is

   type Feet is new Natural;

   type Knots is new Natural;

   protected Flight_Data is

      function Get_Altitude return Feet;

      procedure Set_Altitude (Altitude : Feet);

      function Get_Airspeed return Knots;

      procedure Set_Airspeed (Airspeed : Knots);

   private

      Current_Altitude : Feet  := 0;
      Current_Airspeed : Knots := 0;

   end Flight_Data;

   Flight_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Flight_Data;

   procedure PBIT;

   procedure CBIT;

   Flight_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Flight_Data'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Flight_Task_Control'Access,
      Priority  => System.Priority'Last);

end Flight_Subsystem;
